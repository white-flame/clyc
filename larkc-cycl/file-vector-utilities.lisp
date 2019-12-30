#|
  Copyright (c) 2019 White Flame

  This file is part of Clyc
 
  Clyc is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
 
  Clyc is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.
 
  You should have received a copy of the GNU Affero General Public License
  along with Clyc.  If not, see <https://www.gnu.org/licenses/>.
 
This file derives from work covered by the following copyright
and permission notice:

  Copyright (c) 1995-2009 Cycorp Inc.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at
  
  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
|#





(in-package :clyc)



(defparameter *show-file-vector-reference-payload-in-print* nil
    "[Cyc] Rebind this to T in order to see the contents of the file-vector reference payloads.")

(defstruct (file-vector-reference (:conc-name "FVECTOR-REF-"))
  index
  payload)

(defconstant *cfasl-opcode-fvector-reference* 69)

;; TODO - who creates the lock?  the NIL route for this is missing-larkc around all the locks
(defparameter *file-vector-backed-map-read-lock* nil
    "[Cyc] A lock that may or may not be defined to gate access to the underlying data stream.")

(defun file-vector-reference-present? (ref)
  (or (file-vector-reference-present-pristine? ref)
      (file-vector-reference-present-mutated? ref)))

(defun file-vector-reference-present-mutated? (ref)
  "[Cyc] The file vector reference stands in for an object that is not the one that was swapped in."
  (or (fvector-ref-has-mutated-index-p ref)
      (fvector-ref-payload-in-memory-p ref)))

(defun fvector-ref-has-mutated-index-p (ref)
  (fvector-ref-mutated-index-p (fvector-ref-index ref)))

(defun fvector-ref-payload-in-memory-p (ref)
  (fvector-ref-payload ref))

(defun fvector-ref-mutated-index-p (index)
  (declare (fixnum index))
  (minusp index))

(defun file-vector-reference-present-pristine? (ref)
  "[Cyc] The file vector reference has brought the referenced object in from teh index in the reference and holds on to it in the payload."
  (and (fvector-ref-has-valid-index-p ref)
       (fvector-ref-payload-in-memory-p ref)))

(defun fvactor-ref-has-valid-index-p (ref)
  (fvector-ref-valid-index-p (fvector-ref-index ref)))

(defun file-vector-backed-map-w/-cache-get (map file-vector cache-strategy key &optional not-found)
  "[Cyc] Lookup the value. If the stored result is a FILE-VECTOR-REFERENCE-P, check whetehr it is loaded in. If it is present & pristine, update the cache-strategy's tracking. If it is not loaded, load the information, enable tracking with the cache-strategy for the key, and page out the key least likely to be needed according to the cache-strategy.
CACHE-STRATEGY can be SYMBOLP if no cache strategy is needed.
Returns the value retrieved under the KEY or NOT-FOUDN if not present."
  (let ((datum (map-get-without-values map key not-found)))
    (when (file-vector-reference-p datum)
      (cond
        ((file-vector-reference-present? datum)
         (let ((value (file-vector-reference-referenced-object datum)))
           (when (and (cache-strategy-p cache-strategy)
                      (file-vector-reference-present-pristine? datum))
             (bt:with-lock-held (*file-vector-backed-map-read-lock*)
               (cache-strategy-note-cache-hit cache-strategy)
               (cache-strategy-note-reference cache-strategy key)))
           value))

        ((file-vector-reference-deleted? datum) not-found)

        ((file-vector-reference-swapped-out? datum)
         (let ((index (file-vector-reference-index datum))
               (data-stream (get-file-vector-data-stream file-vector)))
           (prog1 (bt:with-lock-held (*file-vector-backed-map-read-lock*)
                    (position-file-vector file-vector index)
                    (file-vector-backed-map-read-value data-stream))
             (let ((potential-loser (cache-strategy-track cache-strategy key)))
               (unless (eq potential-loser key)
                 (missing-larkc 6234)))
             (cache-strategy-note-cache-miss cache-strategy))))

        (t (error "Invalid state transition: ~A is neither present, nor deleted nor swapped out." datum))))))

(defun file-vector-reference-referenced-object (ref)
  (fvector-ref-payload ref))

(defun file-vector-reference-index (ref)
  (fvector-ref-index ref))

(defun file-vector-reference-swapped-out? (ref)
  "[Cyc] The file vector reference refers to an object in the file vector but that object does not reside yet in memory."
  (and (fvector-ref-has-valid-index-p ref)
       (not (fvector-ref-payload-in-memory-p ref))))

(defun file-vector-backed-map-read-value (data-stream)
  (cfasl-input data-stream))

(defun file-vector-reference-deleted? (ref)
  "[Cyc] The file vector reference refers to a deleted object."
  (and (fvector-ref-has-mutated-index-p ref)
       (not (fvector-ref-payload-in-memory-p ref))))

(defun file-vector-backed-map-m/-cache-put (map cache-strategy key value)
  "[Cyc] Put the value into the file-vector backed map. if the entry denoted by the key has a file vector backed reference, then mark the file vector reference as mutated and replace the payload with the value. If the CACHE-STRATEGY is valid, then untrack the key.
Otherwise, simply store the passed-in new value.
CACHE-STRATEGY can be SYMBOLP if no cache strategy is needed.
Returns :MUTATED if the entry was a file-vector reference, NIL otherwise."
  (let ((new-value value)
        (current-value (map-get-without-values map key :not-found))
        (mutated-p nil))
    (if (file-vector-reference-p current-value)
        (missing-larkc 6215)
        (map-put map key new-value))
    mutated-p))

(defun file-vector-backed-map-w/-cache-remove (map cache-strategy key &optional support-undo-p)
  (declare (ignore cache-strategy))
  (let* ((current-value (map-get-without-values map key :not-found))
        (is-file-vector-reference (file-vector-reference-p current-value))
        (deleted-p nil))
    (if support-undo-p
        (missing-larkc 6214)
        (map-remove map key))
    (if (and is-file-vector-reference
             (missing-larkc 31228))
        (bt:with-lock-held (*file-vector-backed-map-read-lock*)
          (missing-larkc 31248)))
    deleted-p))

(defun fvector-ref-valid-index-p (index)
  (declare (fixnum index))
  (plusp index))

(defun new-file-vector-reference (index)
  (declare (fixnum index))
  (must (> index 0) "File Vector references cannot be zero.")
  (let ((ref (make-file-vector-reference :index index)))
    (clear-file-vector-reference-referenced-object ref)
    ref))

(defun clear-file-vector-reference-referenced-object (ref)
  (set-file-vector-reference-referenced-object ref nil))

(defun set-file-vector-reference-referenced-object (ref object)
  (setf (fvector-ref-payload ref) object)
  ref)

(defun cfasl-input-file-vector-reference (stream)
  (let ((index (cfasl-input stream)))
    (if (fvector-ref-valid-index-p index)
        (new-file-vector-reference index)
        (new-file-vector-reference-w/-payload index (cfasl-input stream)))))

(defun new-file-vector-reference-w/-payload (index payload)
  (let ((ref (new-file-vector-reference index)))
    (set-file-vector-reference-referenced-object ref payload)
    ref))

(defun file-vector-backed-map-w/cache-touch (map cache-strategy key &optional fvector)
  "[Cyc] If the entry denoted by key has a file-vector backed reference, then mark the reference as mutated. This allows to percolate change information properly in situations where the value of a map is a container.
Touched items have to be untracked in the cache strategy if caching is active.
CACHE-STRATEGY can be SYMBOLP if no cache strategy is needed.
FVECTOR need only be valid if the entry is swapped out at the time of the call, because touch must paget he absent values in.
Returns :MUTATED if the entry was a file vector reference, nil otherwise."
  (let ((current-value (map-get-without-values map key :not-found)))
    (when (file-vector-reference-p current-value)
      (let ((ref current-value))
        (when (file-vector-reference-swapped-out? ref)
          (file-vector-backed-map-w/-cache-get map fvector cache-strategy key))
        (mark-file-vector-reference-as-mutated ref)
        (when (cache-strategy-p cache-strategy)
          (missing-larkc 31250))
        :mutated))))

(defun mark-file-vector-reference-as-mutated (ref)
  (let* ((index (fvector-ref-index ref))
         (mutated-index (- (abs index))))
    (setf (fvector-ref-index ref) mutated-index))
  ref)

(defun swap-out-all-pristine-file-vector-backed-map-objects (map)
  "[Cyc] For all values in the MAP, if the value is a pristine file vector reference, then zero out its payload to make that data available for garbage collection.
Return 0: the MAP
Return 1: the count of paged out items."
  (let ((swapped-out 0)
        (iterator (new-map-iterator map))
        (done nil))
    (until done
      (multiple-value-bind (var valid) (iteration-next iterator)
        (if valid
            (multiple-value-bind (key value) var
              (declare (ignore key))
              (when (potentially-swap-out-pristine-file-vector-backed-map-object value)
                (incf swapped-out)))
            (return (values map swapped-out)))))))

(defun potentially-swap-out-pristine-file-vector-backed-map-object (value)
  "[Cyc] Helper for swapping out, both in the larger context of swapping out all and in the more specific context of swapping out some.
Returns T if there was a file vector reference that was pristine and swapped out, NIL otherwise."
  (and (file-vector-reference-p value)
       (file-vector-reference-present-pristine? value)
       (progn
         (clear-file-vector-reference-referenced-object value)
         t)))

(defstruct backed-map
  map
  fvector
  common-symbols)

(defconstant *cfasl-opcode-backed-map* 76)
(defparameter *current-backed-map-cache-strategy* nil
    "[Cyc] The current cache strategy to use for this backed-map operation. Defaults to none.")

;; TODO - most of the original defmethods were missing-larkc.  Does this implemented one matter?  Where's the defgeneric/defpolymorphic?
(defmethod is-map-object-p ((object backed-map))
  t)


