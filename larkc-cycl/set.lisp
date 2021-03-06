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


;; Replaced the implemenation with key->T hashtables

;; TODO - could defer the inlines to set-contents instead of hash-table

(deflexical *new-set-default-test-function* #'eql)

(defun* new-set (&optional (test *new-set-default-test-function*) (size 0))
    (:inline t)
  "[Cyc] Allocate a new set with TEST as the equality test.
Assume that SIZE elements will likely be immediately added."
  (make-hash-table :test test :size size))

(defun* set-size (set) (:inline t)
  "[Cyc] Return the number of items currently entered in SET"
  (hash-table-count set))

(defun* set-empty? (set) (:inline t)
  "[CYc] Return non-NIL iff SET is empty, NIL otherwise."
  (hash-table-empty-p set))

(defun* set-member? (element set) (:inline t)
  "[Cyc] Return T iff ELEMENT is in SET."
  (gethash element set))

;; TODO DESIGN - this is a lot slower than if it didn't have to have the return value test. SBCL internals might allow us to do this more directly, but we should first check if the return value is ever actually used.
(defun* set-add (element set) (:inline t)
  "[Cyc] Add this ELEMENT into the SET.
Return T iff ELEMENT was not already there."
  (unless (set-member? element set)
      (setf (gethash element set) t)))

(defun* set-remove (element set) (:inline t)
  "[Cyc] If ELEMENT is present in SET, then take it out of SET.
Returns T iff ELEMENT was in SET to begin with."
  ;; remhash matches this return behavior
  (remhash element set))

(defun* clear-set (set) (:inline t)
  "[Cyc] Reset SET to the status of being just allocated.
Returns SET."
  ;; TODO - we're not remembering its initial size.  Oh well.
  (clrhash set))

(defun* new-set-iterator (set) (:inline t)
  (new-hash-table-iterator set))

(defconstant *cfasl-opcode-set* 60)

(defun cfasl-input-set (stream)
  (let* ((test (cfasl-input stream))
         (size (cfasl-input stream))
         (set (new-set test size)))
    (cfasl-input-set-contents stream set size)))

(defconstant *cfasl-opcode-legacy-set* 67)

(defun* set-element-list (set) (:inline t)
  "[Cyc] Returns a list of the elements of SET."
  (hash-table-keys set))

;; TODO - deprecate this
(defun* set-rebuild (set) (:inline t)
  set)


(defun* set-p (obj) (:inline t)
  "Since Clyc sets are hashtables, this overfits."
  ;; TODO - See if there are any places where set-p is used as a peer of dictionary-p or hash-table-p etc
  (hash-table-p obj))

(defmacro do-set ((item set &optional done-form) &body body)
  (alexandria:with-gensyms (val)
    `(block nil
       (maphash (lambda (,item ,val)
                  (declare (ignore ,val))
                  ,(when done-form
                     `(when ,done-form
                        (return nil)))
                  ,@body)
                ,set))))



