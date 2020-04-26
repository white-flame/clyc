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


(defstruct (kb-object-manager (:conc-name "KBOM-"))
  name
  content-lock
  lru-size-percentage
  content-table
  usage-table
  lru-information
  file-vector
  id-threshold
  load-func
  meter-swap-time?
  swap-time
  dummy1
  dummy2
  dummy3)

(defun new-kb-object-manager (name size lru-size-percentage load-func exact-size?)
  (let ((kbom (make-kb-object-manager :name name
                                      :content-lock (bt:make-lock
                                                     (format nil "~a content manager lock" name))
                                      :lru-size-percentage lru-size-percentage
                                      :usage-table :uninitialized
                                      :file-vector nil
                                      :id-threshold 0
                                      :load-func load-func
                                      :meter-swap-time? nil
                                      :swap-time nil)))
    ;; TODO - only the first 3 slots were initialized before this was called. Ensure that this doesn't trample any of the later ones
    (setup-kb-object-content-table kbom size exact-size?)
    kbom))

(defun setup-kb-object-content-table (kbom size exact?)
  "[Cyc] EXACT?: Whether SIZE is the exact desired size.  If so, we'll allocate the table in static space, otherwise we'll wait for OPTIMIZE-KB-OBJECT-CONTENT-TABLE to do that."
  (declare (ignore exact?))
  (let ((did-setup? nil))
    (bt:with-lock-held ((kbom-content-lock kbom))
      (let ((content-table (kbom-content-table kbom)))
        (unless (id-index-p content-table)
          (setf (kbom-content-table kbom) (new-id-index size 0))
          ;; TODO - this setf immediately gets trampled?
          (setf did-setup? t))
        (setf did-setup? (setup-kb-object-content-support kbom nil size))))
    did-setup?))

(deflexical *min-kb-object-lru-size* 212)

(defun setup-kb-object-content-support (kbom &optional initialize-usage-counts? size)
  (unless (fixnump size)
    (setf size (id-index-new-id-threshold (kbom-content-table kbom))))
  (let ((did-setup? nil))
    (when initialize-usage-counts?
      (unless (id-index-p (kbom-usage-table kbom))
        (setf (kbom-usage-table kbom) (new-id-index size 0))
        ;; TODO - this setf immediately gets trampled?
        (setf did-setup? t)))
    (unless (cache-p (kbom-lru-information kbom))
      (let ((lru-size (max *min-kb-object-lru-size*
                           (* (floor size 100) (kbom-lru-size-percentage kbom)))))
        (setf (kbom-lru-information kbom) (new-preallocated-cache lru-size #'eq))))
    did-setup?))

(defun initialize-kb-object-hl-store-cache (kbom content-filename index-filename)
  (must-not (ends-with content-filename "cfasl")
            "Got ~s, expected an extensionless filename" content-filename)
  (must-not (ends-with index-filename "cfasl")
            "Got ~s, expected an extensionless filename" index-filename)
  (let ((kb-object-cfasl-file (get-hl-store-cache-filename content-filename "cfasl"))
        (kb-object-index-file (get-hl-store-cache-filename index-filename "cfasl")))
    (when (and (probe-file kb-object-cfasl-file)
               (probe-file kb-object-index-file))
      ;; TODO - looks like a macro, but not sure how often it's used
      (let ((old-file-vector (kbom-file-vector kbom)))
        (when (file-vector-p old-file-vector)
          (close-file-vector old-file-vector)))
      (let ((file-vector (new-kb-object-content-file-vector kb-object-cfasl-file
                                                            kb-object-index-file)))
        (setf (kbom-file-vector kbom) file-vector)
        (setf (kbom-id-threshold kbom) (file-vector-length file-vector))
        (kb-object-content-file-vector-p file-vector)))))

(defun clear-kb-object-content-table (kbom)
  (bt:with-lock-held ((kbom-content-lock kbom))
    (let ((usage-table (kbom-usage-table kbom)))
      (when (id-index-p usage-table)
        (clear-id-index usage-table)))
    (let ((lru-information (kbom-lru-information kbom)))
      (when (cache-p lru-information)
        (cache-clear lru-information)))
    (let ((content-table (kbom-content-table kbom)))
      (clear-id-index content-table))))

(defun* kb-object-manager-name (kbom) (:inline t) (kbom-name kbom))
(defun* kb-object-manager-content-lock (kbom) (:inline t) (kbom-content-lock kbom))
(defun* kb-object-manager-lru-size-percentage (kbom) (:inline t) (kbom-lru-size-percentage kbom))
(defun* kb-object-manager-content-table (kbom) (:inline t) (kbom-content-table kbom))
(defun* kb-object-manager-usage-table (kbom) (:inline t) (kbom-usage-table kbom))
(defun* kb-object-manager-lru-information (kbom) (:inline t) (kbom-lru-information kbom))
(defun* kb-object-manager-file-vector (kbom) (:inline t) (kbom-file-vector kbom))
(defun* kb-object-manager-id-threshold (kbom) (:inline t) (kbom-id-threshold kbom))
(defun* kb-object-manager-load-func (kbom) (:inline t) (kbom-load-func kbom))
(defun* kb-object-manager-meter-swap-time? (kbom) (:inline t) (kbom-meter-swap-time? kbom))

(defun kb-object-usage-counts-enabled? (kbom)
  (id-index-p (kb-object-manager-usage-table kbom)))

(defun cached-kb-object-count (kbom)
  (let ((content-table (kb-object-manager-content-table kbom)))
    (if (id-index-p content-table)
        (id-index-count content-table)
        0)))

(defun lookup-kb-object-content (kbom id)
  (let ((content nil))
    (bt:with-lock-held ((kbom-content-lock kbom))
      (let ((content-table (kb-object-manager-content-table kbom)))
        (setf content (id-index-lookup content-table id (uninitialized)))
        (if (uninitialized-p content)
            (if (is-lru-cachable-kb-object-content-id? kbom id)
                (progn
                  (swap-in-kb-object-content kbom id)
                  (setf content (id-index-lookup content-table id)))
                (setf content nil))
            (update-kb-object-usage kbom id))))
    content))

(defun kb-object-manager-unbuilt? (kbom)
  (unless (file-vector-p (kb-object-manager-file-vector kbom))
    (let ((id-index (kb-object-manager-content-table kbom)))
      (not (and (id-index-p id-index)
                (plusp (id-index-count id-index)))))))

(defun register-kb-object-content (kbom id kb-object-content)
  "[Cyc] Note that ID will be used as the id for KB-OBJECT-CONTENT."
  (bt:with-lock-held ((kbom-content-lock kbom))
    (id-index-enter (kb-object-manager-content-table kbom) id kb-object-content)))

(defun deregister-kb-object-content (kbom id)
  "[Cyc] Note that ID is not in use as an KB-OBJECT-CONTENT id."
  (bt:with-lock-held ((kbom-content-lock kbom))
    (prog1 (id-index-remove (kb-object-manager-content-table kbom) id)
      (drop-kb-object-usage kbom id))))

(defun new-kb-object-content-file-vector (cfasl-file index-file)
  (new-file-vector cfasl-file index-file))

(defun kb-object-content-file-vector-p (object)
  (file-vector-p object))

(defun kb-object-content-file-vector-lookup (kbom id)
  (let* ((cfasl-stream (position-file-vector (kb-object-manager-file-vector kbom) id))
         (kb-object-id (cfasl-input cfasl-stream)))
    (unless (and (fixnump kb-object-id)
                 (= kb-object-id id))
      (error "We did not manage to swap the KB object ~d back in from the CFASL stream ~a; got ~d instead."
             id cfasl-stream kb-object-id))
    (let ((load-func (kb-object-manager-load-func kbom)))
      (funcall load-func id cfasl-stream)))
  id)

(defun is-lru-cachable-kb-object-content-id? (kbom id)
  (< id (kb-object-manager-id-threshold kbom)))

(defun update-kb-object-usage (kbom id)
  (when (kb-object-usage-counts-enabled? kbom)
    (increment-kb-object-usage-count kbom id))
  (let ((lru-information (kb-object-manager-lru-information kbom)))
    (when (and (cache-p lru-information)
               (is-lru-cachable-kb-object-content-id? kbom id))
      (cache-get-without-values lru-information id))))

(defun drop-kb-object-usage (kbom id)
  (let ((lru-information (kb-object-manager-lru-information kbom)))
    (when (and (cache-p lru-information)
               (is-lru-cachable-kb-object-content-id? kbom id))
      (cache-remove lru-information id))))

(defun mark-kb-object-content-as-muted (kbom id)
  (let ((lru-information (kb-object-manager-lru-information kbom)))
    (when (and (cache-p lru-information)
               (is-lru-cachable-kb-object-content-id? kbom id))
      (cache-remove lru-information id))))

(defun swap-in-kb-object-content (kbom id)
  (let ((*cfasl-constant-handle-lookup-func* nil)
        (*cfasl-nart-handle-lookup-func* nil)
        (*cfasl-assertion-handle-lookup-func* nil)
        (*cfasl-deduction-handle-lookup-func* nil)
        (*cfasl-kb-hl-support-handle-lookup-func* nil)
        (*cfasl-clause-struc-handle-lookup-func* nil))
    (if (kb-object-manager-meter-swap-time? kbom)
        (missing-larkc 32087)
        (swap-in-kb-object-content-internal kbom id))))

(defun swap-in-kb-object-content-internal (kbom id)
  (let ((*cfasl-common-symbols* nil))
    (cfasl-set-common-symbols (get-hl-store-caches-shared-symbols))
    (kb-object-content-file-vector-lookup kbom id))
  (increment-kb-object-usage-count kbom id)
  (multiple-value-bind (loser oldest-value pair?)
      (cache-set-return-dropped (kb-object-manager-lru-information kbom) id id)
    (declare (ignore oldest-value pair?))
    (when (fixnump loser)            
      (swap-out-pristine-kb-object-content kbom loser))))

(defun swap-out-pristine-kb-object-content (kbom loser)
  (id-index-remove (kb-object-manager-content-table kbom) loser))

(defun swap-out-all-pristine-kb-objects-int (kbom)
  (let ((pristine-ids nil)
        (lru-information (kb-object-manager-lru-information kbom)))
    (do-cache (id val lru-information)
      (push id pristine-ids))
    (setf pristine-ids (sort pristine-ids #'<))
    (progress-dolist (id pristine-ids (concatenate 'string "Swapping out "
                                                   (kb-object-manager-name kbom)
                                                   " objects"))
      (cache-remove lru-information id)
      (swap-out-pristine-kb-object-content kbom id))
    (length pristine-ids)))

(defun increment-kb-object-usage-count (kbom id)
  (when (kb-object-usage-counts-enabled? kbom)
    (let* ((usage-table (kb-object-manager-usage-table kbom))
           (old-counter (id-index-lookup usage-table id))
           (new-counter (if old-counter
                            (1+ old-counter)
                            1)))
      (id-index-enter usage-table id new-counter))))


