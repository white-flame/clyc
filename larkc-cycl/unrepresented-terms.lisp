#|
  Copyright (c) 2019-2020 White Flame

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


(defglobal *unrepresented-term-to-suid* nil
    "[Cyc] The UNREPRESENTED-TERM -> SUID mapping table.")

(defglobal *unrepresented-term-from-suid* nil
    "[Cyc] The SUID -> UNREPRESENTED-TERM mapping table.")

(defun-inline do-unrepresented-terms-table ()
  *unrepresented-term-from-suid*)

(defun setup-unrepresented-term-suid-table (size exact?)
  (declare (ignore exact?))
  (let ((setup? nil))
    (unless *unrepresented-term-from-suid*
      (setf *unrepresented-term-from-suid* (new-id-index size 0))
      (setf setup? t))
    (unless *unrepresented-term-to-suid*
      (setf *unrepresented-term-to-suid* (make-hash-table :size size
                                                          :test #'equal))
      (setf setup? t))
    setup?))

(defun finalize-unrepresented-term-suid-table (&optional max-unrepresented-term-id)
  (set-next-unrepresented-term-suid max-unrepresented-term-id)
  (unless max-unrepresented-term-id
    ;; TODO - elided *current-area* storage binding
    ;; TODO - probably obsoleted
    (optimize-id-index *unrepresented-term-from-suid*)))

(defun clear-unrepresented-term-suid-table ()
  (clear-id-index *unrepresented-term-from-suid*)
  (clrhash *unrepresented-term-to-suid*)
  (set-next-unrepresented-term-suid))

(defun kb-unrepresented-term-count ()
  "[Cyc] Return the total number of unrepresented-terms mentioned in the KB."
  (if-let ((idx *unrepresented-term-from-suid*))
    (id-index-count idx)
    0))

(defun-inline lookup-unrepresented-term-by-suid (suid)
  (id-index-lookup *unrepresented-term-from-suid* suid))

(defun-inline lookup-unrepresented-term-suid (term)
  (gethash term *unrepresented-term-to-suid*))

(defun-inline find-unrepresented-term-by-suid (suid)
  (lookup-unrepresented-term-by-suid suid))

(defun-inline unrepresented-term-suid (term)
  (lookup-unrepresented-term-suid term))

(defun kb-unrepresented-term-p (object)
  (and (indexed-unrepresented-term-p object)
       (unrepresented-term-suid object)))

;; TODO DESIGN - similar pattern with a lot of set-next- functions
(defun set-next-unrepresented-term-suid (&optional max-unrepresented-term-id)
  (let* ((max (or max-unrepresented-term-id
                  (let ((max -1))
                    (do-id-index (id unrepresented-term (do-unrepresented-terms-table)
                                     :progress-message "Determining maximum unrepresented-term SUID")
                      (setf max (max max (unrepresented-term-suid unrepresented-term))))
                    max)))
         (next-suid (1+ max)))
    (set-id-index-next-id *unrepresented-term-from-suid* next-suid)
    next-suid))

(defun register-unrepresented-term-suid (term suid)
  "[Cyc] Note that SUID will be used as the suid for UNREPRESENTED-TERM."
  (id-index-enter *unrepresented-term-from-suid* suid term)
  (setf (gethash term *unrepresented-term-to-suid*) suid))

(defun deregister-unrepresented-term-suid (suid)
  "[Cyc] Note that SUID is not in use as an unrepresented term suid."
  (prog1-let ((term (lookup-unrepresented-term-by-suid suid)))
    (when term
      (id-index-remove *unrepresented-term-from-suid* suid)
      (remhash term *unrepresented-term-to-suid*))))

(defun-inline make-unrepresented-term-suid ()
  "[Cyc] Return a new integer suid for a unrepresented-term."
  (id-index-reserve *unrepresented-term-from-suid*))

(defun find-or-create-unrepresented-term-suid (term)
  (or (unrepresented-term-suid term)
      (prog1-let ((suid (make-unrepresented-term-suid)))
        (register-unrepresented-term-suid term suid))))

(defun finalize-unrepresented-terms (&optional max-unrepresented-term-id)
  (finalize-unrepresented-term-suid-table max-unrepresented-term-id))

(defun unrepresented-term-index (term)
  (when-let ((suid (unrepresented-term-suid term)))
    (lookup-unrepresented-term-index suid)))

(defun setup-unrepresented-term-table (size exact?)
  (setup-unrepresented-term-suid-table size exact?)
  (setup-unrepresented-term-index-table size exact?))

(defun clear-unrepresented-term-table ()
  (clear-unrepresented-term-suid-table)
  (clear-unrepresented-term-index-table))

(defun reset-unrepresented-term-index (term new-index &optional bootstrap?)
  "[Cyc] Primitively change he assertion index for TERM to NEW-INDEX.
If BOOTSTRAP? is non-NIL, then a new SUID will be created for TERM if one does not already exist."
  (if (not new-index)
      (when-let ((suid (unrepresented-term-suid term)))
        (deregister-unrepresented-term-index suid)
        (deregister-unrepresented-term-suid suid))
      (when-let ((suid (if bootstrap?
                           (find-or-create-unrepresented-term-suid term)
                           (unrepresented-term-suid term))))
        (register-unrepresented-term-index suid new-index))))

(defparameter *unrepresented-term-dump-id-table* nil)

(defun-inline find-unrepresented-term-by-dump-id (dump-id)
  (find-unrepresented-term-by-suid dump-id))
