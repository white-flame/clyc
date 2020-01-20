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


(defglobal *deduction-from-id* nil
    "[Cyc] The ID -> DEDUCTION mapping table.")

(defun-inline do-deductions-table ()
  *deduction-from-id*)

(defun setup-deduction-table (size exact?)
  (declare (ignore exact?))
  (unless *deduction-from-id*
    (setf *deduction-from-id* (new-id-index size 0))))

(defun finalize-deductions (&optional max-deduction-id)
  (set-next-deduction-id max-deduction-id)
  (unless max-deduction-id
    (missing-larkc 30888)))

(defun clear-deduction-table ()
  (clear-id-index *deduction-from-id*))

(defun deduction-count ()
  "[Cyc] Return the total number of deductions."
  (let ((index *deduction-from-id*))
    (if index
        (id-index-count index)
        0)))

(defun-inline lookup-deduction (id)
  (id-index-lookup *deduction-from-id* id))

(defun set-next-deduction-id (&optional max-deduction-id)
  (let ((next-id (1+ (or max-deduction-id
                         (let ((max -1))
                           (do-id-index (id deduction *deduction-from-id*
                                            :progress-message "Determining maximum deduction ID.")
                             (setf max (max max id)))
                           max)))))
    (set-id-index-next-id *deduction-from-id* next-id)
    next-id))

(defun register-deduction-id (deduction id)
  "[Cyc] Note that ID will eb used as the id for DEDUCTION."
  (reset-deduction-id deduction id)
  (id-index-enter *deduction-from-id* id deduction))

(defun deregister-deduction-id (id)
  "[Cyc] Note that ID is not in use as a deduction id."
  (id-index-remove *deduction-from-id* id))

(defun-inline make-deduction-id ()
  "[Cyc] Return a new integer id for a deduction."
  (id-index-reserve *deduction-from-id*))

(defstruct (deduction (:conc-name "D-"))
  id)

(defmethod sxhash ((object deduction))
  (let ((id (d-id object)))
    (or id 786)))

(defun-inline get-deduction ()
  "[Cyc] Make a new deduction shell, potentially in static space."
  (make-deduction))

(defun free-deduction (deduction)
  "[Cyc] Invalidate DEDUCTION."
  (setf (d-id deduction) nil))

(defun valid-deduction-handle? (object)
  (and (deduction-p object)
       (deduction-handle-valid? object)))

(defun-inline valid-deduction (deduction &optional robust?)
  (valid-deduction? deduction robust?))

(defun valid-deduction? (deduction &optional robust?)
  (if (valid-deduction-handle? deduction)
      (or (not robust?)
          (let ((supports (deduction-supports deduction)))
            (and (valid-support? (deduction-assertion deduction))
                 (consp supports)
                 (every-in-list #'valid-support? supports))))))

(defun make-deduction-shell (&optional id)
  (unless id
    (setf id (make-deduction-id)))
  (let ((deduction (get-deduction)))
    (register-deduction-id deduction id)
    deduction))

(defun-inline create-sample-invalid-deduction ()
  (get-deduction))

(defun free-all-deductions ()
  (do-id-index (id deduction (do-deductions-table)
                   :progress-message "Freeing deductions")
    (free-deduction deduction))
  (clear-deduction-table)
  (clear-deduction-content-table))

(defun-inline deduction-id (deduction)
  "[Cyc] Return the id of DEDUCTION."
  (d-id deduction))

(defun reset-deduction-id (deduction new-id)
  "[Cyc] Primitively change the id of DEDUCTION to NEW-ID."
  (setf (d-id deduction) new-id))

(defun-inline deduction-valid-handle? (deduction)
  ;; TODO - original checked for integerp
  (d-id deduction))

(defun-inline find-deduction-by-id (id)
  (lookup-deduction id))

