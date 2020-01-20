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


(defglobal *assertion-from-id* nil
  ;; This is an id-index
  "[Cyc] The ID -> ASSERTION mapping table.")

(defun-inline do-assertions-table ()
  *assertion-from-id*)

(defun setup-assertion-table (size exact?)
  (declare (ignore exact?))
  (unless *assertion-from-id*
    (setf *assertion-from-id* (new-id-index size 0))
    t))

(defun finalize-assertions (&optional max-assertion-id)
  (set-next-assertion-id max-assertion-id)
  (unless max-assertion-id
    (missing-larkc 30896)))

(defun clear-assertion-table ()
  (clear-id-index *assertion-from-id*))

(defun assertion-count ()
  "[Cyc] Return the total number of assertions."
  (if *assertion-from-id*
      (id-index-count *assertion-from-id*)
      0))

(defun-inline lookup-assertion (id)
  (id-index-lookup *assertion-from-id* id))

(defun set-next-assertion-id (&optional max-assertion-id)
  (let ((max -1))
    (if max-assertion-id
        (setf max max-assertion-id)
        (do-id-index (id assertion (do-assertions-table)
                         :progress-message "Determining maximum assertion ID")
          (setf max (max max (assertion-id assertion)))))
    (let ((next-id (+ max 1)))
      (set-id-index-next-id *assertion-from-id* next-id)
      next-id)))

(defun register-assertion-id (assertion id)
  "[Cyc] Note that ID will be used as the id for ASSERTION."
  (reset-assertion-id assertion id)
  (id-index-enter *assertion-from-id* id assertion)
  assertion)

(defun deregister-assertion-id (id)
  "[Cyc] Note that ID is not in use as an assertion id."
  (id-index-remove *assertion-from-id* id))

(defun make-assertion-id ()
  "[Cyc] Return a new integer id for an assertion."
  (id-index-reserve *assertion-from-id*))

(defstruct (assertion (:conc-name as-))
  id)

(defparameter *print-assertions-in-cnf* nil)

(defmethod sxhash ((object assertion))
  (let ((id (as-id object)))
    (if (integerp id)
        id
        23)))

(defun-inline get-assertion ()
  "[Cyc] Make a new assertion shell, potentially in static space."
  (make-assertion))

(defun-inline free-assertion (assertion)
  "[Cyc] Invalidate ASSERTION."
  (setf (as-id assertion) nil))

(defun valid-assertion-handle? (object)
  "[Cyc] Return T iff OBJECT is a valid assertion handle."
  (and (assertion-p object)
       (assertion-handle-valid? object)))

(defun-inline valid-assertion? (assertion &optional robust?)
  "[Cyc] Return T if ASSERTION is a valid assertion."
  (declare (ignore robust?))
  (valid-assertion-handle? assertion))

(defun make-assertion-shell (&optional id)
  (unless id
    (setf id (make-assertion-id)))
  (check-type id #'fixnump)
  (let ((assertion (get-assertion)))
    (register-assertion-id assertion id)
    assertion))

(defun-inline create-sample-invalid-assertion ()
  "[Cyc] Create a sample invalid-assertion."
  (get-assertion))

(defun free-all-assertions ()
  (do-id-index (id assertion (do-assertions-table)
                   :progress-message "Freeing assertions")
    (free-assertion assertion))
  (clear-assertion-table)
  (clear-assertion-content-table)) 

(defun-inline assertion-id (assertion)
  "[Cyc] Return the id of this ASSERTION."
  (as-id assertion))

(defun-inline reset-assertion-id (assertion new-id)
  "[Cyc] Primitively change the assertion id for ASSERTION to NEW-ID."
  (setf (as-id assertion) new-id))

(defun-inline assertion-handle-valid? (assertion)
  (integerp (as-id assertion)))

(defun-inline find-assertion-by-id (id)
  "[Cyc] Return the assertion with ID, or NIL if not present."
  (lookup-assertion id))

