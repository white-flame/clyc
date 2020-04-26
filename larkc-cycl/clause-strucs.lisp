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


(defstruct (clause-struc (:conc-name "CLS-"))
  id
  cnf
  assertions)

(defmethod sxhash ((object clause-struc))
  (declare (ignore object))
  ;; TODO - simple ID accessor?  why is this missing-larkc?  reconstructable
  (missing-larkc 11340))

(deflexical *clause-struc-free-list* nil
    "[Cyc] Free list for CLAUSE-STRUC objects.")

(deflexical *clause-struc-free-lock* (bt:make-lock "CLAUSE-STRUC resource lock")
    "[Cyc] Lock for CLAUSE-STRUC object free list.")

(defun make-static-clause-struc ()
  "[Cyc] Make a new CLAUSE-STRUC in the static area."
  (make-clause-struc))

(defun init-clause-struc (clause-struc)
  (setf (cls-id clause-struc)
        (setf (cls-cnf clause-struc)
              (setf (cls-assertions clause-struc)
                    nil)))
  clause-struc)

;; TODO - collapse resourcing into a macro
(defun get-clause-struc ()
  "[Cyc] Get a CLAUSE-STRUC from the free list, or make a new one if needed."
  (if (not *structure-resourcing-enabled*)
      (init-clause-struc (if *structure-resourcing-make-static*
                             (make-static-clause-struc)
                             (make-clause-struc)))
      (missing-larkc 11349)))

(defun* reset-clause-struc-id (clause-struc new-id) (:inline t)
  "[Cyc] Primitively change the clause-struc id for CLAUSE-STRUC to NEW-ID."
  (setf (cls-id clause-struc) new-id))

(defun* clause-struc-cnf (clause-struc) (:inline t)
  "[Cyc] Return the CNF of CLAUSE-STRUC."
  (cls-cnf clause-struc))

(defun* reset-clause-struc-assertions (clause-struc new-assertions) (:inline t)
  "[Cyc] Primitively set the assertions for CLAUSE-STRUC to NEW-ASSERTIONS."
  (setf (cls-assertions clause-struc) new-assertions))

(defun* find-clause-struc-by-id (id) (:inline t)
  "[Cyc] Return the clause-struc with ID, or NIL if not present."
  (lookup-clause-struc id))

(defun make-clause-struc-shell (cnf &optional id)
  (unless id
    (missing-larkc 11351))
  (let ((clause-struc (get-clause-struc)))
    (register-clause-struc-id clause-struc id)
    (setf (cls-cnf clause-struc) cnf)
    clause-struc))

(defun* create-sample-invalid-clause-struc () (:inline t)
  "[Cyc] Create a sample invalid clause-struc."
  (get-clause-struc))

(defglobal *clause-struc-from-id* nil
    "[Cyc] The ID -> CLAUSE-STRUC mapping table.")

(defun* clause-struc-table () (:inline t)
  *clause-struc-from-id*)

(defun setup-clause-struc-table (size exact?)
  (unless *clause-struc-from-id*
    (setf *clause-struc-from-id* (new-id-index size 0))))

(defun finalize-clause-strucs (&optional max-clause-struc-id)
  (set-next-clause-struc-id max-clause-struc-id)
  (unless max-clause-struc-id
    (optimize-id-index *clause-struc-from-id*)))

(defun free-all-clause-strucs ()
  (do-id-index (id clause-struc (clause-struc-table)
                   :progress-message "Freeing clause strucs")
    (missing-larkc 11347))
  (clear-clause-struc-table))

(defun* clear-clause-struc-table () (:inline t)
  (clear-id-index *clause-struc-from-id*))

(defun set-next-clause-struc-id (&optional max-clause-struc-id)
  (let ((next-id (1+ (or max-clause-struc-id
                         (let ((max -1))
                           (do-id-index (id clause-struc (clause-struc-table))
                             (setf max (max max (missing-larkc 11338))))
                           max)))))
    (set-id-index-next-id *clause-struc-from-id* next-id)))

(defun register-clause-struc-id (clause-struc id)
  (reset-clause-struc-id clause-struc id)
  (id-index-enter *clause-struc-from-id* id clause-struc))

(defun* lookup-clause-struc (id) (:inline t)
  (id-index-lookup *clause-struc-from-id* id))

;; TODO - these dump-id-tables are scattered around, but unused.  Part of serialization identity?
(defparameter *clause-struc-dump-id-table* nil)
