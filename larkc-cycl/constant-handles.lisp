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



;; TODO - there are do-constants and such macros that might be involved in the big macroexpansions in this file.

(defstruct (constant (:conc-name "C-"))
  suid
  name)

(declaim (inline constant-handle-valid?))
(defun constant-handle-valid? (constant)
  (integerp (c-suid constant)))


(defglobal *constant-from-suid* nil
  "[Cyc] The SUID->CONSTANT mapping table.")

(defun-inline do-constants-table ()
  *constant-from-suid*)

(defun setup-constant-suid-table (size exact?)
  (declare (ignore exact?))
  (unless *constant-from-suid*
    (initialize-constant-names-in-code)
    (setf *constant-from-suid* (new-id-index size 0))))

(defun finalize-constant-suid-table (&optional max-constant-suid)
  (set-next-constant-suid max-constant-suid)
  (unless max-constant-suid
    ;; TODO DESIGN - I don't think we'll support memory areas
    ;;(let ((*current-area* (get-static-area))))
    (optimize-id-index *constant-from-suid*)))

(defun clear-constant-suid-table ()
  (clear-id-index *constant-from-suid*))

(defun constant-count ()
  "[Cyc] Return the total number of constants."
  (if *constant-from-suid*
      (id-index-count *constant-from-suid*)
      0))

(defun lookup-constant-by-suid (suid)
  (id-index-lookup *constant-from-suid* suid))

(defun new-constant-suid-threshold ()
  (id-index-new-id-threshold *constant-from-suid*))

(defun set-next-constant-suid (&optional max-constant-suid)
  (let ((max -1))
    (if max-constant-suid
        (setf max max-constant-suid)
        (do-id-index (idx constant (do-constants-table)
                          :progress-message "Determining maximum constant SUID"
                          ;; TODO - original code was in order, not sure why for a max scan
                          :ordered t)
          (let ((suid (constant-suid constant)))
            (setf max (max max suid)))))
    (let ((next-suid (1+ max)))
      (set-id-index-next-id *constant-from-suid* next-suid)
      next-suid)))


(declaim (inline reset-constant-suid))
(defun reset-constant-suid (constant new-suid)
  "[Cyc] Primitively change the SUID of CONSTANT to NEW-SUID."
  (setf (c-suid constant) new-suid)
  constant)

(defun register-constant-suid (constant suid)
  "[Cyc] Note that SUID will be used as the suid for CONSTANT."
  (reset-constant-suid constant suid)
  (id-index-enter *constant-from-suid* suid constant)
  constant)

(defun deregister-constant-suid (suid)
  "[Cyc] Note that SUID is not in use as a constant suid."
  (id-index-remove *constant-from-suid* suid))

(defun make-constant-suid ()
  "[Cyc] Return a new integer suid for a constant."
  (id-index-reserve *constant-from-suid*))

;; TODO - this doesn't seem like a good hash, see how it's used.  Better to add a cached hash slot to the constant object, if it's used often, and hash on the name string
(defmethod sxhash ((obj constant))
  (let ((suid (c-suid obj)))
    (if (integerp suid)
        suid
        0)))

(defun get-constant ()
  "[Cyc] Make a new constant shell, potentially in static space."
  (make-constant))

(declaim (inline init-constant))
(defun init-constant (constant)
  (setf (c-suid constant) nil)
  constant)

(declaim (inline free-constant))
(defun free-constant (constant)
  "[Cyc] Invalidate constant."
  (init-constant constant))

(defun valid-constant-handle? (constant)
  "[Cyc] Return T iff OBJECT is a valid constant handle."
  (and (constant-p constant)
       (constant-handle-valid? constant)))

(declaim (inline valid-constant?))
(defun valid-constant? (constant &optional robust)
  (declare (ignore robust))
  (valid-constant-handle? constant))

(defun invalid-constant-handle? (constant)
  (and (constant-p constant)
       (not (valid-constant-handle? constant))))

(defun invalid-constant? (constant &optional robust)
  (declare (ignore robust))
  (and (constant-p constant)
       (not (valid-constant? constant))))

(defglobal *invalid-constants* (make-hash-table :test #'equal))

(defun invalid-constant-names ()
  (hash-table-keys *invalid-constants*))

(defun find-invalid-constant (name)
  (gethash name *invalid-constants*))

(defun register-invalid-constant-by-name (constant name)
  (setf (gethash name *invalid-constants*) constant))

(defun deregister-invalid-constant-by-name (name)
  (remhash name *invalid-constants*))

(defun make-constant-shell (name &optional use-existing?)
  (check-type name 'constant-name-spec-p)
  (when (and use-existing? (stringp name))
    (or (constant-shell-from-name name)
        (find-invalid-constant name)
        (let ((constant (make-constant-shell-internal name t)))
          (when (stringp name)
            (register-invalid-constant-by-name constant name))
          constant))))

;; TODO - any reason this isn't setting NAME via the constructor?
(defun make-constant-shell-internal (name static)
  (declare (ignore static))
  (let ((constant (get-constant)))
    (setf (c-name constant) name)
    constant))

(defun reader-make-constant-shell (constant-name use-existing?)
  "[Cyc] Trampoline called by the #$ reader"
  (make-constant-shell constant-name use-existing?))

(defun create-sample-invalid-constant ()
  "[Cyc] Create a sample invalid constant."
  ;; wow, great docstring. such explain.
  (make-constant-shell-internal nil nil))

(defun free-all-constants ()
  (do-id-index (id constant (do-constants-table)
                   :progress-message "Freeing constants"
                   :ordered t)
    (free-term-index constant)
    (free-constant constant))
  (clear-constant-tables))

(declaim (inline constant-suid))
(defun constant-suid (constant)
  "[Cyc] Return the SUID of CONSTANT."
  (c-suid constant))

(defun install-constant-suid (constant suid)
  (declare (integer suid))
  (unless (integerp (constant-suid constant))
    (register-constant-suid constant suid))
  constant)

(declaim (inline find-constant-by-suid))
(defun find-constant-by-suid (suid)
  (declare (integer suid))
  (lookup-constant-by-suid suid))

(defun setup-constant-tables (size exact?)
  (setup-constant-guid-table size exact?)
  (set-constant-suid-table size exact?)
  (setup-constant-index-table size exact?))

(defun finalize-constants (&optional max-constant-suid)
  (finalize-constant-suid-table max-constant-suid))

(defun clear-constant-tables ()
  (clear-constant-guid-table)
  (clear-constant-suid-table))


