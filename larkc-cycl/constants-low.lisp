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


(defglobal *arete-constants-touched* (make-hash-table :test #'eq))
(defglobal *constant-guid-table* :uninitialized
    "[Cyc] ID -> constant-guid table")
(defglobal *constant-merged-guid-table* :uninitialized
    "[Cyc] ID -> merged constant-guid table.  Use for keeping track of merged guids for a Constant.")
(defglobal *constant-from-guid* nil
    "[Cyc] The GUID -> CONSTANT mapping table.")

(defun setup-constant-guid-table (size exact?)
  (let ((setup? nil))
    (unless (id-index-p *constant-guid-table*)
      (setf *constant-guid-table* (new-id-index size 0))
      (setf setup? t))
    (unless (id-index-p *constant-merged-guid-table*)
      (setf *constant-merged-guid-table* (new-id-index 750 0))
      (setf setup? t))
    (unless (hash-table-p *constant-from-guid*)
      (setf *constant-from-guid* (make-hash-table :size size :test #'equalp))
      (setf setup? t))
    setup?))

(defun lookup-constant-guid (id)
  (id-index-lookup *constant-guid-table* id))

(defun lookup-constant-merged-guid (id)
  (id-index-lookup *constant-merged-guid-table* id))

(defun lookup-constant-by-guid (guid)
  (gethash guid *constant-from-guid*))

(defun register-constant-guid (id constant-guid constant)
  "[Cyc] Note that ID will be used as the id for CONSTANT-GUID, and that the constant with guid CONSTANT-GUID is CONSTANT."
  (id-index-enter *constant-guid-table* id constant-guid)
  (setf (gethash constant-guid *constant-from-guid*) constant)
  constant-guid)

(defun deregister-constant-guid (id guid)
  "[Cyc] Note that ID is not in use as a CONSTANT id, i.e. no longer points to GUID."
  (id-index-remove *constant-guid-table* id)
  (remhash guid *constant-from-guid*))

(defun clear-constant-guid-table ()
  (clear-id-index *constant-guid-table*)
  (clear-id-index *constant-merged-guid-table*)
  (clrhash *constant-from-guid*))

(defun kb-create-constant-kb-store (name external-id)
  "[Cyc] Create a new constant named NAME with id EXTERNAL-ID. Return a SUID."
  (let ((constant (lookup-constant-by-guid external-id)))
    (if constant
        (constant-internal-id constant)
        (let ((suid (make-constant-suid))
              (constant (make-constant-shell name t)))
          (install-constant-suid constant suid)
          (when (stringp name)
            (deregister-invalid-constant-by-name name))
          (kb-create-constant-int constant name external-id)
          suid))))

(defun kb-create-constant-int (constant name external-id)
  (install-constant-external-id constant external-id)
  (when (stringp name)
    (add-constant-to-completions constant name)))

(defun install-constant-external-id (constant external-id)
  (let ((guid (if (constant-legacy-id-p external-id)
                  (missing-larkc 31626)
                  (and (guid-p external-id)
                       external-id))))
    (install-constant-guid constant guid)
    constant))

(defun kb-remove-constant-internal (constant)
  (let ((name (constant-name-internal constant)))
    (when (stringp name)
      (remove-constant-from-completions constant name)
      (deregister-invalid-constant-by-name name)))
  (deregister-constant-guts constant)
  (deregister-constant-ids constant)
  (free-constant constant))

(defun deregister-constant-guts (constant)
  (let ((suid (constant-suid constant)))
    (when (integerp suid)
      (deregister-constant-index suid)))
  constant)

(defun deregister-constant-ids (constant)
  "[Cyc] Remove all the id indexing to CONSTANT."
  (let ((guid (constant-guid constant)))
    (when (guid-p guid)
      (deregister-constant-guid (constant-suid constant) guid)))
  (let ((guid (constant-merged-guid constant)))
    (when (guid-p guid)
      (missing-larkc 32190)))
  (let ((suid (constant-suid constant)))
    (when (integerp suid)
      (deregister-constant-suid suid)))
  constant)

(defun constant-guid-internal (constant)
  (lookup-constant-guid (constant-suid constant)))

(defun constant-merged-guid-internal (constant)
  (lookup-constant-merged-guid (constant-suid constant)))

(defun constant-name-internal (constant)
  (c-name constant))

(defun constant-index (constant)
  "[Cyc] Return the indexing structure for CONSTANT."
  (let ((id (constant-suid constant)))
    (and id (lookup-constant-index id))))

(defun kb-rename-constant-internal (constant new-name)
  (when (kb-lookup-constant-by-name new-name)
    (error "A constant with the name ~s already exists." new-name))
  (let ((old-name (constant-name constant)))
    (when (stringp old-name)
      (remove-constant-from-completions constant old-name)
      (deregister-invalid-constant-by-name old-name)))
  (reset-constant-name constant new-name)
  (when (stringp new-name)
    (add-constant-to-completions constant new-name)
    (register-invalid-constant-by-name new-name constant))
  constant)

(defun reset-constant-name (constant new-name)
  "[Cyc] Primitively change the name of CONSTANT to NEW-NAME."
  (setf (c-name constant) new-name)
  constant)

(defun reset-constant-index (constant new-index)
  "[Cyc] Primitively change the assertion index for CONSTANT to NEW-INDEX."
  (register-constant-index (constant-suid constant) new-index)
  constant)

(defun install-constant-guid (constant guid)
  (unless (guid-p (constant-guid constant))
    (reset-constant-guid constant guid))
  constant)

(defun reset-constant-guid (constant new-guid)
  "[Cyc] Primitively change teh GUID of CONSTANT to NEW-GUID."
  (register-constant-guid (constant-suid constant) new-guid constant)
  constant)

(defun load-install-constant-ids (constant dump-id guid)
  "[Cyc] Install GUID for COSNTANT with DUMP-ID in a KB load."
  (let ((name (constant-name-internal constant))
        (suid dump-id))
    (install-constant-suid constant suid)
    (deregister-invalid-constant-by-name name)
    (kb-create-constant-int constant name guid)
    constant))


