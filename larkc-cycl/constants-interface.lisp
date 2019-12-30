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


(defun kb-create-constant (name external-id)
  "[Cyc] Return a new constant named NAME with EXTERNAL-ID.
If NAME is :UNNAMED, returns a constant with no name."
  (define-hl-modifier-preamble)
  (note-hl-modifier-invocation 'kb-create-constant name external-id)
  (when (hl-modify-anywhere?)
    (bt:with-lock-held (*hl-lock*)
      (prog1 (if (hl-modify-remote?)
                 (missing-larkc 32155)
                 (kb-create-constant-local name external-id))
        (define-hl-modifier-postamble)))))

(defun kb-create-constant-local (name external-id)
  (find-constant-by-internal-id (kb-create-constant-kb-store name external-id)))

(defun kb-remove-constant (constant)
  "[Cyc] Remove CONSTANT from the KB."
  (let ((result nil))
    (define-hl-modifier-preamble)
    (note-hl-modifier-invocation 'kb-remove-constant constant)
    (when (hl-modify-remote?)
      (setf result (missing-larkc 29543)))
    (if (hl-modify-local?)
        (let ((*override-hl-store-remote-access?* t))
          (bt:with-lock-held (*hl-lock*)
            (kb-remove-constant-internal constant)))
        result)))

(defun kb-lookup-constant-by-name (name)
  "[Cyc] Return the constant named NAME, if it exists.  Return NIL otherwise."
  (if (hl-access-remote?)
      (missing-larkc 29544)
      (constant-shell-from-name name)))

(defun kb-constant-name (constant)
  "[Cyc] Return the name for CONSTANT."
  (if (hl-access-remote?)
      (missing-larkc 29545)
      (constant-name-internal constant)))

(defun kb-lookup-constant-by-guid (guid)
  "[Cyc] Return the constant with GUID, if it exists. Return NIL otherwise."
  (if (hl-access-remote?)
      (missing-larkc 29546)
      (lookup-constant-by-guid guid)))

(defun kb-constant-guid (constant)
  "[Cyc] Return the external ID for CONSTANT."
  (if (hl-access-remote?)
      (missing-larkc 29548)
      (constant-merged-guid-internal constant)))

(defun kb-rename-constant (constant new-name)
  "[Cyc] Rename CONSTANT to have NEW-NAME as its name.  The constant is returned."
  (let ((result nil))
    (define-hl-modifier-preamble)
    (note-hl-modifier-invocation 'kb-rename-constant constant new-name)
    (when (hl-modify-remote?)
      (setf result (missing-larkc 29549)))
    (if (hl-modify-local?)
        (let ((*override-hl-store-remote-access?* t))
          (bt:with-lock-held (*hl-lock*)
            ;; TODO - this was stored to an unused variable, "old_name"
            (constant-name constant)
            (kb-rename-constant-internal constant new-name)))
        result)))

