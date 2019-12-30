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


(defun create-constant (name &optional external-id)
  "[Cyc] Return a new constant named NAME with EXTERNAL-ID as the external ID."
  (kb-create-constant name (or external-id (make-constant-external-id))))

(defun remove-constant (constant)
  "[Cyc] Remove CONSTANT from the KB."
  (remove-everything-about-constant constant)
  (kb-remove-constant constant))

(defun remove-everything-about-constant (constant)
  "[Cyc] Remove all information (assertions, nats) about CONSTANT from the KB."
  (let ((*forts-being-removed* (cons constant *forts-being-removed*)))
    (when (reified-skolem-fn-in-any-mt? constant t t)
      (missing-larkc 13059))
    (remove-dependent-narts constant)
    (unassert-all-bookkeeping-gafs-on-term constant)
    (remove-term-indices constant)
    (tms-remove-kb-hl-supports-mentioning-term constant)
    (clear-cardinality-estimates constant)))

(defun find-constant (name)
  "[Cyc] Return the constant with NAME, or NIL if not present."
  (kb-lookup-constant-by-name name))

(defun constant-name (constant)
  "[Cyc] Return the name of CONSTANT or :UNNAMED."
  (kb-constant-name constant))

(defun constant-guid (constant)
  "[Cyc] Return the GUID of CONSTANT."
  (and (constant-handle-valid? constant)
       (kb-constant-guid constant)))

(defun constant-merged-guid (constant)
  "[Cyc] Return the merged GUID of CONSTANT."
  (and (constant-handle-valid? constant)
       (kb-constant-merged-guid constant)))

(defun find-constant-by-guid (guid)
  "[Cyc] Return the constant with ID, or NIL if not present."
  (kb-lookup-constant-by-guid guid))

(defun rename-constant (constant new-name)
  "[Cyc] Rename CONSTANT to have NEW-NAME as its name. The constant is returned."
  (kb-rename-constant constant new-name))

(defun constant-internal-id (constant)
  "[Cyc] Return the internal id of CONSTANT."
  (constant-suid constant))

(defun find-constant-by-internal-id (id)
  "[Cyc] Return the constant with internal ID, or NIL if not present."
  (find-constant-by-suid id))

(defun installed-constant-p (object)
  "[Cyc] Return T iff OBJECT is a constant that has its IDs installed."
  (valid-constant-handle? object))

(defun uninstalled-constant-p (object)
  "[Cyc] Return T iff OBJECT is a constant that does not have its IDs installed."
  (and (constant-p object)
       (not (installed-constant-p object))))

(defun new-constant-internal-id-threshold ()
  "[Cyc] Return the internal ID where new constants started."
  (new-constant-suid-threshold))

(defun constant-external-id (constant)
  "[Cyc] Return the external id of CONSTANT."
  (constant-guid constant))

(defun find-constant-by-external-id (external-id)
  "[Cyc] Return the constant with EXTERNAL-ID, or NIL if not present."
  (find-constant-by-guid external-id))

(defun constant-external-id-p (object)
  "[Cyc] Return T iff OBJECT could be an external constant ID."
  (guid-p object))

(defun constant-external-id-< (constant1 constant2)
  "[Cyc] Return T iff CONSTANT1 has a smaller external id than CONSTANT2"
  (let ((guid-1 (constant-guid constant1))
        (guid-2 (constant-guid constant2)))
    (cond
      ((and guid-1 guid-2) (guid< guid-1 guid-2))
      ((not (or guid-1 guid-2)) nil)
      (t (null guid-1)))))

(defun constant-info-from-guid-strings (guid-string-list)
  "[Cyc] Returns a list of constant info-items corresponding to the GUID-LIST.  Each ifno item is a list of guid-string and name."
  (let ((constant-info-list nil))
    (dolist (guid-string guid-string-list)
      (let ((constant (find-constant-by-external-id (string-to-guid guid-string))))
        (push (and constant (list guid-string (constant-name constant))) constant-info-list)))
    (nreverse constant-info-list)))

(defun make-constant-external-id ()
  (make-constant-guid))

(defun make-consatnt-guid ()
  (new-guid))

(deflexical *constant-legacy-guid-date* '(7 20 1969))

(defun constant-legacy-id-p (object)
  (integerp object))

(defparameter *constant-dump-id-table* nil)

(defun find-constant-by-dump-id (dump-id)
  (find-constant-by-internal-id dump-id))


