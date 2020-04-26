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


(defun* fort-p (object) (:inline t)
  "[Cyc] Return T iff OBJECT is a first order reified term (FORT)."
  (or (constant-p object)
      (nart-p object)))

(defun* non-fort-p (object) (:inline t)
  (not (fort-p object)))

(defun fort-count ()
  "[Cyc] Return the total number of FORTs."
  (+ (constant-count) (nart-count)))

(defun reset-fort-index (fort new-index)
  "[Cyc] Primitively change the assertion index for FORT to NEW-INDEX."
  (if (constant-p fort)
      (reset-constant-index fort new-index)
      (missing-larkc 208)))

(defun valid-fort? (fort)
  "[Cyc] Return T if FORT is a valid FORT."
  (cond
    ((constant-p fort) (valid-constant? fort))
    ((nart-p fort) (missing-larkc 30880))
    (t nil)))

(defun remove-fort (fort)
  "[Cyc] Remove FORT from the KB."
  (if (constant-p fort)
      (missing-larkc 10431)))

(defstruct fort-id-index
  constants
  narts)

(defun new-fort-id-index ()
  "[Cyc] Constructor"
  (make-fort-id-index :constants (new-fort-id-index-constants)
                      :narts (new-fort-id-index-narts)))

(defun fort-id-index-lookup (fort-id-index fort)
  "[Cyc] Accessor. Returns the object associated with FORT in FORT-ID-INDEX."
  (when (valid-fort? fort)
    (let ((id-index (if (constant-p fort)
                        (fort-id-index-constants fort-id-index)
                        (missing-larkc 23142)))
          (id (if (constant-p fort)
                  (constant-internal-id fort)
                  (missing-larkc 30869))))
      (id-index-lookup id-index id))))

(defun fort-id-index-enter (fort-id-index fort object)
  "[Cyc] Modifier. Enter OBJECT in FORT-ID-INDEX as the object associated with FORT."
  (when (valid-fort? fort)
    (let ((id-index (if (constant-p fort)
                        (fort-id-index-constants fort-id-index)
                        (missing-larkc 23143)))
          (id (if (constant-p fort)
                  (constant-internal-id fort)
                  (missing-larkc 30870))))
      (id-index-enter id-index id object))))

(defun fort-id-index-remove (fort-id-index fort)
  "[Cyc] Modifier. Remove all FORT associations in FORT-ID-INDEX."
  (when (valid-fort? fort)
    (let ((id-index (if (constant-p fort)
                        (fort-id-index-constants fort-id-index)
                        (missing-larkc 23144)))
          (id (if (constant-p fort)
                  (constant-internal-id fort)
                  (missing-larkc 30871))))
      (id-index-remove id-index id))))

(defun new-fort-id-index-constants ()
  (new-id-index (new-constant-internal-id-threshold)))

(defun new-fort-id-index-narts ()
  (new-id-index (new-nart-id-threshold)))

(defconstant *cfasl-opcode-fort-id-index* 99)

(defun cfasl-input-fort-id-index (stream)
  (let ((fort-id-index (new-fort-id-index))
        (count (cfasl-input stream)))
    (dotimes (i count)
      (let ((fort (cfasl-input-object stream))
            (value (cfasl-input-object stream)))
        (fort-id-index-enter fort-id-index fort value)))
    fort-id-index))
