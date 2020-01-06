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


(defconstant *valid-hash-test-symbols* '(eq eql equal equalp)
  "[Cyc] All function symbols which are permitted tests for hashtable-based algorithms.")
(defconstant *valid-hash-test-functions* (list #'eq #'eql #'equal #'equalp)
  "[Cyc] All functions which are permitted test for hashtable-based algorithms.")

(defun valid-hash-test-symbols ()
  *valid-hash-test-symbols*)

(defun hash-test-to-symbol (test)
  "[Cyc] Return the symbol form of TEST, which is a valid hash-test function."
  (check-type test 'valid-hash-test-p)
  (if (symbolp test)
      test
      (find test *valid-hash-test-symbols* :key #'symbol-function)))

(defun hash-table-empty-p (table)
  "[Cyc] Return T iff TABLE is an empty hashtable"
  (zerop (hash-table-count table)))

(defun rehash (table)
  "[Cyc] Rehash every KEY VALUE pair in the hashtable TABLE."
  ;; Relying on CL doing a good job on its own
  table)

(defun push-hash (key item table)
  (push item (gethash key table)))

(defun pop-hash (key table)
  "[Cyc] Pops off the first element of the value of KEY in TABLE. More precisely, returns the first element of the value of KEY in TABLE, and sets that value to be the rest of itself."
  (pop (gethash key table)))

(defun delete-hash (key item table &optional (test #'eql) (test-key #'identity))
  (setf (gethash key table)
        (delete item (gethash key table) :test test :key test-key)))

(defun hash-table-keys (hash-table)
  "[Cyc] Return a list of all the keys of HASH-TABLE."
  (loop for key being the hash-key of hash-table
     collect key))

(defun hash-table-values (hash-table)
  (loop for val being the hash-value of hash-table
       collect val))
