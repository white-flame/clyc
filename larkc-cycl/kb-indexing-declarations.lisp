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


(deflexical *default-intermediate-index-equal-test* #'eq)
(defglobal *kb-indexing-declaration-store* (make-hash-table :test #'eq))

(defun* kb-indexing-declaration-store () (:inline t)
  *kb-indexing-declaration-store*)

(defun* add-index-to-kb-indexing-declaration-store (index plist) (:inline t)
  (setf (gethash index *kb-indexing-declaration-store*) plist))

(defun* get-index-from-kb-indexing-declaration-store (index) (:inline t)
  (gethash index *kb-indexing-declaration-store*))

(defun find-index-by-top-level-key (top-level-key)
  "[Cyc] Returns the index with a top-level key of TOP-LEVEL-KEY."
  (let ((index (get-index-from-kb-indexing-declaration-store top-level-key)))
    (if (and index
             (eq top-level-key (get-index-prop index :top-level-key)))
        index
        (dohash (index plist (kb-indexing-declaration-store))
          (when (eq top-level-key (get-index-prop index :top-level-key))
            (return index))))))

(defun* get-index-key-prop (key-info indicator &optional default) (:inline t)
  (getf key-info indicator default))

(defun* get-index-prop (index indicator) (:inline t)
  (getf (get-index-from-kb-indexing-declaration-store index) indicator))

(defun* declare-index (index plist) (:inline t)
  "[Cyc] See below for an explanation of what fields go in the plist, what they mean, and a bunch of examples."
  (add-index-to-kb-indexing-declaration-store index plist))

(defun index-equality-test-for-keys (keys)
  "[Cyc] Return the test appropriate for distinguishing the last key in KEYS.
KEYS: a list of keys, starting from the top level."
  (destructuring-bind (top-level-key . rest-keys) keys
    (let ((index (find-index-by-top-level-key top-level-key)))
      (must index
            "Could not find an index with top-level key ~s" top-level-key)
      (let* ((key-info-list (get-index-prop index :keys))
             (levels-deep (length rest-keys))
             (key-info-for-this-level (nth levels-deep key-info-list))
             (equal-test (get-index-key-prop key-info-for-this-level :equal-test
                                             *default-intermediate-index-equal-test*)))
        equal-test))))
