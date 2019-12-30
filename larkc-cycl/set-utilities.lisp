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


(defun set-union (set-list &optional (test #'eql))
  "Returns a new set that is the union of the sets in the given list."
  (cond
    ((null set-list) (new-set :test test))
    ((singleton? set-list) (copy-set-contents (car set-list)))
    ;; TODO DESIGN - it could be better to copy the test of the 1st set, rather than having to remember to specify it.
    (t (let ((union (make-hash-table :test test)))
         (dolist (set set-list union)
           (dohash (key val set)
             (setf (gethash key union) val)))))))

(defun set-intersection (set-list &optional (test #'eql))
  (cond
    ((null set-list) (new-set :test test))
    ((singleton? set-list) (copy-set-contents (car set-list)))
    (t (let* ((smallest (extremal set-list #'< #'set-contents-size))
              (other-sets (remove smallest set-list))
              (intersection (make-hash-table :test test)))
         (dohash (key val smallest)
           (declare (ignore val))
           (when (every (lambda (set) (gethash key set)) other-sets)
             (setf (gethash key intersection) t)))
         intersection))))

(defun construct-set-from-list (list &optional (test #'eql) (size (length list)))
  "[Cyc] Returns a set-contents object constructed from the objects in LIST."
  (let ((set (make-hash-table :test test :size size)))
    (dolist (item list set)
      (setf (gethash item set) t))))

(defun set-add-all (elements set)
  (dolist (element elements set)
    (setf (gethash element set) t)))

