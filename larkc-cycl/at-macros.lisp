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

(defun make-defn-fn-history-table ()
  (make-hash-table :size *defn-fn-history-default-size*))

(defun make-defn-col-history-table ()
  (make-hash-table :size *defn-col-history-default-size*))

(defun* possibly-make-defn-fn-history-table ()
    (:inline t)
  (let ((val *defn-fn-history*))
    (if (uninitialized-p val)
        (make-defn-fn-history-table)
        val)))

(defun* possibly-make-quoted-defn-fn-history-table () (:inline t)
  (possibly-make-defn-fn-history-table))

(defun* possibly-make-defn-col-history-table () (:inline t)
  (let ((val *defn-col-history*))
    (if (uninitialized-p val)
        (make-defn-col-history-table)
        val)))

(defun* possibly-make-quoted-defn-col-history-table () (:inline t)
  (possibly-make-defn-col-history-table))

