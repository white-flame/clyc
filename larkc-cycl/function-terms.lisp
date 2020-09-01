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


(defun term-of-unit-assertion-p (object)
  (when (gaf-assertion? object)
    (eq #$termOfUnit (gaf-predicate object))))

(defun nat-formula-p (object)
  "[Cyc] Return T iff OBJECT could be interpreted as a nat formula."
  (possibly-naut-p object))

(defun* tou-assertion? (assertion) (:obsolete t)
  "[Cyc] Obsolete."
  (term-of-unit-assertion-p assertion))

(defun term-functional-complexity (object)
  "[Cyc] Return the maximum functional nesting depth of OBJECT."
  (with-all-mts
    (term-functional-complexity-internal object)))

(defpolymorphic term-functional-complexity-internal (object)
  0)

(defmethod term-functional-complexity-internal ((object constant))
  0)

(defmethod term-functional-complexity-internal ((object nart))
  (missing-larkc 10746))

(defmethod term-functional-complexity-internal ((object cons))
  "[Cyc] Fancy way of returning max term functional complexity within a NART."
  (destructuring-bind (function &rest args) object
    (if (and (fort-p function)
             (not (non-predicate-function? function)))
        0
        (let ((arg-max-complexity (term-functional-complexity-internal function)))
          (dolist (arg args)
            (let ((arg-complexity (term-functional-complexity-internal arg)))
              (setf arg-max-complexity (max arg-max-complexity arg-complexity))))
          (1+ arg-max-complexity)))))

;; TODO - no default implementation, only specialized methods?  so defgeneric instead of defpolymorphic for here
(defgeneric term-relational-complexity-internal (object))

(defmethod term-relational-complexity-internal ((object constant))
  0)

(defmethod term-relational-complexity-internal ((object nart))
  (missing-larkc 10766))

(defmethod term-relational-complexity-internal ((object cons))
  (missing-larkc 10764))

(defun naut-to-nart (obj)
  "[Cyc] If OBJ is a ground NAUT (EL nat), convert it to an HL nart and return it, else return OBJ."
  (if (possibly-naut-p obj)
      (or (find-nart obj)
          obj)
      obj))
