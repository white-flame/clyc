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


(deflexical *kb-arity-table-equality-test* #'eq
  "[Cyc] The equality test used for the KB arity tables.")

(defglobal *kb-arity-table* nil)

(defun* arity-lookup (relation) (:inline t)
  (gethash relation *kb-arity-table*))

(defun* set-arity (relation arity) (:inline t)
  (setf (gethash relation *kb-arity-table*) arity))

(defun* rem-arity (relation) (:inline t)
  (remhash relation *kb-arity-table*))

(defun arity (relation)
  (cond
    ((fort-p relation) (arity-lookup relation))
    ((reifiable-nat? relation #'cyc-var? *anect-mt*) (missing-larkc 10320))
    ((kappa-predicate-p relation) (missing-larkc 30572))
    ((lambda-function-p relation) (missing-larkc 30574))))

(defun* possibly-simplify-arity (arity) (:inline t)
  arity)

(defun maybe-add-arity-for-relation (relation arity)
  (setf arity (possibly-simplify-arity arity))
  (let ((arity-in-table (arity relation)))
    (when (and arity-in-table
               (not (eql arity-in-table arity)))
      (error "Trying to overload arity for ~a from ~a to ~a" relation arity-in-table arity))
    (set-arity relation arity)))

(defun maybe-remove-arity-for-relation (relation arity)
  (let ((dont-remove nil)
        (other-arity nil)
        (pred-var #$arity))
    (kmu-do-index-iteration (assertion gaf-arg (relation 1 pred-var) (:gaf :true nil)
                                       :done-place dont-remove)
      (let ((asserted-arity (gaf-arg2 assertion)))
        (if (= arity asserted-arity)
            (setf dont-remove (assertion-still-there? assertion :true))
            (setf other-arity asserted-arity))))
    (unless dont-remove
      (rem-arity relation))
    (when other-arity
      (set-arity relation other-arity))))

(defglobal *kb-arity-min-table* nil)

(defun* arity-min-lookup (relation) (:inline t)
  (gethash relation *kb-arity-min-table*))

(defun arity-min (relation)
  "[Cyc] Return the arity-min for RELATION."
  (or (arity-min-int relation)
      0))

(defun arity-min-int (relation)
  (cond
    ((fort-p relation)
     (or (arity-min-lookup relation)
         (missing-larkc 12071)))

    ((reifiable-nat? relation #'cyc-var? *anect-mt*)
     (missing-larkc 10321))))

(defglobal *kb-arity-max-table* nil)

(defun* arity-max-lookup (relation) (:inline t)
  (gethash relation *kb-arity-max-table*))

(defun arity-max (relation)
  "[Cyc] Return the arityMax for RELATION."
  (cond
    ((fort-p relation)
     (or (arity-max-lookup relation)
         (initialize-arity-max-for-relation relation)))

    ((reifiable-nat? relation #'cyc-var? *anect-mt*)
     (missing-larkc 10322))))

(defun initialize-arity-max-for-relation (relation)
  (and (fpred-value-in-any-mt relation #$arityMax)
       (missing-larkc 12068)))

(defun* binary? (relation) (:inline t)
  (eql (arity relation) 2))

(defun binary-arg-swap (arg)
  (case arg
    (1 2)
    (2 1)
    (otherwise arg)))

(defun variable-arity? (relation)
  (isa-variable-arity-relation? relation *anect-mt*))

(defun* arity-cache-unbuilt? () (:inline t)
  (not *kb-arity-table*))

(defun load-arity-cache-from-stream (stream)
  (setf *kb-arity-table* (cfasl-input stream))
  (setf *kb-arity-min-table* (cfasl-input stream))
  (setf *kb-arity-max-table* (cfasl-input stream))
  (cfasl-input stream))
