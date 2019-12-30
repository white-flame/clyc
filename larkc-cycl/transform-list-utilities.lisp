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


(defparameter *default-recursion-limit* 212)
(defparameter *default-transformation-max* nil)
(defparameter *default-quiescent-transformation-max* 1024)

(defun transform-pred-funcall (pred object)
  (possibly-cyc-api-funcall pred object))

(defun transform-transform-funcall (transform object)
  (possibly-cyc-api-funcall transform object))

(defun transform (object pred transform &optional (key #'identity))
  "[Cyc] Recursively descends through OBJECT, destructively applying TRANSFORM when PRED succeeds.
Make sure that the results of TRANSFORM will not succeed on PRED, otherwise it may 'infinitely' recurse.
example: (transform '(1 \"2\" (3 4 \"5\")) #'stringp 'read-from-string) -> (1 2 (3 4 5))"
  (ntransform (copy-tree object) pred transform key))

(defun ntransform (object pred transform &optional
                                           (key #'identity)
                                           (recursion-limit *default-recursion-limit*)
                                           (transformation-max *default-transformation-max*))
  "[Cyc] A destructive version of TRANSFORM.
OBJECT: The object to be transformed.
PRED: A predicate that returns true if an object is to be transformed by TRANSFORM.
KEY: A function that maps an object to a value that is to be transformed.
RECURSION-LIMIT: The maximum depth that the algorith mis allowed to recurse. Once this limit is exceeded, the transformation continues by means of an iterative algorithm.
TRANSFORMATION-MAX: The maximum number of transformations to be performed before throwing :TRANSFORMATION-LIMIT-EXCEEDED. If this parameter is given NIL as a value, no limit is imposed on the number of transformations."
  (if transformation-max
      (missing-larkc 7713)
      (ntransform-recursive object pred transform key recursion-limit 0)))

(defun ntransform-recursive (object pred transform key recursion-limit recursion-level)
  "[Cyc] A destructive recursive version of TRANSFORM.
This function transforms as it iterates down the CDR of transformations and recurses as it transforms their CARs.  When the recursion limit is reached, it switches to a purely iterative algorithm by calling NTRANSFORM-ITERATIVE."
  (declare (fixnum recursion-level
                   recursion-limit))
  (if (>= recursion-level recursion-limit)
      (missing-larkc 7700)
      (let* ((initial-transformed-object (ntransform-perform-transform object pred transform key))
             (transformed-list-tail initial-transformed-object))
        (until (atom transformed-list-tail)
          (rplaca transformed-list-tail (ntransform-recursive (car transformed-list-tail) pred transform key recursion-limit (1+ recursion-level)))
          (rplacd transformed-list-tail (ntransform-perform-transform (cdr transformed-list-tail) pred transform key)))
        initial-transformed-object)))


(defun ntransform-perform-transform (object pred transform &optional (key #'identity))
  (if (or (eq #'identity key)
          (eq 'identity key))
      ;; identity-optimized version
      (let ((previous-transformation object))
        (do ((transformation (if (transform-pred-funcall pred object)
                                 (copy-tree (transform-transform-funcall transform object))
                                 object)
                             (if (transform-pred-funcall pred transformation)
                                 (copy-tree (transform-transform-funcall transform transformation))
                                 transformation)))
            ((eq previous-transformation transformation)
             transformation)
          (setf previous-transformation transformation)))
      ;; key-calling version
      (let ((previous-transformation object))
        (do ((transformation (if (transform-pred-funcall pred object)
                                 (copy-tree (transform-transform-funcall transform (missing-larkc 7716)))
                                 object)
                             (if (transform-pred-funcall pred transformation)
                                 (copy-tree (transform-transform-funcall transform (missing-larkc 7717)))
                                 transformation)))
            ((eq previous-transformation transformation)
             transformation)
          (setf previous-transformation transformation)))))

(defun quiescent-transform (object pred transform &optional (key #'identity) (quiescence #'equal))
  (quiescent-ntransform (copy-tree object) pred transform key quiescence))

(defun quiescent-ntransform (object pred transform &optional
                                                     (key #'identity)
                                                     (quiescence #'equal)
                                                     (recursion-limit *default-recursion-limit*)
                                                     (transformation-max *default-quiescent-transformation-max*))
  "[Cyc] Calls QUIESCENT-NTRANSFORM-RECURSIVE to iteratively transfrom object and then its transformation using TRANSFORM so long as PRED succeeds and the application of QUIESCENCE to the transformation of the object fails. Upon finishign a series of transformations, QUIESCENT-NTRANSFORM-RECURSIVE is then recursively applied to the CAR of teh transformation while successive CDRs of the transformation are transfromed according to the same algorithm.  If, in the process of transformation, the recursion limit RECURSION-LIMIT is reached, then QUIESCENT-NTRANSFORM-ITERATIVE is called to solve the subtransformation without recursion.
RECURSION-LIMIT: The maximum depth that the algorithm is allowed to recurse. Once this limit is exceeded, the transformation continues by means of an iterative algorithm.
TRANSFORMATION-MAX: The maximum number of transformations to be performed before throwing :TRANSFORMATION-LIMIT-EXCEEDED. If this parameter is given NIL as a value, no limit is imposed on the number of transformations."
  (if transformation-max
      (shy-quiescent-ntransform-recursive object pred transform key quiescence
                                          recursion-limit 0 transformation-max 0)
      (missing-larkc 7706)))

(defun shy-quiescent-ntransform-recursive (object pred transform key quiescence
                                           recursion-limit recursion-level
                                           transformation-max transformation-count)
  "[Cyc] See documentation for QUIESCENT-NTRANSFORM."
  (if (>= recursion-level recursion-limit)
      (missing-larkc 7715)
      (multiple-value-bind (initial-transformed-object new-transformation-count)
          (shy-ntransform-perform-quiescent-transform object pred transform key quiescence
                                                      transformation-max transformation-count)
        (let ((transformed-list-tail initial-transformed-object)
              (transformed-object nil))
          (until (atom transformed-list-tail)
            (multiple-value-bind (obj count) (shy-quiescent-ntransform-recursive
                                              (car transformed-list-tail) pred transform key
                                              quiescence recursion-limit (1+ recursion-level)
                                              transformation-max new-transformation-count)
              (setf transformed-object obj)
              (setf new-transformation-count count))
            (rplaca transformed-list-tail transformed-object)
            (multiple-value-bind (obj count) (shy-ntransform-perform-quiescent-transform
                                              (cdr transformed-list-tail) pred transform key
                                              quiescence
                                              transformation-max new-transformation-count)
              (setf transformed-object obj)
              (setf new-transformation-count count)))
          (values initial-transformed-object new-transformation-count)))))

(defun shy-ntransform-perform-quiescent-transform (object pred transform key quiescence
                                                   transformation-max transformation-count)
  ;; 4 versions, for optimizing default values of key & quiescence
  (if (or (eq #'identity key)
          (eq 'identity key))
      (if (or (eq #'equal quiescence)
              (eq 'equal quiescence))
          ;; key = identity, quiescence = equal
          (let ((previous-transformation object))
            (do ((transformation (if (transform-pred-funcall pred object)
                                     (copy-tree (transform-transform-funcall transform object))
                                     object)
                                 (if (transform-pred-funcall pred transformation)
                                     (copy-tree (transform-transform-funcall transform transformation))
                                     transformation)))
                ((or (eq previous-transformation transformation)
                     (equal previous-transformation transformation))
                 (values transformation transformation-count))
              (incf transformation-count)
              (setf previous-transformation transformation)))
          ;; key = identity
          (let ((previous-transformation object))
            (do ((transformation (if (transform-pred-funcall pred object)
                                     (copy-tree (transform-transform-funcall transform object))
                                     object)
                                 (if (transform-pred-funcall pred object)
                                     (copy-tree (transform-transform-funcall transform transformation))
                                     transformation)))
                ((or (eq previous-transformation transformation)
                     (missing-larkc 7722))
                 (values transformation transformation-count))
              (when (>= transformation-count transformation-max)
                (throw :transformation-limit-exceeded
                  (list :transformation-limit-exceeded transformation-count transformation-max)))
              (incf transformation-count)
              (setf previous-transformation transformation))))
      (if (or (eq #'equal quiescence)
              (eq 'equal quiescence))
          ;; quiescence = equal
          (let ((previous-transformation object))
            (do ((transformation (if (transform-pred-funcall pred object)
                                     (copy-tree (transform-transform-funcall
                                                 transform (missing-larkc 7718)))
                                     object)
                                 (if (transform-pred-funcall pred transformation)
                                     (copy-tree (transform-transform-funcall
                                                 transform (missing-larkc 7719)))
                                     transformation)))
                ((or (eq previous-transformation transformation)
                     (equal previous-transformation transformation))
                 (values transformation transformation-count))
              (when (>= transformation-count transformation-max)
                (throw :transformation-limit-exceeded
                  (list :transformation-limit-exceeded transformation-count transformation-max)))
              (incf transformation-count)
              (setf previous-transformation transformation)))
          ;; No optimization
          (let ((previous-transformation object))
            (do ((transformation (if (transform-pred-funcall pred object)
                                     (copy-tree (transform-transform-funcall
                                                 transform (missing-larkc 7720)))
                                     object)
                                 (if (transform-pred-funcall pred transformation)
                                     (copy-tree (transform-transform-funcall
                                                 transform (missing-larkc 7721)))
                                     transformation)))
                ((or (eq previous-transformation transformation)
                     (missing-larkc 7723)))
              (when (>= transformation-count transformation-max)
                (throw :transformation-limit-exceeded
                  (list :transformation-limit-exceeded transformation-count transformation-max)))
              (incf transformation-count)
              (setf previous-transformation transformation))))))
