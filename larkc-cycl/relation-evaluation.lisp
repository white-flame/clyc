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


(defun evaluatable-function? (function)
  "[Cyc] Admits more than evaluatable-function-p."
  (or (and (fort-p function)
           (evaluatable-function-p function))
      (function-to-arg-function-p function)
      (lambda-function-p function)))

(defun evaluatable-predicate? (predicate &optional mt)
  (and (fort-p predicate)
       (evaluatable-predicate-p predicate mt)))

(defun evaluatable-relation? (relation)
  (with-all-mts
    (or (evaluatable-function? relation)
        (evaluatable-predicate? relation))))

(defun evaluatable-expression? (object)
  (and (el-formula-p object)
       (missing-larkc 30349)))

(defun evaluation-defn (fort &optional mt)
  (cycl-subl-symbol-symbol (fpred-value-in-relevant-mts fort #$evaluationDefn mt)))

(defparameter *cyc-evaluate-gather-justifications?* nil)

(defparameter *cyc-evaluate-supports* nil)

(defun* cyc-evaluate-gathering-justifications? () (:inline t)
  *cyc-evaluate-gather-justifications?*)

(defun note-evaluation-function-support (relation evaluation-function)
  (declare (ignore relation evaluation-function))
  (when (cyc-evaluate-gathering-justifications?)
    (missing-larkc 30022)))

(defparameter *cyc-evaluate-relation* nil
  "[Cyc] This is bound to the current relation being evaluated.")

(defparameter *cyc-evaluate-some-contextualized-relation?* nil ;; TODO - was set to :UNBOUND in java, but this is used in boolean truth tests?
  "[Cyc] Bound to T when a contextualized relation is evaluated.")

(defun possibly-note-contextualized-evaluatable-relation (relation)
  "[Cyc] Unless we've already found one, check if RELATION is contextualized, and note if it is."
  (or *cyc-evaluate-some-contextualized-relation?*
      (setf *cyc-evaluate-some-contextualized-relation?* (evaluatable-relation-contextualized-p relation))))

(defun cyc-evaluate (expression)
  "[Cyc] Evaluate the evaluatable EXPRESSION and return the result.
A second returned value is T iff the result was valid.
A third returned value is T iff the evaluation included evaluation of a contextualized relation."
  (let ((answer nil)
        (unevaluatable nil)
        (contextualized? nil)
        (*cyc-evaluate-some-contextualized-relation?* nil))
    (let ((unevaluatable (catch :unevaluatable
                           (setf answer (cyc-evaluate-internal expression t))
                           (setf contextualized? *cyc-evaluate-some-contextualized-relation?*)
                           ;; Non-caught value
                           nil)))
      (if unevaluatable
          (missing-larkc 30334)
          (valid-evaluation answer contextualized?)))))

(defun* valid-evaluation (answer contextualized?) (:inline t)
  (values answer t contextualized?))

(defun cyc-evaluate-internal (formula consider-expansion?)
  (let ((relation (formula-operator formula)))
    (unless (evaluatable-relation? relation)
      (missing-larkc 30342))
    (let ((evaluation-function (evaluation-function relation)))
      (unless (or (possibly-cyc-api-function-spec-p evaluation-function)
                  (and (consider-expansion?)
                       (missing-larkc 8857)))
        (missing-larkc 30343))
      (let ((input-args (formula-args formula))
            (args (cyc-evaluate-args input-args))
            (answer nil))
        (possibly-note-contextualized-evaluatable-relation relation)
        (if (possibly-cyc-api-function-spec-p evaluation-function)
            (progn
              (let ((*cyc-evaluate-relation* relation))
                (let ((arity (evaluation-arity relation)))
                  (if (not arity)
                      (setf answer (possibly-cyc-api-funcall-1 evaluation-function args))
                      (progn
                        (unless (= arity (length args))
                          (missing-larkc 30344))
                        (missing-larkc 10864)))))
              (if (equal input-args args)
                  (note-evaluation-function-support relation evaluation-function)
                  (missing-larkc 30337))
              answer)
            (missing-larkc 4358))))))

(defun* cyc-evaluate-args (args) (:inline t)
  "[Cyc] Recursively evaluate any evaluatable ARGS."
  (mapcar #'cyc-evaluate-args args))

(defun* cyc-evaluate-arg (input-arg) (:inline t)
  (cyc-evaluate-if-evaluatable input-arg t))

(defun cyc-evaluate-if-evaluatable (expression &optional consider-expansion?)
  "[Cyc] Recursively evaluate EXPRESSION if needed."
  (if (not (evaluatable-expression? expression))
      expression
      (let ((evaluated-expression nil))
        (let ((*cyc-evaluate-gather-justifications?* nil))
          (setf evaluated-expression (cyc-evaluate-internal expression consider-expansion?)))
        (missing-larkc 30335))))

(defun evaluation-function (relation)
  (cond
    ((fort-p relation) (cached-evaluation-function relation))
    ((function-to-arg-function-p relation) 'cyc-function-to-arg) ;; TODO - symbol for function?
    ((lambda-function-p relation) 'cyc-lambda))) ;; TODO - symbol for function?

(defun evaluation-arity (relation)
  "[Cyc] Return the expected evaluation arity for RELATION."
  (cond
    ((fort-p relation) (arity-relation))
    ((function-to-arg-function-p relation) (missing-larkc 29793))
    ((lambda-function-p relation) (missing-larkc 30575))))

(defun-memoized cached-evaluation-function (relation)
    (:initial-size 10
     :clear-when :hl-store-modified)
  (with-all-mts
    (evaluation-defn relation)))

(defun function-to-arg-function-p (object)
  (and (el-formula-with-operator-p object #$FunctionToArg)
       (formula-arity= object 2)
       (integerp (nat-arg1 object))
       (missing-larkc 29794)))

