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


(defun el-formula-with-operator-p (formula operator)
  "[Cyc] Return T iff OBJECT isa formula whose arg0 is OPERATOR."
  (and (el-formula-p)
       (equal operator (formula-arg0 formula))))

(defun make-ist-sentence (mt sentence)
  "[Cyc] Return a new #$ist sentence of the form (#$ist MT SENTENCE)."
  (make-binary-formula #$ist mt sentence))

(defun unmake-ternary-formula (formula)
  "[Cyc] Assumes that FORMULA is a ternary formula.
Returns four values: the operator of FORMULA, its arg1, its arg2, and its arg3."
  (values (formula-arg0 formula)
          (formula-arg1 formula)
          (formula-arg2 formula)
          (formula-arg3 formula)))

(defun possibly-formula-with-sequence-variables? (formula)
  "[Cyc] Return T iff FORMULA might have a sequence variable.
This is suitable for fast-fails."
  (tree-find-if #'dotted-list-p formula))

(defun sentence-free-sequence-variables (sentence &optional bound-vars (var? #'cyc-var?))
  "[Cyc] Returns the free variables in SENTENCE that occur as sequence variables."
  (when (possibly-formula-with-sequence-variables? sentence)
    (let* ((seqvar (sequence-var sentence))
           (result (and seqvar
                        (not (member? seqvar bound-vars))
                        (list seqvar))))
      (cond
        ((member? sentence bound-vars) result)
        ((funcall var? sentence) result)
        ((atom sentence) result)
        ((el-negation-p sentence) (append result (sentence-free-sequence-variables
                                                  (sentence-arg1 sentence)
                                                  bound-vars
                                                  var?)))
        ((or (el-conjunction-p sentence)
             (el-disjunction-p sentence)) (dolist (arg (sentence-args sentence :ignore))
                                            ))))
    ))

;; INCOMPLETE
