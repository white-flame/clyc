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


(defun el-fort-p (object)
  "[Cyc] Returns T iff OBJECT is a fort or an EL formula."
  (or (fort-p object)
      (null object)
      (el-formula-p object)))

(defun kb-assertion? (object)
  (assertion-p object))

(defun kb-predicate? (symbol)
  (if (fort-p symbol)
      (predicate? symbol)
      (missing-larkc 31575)))

(defun mt-designating-relation? (term)
  (and (fort-p term)
       (microtheory-designating-relation-p term)))

(defun kb-relation? (object)
  (cond
    ((fort-p object) (relation? object))
    ((naut? object) (missing-larkc 3707))))

(defun reified-skolen-term? (term)
  "[Cyc] e.g. (#$SKF-1234 #$Muffet"
  (and (el-formula-p term)
       (missing-larkc 31573)))

(defun reified-skolem-fn-in-any-mt? (fn &optional robust? assume?)
  (when (fort-p fn)
    (or (skolem-function-p fn)
        (and robust? (or
                      (pred-u-v-holds-in-any-mt #$isa fn #$SkolemFuncN)
                      (pred-u-v-holds-in-any-mt #$isa fn #$SkolemFunction)))
        (and assume?
             (not (any-subhl-predicate-links-p fn #$isa))
             (has-skolem-name? fn)))))

(defun has-skolem-name? (fort)
  (cond
    ((constant-p fort) (let ((name (constant-name fort)))
                         (when (stringp name)
                           (starts-with name "SKF"))))
    ((nart-p fort) (has-skolem-name? (nat-functor fort)))))

(defun fast-reified-skolem? (fort)
  (cond
    ((nart-p fort) (fast-reified-skolem? (nat-functor fort)))
    ((constant-p fort) (and (has-skolem-name? fort)
                            (reified-skolem-fn-in-any-mt? fort)))))

(defun fast-skolem-nart? (term)
  "[Cyc] Like skolem-nart except this assumes that all skolem functions begin with SKF."
  (when (nart-p term)
    (let ((functor (nat-functor term)))
      (when (has-skolem-name? functor)
        (missing-larkc 31578)))))

(defun fast-skolem-nat? (term)
  "[Cyc] Like skolem-nart except this assumes that all skolem functions begin with SKF."
  (or (fast-skolem-nart? term)
      (and (naut-p term)
           (has-skolem-name? (nat-functor term)))))

(defun unreified-skolem-term? (term)
  (unreified-skolem-fn-term? term))

(defun unreified-skolem-fn-term? (term)
  (when (and (proper-list-p term)
             (>= 5 (length term) 4))
    (destructuring-bind (fn var-list var &optional seqvar num) term
      (declare (ignore seqvar num))
      (and (skolem-fn-function? fn)
           (listp var-list)
           (el-var? var)))))

(defun skolem-fn-function? (symbol)
  (member symbol *skolem-function-functions*))

(defun ground-naut? (naut &optional (var? #'cyc-var?))
  (and (possibly-naut-p naut)
       (not (sequence-var naut))
       (cons-tree-find-if var? naut)
       (naut? naut var?)))

(defun hl-ground-naut? (object)
  "[Cyc} Returns whether OBJECT is a naut which is ground at the HL, i.e. contains no HL variables."
  (ground-naut? object #'variable-p))

(defun closed-naut? (object &optional (var? #'cyc-var?))
  (and (naut? object var?)
       (closed? object var?)))

(defun first-order-naut? (object)
  "[Cyc] is OBJECT a first-order non-atomic unreified term?"
  (and (possibly-naut-p object)
       (non-predicate-function? (nat-functor object))))

(defun naut? (nat &optional (var? #'el-var?))
  (when (possibly-naut-p nat)
    (let* ((functor (nat-functor nat))
           (naut? (funcall var? functor)))
      (or naut?
          (let ((*relevant-mt-function* 'relevant-mt-is-everything)
                (*mt* #$EverythingPSC))
            (or (non-predicate-function? functor)
                (and (not (fort-p functor))
                     (missing-larkc 31568))))))))

(defun function-term? (term)
  "[Cyc] Return T iff TERM is a nat."
  (or (and (relation-syntax? term)
           (or (cyc-var? (nat-functor term))
               (function-symbol? (nat-functor term)))
           (or (not *within-wff?*)
               (missing-larkc 31567)))
      (find-ground-naut term)))

(defun function-symbol? (symbol)
  (if (fort-p symbol)
      (function? symbol)
      (represented-first-order-term? symbol)))

(defun represented-first-order-term? (term)
  (and term
       (or (fort-p term)
           (cl-var? term)
           (function-term? term))))

(defun sentence? (formula &optional (var? #'el-var?))
  (when (posibly-sentence-p formula)
    (let* ((predicate (formula-arg0 formula))
           (sentence? (funcall var? predicate)))
      (or sentence?
          (let ((*relevant-mt-function* 'relevant-mt-is-everything)
                (*mt* #$EverythingPSC))
            (or (sentential-relation-p predicate)
                (predicate? predicate)
                (isa-predicate? predicate)))))))

(defun relation-syntax? (term &optional (var? #'cyc-var?))
  (or (and *el-supports-dot-syntax?*
           (dotted-args? term var?)
           (wf-wrt-sequence-vars? term))
      (proper-list-p term)))

(defun dotted-args? (args &optional (var? #'cyc-var?))
  (when (consp args)
    (let ((cdr (cdr args)))
      (cond
        ((consp cdr) (dotted-args? cdr var?))
        ((null cdr) nil)
        (t (funcall var? cdr))))))

(defun var-spec? (object)
  (or (el-var? object)
      (kb-var? object)
      (permissible-keyword-var? object)
      (generic-arg-var? object)
      (unreified-skolem-term? object)))

