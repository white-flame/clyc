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


;; TODO DESIGN - a lot of tools pass around argument numbers, which means constantly having to iterate a list, instead of simply passing the element value itself.  Eventually switch to the latter.


(in-package :clyc)


(defun el-formula-with-operator-p (formula operator)
  "[Cyc] Return T iff OBJECT isa formula whose arg0 is OPERATOR."
  (and (el-formula-p formula)
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
             (el-disjunction-p sentence))
         (dolist (arg (sentence-args sentence :ignore))
           (dolist (var (sentence-free-sequence-variables arg bound-vars var?))
             (pushnew var result)))
         result)

        ((or (el-implication-p sentence)
             (el-exception-p sentence))
         (setf result (append result (sentence-free-sequence-variables (sentence-arg1 sentence)
                                                                       bound-vars var?)))
         (dolist (var (sentence-free-sequence-variables (sentence-arg2 sentence)
                                                        bound-vars var?))
           (pushnew var result))
         (nreverse result))

        ((possibly-el-quantified-sentence-p sentence)
         (append result (sentence-free-sequence-variables (quantified-sub-sentence sentence)
                                                          (cons (quantified-var sentence)
                                                                bound-vars)
                                                          var?)))

        ((mt-designating-literal? sentence)
         (let* ((mt (designated-mt sentence))
                (mt? (mt? mt))
                (subsentence (designated-sentence sentence))
                (result nil))
           ;; TODO - expansion from mt-relevance-macros.lisp
           (let ((mt-var (and mt? mt)))
             (let ((*relevant-mt-function* (possibly-in-mt-determine-function mt-var))
                   (*mt* (possibly-in-mt-determine-mt mt-var)))
               (setf result (sentence-free-sequence-variables subsentence bound-vars var?))))
           (if mt?
               result
               (append result (naut-free-sequence-variables mt bound-vars var?)))))

        ;; TODO - next 2 cond clauses have the same body.  double check and merge?
        ((atomic-sentence? sentence)
         (append result (literal-free-sequence-variables sentence bound-vars var?)))

        ((and (el-formula-p sentence)
              (funcall var? (sentence-arg0 sentence)))
         (append result (literal-free-sequence-variables sentence bound-vars var?)))

        ((relation-expression? sentence)
         (append result (relation-free-sequence-variables sentence bound-vars var?)))

        (t result)))))

(defun literal-free-sequence-variables (literal &optional old-bound-vars (var? #'cyc-var?))
  (relation-free-sequence-variables literal old-bound-vars var?))

(defun relation-free-sequence-variables (relation &optional old-bound-vars (var? #'cyc-var?))
  (cond
    ((not (possibly-formula-with-sequence-varaibles? relation))
     nil)

    ((member? relation old-bound-vars)
     nil)

    ((funcall var? relation)
     nil)

    ((el-negation-p relation)
     (relation-free-sequence-variables (formula-arg1 relation) old-bound-vars var?))

    ((el-formula-p relation)
     (let* ((reln (formula-arg0 relation))
            (new-bound-vars (scoped-vars relation var?))
            (bound-vars (union old-bound-vars new-bound-vars))
            (psn 0)
            (sequence-var (sequence-var relation var?))
            (ans nil))
       (when (and sequence-var
                  (not (member? sequence-var bound-vars)))
         (push sequence-var ans))
       (dolist (term (formula-terms relation :ignore))
         (cond
           ;; Lots of empty terms here, but still fall through to the post-cond behavior
           ((member? term bound-vars))
           ((and (proper-list-p term)
                 (subsetp term bound-vars)))
           ((leave-variables-at-el-for-arg? reln psn))
           ((funcall var? term))
           ((and (within-tou-gaf?)
                 (eq psn 2)))
           
           ;; These have actual bodies, but I don't think the return value matters
           ((unrefied-soken-term? term)
            (let ((seqvar (sequence-var (second term))))
              (and (not (member? seqvar bound-vars))
                   (funcall var? seqvar)
                   (pushnew seqvar ans))))

           ((naut? term)
            (dolist (var (naut-free-sequence-variables term bound-vars var?))
              (pushnew var ans)))

           ((or (sentence? term)
                (expression-find-if #'scoping-relation-expression? term nil))
            (dolist (var (sentence-free-sequence-variables term bound-vars var?))
              (pushnew var ans))))
         (incf psn))
       (nreverse ans)))))

(defun naut-free-sequence-variables (naut &optional old-bound-vars (var? #'cyc-var?))
  (relation-free-sequence-variables naut old-bound-vars var?))

(defun designated-sequence (literal)
  "[Cyc] Returns EL-FORMULA-P or NIL; the designated CYcL sentence in LITERAL."
  (literal-arg literal (sentence-designation-argnum literal)))

(defun sentence-designation-argnum (literal)
  "[Cyc] Returns INTEGERP or NIL; which arg position of LITERAL contains the designated CycL sentence."
  (let ((pred (literal-predicate literal)))
    (if (eq #$ist pred)
        2
        (fpred-value-in-any-mt pred #$sentenceDesignationArgnum))))

(defun designated-mt (literal)
  "[Cyc] Return HLMT-P or NIL: the designated microtheory in LITERAL."
  (literal-arg literal (mt-designation-argnum literal)))

(defun mt-designation-argnum (literal)
  (let ((pred (literal-predicate literal)))
    (if (eq #$ist pred)
        1
        (fpred-value-in-any-mt pred #$microtheoryDesignationArgnum))))

(defun occurs-as-sequence-variable? (var sentence)
  "[Cyc] Returns T iff VAR occurs as a sequence variable in SENTENCE."
  (member? var (sentence-sequence-variables sentence)))

(defun sentence-sequence-variables (formula &optional (var? #'cyc-var?))
  (let* ((seqvar (sequence-var formula var?))
         (seqvars (and seqvar (list seqvar))))
    (when (el-formula-p formula)
      (dolist (term (formula-terms formula :ignore))
        (setf seqvars (append seqvars (sentence-sequence-variables term)))))
    (fast-delete-duplicates seqvars)))

(defun unmake-binary-formula (formula)
  "[Cyc] Assumes that FORMULA is a binary formula.
Returns three values: the operator of FORMULA, its arg1, and its arg2."
  (values (formula-arg0 formula)
          (formula-arg1 formula)
          (formula-arg2 formula)))

(defun sentence-free-term-variables (formula &optional bound-vars (var? #'cyc-var?))
  "[Cyc] Returns the free variables in FORMULA that occur as term variables (the counterpart of sequence variables)."
  (sentence-free-variables formula bound-vars var? nil))

(defun formula-denoting-collection? (collection)
  "[Cyc] Returns T iff the instances of COLLECTION are constrained to be CycL formulas."
  (genl? collection #$CycLFormula *anect-mt*))

(defun sentence-denoting-collection? (collection)
  "[Cyc] Returns T iff the instances of COLLECTION are constrained to be CycL sentences."
  (genl? collection #$CycLSentence *anect-mt*))

(defun user-defined-logical-operator-p (object)
  "[Cyc] Returns T iff OBJECT is one of the user-defined bounded logical operators."
  (and (fort-p object)
       (not (cyc-const-logical-operator-p object))
       (logical-connective-p object)))

(defun el-negation-p (object)
  "[Cyc] Returns T iff OBJECT is a unary formula whose arg0 is the constant #$not.
The formula is not required to be well-formed."
  (and (el-formula-with-operator-p object #$not)
       (el-unary-formula-p object)))

(defun el-disjunction-p (object)
  "[Cyc] Returns T iff OBJECT is a formula whose arg0 is the constant #$or.
The formula is not required to be well-formed."
  (el-formula-with-operator-p object #$or))

(defun el-conjunction-p (object)
  "[Cyc] Returns T iff OBJECT is a formula whose arg0 is the constant #$and.
The formula is not required to be well-formed."
  (el-formula-with-operator-p object #$and))

(defun el-implication-p (object)
  "[Cyc] Returns T iff OBJECT is an implication, i.e. a binary formula whose arg0 is #$implies.
Note that OJBECT does not need to be well-formed."
  (and (el-formula-with-operator-p object #$implies)
       (el-binary-formula-p object)))

(defun cyc-const-general-implication-operator-p (object)
  "[Cyc] Return T iff OBJECT is a Cyc EL implication operator (#$implies or #$equiv)."
  (or (eq #$implies object)
      (eq #$equiv object)))

(defun el-general-implication-p (object)
  "[Cyc] Return T iff OBJECT is an implicative formula, i.e. a binary formula whose arg0 is #$implies or #$equiv.
Note that OBJECT does not need to be well-formed."
  (and (el-formula-p object)
       (cyc-const-general-implication-operator-p (formula-arg0 object))
       (el-binary-formula-p object)))

(defun el-universal-p (object)
  "[Cyc] Return T iff OBJECT is a universally quantified formula, i.e. one whose arg0 is #$forAll.
Note that OBJECT does not need to be well-formed."
  (and (el-formula-with-operator-p object #$forAll)
       (el-binary-formula-p object)
       (el-var? (formula-arg1 object))))

(defun cycl-universal-p (object)
  "[Cyc] Like EL-UNIVERSAL-P but admits more terms as variables, e.g. HL variables."
  (and (el-formula-with-operator-p object #$forAll)
       (el-binary-formula-p object)
       (cyc-var? (formula-arg1 object))))

(defun el-existential-p (object)
  "[Cyc] Return T iff OBJECT is an exitentially quantified formula, i.e. one whose arg0 is #$thereExists, whose arg1 is an el variable, and also has an arg2.
NOte that arg2 does not need to be well-formed."
  (and (el-formula-with-operator-p object #$thereExists)
       (el-binary-formula-p object)
       (el-var? (formula-arg1 object))))

(defun el-existential-min-p (object)
  "[Cyc] Return T iff OBJECT is a ternary formula whose arg0 is #$thereExistAtLeast.
Note that OBJECT does not need to be well-formed."
  (and (el-formula-with-operator-p object #$thereExistAtLeast)
       (missing-larkc 30544)))

(defun el-existential-max-p (object)
  "[Cyc] Return T iff OBJECT is a ternary formula whose arg0 is #$thereExistAtMost.
Note that OBJECT does not need to be well-formed."
  (and (el-formula-with-operator-p object #$thereExistAtMost)
       (missing-larkc 30545)))

(defun el-existential-exact-p (object)
  "[Cyc] Return T iff OBJECT is a ternary formula whose arg0 is #$thereExistExactly.
Note that OBJECT does not need to be well-formed."
  (and (el-formula-with-operator-p object #$thereExistExactly)
       (missing-larkc 30546)))

(defconstant *cyc-const-unary-logical-ops* (list #$not)
  "[Cyc] Used in the syntax checker.")

(defun cyc-const-unary-logical-op-p (object)
  "[Cyc] Return T iff OBJECT is one of the predefined unary operators."
  (eq object #$not))

(defconstant *cyc-const-binary-logical-ops* (list #$implies
                                                  #$xor
                                                  #$equiv)
  "[Cyc] Used in the syntax checker.")

(defun cyc-const-binary-logical-op-p (object)
  "[Cyc} Return T iff OBJECT is one of the predefined binary operators."
  (or (eq object #$implies)
      (eq object #$xor)
      (eq object #$equiv)))

(defconstant *cyc-const-ternary-logical-ops* nil
    "[Cyc] Used in the syntax checker.")

(defun-ignore cyc-const-ternary-logical-op-p (object)
  "[Cyc] Return T iff OBJECT is one of the predefined ternary operators."
  nil)

(defconstant *cyc-const-quaternary-logical-ops* nil
    "[Cyc] Used in the syntax checker.")

(defun-ignore cyc-const-quaternary-logical-op-p (object)
  "[Cyc] Return T iff OBJECT is one of the predefined quaternary operators."
  nil)

(defconstant *cyc-const-quintary-logical-ops* nil
    "[Cyc] Used in the syntax checker.")

(defun-ignore cyc-const-quintary-logical-op-p (object)
  "[Cyc] Return T iff OBJECT is one of the predefined quintary operators."
  nil)

(defconstant *cyc-const-variable-arity-logical-ops* (list #$and
                                                          #$or)
    "[Cyc] Used in the syntax checker.")

(defun cyc-const-variable-arity-logical-op-p (object)
  "[Cyc] Return T iff OBJECT is one of the predefined variable-arity-operators."
  (or (eq object #$and)
      (eq object #$or)))

(defun cyc-const-existential-p (object)
  "[Cyc] Return T iff OBJECT is #$thereExists"
  (eq object #$thereExists))

(defun cyc-const-universal-p (object)
  "[Cyc] Return T iff OBJECT is #$forAll."
  (eq object #$forAll))

(defconstant *cyc-const-regular-quantifiers* (list #$thereExists
                                                   #$forAll))

(defun cyc-const-regular-quantifier-p (object)
  "[Cyc] Return T iff OBJECT is one of the predefined regular quantifiers."
  (or (cyc-const-existential-p object)
      (cyc-const-universal-p object)))

(defun possibly-el-regularly-quantified-sentence-p (object)
  "[Cyc] Return T iff OBJECT is a binary sentence whose arg0 is one of the predefined regular quantifiers (#$forAll or #$thereExists)."
  (and (el-binary-formula-p object)
       (cyc-const-regular-quantifier-p (sentence-arg0 object))))

(defconstant *cyc-const-bounded-existentials* (list #$thereExistsExactly
                                                    #$thereExistAtMost))

(defun cyc-const-bounded-existential-operator-p (object)
  "[Cyc] Return T iff OBJECT is one of th epredefined bounded existentials."
  (or (eq object #$thereExistExactly)
      (eq object #$thereExistAtMost)
      (eq object #$thereExistAtLeast)))

(defun user-defined-bounded-existential-operator-p (object)
  "[Cyc] Return T iff OBJECT is one of the user-defined bounded existential quantifiers."
  (and (fort-p object)
       (cyc-const-bounded-existential-operator-p object)
       (bounded-existential-quantifier-p object)))

(defun el-bounded-existential-p (object)
  "[Cyc] Return T iff OBJECT is a ternary formula whose arg0 is one of the predefined bounded existential quantifiers."
  (and (el-formula-p object)
       (cyc-const-bounded-existential-operator-p (formula-arg0 object))
       (missing-larkc 30547)))

(defun cyc-const-general-existential-operator-p (object)
  "[Cyc} Return T iff OBJECT is one fo the predefined existentials, either regular or bounded."
  (or (eq object #$thereExists)
      (cyc-const-bounded-existential-operator-p object)))

(defun cyc-const-quantifier-p (object)
  "[Cyc] Return T iff OBJECT is one of the predefined quantifiers, either regular or bounded."
  (or (cyc-const-regular-quantifier-p object)
      (cyc-const-bounded-existential-operator-p object)))

(defun possibly-el-quantified-sentence-p (object)
  "[Cyc] Return T iff OBJECT is either a regularly quantified formula or a bounded existential formula."
  (or (possibly-el-reguarly-quantified-sentence-p object)
      (el-bounded-existential-p object)))

(defun cyc-const-logical-operator-p (object)
  "[Cyc] Return T iff OBJECT is any of the predefined logical operators."
  (or (cyc-const-unary-logical-op object)
      (cyc-const-ternary-logical-op-p object)
      (cyc-const-quaternary-logical-op-p object)
      (cyc-const-quintary-logical-op-p object)
      (cyc-const-variable-arity-logical-op-p object)))

(defconstant *cyc-const-tense-operators* (list #$was
                                               #$hasAlwaysBeen
                                               #$willBe
                                               #$willAlwaysBe))
(defconstant *cyc-const-metric-tense-operators* (list #$was-Metric
                                                      #$willBe-Metric))

(defun cyc-const-tense-operator-p (object)
  (or (eq object #$was)
      (eq object #$hasAlwaysBeen)
      (eq object #$willBe)
      (eq object #$willAlwaysBe)))

(defun cyc-const-metric-tense-operator-p (object)
  (or (eq object #$was-Metric)
      (eq object #$willBe-Metric)))

(defun cyc-const-generalized-tense-operator-p (object)
  (or (cyc-const-tense-operator-p object)
      (cyc-const-metric-tense-operator-p object)))

(defun cyc-const-sentential-relation-p (object)
  "[Cyc] Return T iff OBJECT is any of the predefined logical operators or quantifiers, either regular or bounded."
  (or (cyc-const-logical-operator-p object)
      (cyc-const-quantifier-p object)))

(defun el-logical-operator-formula-p (object)
  "[Cyc] Return T iff OBJECT is an EL formula with a logical operator as its arg0."
  (and (el-formula-p object)
       (cyc-const-logical-operator-p (formula-operator object))))

(defun el-unary-formula-p (object)
  "[Cyc] Return T iff OBJECT is a formula of arity 1.
OBJECT is not required to be well-formed."
  (and (el-formula-p object)
       (formula-arity= object 1)))

;; TODO - subl -> cl
(defun subl-escape-p (object)
  "[Cyc] Return T iff OBJECT is an escape to SubL, which should not be analyzed recursively."
  (and (el-formula-p object)
       (or (expand-subl-fn-p object)
           (subl-quote-p object))))

;; TODO - subl -> cl
(defun expand-subl-fn-p (object)
  "[Cyc] Return T iff OBJECT is an escape to SUbL using #$ExpandSubLFn."
  (and (eq #$ExpandSubLFn (formula-operator object))
       (listp (formula-arg1 object))
       (cyc-subl-template (formula-arg2 object))))

(defun subl-quote-p (object)
  "[Cyc] Return T iff OBJECT is an escape to SubL using #$SubLQuoteFn."
  (and (eq #$SubLQuoteFn (formula-operator object))
       (formula-arity= object 1)
       (cyc-subl-template (formula-arg1 object))))

(defun epsilon-p (object)
  "[Cyc] Return T iff OBJECT is NIL (epsilon)."
  (null object))

(defconstant *cyc-const-exception-operators* (list #$exceptFor
                                                   #$exceptWhen)
  "[Cyc] Used in the precanonicalizer.")

(defun cyc-const-exception-operator-p (object)
  "[Cyc] Return T iff OBJECT is one of the predefined exception operators."
  (or (eq object #$exceptFor)
      (eq object #$exceptWhen)))

(defconstant *cyc-const-pragmatic-requirement-operators* (list #$pragmaticRequirement)
  "[Cyc] Used in the precanonicalizer.")

(defun-inline cyc-const-pragmatic-requirement-operator-p (object)
  "[Cyc] Return T iff OBJECT is one fo the predefined exception operators."
  (eq object #$pragmaticRequirement))

(defun el-pragmatic-requirement-operator-p (object)
  (and (cyc-const-pragmatic-requirement-operator-p (formula-operator object))
       (el-binary-formula-p object)))

(defun el-meets-pragmatic-requirement-p (object)
  "[Cyc] Assumes that #$meetsPragmaticRequirement is a binary operator.
Return T iff OBJECT is a binary formula whose arg0 is #$meetsPragmaticRequirement."
  (and (el-binary-formula-p object)
       (eq #$meetsPragmaticRequirement (formula-operator object))))

(defun el-implicit-meta-literal-sentence-p (object)
  "[Cyc] Return T iff OBJECT is an EL setence which indicates an implicit meta-literal. The two kinds of EL sentences which indicate implicit meta-literals are EL exceptions (e.g. #$exceptWhen, #$exceptFor) and pragmatic requirements (#$pragmaticRequirement)."
  (or (el-exception-p object)
      (el-pragmatic-requirement-p object)))

(defun elf-p (object)
  "[Cyc] Return T iff OBJECT is a candidate EL formula (not necessarily well-formed).
'formula' refers to a formula in the grammar, not a formula in the logic.
FOr example, a non-atomic term is a formula in the grammar, but not a formula in the logic.
Note that #$True and #$False are not considered formulas by this definition."
  (consp object))

(defun el-formula-p (object)
  "[Cyc] Return T iff OBJECT is a candidate EL formula (not necessarily well-formed).
'formula' refers to a formula in the grammar, not a formula in the logic.
FOr example, a non-atomic term is a formula in the grammar, but not a formula in the logic.
Note that #$True and #$False are not considered formulas by this definition."
  (elf-p object))

(defun possibly-fo-naut-p (object)
  "[Cyc] A quick test for whether OBJECT could possibly be a first order NAUT.
It is certainly not guaranteed that OBJECT is a first order NAUT just because this function returns T."
  (and (el-formula-p object)
       (fort-p (el-formula-operator object))))

(defun possibly-naut-p (object)
  "[Cyc] A quick test for whether OBJECT could possibly be a NAUT.
IT is certainly not guaranteed that OBJECT is a NAUT just because this function returns T."
  (el-formula-p object))

(defun possibly-sentence-p (object)
  "[Cyc] A quick test for whether OBJECT could possibly be a CycL setence.
It is certainly not guaranteed that OBJECT is a CycL sentence just because this function returns T.
Passing this test should guarantee that applying the sentence accessors will not trigger an error."
  (el-formula-p object))

(defun possibly-atomic-sentence-p (object)
  "[Cyc] A quick test for whether OBJECT could possibly be an atomic CycL sentence.
It is certainly not guaranteed that OBJECT is an atomic CycL sentence just because this function returns T."
  (el-formula-pl object))

(defun contains-subformula-p (object)
  "[Cyc] Returns T iff OBJECT si an EL formula which contains an EL subformula."
  (when (el-formula-p object)
    (dolist (term (formula-terms object :ignore))
      (when (el-formula-p term)
        (return t)))))

(defun el-empty-list-p (obj)
  (or (eq obj #$TheEmptyList)
      (and (el-formula-p obj)
           (not (formula-with-sequence-var? obj))
           (eq (formula-operator obj) #$TheList)
           (null (formula-args obj)))))

(defun el-extensional-set-p (object)
  (or (el-empty-set-p object)
      (el-non-empty-set-p object)))

(defun el-empty-set-p (object)
  (or (eq object #$TheEmptySet)
      (and (el-formula-p object)
           (not (formula-with-sequence-var? object))
           (eq (formula-operator object) #$TheSet)
           (null (formula-args object)))))

(defun el-non-empty-set-p (object)
  (and (el-formula-p object)
       (eq (formula-operator object) #$TheSet)
       (formula-args object :include)))

(defun el-sequence-p (object)
  "[Cyc] Return T iff OBJECT is an EL sequence."
  (consp object))

(defun ground? (expression &optional (var? #'cyc-var?))
  "[Cyc] Returns whether EXPRESSION is free of any variables?"
  (null (expression-find-if var? expression nil)))

(defun hl-ground-tree-p (tree)
  (fully-bound-p tree))

(defun closed? (expression &optional (var? #'cyc-var?))
  "[Cyc] Is EXPRESSION free of any free variables? (includes GROUND? check for efficiency)"
  (or (ground? expression var?)
      (no-free-variables? expression var?)))

(defun no-free-variables? (expression &optional (var? #'cyc-var?))
  "[Cyc] Is EXPRESSION free of any free varaibles? (excludes GROUND? check for efficiency)"
  (null (literal-free-variables expression nil var?)))

(deflexical *standard-single-letter-el-var-names* '(?x ?y ?z ?w ?v ?u
                                                    ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j
                                                    ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t))

(defun sequence-term (formula)
  "[Cyc] Returns the sequence term in FORMULA, if there is one. Otherwise returns NIL.
For example, (sequence-term (<pred> ?X ?Y . <term>)) returns <term>.
However, it will not return nested sequence terms;
e.i. (sequence-var (#$and <form1> (<pred> ?X ?Y . <term>))) will return NIL."
  (when *el-supports-dot-syntax?*
    (cond
      ((el-formula-p formula)
       (cdr (last formula)))
      ;; If it's nart-p, also return nil, but that's implicit.
      )))

(defun sequence-var (relation &optional (var? #'cyc-var?))
  "[Cyc] Returns the sequence variable in RELATION, if there is one. Otherwise returns NIL.
For example, (sequence-var (<pred> ?X ?Y . ?Z)) returns ?Z.
However, it will nto return nested sequence variables;
i.e. (sequence-var (#$and <form1> (<pred> ?X ?Y . ?Z))) will return NIL."
  (let ((seqterm (sequence-term relation)))
    (when seqterm
      (cond
        ((eq var? #'cyc-var?) (and (cyc-var? seqterm)
                                   seqterm))
        ((funcall var? seqterm) seqterm)))))

(defun sequence-non-var (relation &optional (var? #'cyc-var?))
  "[Cyc] Returns the part of RELATION that should be a sequence variable by its syntax, but isn't.
Returns NIL if there is no such ill-formed thing in RELATION."
  (let ((seqterm (sequence-term relation)))
    (and seqterm
         (funcall var? seqterm)
         seqterm)))

(defun maybe-add-sequence-var-to-end (seqvar formula)
  "[Cyc] Return FORMULA with SEQVAR added to the end, if SEQVAR is non-nil. Otherwise returns FORMULA unadulterated.
Example: (maybe-add-sequence-var-to-end ?X (#$different #$Muffet #$Pace)) => (#$different #$Muffet #$Pace . ?X)
Example: (maybe-add-sequence-var-to-end NIL (#$different #$Muffet #$Pace)) => (#$different #$Muffet #$Pace)"
  (if seqvar
      (missing-larkc 9016)
      formula))

(defun formula-with-sequence-term? (formula)
  (sequence-term formula))

(defun el-formula-without-sequence-term? (formula)
  (proper-list-p formula))

(defun formula-with-sequence-var? (formula)
  (sequence-var formula))

(defun formula-with-sequence-non-var? (formula)
  "[Cyc] Return T iff FORMULA contains someting in sequence variable position, but it's not a variable."
  (sequence-non-var formula))

(defun ignore-sequence-vars (formula)
  "[Cyc] e.g. transforms (<pref> ?X ?Y . ?Z) into (<pred> ?X ?Y)."
  (if (sequence-var formula)
      (missing-larkc 30664)
      formula))

(defun term-is-one-of-args? (term formula)
  (dolist (arg (formula-args formula :ignore))
    (when (equal term arg)
      (return t))))

(defun formula-arity (formula &optional (seqvar-handling :ignore))
  "[Cyc] Returns the arity of FORMULA (the number of arguments).
This will be an integer if FORMULA is an EL formula, NART or assertion, and NIL otherwise (e.g. the arity of #$True is NIL).
The operator itself does not count as an argument.
If SEQVAR-HANDLING is :REGULARIZE, then sequence variables count as a single argument.
Hence, (#$and <form1> <form2> <form3>) is a formula of arity 3,
and (#$and <form1> <form2> <form3> . ?SEQ) is a formula of arity 4.
If SEQVAR-HANDLING is :IGNORE< then sequence variables don't count, so both of those would be arity 3.
Don't pass it :INCLUDE."
  (when (possibly-cycl-formula-p formula)
    (length (formula-args formula seqvar-handling))))

(defun expression-arity (relational-expression &optional (seqvar-handling :ignore))
  (formula-arity relational-expression seqvar-handling))

(defun formula-length (formula &optional (seqvar-handling :ignore))
  "[Cyc] Returns the length of the formula.
In other words, the number of terms. This is one greater than the arity.
If SEQVAR-HANDLING is :REGULARIZE, then sequence variables count as a single argument.
If SEQVAR-HANDLING is :IGNORE, then sequence variables don't count at all."
  (length (formula-terms formula seqvar-handling)))

(defun formula-arity< (formula arity &optional (seqvar-handling :ignore))
  "[Cyc] Return T iff FORMULA's arity is less than ARITY."
  (and (possibly-cycl-formula-p formula)
       (positive-integer-p arity)
       (length< (formula-args formula seqvar-handling) arity nil)))

(defun formula-arity> (formula arity &optional (seqvar-handling :ignore))
  "[Cyc] Return T iff FORMULA's arity is greater than ARITY."
  (and (possibly-cycl-formula-p formula)
       (integerp arity)
       (length> (formula-args formula seqvar-handling) arity nil)))

(defun formula-arity>= (formula arity &optional (seqvar-handling :ignore))
  "[Cyc] Return T iff FORMULA's arity is greater than or equal to ARITY."
  (and (possibly-cycl-formula-p formula)
       (integerp arity)
       (length>= (formula-args formula seqvar-handling) arity nil)))

(defun el-formula-arity>= (el-formula arity &optional (seqvar-handling :ignore))
  "[Cyc] Return T iff EL_FORMULA's arity is greater than or equal to ARITY."
  (and (el-formula-p el-formula)
       (integerp arity)
       (length>= (el-formula-args el-formula seqvar-handling) arity nil)))

(defun formula-arity= (formula arity &optional (seqvar-handling :ignore))
  "[Cyc] Return T iff FORMULA's arity is equal to ARITY."
  (and (possibly-cycl-formula-p formula)
       (integerp arity)
       (length= (formula-args formula seqvar-handling) arity nil)))

(defun first-in-sequence (sequence)
  "[Cyc] Returns the first element of SEQUENCE."
  (car sequence))

(defun rest-of-sequence (sequence)
  "[Cyc] Returns (as a list or a variable) all elements but the first of SEQUENCE."
  (rest sequence))

(defun sentence-quantifier (sentence)
  "[Cyc] If SENTENCE is a quantified sentence, returns teh quantifier. Otherwise returns NIL."
  (when (possibly-el-quantified-sentence-p sentence)
    (sentence-arg0 sentence)))

(defun quantified-var (sentence)
  "[Cyc] Returns the quantified variable in a quantified sentence.
Returns NIL if SENTENCE is not a quantified sentence.
e.g. (quantified-var (#$forAll ?X <form1>)) would return ?X.
and (quantified-var (#$thereExistsExactly 539 ?EVENT <form2>)) would return ?EVENT."
  (cond
    ((cycl-bounded-existential-sentence-p sentence)
     (sentence-arg2 sentence))
    ((possibly-el-regularly-quantified-sentence-p sentence)
     (sentence-arg1 sentence))))

(defun quantified-sub-sentence (sentence)
  "[Cyc] Returns the quantified subsentence in a quantified sentence.
Yields an error if SENTENCE is not a quantified sentence.
e.g. (quantified-sub-sentence (#$forAll ?X <form1>)) would return <form1>.
and (quantified-sub-sentence (#$thereExistsExactly 539 ?EVENT <form2>)) would return <form2>."
  (cond
    ((possibly-el-regularly-quantified-sentence-p sentence)
     (sentence-arg2 sentence))
    ((cycl-bounded-existential-sentence-p sentence)
     (missing-larkc 29838))
    (t (el-error 4 "not quantified sentence: ~s" sentence))))

(defun quantified-sub-sentence-argnum (sentence)
  "[Cyc] Returns the argnum that contains the quantified subsentence of SENTENCE.
Yields an error and returns NIL if SENTENCE is not a quantified sentence."
  (quantified-sub-sentence-argnum-for-operator (sentence-arg0 sentence)))

(defun quantified-sub-sentence-argnum-for-operator (operator)
  (cond
    ((cycl-regular-quantifier-p operator)
     2)
    ((cycl-bounded-existential-quantifier-p operator)
     3)
    (t (el-error 4 "not a known quantifier: ~s" operator))))

(defparameter *dont-enforce-subl-escape-in-symbols* t)

(defun cycl-subl-symbol-symbol (object)
  "[Cyc] When OBJECT is a CYCL-SUBL-SYMBOL-P, returns the SubL symbol part."
  (cond
    ((cycl-subl-symbol-p object)
     (formula-arg1 object))
    (*dont-enforce-subl-escape-in-symbols*
     object)))

(defun list-to-elf (list)
  "[Cyc] Returns an EL formula constructe from the list LIST.
Currently this just returns the list itself, since we implement EL formulas as lists. You can't destructively modify the returned EL formula without affecting LIST."
  list)

(defun make-el-formula (operator args &optional sequence-var)
  "[Cyc} Returns a new EL formula with the operator OPERATOR, the arguments ARGS, and teh optional sequence variable SEQUENCE-VAR.
This formula is destructible at the top level."
  (if sequence-var
      (append (cons operator (copy-list args)) sequence-var)
      (cons operator (copy-list args))))

(defun make-formula (operator args &optional sequence-var)
  "[Cyc] Returns a new EL formula with the operator OPERATOR, the arguments ARGS, and the optional sequence variable SEQUENCE-VAR.
This formula is destructible at the top level."
  (make-el-formula operator args sequence-var))

(defun copy-formula (formula)
  "[Cyc] Returns a copy of the EL formula FORMULA."
  (copy-tree formula))

(defun copy-sentence (sentence)
  "[Cyc] Returns a copy of the EL sentence SENTENCE."
  (copy-tree sentence))

(defun copy-clause (clause)
  "[Cyc] Returns a copy of CLAUSE."
  (copy-tree clause))

(defun copy-expression (expression)
  "[Cyc] Returns a copy of EXPRESSION."
  (copy-tree expression))

(defun make-el-literal (predicate args &optional sequence-var)
  "[Cyc] Returns a new EL sentence with the operator PREDICATE and the arguments ARGS.
This sentence is destructible at the top level."
  (make-el-formula predicate args sequence-var))

(defun make-unary-formula (operator arg)
  "[Cyc] Returns a new unary formula with the operator OPERATOR and the single argument ARG."
  (list operator arg))

(defun make-binary-formula (operator arg1 arg2)
  "[Cyc] Returns a new binary formula with the operator OPERATOR and the two arguments ARG1 and ARG2."
  (list operator arg1 arg2))

(defun make-ternary-formula (operator arg1 arg2 arg3)
  "[Cyc] Returns a new ternary formula with the operator OPERATOR and the three arguments ARG1, ARG2, and ARG3"
  (list operator arg1 arg2 arg3))


(defun make-negation (sentence)
  "[Cyc] Returns the negation of SENTENCE. Does not perform any simplification.
i.e. just returns (#$not <sentence>)."
  (make-formula #$not sentence))

(defun make-disjunction (args)
  "[Cyc] Returns a new disjunction. Each member of the list ARGS is a disjunct in the new disjunction.
e.g. (make-disjunction (<form1> <form2> <form3>)) returns (#$or <form1> <form2> <form3>)."
  (make-formula #$or args))

(defun make-conjunction (args)
  "[Cyc] Returns a new conjunction. Each member of the list ARGS is a conjunct in the new conjunction.
e.g. (make-conjunction (<form1> <form2> <form3>)) returns (#$and <form1> <form2> <form3>)."
  (make-formula #$and args))

(defun make-universal (var sentence)
  "[Cyc] Returns a new universally quantified sentence of the form (#$forAll <var> <sentence>."
  (make-binary-formula #$forAll var sentence))

(defun make-regularly-quantified-sentence (quantifier var subsent)
  (make-binary-formula quantifier var subsent))

(defun map-formula-args (func formula &optional map-seqvar?)
  "[Cyc] Execute FUNC on each argument (in order) of FORMULA, unless it is an opaque argument.
e.g. (map-formula-args #'func (#$and <form1> <form2> <form3>))
would return (#$and (func <form1>) (func <form2>) (func <form3>)).
By default, FUNC is not applied to sequence variables, because they are not arguments per se;
rather they are variables that can bind to argument (sub)sequences,
but if MAP-SEQVAR? is T, then FUNC will be applied to sequence variables.
example: (map-formula-args #'el-var? '(#$isa ?X #$Dog . ?Z))   => (#$isa T NIL . ?Z)
example: (map-formula-args #'el-var? '(#$isa ?X #$Dog . ?Z) t) => (#$isa T NIL . T)"
  (when (el-formula-p formula)
    (let ((args (formula-args formula :include)))
      (if (consp args)
          (let ((new-args (car args)))
            (do ((arg (car args) (car remaining-args))
                 (remaining-args (cdr args) (cdr remaining-args))
                 (argnum 1 (1+ argnum)))
                ;; Exit test
                ((atom remaining-args) ;; also matches NIL
                 ;; Return forms
                 (push (if (opaque-arg? formula argnum)
                           arg
                           (funcall func arg))
                       new-args)
                 (if remaining-args
                     (let ((final-args (if (and map-seqvar?
                                                (missing-larkc 29830))
                                           (funcall func remaining-args)
                                           remaining-args)))
                       ;; TODO - why not append?
                       (dolist (new-arg new-args)
                         (push new-arg final-args))
                       (make-formula (formula-arg0 formula) final-args))
                     (make-formula (formula-arg0 formula) (nreverse new-args))))
              ;; Iteration body
              (push (if (opaque-arg? formula argnum)
                        arg
                        (funcall func arg))
                    new-args)))
          formula))))

(defun nmap-formula-args (func formula &optional map-seqvar?)
  "[Cyc] A destructive version fo MAP-FORMULA-ARGS."
  (when (el-formula-p formula)
    (let ((args (formula-args formula :include)))
      (when (consp args)
        (do* ((curr-args args (cdr curr-args))
              (arg (car curr-args) (car curr-args))
              (argnum 1 (1+ argnum))
              (remaining-args (cdr curr-args) (cdr curr-args)))
             ;; Exit test
             ((atom remaining-args) ;; also matches NIL
              ;; Return forms
              (rplaca curr-args (if (opaque-arg? formula argnum)
                                    arg
                                    (funcall func arg)))
              (when (and remaining-args
                         map-seqvar?
                         (missing-larkc 29833))
                (rplacd curr-args (funcall func remaining-args))))
          ;; Iteration body
          (rplaca curr-args (if (opaque-arg? formula argnum)
                                arg
                                (funcall func arg))))))
    formula))

(defun pass-through-if-negation (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to its arg1 iff FORMULA is a negation and its arg1 is not opaque.
Return 1: Boolean, whether FORMULA was altered."
  (if (and (el-negation-p formula)
           (not (opaque-arg? formula 1)))
      (let ((*within-negation* t))
        (values (make-negation (funcall function (formula-arg1 formula)))
                t))
      (values formula nil)))

(defun pass-through-if-disjunction (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to its args iff FORMULA is a disjunction and its args are not opaque.
Return 1: Boolean, whether FORMULA was altered."
  (if (el-disjunction-p formula)
      (let ((*within-disjunction?* t)
            (*within-negated-disjunction?* *within-negation?*))
        (values (nmap-formula-args function formula) t))
      (values formula nil)))

(defun pass-through-if-conjunction (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to its args iff FORMULA is a conjunction and its args are not opaque.
Return 1: Boolean, whether FORMULA was altered."
  (if (el-conjunction-p formula)
      (values (nmap-formula-args function formula) t)
      (values formula nil)))

(defun pass-through-if-regularly-quantified (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to its quantified subformula iff FORMULA is a regularly quantified formula and its quantified subformula is not opaque.
Return 1: Boolean, whetehr FORMULA was aletered."
  (if (and (possibly-el-regularly-quantified-sentence-p formula)
           (not (opaque-arg? formula (quantified-sub-sentence-argnum formula))))
      (values (make-regularly-quantified-sentence (sentence-quantifier formula)
                                                  (quantified-var formula)
                                                  (funcall function (quantified-sub-sentence formula)))
              t)
      (values formula nil)))

(defun pass-through-if-bounded-existential (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to its quantified subformula iff FORMULA is a bounded existential formula and its quantified subformula is not opaque.
Return 1: Boolean, whether FORMULA was altered."
  (declare (ignore function))
  (if (and (el-bounded-existential-p formula)
           (not (opaque-arg? formula (quantified-sub-sentence-argnum formula))))
      (missing-larkc 30588)
      (values formula nil)))

(defun pass-through-if-junction (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to its args iff FORMULA is a conjunction or disjunction and its args are not opaque.
Return 1: Boolean, whether FORMULA was altered."
  (multiple-value-bind (result changed?) (pass-through-if-disjunction function formula)
    (if changed?
        (values result changed?)
        (pass-through-if-conjunction function formula))))

(defun pass-through-if-logical-op (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to its args iff FORMULA is an EL formula with a logical operator as its arg0 and its args are not opaque.
Return 1: Boolean, whether FORMULA was altered."
  (if (el-logical-operator-formula-p formula)
      (multiple-value-bind (result changed?) (pass-through-if-negation function formula)
        (if changed?
            (values result t)
            (multiple-value-bind (result-2 changed-2?) (pass-through-if-junction function formula)
              (if changed-2?
                  (values result-2 t)
                  (values (nmap-formula-args function formula) t)))))
      (values formula nil)))

(defun pass-through-if-quantified (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to its quantified subformula iff FORMULA is a quantified formula and its quantified subformula is not opaque.
Return 1: Boolean, whether FORMULA was altered."
  (multiple-value-bind (result changed?) (pass-through-if-regularly-quantified function formula)
    (if changed?
        (values result t)
        (pass-through-if-bounded-existential function formula))))

(defun pass-through-if-logical-op-or-quantified (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to its quantified subformula iff FORMULA is an EL formula with a quantifier or logical operator as its arg0 and its args are not opaque.
Return 1: Boolean, whether FORMULA was altered."
  (multiple-value-bind (result changed?) (pass-through-if-logical-op function formula)
    (if changed?
        (values result t)
        (pass-through-if-bounded-existential function formula))))

(defun pass-through-if-relation-syntax (function formula)
  "[Cyc] Return 0: FORMULA with FUNCTION applied to all its terms (including the arg0) iff FORMULA is an EL formula with the syntax of a relation as its arg0 and its args are not opaque.
Return 1: Boolean, whether FORMULA was altered."
  (multiple-value-bind (result changed?) (pass-through-if-logical-op-or-quantified function formula)
    (if changed?
        (values result t)
        (if (relation-syntax? formula)
            (values (nmap-formula-terms function formula) t)
            (values formula nil)))))

(defun funcall-formula-arg (function formula argnum)
  "[Cyc] Calls FUNCTION on the ARGNUMth arg of FORMULA, unless that arg position is opaque."
  (let ((arg (formula-arg formula argnum)))
    (if (opaque-arg? formula argnum)
        arg
        (funcall function arg))))

(defun replace-formula-arg (argnum new-arg formula)
  "[Cyc] Replaces the ARGNUMth argument of FORMULA with NEW-ARG.
This is constructive."
  (let ((new-terms nil)
        (seqvar (sequence-var formula))
        (terms (formula-terms formula :ignore)))
    (dolistn (n arg terms)
      (push (if (eq n argnum) new-arg arg)
            new-terms))
    (let* ((new-formula (nreverse new-terms))
           (new-operator (formula-operator new-formula))
           (new-args (formula-args new-formula)))
      (make-formula new-operator new-args seqvar))))

(defun nreplace-formula-arg (argnum new-arg formula)
  "[Cyc] Replaces the ARGNUMth argument of FORMULA with NEW-ARG.
This is destructive."
  (let* ((seqvar (sequence-var formula))
         (formula (if seqvar
                      (missing-larkc 30665)
                      formula))
         (result (nreplace-nth argnum new-arg formula)))
    (if seqvar
        (missing-larkc 30614) result)))

(defun literal-atomic-sentence (literal)
  "[Cyc] Returns the atomic formula within LITERAL. This will either be LITERAL itself or (if LITERAL is a negation) the atomic formula inside the negation.
Assumes that LITERAL is a literal. (#$not . ?X) is not a literal."
  (when (el-formula-p literal)
    (if (el-negation-p literal)
        (formula-arg1 literal :regularize)
        literal)))

(defun literal-truth (literal)
  (if (el-negation-p literal)
      :false
      :true))

(defun literal-sense (literal)
  (truth-sense (literal-truth literal)))

(defun literal-args (literal &optional (seqvar-handling :ignore))
  "[Cyc] Returns the arguments of LITERAL.
Assumes that LITERAL is a literal. (#$not . ?X) is not a literal.
If SEQVAR-HANDLING is :IGNORE, it chops off the sequence var if there is one.
If SEQVAR-HANDLING is :REGULARIZE, it treats the sequence var as a regular variable.
If SEQVAR-HANDLING is :INCLUDE, it returns it as a sequence var."
  (formula-args (literal-atomic-sentence literal) seqvar-handling))

(defun literal-arg (literal argnum &optional (seqvar-handling :ignore))
  "[Cyc] Returns the ARGNUMth argument of LITERAL. An ARGNUM of 0 will return the predicate.
Returns NIL if LITERAL is not a formula, or if ARGNUM is out of bounds or not an integer.
Assumes that LITERAL is a literal. (#$not . ?X) is not a literal.
If SEQVAR-HANDLING is :IGNORE, it will return NIL if asked for the arg position where the sequence variable is.
If SEQVAR-HANDLING is :REGULARIZE, it will return the sequence variable if asked for its position.
e.g. (literal-arg (<pred> <arg1> . ?SEQ) 2 :ignore)     -> NIL
but  (literal-arg (<pred> <arg1> . ?SEQ) 2 :regularize) -> ?SEQ"
  (formula-arg (literal-atomic-sentence literal) argnum seqvar-handling))

(defun literal-predicate (literal &optional (seqvar-handling :ignore))
  "[Cyc] Returns the predicate (the 0th argument) of LITERAL.
For example, (literal-predicate (#$isa #$Muffet #$Dog)) would return #$isa.
Assumes that LITERAL is a literal.  (#$not . ?X) is not a literal."
  (literal-arg0 literal seqvar-handling))

(defun literal-arg0 (literal &optional (seqvar-handling :ignore))
  "[Cyc] Returns the 0th argument (the predicate) of LITERAL.
For example, (literal-arg0 (#$isa #$Muffet #$Dog)) would return #$isa.
Assumes that LITERAL is a literal. (#$not . ?X) is not a literal."
  (declare (ignore seqvar-handling))
  (formula-arg0 (literal-atomic-sentence literal)))

(defun literal-arg1 (literal &optional (seqvar-handling :ignore))
  "[Cyc] Returns the 1st argument of LITERAL.
For example, (literal-arg1 (#$isa #$Muffet #$Dog)) would return #$Muffet.
Assumes that LITERAL is a literal. (#$not . ?X) is not a literal."
  (literal-arg literal 1 seqvar-handling))

(defun literal-arg2 (literal &optional (seqvar-handling :ignore))
  "[Cyc] Returns the 2nd argument of LITERAL.
For example, (literal-arg2 (#$isa #$Muffet #$Dog)) would return #$Dog.
Assumes that LITERAL is a literal. (#$not . ?X) is not a literal."
  (literal-arg literal 2 seqvar-handling))

(defun literal-arity (literal &optional (seqvar-handling :ignore))
  "[Cyc] Returns the arity of a literal (the number of arguments).
The predicate itself does not count as an argument.
Assumes that LITERAL is a literal  (#$not . ?X) is not a literal.
For example, the arity of (#$not (#$isa #$Dog #$Collection)) would be 2, because there are 2 arguments, #$Dog and #$Collection.
The #$not, if there is one, is ignored.
If SEQVAR-HANDLING is :REGULARIZE, then sequence variables count as a single argument.
Hence, (<pred> <form1> <form2> <form3>) is a literal of arity 3, and (<pred> <form1> <form2> <form3> . ?SEQ) is a literal of arity 4.
If SEQVAR-HANDLING is :IGNORE, then sequence variables don't count, so both of those would be arity 3.
Don't pass it :INCLUDE."
  (formula-arity (literal-atomic-sentence literal) seqvar-handling))

(defun asent-and-sense-to-literal (asent sense)
  (asent-and-truth-to-literal asent (sense-truth sense)))

(defun asent-and-truth-to-literal (asent truth)
  (if (eq :false truth)
      (negate asent)
      asent))

(defun el-negative-sentences (sentences)
  "[Cyc] Returns all the negations in SENTENCES, which is a list of EL sentences."
  (remove-if-not #'el-negation-p sentences))

(defun el-positive-sentences (sentences)
  "[Cyc] Returns all the sentences in SENTENCES that are not negations.  SENTENCES is a list of EL sentences."
  (remove-if #'el-negation-p sentences))

(defun unary-lit-p (literal)
  "[Cyc] Returns T iff LITERAL has exactly one argument.
Assumes that LITERAL is a literal."
  (el-unary-formula-p (literal-atomic-sentence literal)))

(defun binary-lit-p (literal)
  "[Cyc] Returns T iff LITERAL has exactly two arguments.
Assumes that LITERAL is a literal."
  (el-binary-formula-p (literal-atomic-sentence literal)))

(defun pred-lit? (literal pred)
  "[Cyc] Returns T iff LITERAL is positive and has PRED as its arg0.
Returns NIL if LITERAL is negative or is not an EL formula."
  (and (el-formula-p literal)
       (not (el-negation-p literal))
       (eq (literal-arg0 literal) pred)))

(defun isa-lit? (lit)
  "[Cyc] Returns T iff LIT is a positive binary literal whose predicate is #$isa."
  (and (pred-lit? lit #$isa)
       (binary-lit-p lit)))

(defun isa-hl-var-hl-var-lit? (lit)
  (and (pred-lit? lit #$isa)
       (binary-lit-p lit)
       (hl-variable-p (formula-arg1 lit))
       (hl-variable-p (formula-arg2 lit))))

(defun quoted-isa-lit? (lit)
  "[Cyc] Returns T iff LIT is a positive binary literal whose predicate is #$quoted-isa."
  (and (pred-lit? lit #$quotedIsa)
       (binary-lit-p lit)))

(defun genls-lit? (lit)
  "[Cyc] Returns T iff LIT is a positive binary literal whose predicate is #$genls."
  (and (pred-lit? lit #$genls)
       (binary-lit-p lit)))

(defun tou-lit? (lit)
  "[Cyc] Returns T iff LIT is a positive binary literal whose predicate is #$termOfUnit."
  (and (pred-lit? lit #$termOfUnit)
       (binary-lit-p lit)))

(defun tou-asent? (asent)
  "[Cyc] Returns T iff ASENT is a binary atomic sentence whose predicate is #$termOfUnit."
  (and (el-binary-formula-p asent)
       (atomic-sentence-with-pred-p asent #$termOfUnit)))

(defun holds-in-lit? (literal)
  "[Cyc] Returns T iff LITERAL is a positive binary literal whose predicate is #$holdsIn."
  (and (pred-lit? literal #$holdsIn)
       (binary-lit-p literal)))

(defun true-sentence-lit? (literal)
  "[Cyc] Returns T iff LITERAL is a positive unary literal whose predicate is #$trueSentence."
  (and (pred-lit? literal #$trueSentence)
       (unary-lit-p literal)))

(defun forward-non-trigger-literal-lit? (literal)
  "[Cyc] Returns T iff LITERAL is a positive unary literal whose predicate is #$forwardNonTriggerLiteral."
  (and (pred-lit? literal #$forwardNonTriggerLiteral)
       (unary-lit-p literal)))

(defun evaluate-lit? (literal &optional (var? 'cyc-var?))
  "[Cyc] Returns T iff LITERAL is a positive binary literal whose predicate is #evaluate, whose arg1 is a variable, string, or number, and whose arg2 is a NAT."
  (and (pred-lit? literal #$evaluate)
       (binary-lit-p literal)
       (naut? (literal-arg2 literal))
       (or (funcall var? (literal-arg1 literal))
           (missing-larkc 30469))))

(defun ist-sentence-p (sentence)
  (and (possibly-sentence-p sentence)
       (eq #$ist (sentence-arg0 sentence))
       (el-binary-formula-p sentence)))

(defun ist-of-atomic-sentence-p (sentence)
  "[Cyc] Returns T iff SENTENCE is an #$ist around an atomic sentence."
  (ist-of-atomic-sentence-int sentence nil))

(defun ist-of-atomic-sentence-int (sentence pred)
  (declare (ignore pred))
  (when (ist-sentence-p sentence)
    (missing-larkc 30501)
    ;; more body in the .java file if we're going to try to fill it in
    ))

(defun ist-sentence-with-chlmt-p (sentence)
  (and (ist-sentence-p sentence)
       (chlmt-p (sentence-arg1 sentence))))

(defun true-sentence-p (sentence)
  (and (possibly-sentence-p sentence)
       (eq #$trueSentence (sentence-arg0 sentence))
       (el-unary-formula-p sentence)))

(defun true-sentence-of-atomic-sentence-p (sentence)
  "[Cyc] Returns T iff SENTENCE is a #$trueSentence around an atomic sentence."
  (when (true-sentence-p sentence)
    (let ((subsentence (sentence-arg1 sentence)))
      (when (possibly-sentence-p subsentence)
        (let ((predicate (sentence-arg0 subsentence)))
          (and (fort-p predicate)
               (predicate? predicate)))))))

(defun kappa-asent-p (object)
  (and (possibly-atomic-sentence-p object)
       (kappa-predicate-p (atomic-sentence-predicate object))))

(defun kappa-predicate-p (object)
  (and (el-formula-with-operator-p object #$Kappa)
       (el-binary-formula-p object)
       (list-of-type-p #'el-var? (nat-arg1 object))))

(defun lambda-function-p (object)
  (and (el-formula-with-operator-p object #$Lambda)
       (el-binary-formula-p object)
       (list-of-type-p #'el-var? (nat-arg1 object))))

(defun mt-designating-literal? (literal)
  "[Cyc] Returns T iff LITERAL is a literal whose arg0 is a #$MicrotheoryDesignatingRelation."
  (with-all-mts
    (and (el-formula-p literal)
         (mt-designating-relation? (literal-arg0 literal)))))

(defun valid-argnum-p (arg)
  "[Cyc] Returns T iff ARG could be a valid arg index for some formula, i.e. if (formula-arg <formula> ARG) is a sensible thing to try."
  (valid-arity-p arg))

(defun valid-arity-p (arity)
  "[Cyc] Returns T iff ARG could be a valid arity for some formula."
  (and (integerp arity)
       (>= arity 0)))

(defun predicate-spec? (term &optional (var-func #'var-spec?))
  "[Cyc] Returns T iff TERM is a predicate, a variable, or a NAT whose result is a predicate."
  (cond
    ((predicate? term) t)
    ((funcall var-func term) t)
    ((function-term? term) (let ((nat (reify-when-closed-naut term)))
                             (if (fort-p nat)
                                 (predicate? nat)
                                 (missing-lakrc 3706))))))

(defun function-spec? (term &optional (var-func #'var-spec?))
  "[Cyc] Returns T iff TERM is a denotational function, a variable, or a NAT whose result is a denotational function."
  (cond
    ((functor? term) t)
    ((funcall var-func term) t)
    ((function-term? term) (let ((nat (reify-when-closed-naut term)))
                             (if (fort-p nat)
                                 (functor? nat)
                                 (missing-larkc 6974))))))

(defun hl-relation-expression? (object)
  "[Cyc] Returns T iff OBJECT is a nart."
  (nart-p object))

(defun relation-expression? (object)
  "[Cyc] Returns T iff OBJECT is either a nat or an EL formula with a predicate, variable, or logical operator as its arg0."
  (or (el-relation-expression? object)
      (hl-relation-expression? object)))

(defun el-formula? (formula)
  "[Cyc] Returns T iff FORMULA is an EL formula with a predicate, variable, or logical operator as its arg0."
  (and (wf-wrt-sequence-vars? formula)
       (or (el-atomic-sentence? formula)
           (el-non-atomic-sentence? formula))))

(defun el-atomic-sentence? (formula)
  "[Cyc] Returns T iff FORMULA is an EL formula with a predicate (or variable) as its arg0."
  (atomic-sentence? formula))

(defun el-non-atomic-sentence? (formula)
  "[Cyc] Returns T iff FORMULA is an EL formula with a logical operator as its arg0."
  (and (el-formula-p formula)
       (logical-op? (formula-arg0 formula))))

(defun atomic-sentence? (object)
  "[Cyc] Returns T iff OBJECT is an EL formula with a predicate as its arg0."
  (and (el-formula-p object)
       (predicate-spec? (formula-arg0 object))
       (relation-syntax? object)))

(defun formula-non-var-symbols (formula &optional (var? #'cyc-var?))
  (remove-if var? (flatten formula)))

(defun referenced-variables (formula &optional (var? #'cyc-var?))
  (formula-gather formula var?))

(defun literal-variables (relation &optional (var? #'cyc-var?) (include-sequence-vars? t))
  (relation-variables relation var? include-sequence-vars?))

(defun relation-variables (literal &optional (var? #'cyc-var?) (include-sequence-vars? t))
  (cond
    ((funcall var? literal) (list literal))
    ((el-negation-p literal) (literal-variables (formula-arg1 literal) var?))
    (t (let ((vars nil))
         (dolistn (arg term (formula-terms literal (if include-sequence-vars?
                                                       :regularize
                                                       :ignore)))
           (cond
             ;; TODO - empty COND body again.
             ((member? term vars :test #'equal))
             ((funcall var? term) (push term vars))
             ((unreified-skolem-term? term) (push term vars))
             ((naut? term) (missing-larkc 30622))
             ((sentence? term) (missing-larkc 30652)))
           (incf arg))
         ;; TODO - this can probably be nreverse?  I don't think it shares structure with any input
         (reverse vars)))))

(defun scoped-vars (formula &optional (var? #'cyc-var?))
  (let ((relation (formula-operator formula))
        (scoped-vars nil))
    (when (fort-p relation)
      (dolist (position (scoping-args relation))
        (let ((arg (formula-arg formula position)))
          (cond
            ;; TODO - this makes the funcall before checking for consp of arg?
            ((funcall var? arg) (push arg scoped-vars))
            ((consp arg) (dolist (sub-arg (formula-terms arg))
                           (when (funcall var? sub-arg)
                             (push sub-arg scoped-vars))))))))
    (nreverse scoped-vars)))

(defun wf-wrt-sequence-vars? (formula)
  "[Cyc] Returns T iff FORMULA has no ill-formed sequence variable syntax at the top level."
  (not (formula-with-sequence-non-var? formula)))


(defun sentence-free-variables (sentence &optional bound-vars (var-func #'cyc-var?)
                                           (include-sequence-vars? t))
  (if (and include-sequence-vars?
           (not (tree-find-if #'scoping-relation-p sentence))
           (not (tree-find-if #'cycl-quoted-term-p sentence))
           (not (tree-find-if #'expand-subl-fn-p sentence)))
      (set-difference (expression-gather sentence var-func)
                      bound-vars)
      (sentence-free-variables-int sentence bound-vars var-func include-sequence-vars?)))

(defun sentence-free-variables-int (sentence &optional bound-vars (var-func #'cyc-var?)
                                               (include-sequence-vars? t))
  (cond
    ((and *inside-quote*
          (not (expression-find-if #'fast-escape-quote-term-p sentence)))
     nil)
    ((member? sentence bound-vars)
     nil)
    ((funcall var-func sentence)
     (list sentence))
    ((atom sentence)
     nil)
    ((not (tree-find-if var-func sentence))
     nil)
    ((el-negation-p sentence)
     (sentence-free-variables-int (sentence-arg1 sentence) bound-vars var-func include-sequence-vars?))
    ((or (el-conjunction-p sentence)
         (el-disjunction-p sentence))
     (let ((result nil))
       (dolist (arg (sentence-args sentence (if include-sequence-vars? :regularize :ignore)))
         (dolist (var (sentence-free-variables-int arg bound-vars var-func include-sequence-vars?))
           (pushnew var result)))
       result))
    
    ((or (el-general-implication-p sentence)
         (el-exception-p sentence))
     (let ((result (nreverse (sentence-free-variables-int (sentence-arg1 sentence)
                                                          bound-vars var-func
                                                          include-sequence-vars?))))
       (dolist (var (sentence-free-variables-int (sentence-arg2 sentence) bound-vars var-func include-sequence-vars?))
         (pushnew var result))
       (nreverse result)))
    
    ((possibly-el-regularly-quantified-sentence-p sentence)
     (sentence-free-variables-int (quantified-sub-sentence sentence)
                                  (cons (quantified-var sentence) bound-vars)
                                  var-func include-sequence-vars?))
    
    ((cycl-bounded-existential-sentence-p sentence)
     (let ((result (nreverse (sentence-free-variables-int (quantified-sub-sentence sentence)
                                                          (cons (quantified-var sentence)
                                                                bound-vars)
                                                          var-func include-sequence-vars?)))
           (possible-var (missing-larkc 30558)))
       (when (cyc-var? possible-var)
         (push possible-var result))
       (nreverse result)))
    
    ((mt-designating-literal? sentence)
     (missing-larkc 30484))
    
    ((atomic-sentence? sentence)
     (literal-free-variables sentence bound-vars var-func include-sequence-vars?))
    
    ((and (el-formula-p sentence)
          (funcall var-func (sentence-arg0 sentence)))
     (literal-free-variables sentence bound-vars var-func include-sequence-vars?))
    
    (t (let ((result nil))
         (dolist (var (formula-gather sentence var-func nil))
           (unless (member? var bound-vars)
             (pushnew var result)))
         result))))


(defun literals-free-variables (literals &optional bound-vars (var? #'cyc-var?)
                                           (include-sequence-vars? t))
  (let (variables)
    (cond
     ((consp literals)
      (dolist (literal literals)
        (setf variables
              (ordered-union variables (literal-free-variables literal
                                                               bound-vars var?
                                                               include-sequence-vars?)))))
     ((member? literals bound-vars)
      variables)
     
     ((funcall var? literals)
      (push literals variables))

     (t variables))))

(defun literal-free-variables (literal &optional old-bound-vars (var? #'cyc-var?)
                                         (include-sequence-vars? t))
  (relation-free-variables literal old-bound-vars var? include-sequence-vars?))

(defun relation-free-variables (relation &optional old-bound-vars (var? #'cyc-var?)
                                           (include-sequence-vars? t))
  (cond
    ((subl-quote-p relation)
     nil)
    
    ((expand-subl-fn-p relation)
     (relation-free-variables (formula-arg1 relation) old-bound-vars
                              var? include-sequence-vars?))
    
    ((fast-escape-quote-term-p relation)
     (let ((*inside-quote* nil))
       (relation-free-variables (formula-arg1 relation) old-bound-vars
                                           var? include-sequence-vars?)))
    
    ((member? relation old-bound-vars)
     nil)
    
    ((and (not *inside-quote*)
          (funcall var? relation))
     (list relation))
    
    ((fast-quote-term-p relation)
     (let ((*inside-quote* t))
       (relation-free-variables (formula-arg1 relation) old-bound-vars
                                var? include-sequence-vars?)))
    
    ((el-negation-p relation)
     (relation-free-variables (formula-arg1 relation) old-bound-vars
                              var? include-sequence-vars?))
    
    ((el-formula-p relation)
     (let* ((reln (formula-arg0 relation))
            (new-bound-vars (scoped-vars relation var?))
            (bound-vars (union old-bound-vars new-bound-vars))
            (ans nil))
       (dolistn (psn term (formula-terms relation (if include-sequence-vars?
                                                      :regularize
                                                      :ignore)))
         (cond
           ;; TODO - empty bodies mean do nothing on this pass?
           ((member? term bound-vars))
           ((and (proper-list-p term)
                 (subsetp term bound-vars)))
           ((and (not (and *within-query*
                           *canonicalize-el-template-vars-during-queries?*))
                 (leave-variables-at-el-for-arg? reln psn)))
           
           ((and (not *inside-quote*)
                 (funcall var? term))
            (pushnew term ans))
           
           ((and (within-tou-gaf?)
                 (eq psn 2)))
           
           ((and (not *inside-quote*)
                 (unreified-skolem-term? term))
            (dolist (var (second term))
              (when (and (not (member? var bound-vars))
                         (funcall var? var))
                (pushnew var ans))))
           
           ((naut? term)
            (dolist (var (missing-larkc 30620))
              (pushnew var ans)))
           
           ((or (sentence? term)
                (and (expression-find-if #'scoping-relation-expression? term nil)))
            (dolist (var (sentence-free-variables-int term bound-vars
                                                      var? include-sequence-vars?))   
              (pushnew var ans)))
           
           (*inside-quote*)
           (t (dolist (var (expression-gather term var?))
                (unless (member? var bound-vars)
                  (pushnew var ans))))))
       (nreverse ans)))))


(defun scoping-relation-expression? (expression)
  (when (relation-expression? expression)
    (let ((relation (formula-operator expression)))
      (scoping-relation-p relation))))


(defun arg-types-prescribe-unreified? (rel pos)
  "[Cyc] Do the arg-types applicable to arg POS of relation REL require the arg to be an unreified function?"
  (and (eq rel #$termOfUnit)
       (eq pos 2)))

(deflexical *arg1-subl-list-relations* (list #$ExpandSubLFn
                                             #$SkolemFunctionFn
                                             #$SkolemFuncNFn
                                             #$Lambda
                                             #$Kappa
                                             #$initialSublists
                                             #$NestedDagFn
                                             #$SubDagFn
                                             #$UnifyFn
                                             #$DagFn))

(deflexical *arg2-subl-list-relations* (list #$initialSublists
                                             #$accessSlotForType
                                             #$ksImportationTemplate
                                             #$NestedDagFn
                                             #$SubDagFn
                                             #$UnifyFn))

(defun arg-types-prescribe-tacit-term-list? (rel pos)
  "[Cyc] Do the arg-types applicable to arg POS of relation REL> require the arg to be a SubL list?
Note that SubL lists are deprecated except for special cases in the CycL grammar itself."
  (case pos
    (1 (member? rel *arg1-subl-list-relations*))
    (2 (member? rel *arg2-subl-list-relations*))))

(defun el-error (level format-str &optional arg1 arg2 arg3)
  (cond
    ((>= *el-trace-level* level)
     (cerror "continue anyway" format-str arg1 arg2 arg3))
    ((>= (+ 2 *el-trace-level*) level)
     (warn format-str arg1 arg2 arg3))))

