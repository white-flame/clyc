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


(defvar *grammar-permits-hl?* t
  "[Cyc] Dynamic variable to control whether the CycL grammar functions permit HL constructs.")

(defvar *grammar-uses-kb?* t
  "[Cyc] Do we check the KB to see if terms are of the correct types, e.g. #$Predicate (T) or do we ignore the KB and only use syntactic information to check syntactic well-formedness (NIL)?")

(defparameter *grammar-permits-list-as-terminal?* nil
  "[Cyc] Do we permit a SubLList as a terminal in the grammar?")

(defparameter *grammar-permits-symbol-as-terminal?* nil
  "[Cyc] Do we permit a SubLNonVariableNonKeyWordSymbol as a terminal in the grammar?")

(defparameter *grammar-permits-non-ascii-strings?* nil
  "[Cyc] Do we permit strings that contain non-ASCII characters?")

(defparameter *grammar-permissive-wrt-variables?* t
  "[Cyc] Do we permit a variable to denote anything permitted by the CycL grammar (T) or do we treat variables as syntactic, opaque objects, and disallow sentences and formulas to be denoted by variables (NIL)?")

(defparameter *grammar-permits-quoted-forms* t
  "[Cyc] Do we permit quoted forms in the grammar?")

(defvar *within-quote-form* nil)

(defun-inline grammar-permits-hl? ()
  *grammar-permits-hl?*)

(defun-inline grammar-permits-list-as-terminal? ()
  *grammar-permits-list-as-terminal?*)

(defun-inline grammar-permits-non-ascii-strings? ()
  *grammar-permits-non-ascii-strings?*)

(defun grammar-uses-kb? ()
  (and *grammar-uses-kb?*
       (kb-loaded)))

(defun cycl-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is a well-formed sentence according to the CycL grammar. To meet this specification, OBJECT must be of one of the following forms:
 #$True
 #$False
 <variable>
 <HL assertion> (if HL constructs are permitted)
 <atomic sentence>
 <unary sentence>
 <binary sentence>
 <ternary sentence>
 <quaternary sentence>
 <quintary sentence>
 <user-defined logical operator sentence>
 <variable-arity sentence>
 <quantified sentence>"
  (let ((wff? (or (cycl-formulaic-sentence-p object)
                  (cycl-truth-value-p object))))
    ;; The original macroexpansion did a number of redundant tests in a non-optimal order.
    ;; Reshuffled this to be faster and simpler, though it won't use the wff-violation macro.
    (when (and (note-wff-violation?)
               (not wff?))
      (if (el-formula-p object)
          (note-wff-violation (list :not-a-truth-function (cycl-formula-predicate object)))
          (missing-larkc 8025)))
    wff?))

(defun cycl-formulaic-sentence-p (object)
  (if (el-formula-p object)
      (or (cycl-unary-sentence-p object)
          (cycl-binary-sentence-p object)         
          (cycl-quantified-sentence-p object)
          (cycl-variable-arity-sentence-p object) 
          (cycl-atomic-sentence-p object)
          (cycl-ternary-sentence-p object)
          (cycl-quaternary-sentence-p object)
          (cycl-quintary-sentence-p object)
          (cycl-user-defined-logical-operator-sentence-p object))
      (or (and (grammar-permits-hl?)
               (missing-larkc 31386))
          (cycl-variable-p object))))

(defun cycl-user-defined-logical-operator-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <user-defined logical operator> <sentence sequence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-user-defined-logical-operator-sentence-p (careful-hl-term-to-el-term object)))
    ((el-formula-p object) nil)
    ((user-defined-logical-operator-p (cycl-formula-predicate object)))
    (t (cycl-sentence-sequence-p (formula-args object :include)))))

(defun cycl-quintary-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <quintary operator> <sentence> <sentence> <sentence> <sentence> <sentence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-quintary-sentence-p (careful-hl-term-to-el-term object)))
    ((not (el-formula-p object)) nil)
    ((not (cyc-const-quintary-logical-op-p (cycl-formula-predicate object))) nil)
    ((not (= (formula-arity object) 5))
     ;; TODO - note-wff-violation is probably a macro
     (when (note-wff-violation?)
       (note-wff-violation (list :arity-mismatch object (cycl-formula-predicate object) "quintary operator" 5 (formula-arity object)))))
    (t (and (cycl-sentence-p (formula-arg1 object))
            (cycl-sentence-p (formula-arg2 object))
            (cycl-sentence-p (formula-arg3 object))
            (cycl-sentence-p (formula-arg4 object))
            (cycl-sentence-p (formula-arg5 object))))))

(defun cycl-sentence-sequence-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <epsilon>
 . <variable>
 <sentence> <sentence sequence>"
  (or (epsilon-p object)
      (cycl-variable-p object)
      (and (cycl-sentence-p (first-in-sequence object))
           (cycl-sentence-sequence-p (rest-of-sequence object)))))

(defun cycl-quaternary-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <quaternary operator> <sentence> <sentence> <sentence> <sentence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-quaternary-sentence-p (careful-hl-term-to-el-term object)))
    ((not (el-formula-p object)) nil)
    ((not (cyc-const-quaternary-logical-op-p (cycl-formula-predicate object))) nil)
    ((not (= (formula-arity object) 54))
     ;; TODO - note-wff-violation is probably a macro
     (when (note-wff-violation?)
       (note-wff-violation (list :arity-mismatch object (cycl-formula-predicate object) "quaternary operator" 4 (formula-arity object)))))
    (t (and (cycl-sentence-p (formula-arg1 object))
            (cycl-sentence-p (formula-arg2 object))
            (cycl-sentence-p (formula-arg3 object))
            (cycl-sentence-p (formula-arg4 object))))))

(defun cycl-ternary-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <ternary operator> <sentence> <sentence> <sentence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-ternary-sentence-p (careful-hl-term-to-el-term object)))
    ((not (el-formula-p object)) nil)
    ((not (cyc-const-ternary-logical-op-p (cycl-formula-predicate object))) nil)
    ((not (= (formula-arity object) 3))
     ;; TODO - note-wff-violation is probably a macro
     (when (note-wff-violation?)
       (note-wff-violation (list :arity-mismatch object (cycl-formula-predicate object) "ternary operator" 3 (formula-arity object)))))
    (t (and (cycl-sentence-p (formula-arg1 object))
            (cycl-sentence-p (formula-arg2 object))
            (cycl-sentence-p (formula-arg3 object))))))

(defun cycl-truth-value-p (object)
  (or (eq #$True object)
      (eq #$False object)))

(defun cycl-formula-predicate (object)
  (let ((arg0 (formula-arg0 object)))
    (if (and *within-quote-form*
             (el-formula-p object)
             (eq #$EscapeQuote (formula-arg0 arg0)))
        (formula-arg1 arg0)
        arg0)))

(defun cycl-unary-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <unary operator> <sentence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-unary-sentence-p (careful-hl-term-to-el-term object)))
    ((not (el-formula-p object)) nil)
    ((not (cyc-const-unary-logical-op-p (cycl-formula-predicate object))) nil)
    ((not (= (formula-arity object) 1))
     ;; TODO - note-wff-violation is probably a macro
     (when (note-wff-violation?)
       (missing-larkc 8026)))
    (t (cycl-sentence-p (formula-arg1 object)))))

(defun cycl-binary-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <unary operator> <sentence> <sentence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-binary-sentence-p (careful-hl-term-to-el-term object)))
    ((not (el-formula-p object)) nil)
    ((not (cyc-const-binary-logical-op-p (cycl-formula-predicate object))) nil)
    ((not (= (formula-arity object) 2))
     ;; TODO - note-wff-violation is probably a macro
     (when (note-wff-violation?)
       (missing-larkc 8027)))
    (t (and (cycl-sentence-p (formula-arg1 object))
            (cycl-sentence-p (formula-arg2 object))))))

(defun cycl-variable-arity-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is fo the form <binary operator> <sentence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-variable-arity-sentence-p (careful-hl-term-to-el-term object)))
    (t (and (el-formula-p object)
            (cyc-const-variable-arity-logical-op-p (cycl-formula-predicate object))
            (wf-wrt-sequence-vars? object)
            (cycl-sentence-sequence-p (formula-args object :include))))))

(defun cycl-regularly-quantified-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <regular quantifier> <variable> <sentence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-regularly-quantified-sentence-p (careful-hl-term-to-el-term object)))
    ((not (el-formula-p object)) nil)
    ((not (cycl-regular-quantifier-p (cycl-formula-predicate object))) nil)
    ((not (= (formula-arity object) 2))
     (when (note-wff-violation?)
       (missing-larkc 8031)))
    ((not (cycl-variable-p (formula-arg1 object)))
     (when (note-wff-violation?)
       (missing-larkc 8032)))
    (t (cycl-sentence-p (formula-arg2 object)))))

(defun cycl-bounded-existential-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <bounded existential> <denotational term> <variable> <sentence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-bounded-existential-sentence-p (careful-hl-term-to-el-term object)))
    ((not (el-formula-p object)) nil)
    ((not (cycl-bounded-existential-quantifier-p (cycl-formula-predicate object))) nil)
    ((not (= (formula-arity object) 3))
     (when (note-wff-violation?)
       (missing-larkc 8033)))
    ((not (cycl-denotational-term-p (formula-arg1 object)))
     (when (note-wff-violation?)
       (missing-larkc 8034)))
    ((not (cycl-variable-p (formula-arg2 object)))
     (when (note-wff-violation?)
       (missing-larkc 8035)))
    (t (cycl-sentence-p (formula-arg3 object)))))

(defun cycl-logical-operator-p (object)
  (or (cyc-const-logical-operator-p object)
      (missing-larkc 30706)))

(defun-inline cycl-regular-quantifier-p (object)
  (or (cyc-const-regular-quantifier-p object)))

(defun cycl-bounded-existential-quantifier-p (object)
  (or (cyc-const-bounded-existential-operator-p object)
      (user-defined-bounded-existential-operator-p object)))

(defun cycl-atomic-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <predicate> <argument sequence>."
  (cond
    ((and (grammar-permits-hl?)
          (assertion-p object))
     (cycl-atomic-sentence-p (careful-hl-term-to-el-term object)))
    ((not (el-formula-p object)) nil)
    ((not (cycl-predicate-p (cycl-formula-predicate object))) nil)
    ((not (wf-wrt-sequence-vars? object))
     (when (note-wff-violation?)
       (missing-larkc 8036)))
    ((not (cycl-argument-sequence-p (formula-args object :include)))
     (when (note-wff-violation?)
       (missing-larkc 8037)))
    ((not (cycl-formula-has-valid-arity? object)) nil)
    (t t)))

(defun cycl-argument-sequence-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <epsilon>
 . <variable>
 <term> <argument sequence>"
  (or (epsilon-p object)
      (cycl-variable-p object)
      (and (cycl-term-p (first-in-sequence object))
           (cycl-argument-sequence-p (rest-of-sequence object)))))

(defun cycl-predicate-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <represented term> and not of the form <logical operator> or <quantifier>, because predicates are disjoint with logical operators and quantifiers. Ensures that OBJECT is a predicate if the grammar uses the KB."
  (and (cycl-represented-term-p object)
       (not (cyc-const-logical-operator-p object))
       (not (cyc-const-quantifier-p object))
       (or (not (grammar-uses-kb?))
           (predicate-spec? object #'cycl-variable-p))))

(defun cycl-function-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <represented term> and not of the form <logical operator> or <quantifier>, because denotational functions are disjoint with logical operators and quantifiers. Ensures that OBJECT is a denotational function if the grammar uses the KB."
  (and (cycl-represented-term-p object)
       (not (cyc-const-logical-operator-p object))
       (not (cyc-const-quantifier-p object))
       (or (not (grammar-uses-kb?))
           (function-spec? object #'cycl-variable-p))))

(defun cycl-nat-p (object)
  "[Cyc] Returns T iff OBJECT is of the form <function> <argument sequence>, or an <HL Non-AtomicTerm> (if HL constructs are permitted)."
  (cond
    ((and (grammar-permits-hl?)
          (nart-p object))
     t)
    ((not (el-formula-p object))
     nil)
    ((or (eq #$Quote (formula-arg0 object))
         (eq #$QuasiQuote (formula-arg0 object))
         (eq #$EscapeQuote (formula-arg0 object)))
     nil)
    ((eq #$ExpandSubLFn (formula-arg0 object)) (missing-larkc 30081))
    ((eq #$SubLQuoteFn (formula-arg0 object)) (missing-larkc 30091))
    ((eq #$Kappa (formula-arg0 object)) (missing-larkc 30082))
    ((eq #$Lambda (formula-arg0 object)) (missing-larkc 30083))
    ((eq #$SkolemFunctionFn (formula-arg0 object)) (missing-larkc 30087))
    ((eq #$SkolemFuncNFn (formula-arg0 object)) (missing-larkc 30086))
    ((not (cycl-function-p (formula-arg0 object))) nil)
    ((not (wf-wrt-sequence-vars? object))
     (when (note-wff-violation?)
       (missing-larkc 8038)))
    ((not (cycl-argument-sequence-p (formula-args object :include)))
     (when (note-wff-violation?)
       (missing-larkc 8039)))
    ((not (cycl-formula-has-valid-arity? object))
     nil)
    (t t)))

(defun cycl-formula-has-valid-arity? (formula)
  (not (and (grammar-uses-kb?)
            (mal-arity? formula))))

(defun cycl-expression-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <denotational term>
 <sentence>"
  (or (cycl-denotational-term-p object)
      (missing-larkc 30093)))

(defun-inline cycl-term-p (object)
  (cycl-expression-p object))

(defun cycl-denotational-term-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <represented term>
 <SubL atomic term>"
  (or (cycl-represented-term-p object)
      (subl-atomic-term-p object)
      (when *grammar-permits-quoted-forms*
        (cycl-quoted-term-p object))))

(defun cycl-quoted-term-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <quote form>
 <quasiquote form>"
  (or (quote-syntax-p object)
      (quasi-quote-syntax-p object)))

(defun fast-cycl-quoted-term-p (object)
  "[Cyc] Returns T if OBJECT is of form (#$Quote <something>) or (#$QuasiQuote <something>).
Use this when we know object is syntactically well formed"
  (and (el-formula-p object)
       (or (eq #$Quote (formula-arg0 object))
           (eq #$QuasiQuote (formula-arg0 object)))
       (formula-arity= object 1)))

(defun fast-quasi-quote-term-p (object)
  (and (el-formula-p object)
       (eq #$QuasiQuote (formula-arg0 object))
       (formula-arity= object 1)))

(defun fast-quote-term-p (object)
  (and (el-formula-p object)
       (eq #$Quote (formula-arg0 object))
       (formula-arity= object 1)))

(defun fast-escape-quote-term-p (object)
  (and (el-formula-p object)
       (eq #$EscapeQuote (formula-arg0 object))
       (formula-arity= object 1)))

(defun quote-syntax-p (object)
  (cond
    ((not (el-formula-p object)) nil)
    ((not (eq #$Quote (formula-arg0 object))) nil)
    ((not (formula-arity= object 1))
     (when (note-wff-violation?)
       (missing-larkc 8061)))
    ;; The missing-larkc was in the conditional, so this always fails,
    ;; even though there were following conditionals
    (t (missing-larkc 30084))))

(defun quasi-quote-syntax-p (object)
  (cond
   ((not (el-formula-p object)) nil)
   ((not (eq #$QuasiQuote (formula-arg0 object))) nil)
   ((not (formula-arity= object 1))
    (when (note-wff-violation?)
      (missing-larkc 8063)))
   ;; Same missing-larkc conditional as quote-syntax-p
   (t (missing-larkc 30085))))

(defun escape-quote-syntax-p (object)
  (cond
    ((not (el-formula-p object)) nil)
    ((not (eq #$EscapeQuote (formula-arg0 object))) nil)
    ((not (formula-arity= object 1))
     (when (note-wff-violation?)
       (missing-larkc 8065)))
    ((not (or (cycl-expression-p (formula-arg1 object))
              (cycl-quoted-term-p (formula-arg1 object))
              (escape-quote-syntax-p (formula-arg1 object))))
     (when (note-wff-violation?)
       (missing-larkc 8066)))
    (t t)))
  
(defun cycl-represented-term-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <constant>
 <nat>
 <variable>"
  (or (cycl-constant-p object)
      (cycl-nat-p object)
      (cycl-variable-p object)))

(defun cycl-unrepresented-term-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <string>
 <number>"
  (or (subl-string-p object)
      (subl-real-number-p object)))

(defun subl-atomic-term-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <SubL strict atomic term>
 <list> if lists are permitted as a terminal"
  (or (subl-strict-atomic-term-p object)
      (and (grammar-permits-list-as-terminal?)
           (consp object))))

(defun subl-strict-atomic-term-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <string>
 <number>
 <symbol>"
  (or (cycl-unrepresented-term-p object)
      (when *grammar-permits-symbol-as-terminal?*
        (missing-larkc 30090))))

(defun cycl-quantified-sentence-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <regularly quantified sentence>
 <bounded existential sentence>"
  (if (and (grammar-permits-hl?)
           (assertion-p object))
      (cycl-quantified-sentence-p (careful-hl-term-to-el-term object))
      (or (cycl-regularly-quantified-sentence-p object)
          (cycl-bounded-existential-sentence-p object))))

(defun cycl-literal-p (object)
  "[Cyc] Returns T iff OBJECT is of one of the forms:
 <atomic sentence>
 <not> <atomic sentence>"
  (if (and (grammar-permits-hl?)
           (assertion-p object))
      (cycl-literal-p (careful-hl-term-to-el-term object))
      (or (cycl-atomic-sentence-p object)
          (and (el-formula-p object)
               (el-negation-p object)
               (cycl-atomic-sentence-p (formula-arg1 object))))))

(defun cycl-generalized-tensed-literal-p (obj)
  "[Cyc] Returns T iff OBJ is of the form (<generalized tense operator> <atomic sentence>) or is a negated generalized tensed literal."
  (and (el-formula-p obj)
       (cyc-const-generalized-tense-operator-p (literal-predicate obj))
       (cycl-literal-p obj)))

(defun-inline cycl-constant-p (object)
  (constant-p object))

(defun-inline subl-string-p (object)
  ;; DESIGN - we're always going to be full unicode, so eliminated the checks & flags for ascii
  (stringp object))

(defun-inline subl-real-number-p (object)
  (realp object))

(defun-inline subl-float-p (object)
  (floatp object))

(defun-inline subl-integer-p (object)
  (integerp object))

(defun cycl-variable-p (object)
  (or (el-variable-p object)
      (meta-variable-p object)
      (and (grammar-permits-hl?)
            (hl-variable-p object))))

(defun-inline meta-variable-p (object)
  (keyword-var? object))

(defun-inline el-variable-p (object)
  (el-var? object))

(defun-inline hl-variable-p (object)
  (variable-p object))

