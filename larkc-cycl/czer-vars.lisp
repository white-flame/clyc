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

(defparameter *required-arg-preds* '(#$requiredArg1Pred #$requiredArg2Pred))

(deflexical *arg-positions* '(1 2 3 4 5)
  "[Cyc] Integers denoting the most common arg positions of fixed-arity CycL relations.")

(defparameter *accumulating-semantic-violations?* nil
  "[Cyc] Suppresses resetting *arg-type-violations*.")

(defparameter *semantic-violations* nil
  "[Cyc] Descriptions of how a relational expression is not semantically valid.")

(defparameter *assertion-key* 'assertion-formula
  "[Cyc] Which function to use when accessing the formula for an assertion.")

(defparameter *nart-key* 'nart-hl-formula
  "[Cyc] Which function to use when accessing the formula for a nart.")

(defparameter *rf-key* 'rf-formula
  "[Cyc] Which function to use when accessing the formula for a reified formula (the genl of nart and assertion).")

(deflexical *implication-operators* (list #$implies))
(deflexical *logical-operators* (list #$not
                                      #$or
                                      #$xor
                                      #$and
                                      #$equiv
                                      #$implies
                                      #$exceptFor
                                      #$exceptWhen
                                      #$forAll
                                      #$thereExists
                                      #$thereExistExactly
                                      #$thereExistAtLeast
                                      #$thereExistAtMost))

(deflexical *skolem-function-functions* (list #$SkolemFunctionFn
                                              #$SkolemFuncNFn)
  "[Cyc] Cyc constants that denote functions whose ranges are skolem functions.")

(deflexical *arg-isa-binary-preds* (list #$arg1Isa
                                         #$arg2Isa
                                         #$arg3Isa
                                         #$arg4Isa
                                         #$arg5Isa
                                         #$arg6Isa
                                         #$argsIsa))

(deflexical *arg-isa-ternary-preds* (list #$argIsa
                                          #$argAndRestIsa))
(deflexical *arg-quoted-isa-binary-preds* (list #$arg1QuotedIsa
                                                #$arg2QuotedIsa
                                                #$arg3QuotedIsa
                                                #$arg4QuotedIsa
                                                #$arg5QuotedIsa
                                                #$arg6QuotedIsa
                                                #$argsQuotedIsa))
(deflexical *arg-quoted-isa-ternary-preds* (list #$argQuotedIsa
                                                 #$argAndRestQuotedIsa))
(deflexical *arg-genl-binary-preds* (list #$arg1Genl
                                          #$arg2Genl
                                          #$argsGenl
                                          #$arg3Genl
                                          #$arg4Genl
                                          #$arg5Genl
                                          #$arg6Genl))
(deflexical *arg-genl-ternary-preds* (list #$argGenl
                                           #$argAndRestGenl))
(deflexical *arg-format-binary-preds* (list #$arg1Format
                                            #$arg2Format
                                            #$arg3Format
                                            #$arg4Format
                                            #$arg5Format
                                            #$arg6Format))
(deflexical *arg-format-ternary-preds* (list #$argFormat))

(deflexical *meta-arg-type* #$CycLAssertion
  "[Cyc] Arg-type for meta predicates.")
(deflexical *possibly-meta-arg-type* #$CycLIndexedTerm
  "[Cyc] Arg-type for meta predicates.")

(defparameter *variables-that-cannot-be-sequence-variables* nil
  "[Cyc] A dynamic stack of variables that are currently not permitted to be used as sequence variables (e.g. because they're scoped).")
(defparameter *el-supports-dot-syntax?* t
  "[Cyc] Are sequence variables permitted?")
(deflexical *el-supports-variable-arity-skolems?* t)
(defparameter *el-supports-contractions?* nil
  "[Cyc] Is support for contractions (inverse #$expansions) enabled?")
(defvar *inside-quote* nil
  "[Cyc] Variable to keep track if we are inside a quote form.")
(defparameter *new-canonicalizer?* nil
  "[Cyc] Whether to use the code for the new canonicalizer.")
(defparameter *within-canonicalizer?* nil
  "[Cyc] Transient state variable; is T during the execution of canonicalizing functions.")
(defparameter *form-of-clausal-form* nil
  "[Cyc] Canonicalizer state variable [:cnf :dnf].")
(defparameter *must-enforce-semantics?* nil)
(defparameter *el-trace-level* 0
  "[Cyc] Controls tracing level for canonicalizer [0..5].")
(defparameter *canon-verbose?* nil
  "[Cyc] Controls whether the formula is printed after each step of canonicalization. Only set to T for debugging purposes.")
;; TODO - resolve this to #'el-var when dealing with dependencies
(defparameter *var?* 'el-var?
  "[Cyc] Default predicate to identify variables.")
(defparameter *subordinate-fi-ops?* nil)
(defparameter *cry?* t
  "[Cyc] Flag to break on error conditions.")
(defparameter *minimal-skolem-arity?* nil
  "[Cyc] Should the canonicalizer include only free vars referenced in existentially quantified formulas in argument lists of the resulting skolem functions?")
(defparameter *skolemize-during-asks?* nil
  "[Cyc] Should the canonicalizer translate existentially quantified vars into skolem functions during asks?")
(defparameter *drop-all-existentials?* nil
  "[Cyc] Should the canonicalizer, when canonicalizing existentials, simply drop them (like it does by default during asks)?
This setting, if true, overrides the combination of *WITHIN-ASK* and *SKOLEMIZE-DURING-ASKS?*, but does not override the case of *TURN-EXISTENTIALS-INTO-SKOLEMS?* being false, which will cause no existential handling at all to be done.")
(defparameter *leave-skolem-constants-alone?* nil
  "[Cyc] Should the canonicalizer, when canonicalizing existentials that are not in the scope of any other variable, simply drop them (like it does by default during asks)?
This setting, if true, overrides the combination of *WITHIN-ASK* and *SKOLEMIZE-DURING-ASKS?*, but does not override the case of *TURN-EXISTENTIALS-INTO-SKOLEMS?* being false, which will cause no existential handling at all to be done.")
(defparameter *forbid-quantified-sequence-variables?* :assert-only
  "[Cyc ]Whether to enforce criterion Q2 in the Sequence Variable Specification, namely:
Q2: Within asserts, sequence variables can only be universally quantified; using existentially quantified variables as sequence variables is not permitted.
If T, Q2 is enforced.
If NIL, Q2 is not enforced.
If :assert-only, Q2 is enforced iff (within-assert?).")
(defparameter *use-skolem-constants?* nil
  "[Cyc] Should the canonicalizer create and reference skolem constants instead of zero-arity skolem functions?")
(defparameter *canonicalize-gaf-commutative-terms?* t
  "[Cyc] Should commutative terms of gafs be sorted?")
(defparameter *canon-var-function* :default
  "[Cyc] The function that the canonicalizer uses internally to check whether something is a variable.
:default means that it will use the default function cyc-var? (this is slightly more efficient than just making the default be #'cyc-var?)")
(defparameter *canonical-variable-type* :kb-var 
  "[Cyc] Determines the type of variables produced by the canonicalzer [:el-var :kb-var].")
(defparameter *standardize-variables-memory* nil
  "[Cyc] Stores the variable rename mappings formed while standardizing variables during canonicalization.")
(defparameter *distributing-meta-knowledge?* nil
  "[Cyc] Is distributing meta-knowledge over multiple assertions permitted?")
(defparameter *distribute-meta-over-common-el?* t
  "[Cyc] Should meta-knowledge distribute over multiple assertions when those assertions all share a common el formula?")
(defparameter *find-uncanonical-decontextualized-assertions?* t
  "[Cyc] If a decontextualized assertion is in the wrong mt, should the canonicalizer, if asked to look up that assertion, find it?  If T, it will find it. If NIL, it will treat it like any other uncanonical assertion and fail to find it.")
(defparameter *canonicalize-el-template-vars-during-queries?* t  
  "[Cyc] Should EL variables in EL template args be canonicalized into HL variables during asks?
If T, then queries like (expansion genls (implies (isa ?OBJ :ARG1) (isa ?OBJ :ARG2))) will not be interpreted as a boolean query, and will return ((?OBJ . ?OBJ)) instead of ((T . T)).  If it is set to nil, then queries like (expansion genls ?WHAT) will be interpreted as a boolean query, and will return NIL instead of the expansion of genls.")
(defparameter *robust-assertion-lookup* nil
  "[Cyc] Controls whether, upon failing to find an assertion, a more thorough (and more time-consuming) lookup is performed.
You can set this to T or NIL if you want to control its behavior.")
(defparameter *robust-nart-lookup* :default
  "[Cyc] Controls whether, upon failing to find a nart, a more thorough (and more time-consuming) lookup is performed.
You can set this to T or NIL if you want to control its behavior.")
(defparameter *recanonicalizing-candidate-nat?* nil
  "[Cyc] Dynamic variable set while recanonicalizing a nat for robust nart lookup.")
(defparameter *el-var-blist* nil 
  "[Cyc] Stores the variable rename mappings formed while standardizing variables during uncanonicalization.")
(defparameter *gathering-quantified-fn-terms?* nil
  "[Cyc] Control var used to collect non-ground reifiable fn terms.")
(defparameter *expand-el-relations?* t
  "[Cyc] Should #$ELRelations be automatically expanded by the precanonicalizer?")
(defparameter *canonicalize-all-sentence-args?* nil
  "[Cyc] Should all sentence args (of literals or denotational functions) be canonicalized into their el version?")
(defparameter *canonicalize-tensed-literals?* t
  "[Cyc] Should tensed literals be canonicalized into their time quantified version?")
(defparameter *add-term-of-unit-lits?* t)
(defparameter *turn-existentials-into-skolems?* t
  "[Cyc] If you set this to NIL, the canonicalizer will be severely crippled. Beware!")
(defparameter *reify-skolems?* t)
(defparameter *create-narts-regardless-of-whether-within-assert?* nil)
(defparameter *canonicalize-functions?* t)
(defparameter *canonicalize-terms?* t)
(defparameter *canonicalize-literals?* t)
(defparameter *canonicalize-disjunction-as-enumeration?* nil
  "[Cyc] Whether to canonicalize a disjunction over a common predicate as an #$elementOf expression.")
(defparameter *canonicalize-variables?* t)
(defparameter *implicitify-universals?* t
  "[Cyc] Whether to eliminate universals which could be removed and still maintain the logical equivalence of the sentence if they are viewed as implicitly encapsulating it.")
(defparameter *assume-free-vars-are-existentially-bound?* nil
  "[Cyc] Whether the clausifier will assume that free variables are existentially bound, as opposed to the default which is universally bound. This ought to be the mode in which the clausifier is run for queries.")
(defparameter *encapsulate-var-formula?* t
  "[Cyc] Translate variables appearing as logical operators into encapsulated literals?")
(defparameter *encapsulate-intensional-formula?* t
  "[Cyc] Translate intensional (e.g., negated universally quantified) formulas into encapsulated negative literals?")
(defparameter *czer-quiescence-iteration-limit* 10
  "[Cyc] If an expression fails to quiesce after 10 iterations, give up and deem it ill-formed.")
(defparameter *clause-el-var-names* nil)
(defparameter *el-symbol-suffix-table* nil
  "dynamic table of uniquifying el var suffixes")
(defparameter *within-negation?* nil
  "[Cyc] Is canonicalization occuring within scope of a negation?")
(deflexical *hl-pred-order* (list #$isa
                                  #$genls)
  "[Cyc] Preferred order of preds wrt canonicalization.")
(defparameter *control?* nil
  "[Cyc] Temp: used to control canonicalizer to include (= nil) or exclude (= t) experimental code.")
(defparameter *control-1* nil
  "[Cyc] Temp: used to control canonicalizer to include (= nil) or exclude (= t) experimental code.")
(defparameter *control-2* nil
  "[Cyc] Temp: used to control canonicalizer to include (= nil) or exclude (= t) experimental code.")
(defparameter *control-3* nil
  "[Cyc] Temp: used to control canonicalizer to include (= nil) or exclude (= t) experimental code.")
(defparameter *control-4* nil
  "[Cyc] Temp: used to control canonicalizer to include (= nil) or exclude (= t) experimental code.")
(defparameter *control-5* nil
  "[Cyc] Temp: used to control canonicalizer to include (= nil) or exclude (= t) experimental code.")
(defparameter *control-6* nil
  "[Cyc] Temp: used to control canonicalizer to include (= nil) or exclude (= t) experimental code.")
(defparameter *control-eca?* nil
  "[Cyc] Temp: used to control canonicalizer to include (= nil) or exclude (= t) experimental code.")
(defparameter *czer-memoization-state* nil
  "[Cyc] Dynamically bound to a memoization state for the canonicalizer.")
(defparameter *use-czer-fort-types?* t
  "[Cyc] Whether to use czer-fort-types at all.")
(defparameter *use-czer-fort-types-globally?* t
  "[Cyc] Whether to always use czer-fort types. If NIL, they will only be used within the canonicalizer. If *use-czer-fort-types?* is NIL, this variable doesn't matter.")
(deflexical *canonicalizer-directive-predicates* (list #$canonicalizerDirectiveForArg
                                                       #$canonicalizerDirectiveForAllArgs
                                                       #$canonicalizerDirectiveForArgAndRest)
  "[Cyc] The set of predicates which specify #$CanonicalizerDirectives. Not to be confused with the set of all instances of #$CanonicalizerDirectivePredicate, which is much broader.")
(defparameter *ununiquify-el-vars?* nil
  "[Cyc] Whether the uncanonicalizer should remove uniquifying numerical suffixes on EL variables.")
(defparameter *unremove-universals?* t
  "[Cyc] Whether the uncanonicalizer should insert #$forAlls around unquantified variables.")
(defparameter *uncanonicalize-tensed-literals?* t
  "[Cyc] Whether the uncanonicalizer should rephrase sentences in terms of #$was, #$willBe, etc.")
(defparameter *recanonicalizing?* nil
  "[Cyc] Is an existing assertion being recanonicalized?")
(defparameter *recanonicalizing-candidate-assertion-stack* nil
  "[Cyc] Used for recursion detection")
(defparameter *noting-ill-formed-meta-args?* nil
  "[Cyc] Whether el-meta should set the value of *RECAN-ILL-FORMED-META-ARGS?*")
(defparameter *recan-ill-formed-meta-args?* nil
  "[Cyc] Bound by el-meta when called from the recanonicalizer, so that the recanonicalizer can correctly analyze problems with finding meta assertions (which may be due to uncanonicality).")
(defparameter *simplify-sentence?* t)
(defparameter *simplify-literal?* t)
(defparameter *simplify-implication?* t)
(defparameter *simplify-non-wff-literal?* t
  "[Cyc] If T, non-wff literals will be reduced to #$False.")
(defparameter *try-to-simplify-non-wff-into-wff?* t
  "[Cyc] If T, non-wffs will be simplified to see if they become wff.  This usually only happens with sequence variables.")
(defparameter *trying-to-simplify-non-wff-into-wff?* nil
  "[Cyc] Transient state variable, t iff we're in the middle of trying to simplify a non-wff into a wff.")
(defparameter *simplify-using-semantics?* t
  "[Cyc] If NIL, simplify-cycl-sentence will only perform syntactic simplifications.")
(defparameter *simplify-redundancies?* nil
  "[Cyc] If T, simplify-cycl-sentence will look for redundant literals and remove them.")
(defparameter *simplify-transitive-redundancies?* nil
  "[Cyc] If T, simplify-transitive-redundancies will look for transitive redundancies and remove them.")
(defparameter *simplify-sequence-vars-using-kb-arity?* t
  "[Cyc] If T, simplify-sequence-vars will use arity information from the KB to eliminate sequence variables or convert them to term variables when possible. (It will always use the arity of logical operators to simplify.)")
(defparameter *sequence-variable-split-limit* 5
  "[Cyc] The maximum number of term variables that a sequence variable can be split into if we're using the arity information to simplify. If 0 or 1, no splitting will be performed, except that EL relations will always be split, no matter what the split limit is.")
(defparameter *simplify-equal-symbols-literal?* nil
  "[Cyc] If T, the simplifier will simplify #$equalSymbols literals with one variable argument and one bound argument by substituting the binding throughout the conjunction.  WARNING: this may cause scoping problems and has not been fully tested.")
(defparameter *simplify-true-sentence-away?* nil
  "[Cyc] If T, the simplifier will be more zealous in simplifying #$trueSentence literals away. WARNING: This may cause inference problems and has not been fully tested.")
(defglobal *skolem-axiom-table* (make-hash-table :size 2048 :test #'equal)
  "[Cyc] Table of definitions of known skolems.")
(defparameter *infer-skolem-result-isa-via-arg-constraints?* t
  "[Cyc] In the absence of explicit #$isa pos-lits, use applicable arg-isa constraints to infer the #$resultIsa of a skolem?")
(defparameter *interpolate-singleton-arg-isa?* nil
  "[Cyc] Should skolem arg-isa constraints be interpolated into a singleton set?")
(defparameter *clothe-naked-skolems?* nil
  "[Cyc] Should newly-created skolems have #$termDependsOn assertions asserted about them.
If NIL, we assume that the caller will assert the definitional assertions of the newly-created skolems.
If T, the czer will assert #$termDependsOn assertions in lieu of having actual definitional assertions.
This is for use in testing code that calls canonicalize-assert directly.")
(defparameter *preds-of-computed-skolem-gafs* (list #$isa
                                                    #$arity
                                                    #$arityMin
                                                    #$arityMax
                                                    #$resultIsa
                                                    #$resultGenl
                                                    #$resultIsaArg
                                                    #$resultGenlArg
                                                    #$arg1Isa
                                                    #$arg2Isa
                                                    #$arg3Isa
                                                    #$arg4Isa
                                                    #$arg5Isa
                                                    #$arg6Isa)
  "[Cyc] Predicates for gafs that reference skolem functions that may be computed and asserted by the canonicalizer and may be manually edited.")
(deflexical *preds-of-editable-skolem-gafs* (append (list #$isa
                                                          #$arity
                                                          #$arityMin
                                                          #$arityMax
                                                          #$resultIsa
                                                          #$resultGenl
                                                          #$resultIsaArg
                                                          #$resultGenlArg
                                                          #$resultQuotedIsa
                                                          #$evaluationResultQuotedIsa)
                                                    *arg-isa-binary-preds*
                                                    *arg-isa-ternary-preds*
                                                    *arg-quoted-isa-binary-preds*
                                                    *arg-quoted-isa-ternary-preds*
                                                    (list #$myCreator
                                                          #$myCreationTime
                                                          #$myCreationSecond
                                                          #$comment
                                                          #$sharedNotes
                                                          #$skolemizeForward))
  "[Cyc] Predicates for gafs that reference skolem functions that may be computed and asserted by the canonicalizer, or the interface time-stamper, or may be manually edited.")
(defparameter *empty-skolems* nil
  "[Cyc] Skolems having no defining assertions encountered while reinitializing *skolem-axiom-table*.")
(defparameter *mal-skolems* nil
  "[Cyc] Skolems diagnosed as having problems while reinitializing *skolem-axiom-table*.")
(defparameter *express-as-rule-macro?* nil)
(defparameter *express-as-genls?* nil)
(defparameter *express-as-arg-isa?* nil)
(defparameter *express-as-arg-genl?* nil)
(defparameter *express-as-genl-predicates?* nil)
(defparameter *express-as-genl-inverse?* nil)
(defparameter *express-as-inter-arg-isa?* nil)
(defparameter *express-as-disjoint-with?* nil)
(defparameter *express-as-negation-predicates?* nil)
(defparameter *express-as-negation-inverse?* nil)
(defparameter *express-as-reflexive?* nil)
(defparameter *express-as-symmetric?* nil)
(defparameter *express-as-transitive?* nil)
(defparameter *express-as-irreflexive?* nil)
(defparameter *express-as-asymmetric?* nil)
(defparameter *express-as-relation-type?* nil)
(defparameter *express-as-required-arg-pred?* nil)
(defparameter *tense-czer-mode* :default)
(deflexical *valid-tense-czer-modes* (list :default
                                           :query
                                           :assert))

(defun valid-tense-czer-mode-p (mode)
  (member-eq? mode *valid-tense-czer-modes*))

