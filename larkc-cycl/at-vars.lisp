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

;; AT = arg-type?

;; For all variable declarations, note-state-variable-documentation registers the docstring, and the variable symbol name is pushnew'd to a state variable list.  Wrapping this into a macro, although none seemed to be defined in this file.
(defmacro def-state-var (name val varlist &body (&optional docstring (definer 'defparameter)))
  `(progn
     (,definer ,name ,val ,docstring)
     (note-state-variable-documentation ',name ,docstring)
     (pushnew ',name ,varlist)))

(defmacro def-at-state-var (name val &body (&optional docstring (definer 'defparameter)))
  `(def-state-var ,name ,val *at-state-variables* ,docstring ,definer))

(defmacro def-defn-state-var (name val &body (&optional docstring (definer 'defparameter)))
  `(def-state-var ,name ,val *defn-state-variables* ,docstring ,definer))





(def-at-state-var *semantic-dnf-hl-filtering-enabled* nil
  "[Cyc] Should the inference engine use arg-type tests in order to filter-out non-wff inferences?")
(def-at-state-var *at-check-fn-symbol?* t
  "[Cyc] Require function symbols be instances of #$Function-Denotational?")
(def-at-state-var *at-check-arg-genls?* t
  "[Cyc] Enforce #$argGenl constraints?")
(def-at-state-var *at-check-arg-isa?* t
  "[Cyc] Enforce argIsa constraints?")
(def-at-state-var *at-check-arg-quoted-isa?* t
  "[Cyc] Enforce argQuotedIsa constraints?")
(def-at-state-var *at-check-arg-not-isa?* t
  "[Cyc] Enforce argNotIsa constraints?")
(def-at-state-var *at-check-arg-types?* t
  "[Cyc] Enforce arg-typing constraints?")
(def-at-state-var *at-possibly-check-defining-mts?* nil
  "[Cyc] Is #$definingMt constraint enforcement available?")
(def-at-state-var *at-check-defining-mts?* t
  "[Cyc] Enforce #$definingMt constraints?")

(defun-inline at-check-defining-mts-p ()
  (and *at-check-defining-mts?*
       *at-possibly-check-defining-mts?*))

(def-at-state-var *at-check-inter-arg-isa?* t
  "[Cyc] Enforce #$interArgIsa constraints?")
(def-at-state-var *at-check-inter-arg-not-isa?* t
  "[Cyc] Enforce #$interArgNotIsa constraints?")
(def-at-state-var *at-check-inter-arg-genl?* nil
  "[Cyc] Enforce #$interArgGenl constraints?")
(def-at-state-var *at-check-non-constant-inter-arg-isa?* t
  "[Cyc] Enforce interArgIsa constraints for non-constants?")
(def-at-state-var *at-check-non-constant-inter-arg-genl?* t
  "[Cyc] Enforce interArgGenl constraints for non-constants?")
(def-at-state-var *at-check-non-constant-inter-arg-format?* t
  "[Cyc] Enforce interArgFormat1-2 (and similar) constraints for non-constants?")
(def-at-state-var *at-check-not-isa-disjoint?* t
  "[Cyc] Enforce #$argIsa constraints for non-constants?")
(def-at-state-var *at-check-not-quoted-isa-disjoint?* t
  "[Cyc] Enforce #$argQuotedIsa constraints for non-constants?")
(def-at-state-var *at-check-not-genls-disjoint?* t
  "[Cyc] Enforce #$argGenl constraints for non-constants?")
(def-at-state-var *at-check-not-mdw?* t
  "[Cyc] Enforce :NOT-DISJOINT constraints using mdw module?")
(def-at-state-var *at-check-not-sdc?* t
  "[Cyc] Enforce :NOT-DISJOINT constraints using sdc module?")
(def-at-state-var *at-check-arg-format?* t
  "[Cyc] Enforce #$argformat constraints?")
(def-at-state-var *at-check-sef?* t
  "[Cyc] Enforce #$singleEntryFormatInArgs #$argformat constraints?")
(def-at-state-var *at-check-relator-constraints?* t
  "[Cyc] Enforce asymmetric-predicate and similar constraints?")
(def-at-state-var *at-ensure-consistency?* nil
  "[Cyc] Enforce consistency at constraint standard (:not-isa-disjoint et al) in addition to entails standard (:isa et al)?")
(def-at-state-var *at-pred-constraints* (list :asymmetric-predicate
                                              :anti-symmetric-predicate
                                              :irreflexive-predicate
                                              :anti-transitive-predicate
                                              :negation-preds
                                              :negation-inverses))
(def-at-state-var *at-predicate-violations* nil
  "[Cyc] Relevant at predicate violations.")
(def-at-state-var *at-check-inter-assert-format-w/o-arg-index?* t
  "[Cyc] Enforce inter-assert formats even when pred is only index?")
(def-at-state-var *at-check-inter-assert-format-w/o-arg-index-gaf-count-threshold* 100
  "[Cyc] Max number of relevant gafs to permit enforcing inter-assert formats when pred is only index?")
(def-at-state-var *fag-search-limit* nil
  "[Cyc] Max number of relevant gafs to permit using find-accessible-gaf?")
(def-at-state-var *at-gaf-search-limit* 100
  "[Cyc] Default max number of relevant gafs to permit enforcing at constraints using find-accessible-gaf (e.g., negation-preds et al)?")
(def-at-state-var *at-check-inter-arg-format?* t
  "[Cyc] Enforce #$interArgFormat1-2 (and similar) constraints?")
(def-at-state-var *at-check-inter-arg-different?* t
  "[Cyc] Enforce #$interArgDifferent (and similar) constraints?")
(def-at-state-var *at-check-genl-preds?* t
  "[Cyc] Enforce arg-constraints of genlPreds?")
(def-at-state-var *at-check-genl-inverses?* t
  "[Cyc] Enforce arg-constraints of genlInverse?")
(def-at-state-var *at-include-isa-literal-constraints* t
  "[Cyc] Include explicit #$isa literals when computing type constraints?")
(def-at-state-var *at-include-genl-literal-constraints* t
  "[Cyc] Include explicit #$genls literals when computing type constraints?")
(def-at-state-var *gather-at-constraints?* nil
  "[Cyc] Collect applicable at constraints?")
(def-at-state-var *gather-at-assertions?* nil
  "[Cyc] Collect applicable at assertions?")
(def-at-state-var *gather-at-format-violations?* nil
  "[Cyc] Collect relevant at format violations?")
(def-at-state-var *gather-at-different-violations?* nil
  "[Cyc] Collect relevant at different violations?")
(def-at-state-var *gather-at-predicate-violations?* nil
  "[Cyc] Collect relevant at predicate violations (e.g., asymmetry, negationPreds)?")
(def-at-state-var *at-format-violations* nil
  "[Cyc] Relevant at format violations.")
(def-at-state-var *at-different-violations* nil
  "[Cyc] Relevant at different violations.")
(def-at-state-var *within-at-suggestion?* nil
  "[Cyc] Is at module currently trying to formula a suggested fix?")
(def-at-state-var *within-at-mapping?* nil
  "[Cyc] Is at module currently executing a mapping search?.")
(def-at-state-var *at-break-on-failure?* nil
  "[Cyc] Break when an at violation is encountered?")
(def-at-state-var *accumulating-at-violations?* nil
  "[Cyc] Continue at checks even after constraint failure?")
(def-at-state-var *noting-at-violations?* nil
  "[Cyc] Should arg-type violations be recorded for presentation?")
(def-at-state-var *at-trace-level* 1
  "[Cyc] Controls extent of tracing, warnings, etc., for the arg-type module [0 .. 5].")
(def-at-state-var *at-test-level* 3
  "[Cyc] Controls extent of testing for the arg-type module [0 .. 5].")
(def-at-state-var *appraising-disjunct?* nil
  "[Cyc] Is the formula being considered by the arg-type module a disjoined sub-formula?")
(def-at-state-var *within-decontextualized?* nil
  "[Cyc] Is the formula being considered by arg-type module a decontextualized literal?")
(def-at-state-var *within-disjunction?* nil
  "[Cyc] Is the subformula being canonicalized within a disjunction?")
(def-at-state-var *within-conjunction?* nil
  "[Cyc] Is the subformula being canonicalized within a conjunction?")
(def-at-state-var *within-negated-disjunction?* nil
  "[Cyc] Is the subformula being canonicalized within both negation and disjunction?")
(def-at-state-var *within-negated-conjunction?* nil
  "[Cyc] Is the subformula being canonicalized within both negation and conjunction?")
(def-at-state-var *within-function?* nil
  "[Cyc] Is the formula being canonicalized referenced within a function expression?")
(def-at-state-var *within-predicate?* nil
  "[Cyc] Is the formula being canonicalized referenced within a predicate?")
(def-at-state-var *within-tou-gaf?* nil
  "[Cyc] Is the formula being canonicalized a termOfUnit gaf?")

(defun-inline within-tou-gaf? () 
  "[Cyc] Return T iff the formula being canonicalized isa a termOfUnit gaf."
  *within-tou-gaf?*)

(def-at-state-var *relax-arg-constraints-for-disjunctions?* t
  "[Cyc] Should arg-type constraints be weakened (possibly-true vs provably-true) within disjuncts?")
(def-at-state-var *at-relax-arg-constraints-for-opaque-expansion-nats?* t
  "[Cyc] Within expansions should arg-type constraints be forgiven for expandable nats?")
(def-at-state-var *at-admit-consistent-nauts?* t
  "[Cyc] Should the arg-type module admit unreified function terms that are consistent (i.e., possibly-true vs provably-true) wrt arg-type constraints?")
(def-at-state-var *at-admit-consistent-narts?* t
  "[Cyc] Should the arg-type module admit reified function terms that are consistent (i.e., possibly-true vs provably-true) wrt arg-type constraints?")
(def-at-state-var *at-result* nil
  "[Cyc] Accumulates results of current at query")
(def-at-state-var *at-some-arg-isa?* nil
  "[Cyc] Is any arg-isa constraint found during at arg-isa analysis for a given arg, relation and argnum?")
(def-at-state-var *at-some-arg-isa-required?* nil
  "[Cyc] Must there be some arg-isa constraint applicable to an arg for a given arg, relation, argnum to be wf?")

(defun-inline at-some-arg-isa-required? () 
  "[Cyc] Returns boolean, must there be some arg-isa constraint applicable to an arg for a given arg, relation, argnum to be WF?"
  *at-some-arg-isa-required?*)

(def-at-state-var *at-consider-multiargs-at-pred?* t
  "[Cyc] During arg-type analysis do we consider multi-arg (argsIsa) constraints for specified args (arg1).")

(defun consider-multiargs-at-pred? () 
  "[Cyc] Returns boolean, during arg-type analysis do we consider multi-arg (argsIsa) constraints for specified args (arg1)?"
  *at-consider-multiargs-at-pred?*)

(def-at-state-var *at-isa-constraints* (make-hash-table)
  "[Cyc] Accumulates applicable at isa constraints.")
(def-at-state-var *at-genl-constraints* (make-hash-table)
  "[Cyc] Accumulates applicable at genl constraints.")
(def-at-state-var *at-format-constraints* (make-hash-table)
  "[Cyc] Accumulates applicable at format constraints.")
(def-at-state-var *at-different-constraints* (make-hash-table)
  "[Cyc] Accumulates applicable at different constraints.")
(def-at-state-var *at-isa-assertions* (make-hash-table)
  "[Cyc] Accumulates applicable at isa assertions.")
(def-at-state-var *at-genl-assertions* (make-hash-table)
  "[Cyc] Accumulates applicable at genl assertions.")
(def-at-state-var *at-format-assertions* (make-hash-table)
  "[Cyc] Accumulates applicable at format assertions.")
(def-at-state-var *at-different-assertions* (make-hash-table)
  "[Cyc] Accumulates applicable at different assertions.")
(def-at-state-var *at-mode* nil
  "[Cyc] The type of at test currently being applied (e.g., :arg-genls).")
(def-at-state-var *at-constraint-type* nil
  "[Cyc] The type of at constraint currently being applied (e.g., :isa :genls).")
(def-at-state-var *at-arg-type* nil
  "[Cyc] Type of arg being considered within arg-type search [:naut :weak-fort :strong-fort].")
(def-at-state-var *at-base-fn* nil
  "[Cyc] Fn applied to each applicable arg-constraint assertion during at search.")
(def-at-state-var *at-formula* nil
  "[Cyc] The formula being appraised.")
(def-at-state-var *at-pred* nil
  "[Cyc] The current at-module constraint pred (e.g., #$interArgIsa1-2).")
(def-at-state-var *at-inverse* nil
  "[Cyc] The inverse of the current at-module constraint pred (e.g., #$interArgIsa2-1).")
(def-at-state-var *at-reln* nil
  "[Cyc] The relation whose args are currently being appraised.")
(def-at-state-var *at-arg* nil
  "[Cyc] The particular arg that is currently being appraised.")
(def-at-state-var *at-argnum* nil
  "[Cyc] The position of the arg that is currently being appraised.")
(def-at-state-var *at-arg1* nil
  "[Cyc] The 1st arg of the literal or function-term that is currently being appraised.")
(def-at-state-var *at-arg2* nil
  "[Cyc] The 2nd arg of the literal or function-term that is currently being appraised.")
(def-at-state-var *at-ind-argnum* nil
  "[Cyc] The position of the independent arg that is constraining the dependent arg.")
(def-at-state-var *at-ind-arg* nil
  "[Cyc] The independent arg that is constraining the dependent arg being appraised.")
(def-at-state-var *at-ind-isa* nil
  "[Cyc] The isa of the independent arg that is constraining the dependent arg.")
(def-at-state-var *at-ind-genl* nil
  "[Cyc] The genl of the independent arg that is constraining the dependent arg.")
(def-at-state-var *at-arg-isa* nil
  "[Cyc] The isa of the dependent arg that is being appraised.")
(def-at-state-var *at-source* nil
  "[Cyc] The constant indexing the current at constraint (e.g., may be a genlPred of *at-reln*).")
(def-at-state-var *at-mapping-genl-inverses?* nil
  "[Cyc] Dynamic state variable: are we looking at inverses instead of genlPreds?")
(def-at-state-var *at-search-genl-preds?* t
  "[Cyc] Consider genlPreds during current at search?")
(def-at-state-var *at-search-genl-inverses?* t
  "[Cyc] Consider genlInverses during current at search?")

(defun-inline at-searching-genl-preds? ()
  *at-search-genl-preds?*)

(defun-inline at-searching-genl-inverses? ()
  *at-search-genl-inverses?*)

(def-at-state-var *at-profile-term* nil
  "[Cyc] The term (var, constant, ...) that is being profiled during this at analysis.")
(def-at-state-var *at-constraint-gaf* nil
  "[Cyc] The arg-type constraint assertion currently being considered.")
(def-at-state-var *include-at-constraint-gaf?* t
  "[Cyc] Boolean: Should errors reference the arg-type constraint assertion currently being considered.")
(def-at-state-var *at-var-isa* nil
  "[Cyc] The accumulating inter-reference isa constraints applicable to a given variable in a formula.")
(def-at-state-var *at-var-genl* nil
  "[Cyc] The accumulating inter-reference genl constraints applicable to a given variable in a formula.")
(def-at-state-var *at-var-types-standard* :not-disjoint
  "[Cyc] The standard for acceptable arg-type constraints applicable to variables.")
(def-at-state-var *at-assume-conjuncts-independent?* t
  "[Cyc] Whether arg-type checking for variables in a conjunction should assume that each of the conjuncts is independent.
This is true, for example, during assert, but false, for example, when wff-checking the results of some parsers.")
(def-at-state-var *current-at-violation* nil
  "[Cyc] Description of most recent violation of applicable arg-type constraints.")
(def-at-state-var *at-violations* nil
  "[Cyc] Descriptions of how a relational expresion is not valid wrt arg-type constraints.")
(def-at-state-var *at-disjoins-space* nil
  "[Cyc] SBHL space used for marking disjoins of arg types.")
(def-at-state-var *at-isa-space* nil
  "[Cyc] SBHL space used for marking all-isa of arg.")
(def-at-state-var *at-genls-space* nil
  "[Cyc] SBHL space used for marking all-genls of arg.")


(def-defn-state-var *at-defns-available?* t
  "[Cyc] Make defns available for at queries?")
(def-defn-state-var *at-apply-necessary-defns?* t
  "[Cyc] Enforce all applicable necessary defns during defn query?")
(def-defn-state-var *sort-suf-defn-assertions?* t
  "[Cyc] Are the cached suf-defn assertions sorted to promote some optimality criteria?")
(def-defn-state-var *sort-suf-function-assertions?* nil
  "[Cyc] Are the cached suf-function assertions sorted to promote some optimality criteria?")
(def-defn-state-var *at-collection-specific-defns* nil
  "[Cyc] Defns which reference (defn-collection) and so cannot be cached in defn-fn-histories."
  defvar)
(def-defn-state-var *defn-trace-level* 1
  "[Cyc] Controls extent of tracing, warnings, etc., for the arg-type module [0 .. 5].")
(def-defn-state-var *defn-test-level* 3
  "[Cyc] Controls extent of testing for the arg-type module [0 .. 5].")
(def-defn-state-var *defn-meters?* nil
  "[Cyc] Activate metering of functions within defn module?")
;; TODO - these are defvar instead of deparameter?  will these tables be dynamically bound?
(def-defn-state-var *suf-defn-cache* (make-hash-table) nil defvar)
(def-defn-state-var *suf-quoted-defn-cache* (make-hash-table) nil defvar)
(def-defn-state-var *defn-meter-caches* nil
  "[Cyc] List of caches used for metering functions in defn module.")
(def-defn-state-var *defn-collection* nil
  "[Cyc] The collection with which the current defn is associated.")
(def-defn-state-var *permitting-denotational-terms-admitted-by-defn-via-isa?* t
  "[Cyc] Should defns admit a denotation term that #$isa the *defn-collection* ?")

(defun permitting-denotational-terms-admitted-by-defn-via-isa? ()
  *permitting-denotational-terms-admitted-by-defn-via-isa?*)

(def-defn-state-var *at-defn* nil
  "[Cyc] Current defn-assertion being considered.")
(def-defn-state-var *at-defns* nil
  "[Cyc] Current defn-assertions being considered.")
(def-defn-state-var *suf-function-cache* (make-hash-table) nil defvar)
(def-defn-state-var *suf-quoted-function-cache* (make-hash-table) nil defvar)
(def-defn-state-var *at-function* nil
  "[Cyc] Current sufficient-function assertion being considered.")
(def-defn-state-var *at-functions* nil
  "[Cyc] Current sufficient-function assertions being considered.")
(def-defn-state-var *defn-fn-history-default-size* 32
  "[Cyc] The initial size of each nested defn-function cache.")
(def-defn-state-var *defn-col-history-default-size* 64
  "[Cyc] The initial size of each nested defn-collection cache.")
(def-defn-state-var *defn-col-history* :uninitialized
  "[Cyc] The cache used (by the current defn invocation) to prevent multiple calls to a single defn collection.")
(def-defn-state-var *quoted-defn-col-history* :uninitialized
  "[Cyc] The cache used (by the current quoted defn invocation) to prevent multiple calls to a single defn collection.")
(def-defn-state-var *defn-fn-history* :uninitialized
  "[Cyc] The cache used (by the current defn invocation) to prevent multiple calls to a single defn function.")
(def-defn-state-var *quoted-defn-fn-history* :uninitialized
  "[Cyc] The cache used (by the current quoted defn invocation) to prevent multiple calls to a single defn function.")
(def-defn-state-var *defn-stack* :uninitialized
  "[Cyc] A stack of defns being called, to prevent infinite recursion. The key for the hashtable is the collection-defn function, and the value is a list of arguments. It's a list because it's fine to recurse on a defn with a different argument.")
