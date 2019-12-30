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


;; No macros declared in this file, should be pretty complete.


(defun fort-has-type? (fort type &optional mt)
  (when (fort-p fort)
    ;; TODO - this is very obviously a macro expansion that encompasses these bindings
    (let ((*mt* (update-inference-mt-relevance-mt mt))
          (*relevant-mt-function* (update-inference-mt-relevance-function mt))
          (*relevant-mts* (update-inference-mt-relevance-function mt))
          (*sbhl-justification-search-p* nil)
          (*sbhl-apply-unwind-function-p* nil)
          (*suspend-sbhl-cache-use?* nil))
      (sbhl-predicate-relation-p (get-sbhl-module #$isa) fort type mt))))

(defun isa-quantifier? (term &optional mt)
  "[Cyc] Is TERM a quantifier?"
  (if (fort-p term)
      (quantifier-p term)
      (or (isa? term #$Quantifier mt)
          (quiet-sufficient-defns-admit? #$Quantifier term mt))))

(defun fort-has-type-in-any-mt? (fort type)
  (with-all-mts
    (fort-has-type? fort #$Collection)))

(defun collection-in-any-mt? (fort)
  "[Cyc] Is FORT a collection in any mt?"
  (with-all-mts
      (fort-has-type? fort #$Collection)))

(defun collection? (fort)
  "[Cyc] Is FORT a collection?"
  (collection-in-any-mt? fort))

(defun collection-p (fort)
  "[Cyc] Is FORT a collection?"
  (collection-in-any-mt? fort))

(defun isa-collection? (term &optional mt)
  "[Cyc] Is TERM a collection?"
  (if (fort-p term)
      (collection? term)
      (or (isa? term #$Collection mt)
          (missing-larkc 5373))))

(defun predicate-in-any-mt? (fort)
  "[Cyc] Is FORT a predicate in any mt?"
  (with-all-mts
      (fort-has-type? fort #$Predicate)))

(defun predicate? (fort)
  "[Cyc] Is FORT a predicate?"
  (predicate-in-any-mt? fort))

(defun predicate-p (fort)
  "[Cyc] Is FORT a predicate?"
  (predicate-in-any-mt? fort))

(defun isa-predicate? (term &optional mt)
  "[Cyc] Is TERM a predicate?"
  (if (fort-p term)
      (predicate-p term)
      (or (isa? term #$Predicate mt)
          (missing-larkc 5375))))

(defun function-in-any-mt? (fort)
  "[Cyc] Is FORT in the *FORTS-TYPED-FUNCTION-DENOTATIONAL*"
  (with-all-mts
      (fort-has-type fort #$Function-Denotational)))

(defun functor? (fort)
  "[Cyc] Is FORT a non-predicate function?"
  (non-predicate-function? fort))

(defun non-predicate-function? (fort)
  "[Cyc] Is FORT a non-predicate function?"
  (function-in-any-mt? fort))

(defun function? (fort)
  "[Cyc] Is FORT a non-predicate function?"
  (non-predicate-function? fort))

(defun mt-in-any-mt? (fort)
  "[Cyc] Return T iff FORT is a microtheory in any mt.
Note Currently (2/00) we assume that microtheories must be defined as such in the *MT-MT*."
  (with-all-mts
      (fort-has-type fort #$Microtheory)))

(defun mt? (fort)
  "[Cyc] Is FORT a microtheory?"
  (mt-in-any-mt? fort))

(defun isa-mt? (term &optional mt)
  "[Cyc] Is TERM a microtheory?"
  (cond
    ((fort-p term) (mt? term))
    ((hlmt-naut-p term) (hlmt? term))
    (t (or (isa? term #$Microtheory)
           (missing-larkc 5377)))))

(defun relation-p (fort)
  "[Cyc] Is FORT a relation?"
  (fort-has-type-in-any-mt? fort #$Relation))

(defun sentential-relation-p (fort)
  "[Cyc] Is FORT a sentential relation?"
  (or (logical-connective-p fort)
      (quantifier-p fort)))

(defun anti-symmetric-binary-predicate-p (fort)
  "[Cyc] Is FORT an anti-symmetric binary predicate?"
  (fort-has-type-in-any-mt? fort #$AntiSymmetricBinaryPredicate))

(defun anti-transitive-binary-predicate-p (fort)
  "[Cyc] Is FORT an anti-transitive binary predicate?"
  (fort-has-type-in-any-mt? fort #$AntiTransitiveBinaryPredicate))

(defun asymmetric-binary-predicate-p (fort)
  "[Cyc] Is FORT an asymmetric binary predicate?"
  (fort-has-type-in-any-mt? fort #$AsymmetricBinaryPredicate))

(defun bookkeeping-predicate-p (fort)
  "[Cyc] Is FORT a bookkeeping predicate?"
  (fort-has-type-in-any-mt? fort #$BookkeepingPredicate))

(defun broad-microtheory-p (fort)
  "[Cyc] Is FORT a broad microtheory?"
  (fort-has-type-in-any-mt? fort #$BroadMicrotheory))

(defun commutative-relation? (relation)
  "[Cyc] Return T iff RELATION is a commutative relation."
  (commutative-relation-p relation))

(defun commutative-relation-p (fort)
  "[Cyc] Is FORT a commutative relation?"
  (fort-has-type-in-any-mt? fort #$CommutativeRelation))

(defun commutative-predicate-p (fort)
  "[Cyc] Is FORT a commutative predicate?"
  (and (commutative-relation-p fort)
       (predicate-p fort)))

(defun distributing-meta-knowledge-predicate-p (fort)
  "[Cyc] Is FORT a distributing meta knowledge predicate?"
  (fort-has-type-in-any-mt? fort #$DistributingMetaKnowledgePredicate))

(defun el-relation-p (fort)
  "[Cyc] Is FORT an EL relation?"
  (fort-has-type-in-any-mt? fort #$ELRelation))

(defun isa-el-relation? (term &optional mt)
  "[Cyc] Is TERM an EL relation?"
  (if (fort-p term)
      (el-relation-p term)
      (or (isa? term #$ELRelation mt)
          (missing-larkc 5395))))

(defun evaluatable-function-p (fort)
  "[Cyc] Is FORT an evaluatable function?"
  (fort-has-type-in-any-mt? fort #$EvaluatableFunction))

(defun evaluatable-predicate-p (fort &optional mt)
  "[Cyc] Is FORT an evaluatable predicate?"
  (fort-has-type? fort #$EvaluatablePredicate mt))

(defun irreflexive-binary-predicate-p (fort)
  "[Cyc] Is FORT an irreflexive binary predicate?"
  (fort-has-type-in-any-mt? fort #$IrreflexiveBinaryPredicate))

(defun logical-connective-p (fort)
  "[Cyc] Is FORT a logical connective?"
  (fort-has-type-in-ny-mt? fort #$LogicalConnective))

(defun isa-logical-connective? (term &optional mt)
  "[Cyc] Is TERM a logical connective?"
  (if (fort-p term)
      (logical-connective-p term)
      (or (isa? term #$LogicalConnective mt)
          (missing-larkc 5400))))

(defun microtheory-designating-relation-p (fort)
  "[Cyc] Is FORT a microtheory designating relation?"
  (fort-has-type-in-any-mt? fort #$MicrotheoryDesignatingRelation))

(defun partially-commutative-relation-p (fort)
  "[Cyc] Is FORT a partially commutative relation?"
  (fort-has-type-in-any-mt? fort #$PartiallyCommutativeRelation))

(defun partially-commutative-predicate-p (fort)
  "[Cyc] Is FORT a partially commutative predicate?"
  (isa? fort #$PartiallyCommutativePredicate *anect-mt*))

(defun quantifier-p (fort)
  "[Cyc] Is FORT a quantifier?"
  (fort-has-type-in-any-mt? fort #$Quantifier))

(defun reflexive-binary-predicate-p (fort)
  "[Cyc] Is FORT a reflexive binary predicate?"
  (fort-has-type-in-any-mt? fort #$ReflexiveBinaryPredicate))

(defun reifiable-function-p (fort)
  "[Cyc] Is FORT a reifiable function?"
  (fort-has-type-in-any-mt? fort #$ReifiableFunction))

(defun isa-reifiable-function? (term &optional mt)
  (if (fort-p term)
      (reifiable-function-p term)
      (or (isa? term #$ReifiableFunction mt)
          (missing-larkc 5407))))

(defun scoping-relation-p (fort)
  "[Cyc] Is FORT a scoping relation?"
  (fort-has-type-in-any-mt? fort #$ScopingRelation))

(defun isa-scoping-relation? (term &optional mt)
  "[Cyc] Is TERM a scoping relation?"
  (if (fort-p term)
      (scoping-relation-p term)
      (or (isa? term #$ScopingRelation mt)
          (missing-larkc 5409))))

(defun sibling-disjoint-collection-p (fort)
  "[Cyc] Is FORT a sibling disjoint collection?"
  (fort-has-type-in-any-mt? fort #$SiblingDisjointCollectionType))

(defun skolem-function-p (fort)
  "[Cyc] Is FORT a skolem function?"
  (fort-has-typein-any-mt? fort #$SkolemFunction))

(defun symmetric-binary-predicate-p (fort)
  "[Cyc] Is FORT a symmetric binary predicate?"
  (fort-has-type-in-any-mt? fort #$SymmetricBinaryPredicate))

(defun transitive-binary-predicate-p (fort)
  "[Cyc] Is FORT a transitive binary predicate?"
  (fort-has-type-in-any-mt? fort #$TransitiveBinaryPredicate))

(defun variable-arity-relation-p (fort)
  "[Cyc] Is FORT a variable arity relation?"
  (fort-has-type-in-any-mt? fort #$VariableArityRelation))

(defun isa-variable-arity-relation? (term &optional mt)
  "[Cyc] Is TERM a variable arity relation?"
  (if (fort-p term)
      (variable-arity-relation-p term)
      (or (isa? term #$VariableArityRelation mt)
          (missing-larkc 5414))))

(defun bounded-existential-quantifier-p (object)
  (with-all-mts
      (fort-has-type? object #$ExistentialQuantifier-Bounded)))

(defun evaluatable-relation-contextualized-p (fort)
  "[Cyc] Is FORT a contextualized evaluatable relation?"
  (fort-has-type-in-any-mt? fort #$EvaluatableRelation-Contextualized))

(deflexical *proprietary-constant?-caching-state* nil)


