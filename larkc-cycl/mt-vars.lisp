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



(deflexical *mt-var-basis-table* (make-hash-table))
(defun note-mt-var (var &optional basis)
  (when basis
    (note-mt-var-basis var basis)))

(defun note-mt-var-basis (var basis)
  (setf (gethash var *mt-var-basis-table*) basis))

(defmacro defglobal-mt-var (var default &optional basis comment)
  ;; TODO - there's an error check message about an illegal basis argument.  No idea what it's checking.
  `(progn
     (defglobal ,var ,default ,comment)
     (note-mt-var ',var ,basis)))
  
(defglobal-mt-var *mt-root* #$BaseKB nil
                  "[Cyc] The root of the microtheory heirarchy.")
(defglobal-mt-var *theory-mt-root* #$BaseKB nil
                  "[Cyc] The highest theory microtheory where assertions/deductions could possibly go.")
(defglobal-mt-var *assertible-mt-root* #$BaseKB nil
                  "[Cyc] The highest microtheory where assertions can normally be made.")
(defglobal-mt-var *assertible-theory-mt-root* #$BaseKB nil
                  "[Cyc] The highest theory microtheory where assertions can normally be made.")
(defglobal-mt-var *core-mt-floor* #$BaseKB nil
                  "[Cyc] The minimum (lowest) core microtheory.")
(defglobal-mt-var *mt-mt* #$UniversalVocabularyMt #$Microtheory
                  "[Cyc] The microtheory in which microtheories are asserted tobe instances of #$MicroTheory, and the microtheory where #$genlMt assertions go.")
(defglobal-mt-var *defining-mt-mt* #$BaseKB #$definingMt
                  "[Cyc] The microtheory where #$definintMt assertions go.  Should be the same as the *MT-MT*")
(defglobal-mt-var *decontextualized-predicate-mt* #$BaseKB #$decontextualizedPredicate
                  "[Cyc] The microtheory where #$decontextualizedPredicate assertions go.")
(defglobal-mt-var *decontextualized-collection-mt* #$BaseKB #$decontextualizedPredicate
                  "[Cyc] The microtheory where #$decontextualizedCollection assertions go.")
(defglobal-mt-var *ephemeral-term-mt* #$BaseKB #$ephemeralTerm
                  "[Cyc] The microtheory where #$ephemeralTerm gafs go.")
(defglobal-mt-var *ist-mt* #$BaseKB #$ist
                  "[Cyc] The microtheory where #$ist code supports are supported from.
It would be the microtheory where #$ist gafs would go, bu those shouldn't really be in the KB at all.")
(defglobal-mt-var *inference-related-bookkeeping-predicate-mt* #$BaseKB #$InferenceRelatedBookkeepingPredicate
                  "[Cyc] The microtheory where isa assertions to #$InferenceRelatedBookkeepingPredicate go.")
(defglobal-mt-var *anect-mt* #$UniversalVocabularyMt #$AtemporalNecessarilyEssentialCollectionType
                  "[Cyc] The mt where isas to instances of #$AtemporalNecessarilyEssentialCollectionType go.
Note that this includes #$AtemporalNecessarilyEssentialCollectionType itself, and the code assumes that these mts are the same.")
(defglobal-mt-var *broad-mt-mt* #$BaseKB #$BroadMicrotheory
                  "[Cyc] The microtheory where isa assertions to #$BroadMicrotheory go.")
(defglobal-mt-var *psc-mt* #$BaseKB #$ProblemSolvingCntxt
                  "[Cyc] The microtheory where isa assertions to #$ProblemSolvingContext go.")
(defglobal-mt-var *tou-mt* #$BaseKB #$termOfUnit
                  "[Cyc] The microtheory where #$termOfUnit assertions go.")
(defglobal-mt-var *skolem-mt* #$BaseKB #$skolem
                  "[Cyc] The microtheory where #$skolem assertions go.")
(defglobal-mt-var *thing-defining-mt* #$BaseKB #$Thing
                  "[Cyc] The microtheory where #$Thing is defined.")
(defglobal-mt-var *relation-defining-mt* #$BaseKB #$Relation
                  "[Cyc] The microtheory where #$Relation is defined.")
(defglobal-mt-var *equals-defining-mt* #$BaseKB #$equals
                  "[Cyc] The microtheory where #$equals is defined.")
(defglobal-mt-var *element-of-defining-mt* #$BaseKB #$elementOf)
(defglobal-mt-var *subset-of-defining-mt* #$BaseKB #$subsetOf)
(defglobal-mt-var *arity-mt* #$UniversalVocabularyMt #$arity
                  "[Cyc] The microtheory where #$arity assertions go.")
(defglobal-mt-var *sublid-mt* #$CycAPIMt #$subLIdentifier
                  "[Cyc] The microtheory from which #$subLIdentifier and #$uniquelyIdentifiedInType assertions would be visible.")
(defglobal-mt-var *not-assertible-mt-convention-mt* #$UniversalVocabularyMt #$notAssertibleMt
                  "[Cyc] The microtheory where #$notAssertibleMt assertions go.")
(defglobal-mt-var *default-ask-mt* #$BaseKB nil
                  "[Cyc] The default mt for asks.")
(defglobal-mt-var *default-assert-mt* #$BaseKB nil
                  "[Cyc] The default mt for asserts.")
(defglobal-mt-var *default-clone-mt* #$BaseKB nil
                  "[Cyc] The default mt for cloning sentences -- should be the common genl of the above two.")
(defglobal-mt-var *default-support-mt* #$BaseKB nil
                  "[Cyc] The default mt for HL supports -- one should be specified, but this is what to use as a backup.")
(defglobal-mt-var *default-comment-mt* #$BaseKB nil
                  "[Cyc] The default mt for asserting comments and cyclistNotes.")
(defglobal-mt-var *default-convention-mt* #$UniversalVocabularyMt nil
                  "[Cyc] The default mt for the convertion mt of a decontextualized predicate or collection, to use if none is specified in the KB.")
  
(defparameter *core-mt-optimization-enabled?* t
  "[Cyc] Temporary control variable; controls whether or not genlMt has special-case optimization for core-microtheory-p.")
(deflexical *core-mts* '(#$LogicalTruthMt
                         #$LogicalTruthImplementationMt
                         #$CoreCycLMt
                         #$CoreCycLImplementationMt
                         #$UniversalVocabularyMt
                         #$UniversalVocabularyImplementationMt
                         #$BaseKB)
  "[Cyc] The cluster of mts up near the root of the microtheory hierarchy.  Ordered from max (topmost) to min (lowest).")
(deflexical *ordered-core-mts* '((#$LogicalTruthMt . 0)
                                 (#$LogicalTruthImplementationMt . 0)
                                 (#$CoreCycLMt . 1)
                                 (#$CoreCycLImplementationMt . 1)
                                 (#$UniversalVocabularyMt . 2)
                                 (#$UniversalVocabularyImplementationMt . 2)
                                 (#$BaseKB . 3))
  "[Cyc] The cluster of mts up near the root of the microtheory hierarchy. Min numbered is topmost.")

(defun core-microtheory-p (object)
  "[Cyc] Return T iff OBJECT is a core microtheory."
  (member-eq? object *core-mts*))

(defun core-microtheory-< (mt1 mt2)
  "[Cyc] Return T iff core microtheory MT1 is lower than MT2 in the #$genlMt hierarchy."
  ;; TODO - this is actually a <= comparison, not <
  (let ((level1 (alist-lookup-without-values *ordered-core-mts* mt1))
        (level2 (alist-lookup-without-values *ordered-core-mts* mt2)))
    (and (integerp level1)
         (integerp level2)
         (<= level2 level1))))

(defun core-microtheory-> (mt1 mt2)
  "[Cyc] Return T iff core microtheory MT1 is higher than MT2 in the #$genlMt hierarchy."
  (let ((level1 (alist-lookup-without-values *ordered-core-mts* mt1))
        (level2 (alist-lookup-without-values *ordered-core-mts* mt2)))
    (and (integerp level1)
         (integerp level2)
         (<= level1 level2))))

(defun core-genl-mt? (mt1 mt2)
  (if (and (special-core-loop-mt-p mt1)
           (special-core-loop-mt-p mt2))
      t
      (core-microtheory-> mt1 mt2)))

(deflexical *special-loop-core-mts* '(#$UniversalVocabularyMt #$BaseKB))

(defun special-core-loop-mt-p (object)
  (member-eq? object *special-loop-core-mts*))

(defun minimize-mts-wrt-core (mts)
  "[Cyc] Reduces MTS by eliminating any core microtheories that are proper gelMt of microtheories in MTS."
  (multiple-value-bind (core-mts non-core-mts) (partition-list mts #'core-microtheory-p)
    (or non-core-mts
        (non-null-answer-to-singleton (extremal core-mts #'core-microtheory-<)))))

(defun maximize-mts-wrt-core (mts)
  "[Cyc] Reduces MTS by eliminating any non-core mts if there are any core mts, and then taking the maximal core mt."
  (let ((core-mts (remove-if-not #'core-microtheory-p mts)))
    (if core-mts
        (list (extremal core-mts #'core-microtheory->))
        mts)))

(defun minimize-mt-sets-wrt-core (mt-sets)
  "[Cyc] Reduces mts in MT-SETS by eliminating any proper genlMts of core microtheories in each element of MT-SETS."
  (let ((reduced-mt-sets nil))
    (dolist (mt-set mt-sets)
      (pushnew (minimize-mts-wrt-core mt-set) reduced-mt-sets :test #'sets-equal?))
    (nreverse reduced-mt-sets)))
  

