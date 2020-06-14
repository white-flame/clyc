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


(deflexical *somewhere-cached-preds-table* `((#$argsIsa . 1)
                                             (#$argAndRestIsa . 1)
                                             (,*backchain-forbidden-unless-arg-chosen* . 1)
                                             (#$canonicalizerDirectiveForArg . 1)
                                             (#$canonicalizerDirectiveForAllArgs . 1)
                                             (#$canonicalizerDirectiveForArgAndRest . 1)
                                             (#$evaluateAtEL . 1)
                                             (#$evaluateImmediately . 1)
                                             (#$genlPreds . 2)
                                             (#$typedGenlPreds . 1)
                                             (#$genlInverse . 2)
                                             (#$functionCorrespondingPredicate . 1)
                                             (#$highlyRelevantTerm . 1)
                                             (#$interArgResultIsa . 1)
                                             (#$interArgResultGenl . 3)
                                             (#$interArgGenl1-2 . 1)
                                             (#$interArgGenl2-1 . 1)
                                             (#$interArgGenl2-4 . 1)
                                             (#$interArgDifferent . 1)
                                             (#$interArgReln . 1)
                                             (#$irrelevantTerm . 1)
                                             (#$ruleTrivialForJustificationParaphrase . 1)
                                             (#$mtTrivialForJustificationParaphrase . 1)
                                             (#$modalInArg . 1)
                                             (#$notAssertible . 1)
                                             (#$relationAllExists . 2)
                                             (#$rewriteOf . 2)
                                             (#$scopingArg . 1)
                                             (#$skolemizeForward . 1)
                                             (#$typeLevelVersionInArg . 1)
                                             (#$relationAllExistsCount . 2)
                                             (#$relationAllExistsMany . 2)
                                             (#$relationAllExistsMin . 2)
                                             (#$relationAllExistsUnique . 2)
                                             (#$relationAllExistsSame . 2)
                                             (#$relationAllExistsAndOnly . 2)
                                             (#$reflexiveOn . 1)
                                             (#$requiredActorSlots-Unique . 1)
                                             (#$collRelation . 2)
                                             (#$collRelationUnique . 2)
                                             (#$requiredActorSlots . 1)
                                             (#$rolesForEventType . 1)
                                             (#$keRequirementPreds . 1)
                                             (#$roleTypesForEventType . 1)
                                             (#$requiredArg1Pred . 1)
                                             (#$preferredSubjectRole . 1)
                                             (#$subjectRoles . 1)
                                             (#$preferredIndirectObjectRole . 1)
                                             (#$indirectObjectRoles . 1)
                                             (#$preferredDirectObjectRole . 1)
                                             (#$directObjectRoles . 1)
                                             (#$posForms . 1)
                                             (#$posBaseForms . 1)
                                             (#$mtTimeIndex . 1)
                                             (#$genStringAssertion-Terse . 2)
                                             (#$genStringAssertion-Precise . 2)
                                             (#$genStringAssertion . 2)
                                             (#$predTrivialForJustificationParaphrase . 1)
                                             (#$assertionTrivialForJustificationParaphrase . 1)
                                             (#$ruleTrivialForJustificationParaphrase . 1)
                                             (#$salientTermImagePathname . 1)
                                             (#$formulaTemplateHasArgumentPositionInformation . 1)
                                             (#$except . 1)
                                             (#$exceptMt . 1)
                                             (#$interArgNotIsa1-2 . 1)
                                             (#$interArgNotIsa2-1 . 1)
                                             (#$completeExtentAsserted . 1)
                                             (#$completeExtentAssertedForValueInArg . 1)
                                             (#$completeExtentEnumerable . 1)
                                             (#$completeExtentEnumerableForArg . 1)
                                             (#$completeExtentEnumerableForValueInArg . 1)
                                             (#$completeExtentDecidable . 1)
                                             (#$completeExtentDecidableForValueInArg . 1)
                                             (#$transitiveViaArg . 1)
                                             (#$transitiveViaArgInverse . 1)
                                             (#$conservativeViaArg . 1)
                                             (#$conservativeViaArgInverse . 1)
                                             (#$functionalInArgs . 1)
                                             (#$strictlyFunctionalInArgs . 1)
                                             (#$parsingConflateTo . 1)
                                             (#$qaConflateTo . 1)
                                             (#$qaConflateToCompletely . 1)
                                             (#$terseDisambiguationString . 1)
                                             (#$denotation . 4)
                                             (#$headMedialString . 5)
                                             (#$compoundString . 4)
                                             (#$hyphenString . 4)
                                             (#$multiWordString . 4)
                                             (#$preferredNameString . 1)
                                             (#$nameString . 1)
                                             (#$placeName-WithRegion . 1)
                                             (#$placeName-WithRegionAbbreviation . 1)
                                             (#$titleOfWork . 1)
                                             (#$movieTitleString . 1)
                                             (#$gospelName-Short . 1)
                                             (#$brandNameOfProductType . 1)
                                             (#$businessName-WithTickerSymbol . 1)
                                             (#$organizationName-Standard . 1)
                                             (#$organizationName-Official . 1)
                                             (#$familyName . 1)
                                             (#$lastName . 1)
                                             (#$placeName-ShortForm . 1)
                                             (#$countryName-ShortForm . 1)
                                             (#$placeName-Standard . 1)
                                             (#$countryName-LongForm . 1)
                                             (#$chemicalFormulaString . 1)
                                             (#$fullName . 1)
                                             (#$alias . 1)
                                             (#$definiteDescriptions . 1)
                                             (#$acronymString . 1)
                                             (#$initialismString . 1)
                                             (#$initialsString . 1)
                                             (#$pseudonym . 1)
                                             (#$countryCodeTrigraph . 1)
                                             (#$atomicSymbol . 1)
                                             (#$codeMapping . 3)
                                             (#$applicableWhenTypedOnlyWhenSpecialization . 3)
                                             (#$mostNotableIsa . 1)
                                             (#$mostNotableGenls . 1)
                                             (#$facetOfCollectionBasedOnBinaryPred . 1)
                                             (#$facetOfCollectionBasedOnBinaryPredInverse . 1)
                                             (#$facetOfCollectionBasedOnTypeBinaryPred . 1)
                                             (#$facetOfCollectionBasedOnTypeBinaryPredInverse . 1)
                                             (#$teamSeed . 2)
                                             (#$denotesArgInReln . 2)
                                             (#$classificationSystemOf-Focally . 2)
                                             (#$genlMt-Vocabulary . 1)
                                             (#$marketTypeDefiningProductType . 2)
                                             (#$collectionDifference . 2)
                                             (#$characteristicActivityTypeOfPersonType-Frequently . 2)
                                             (#$medicalFindingTypeOfType . 1)
                                             (#$medicalDiagnosisTypeOfType . 1)
                                             (#$suppressFacetInstancesFromTaxonomy . 2)
                                             (#$quantifiedBinaryPredicateForPredWithMacro . 3)
                                             (#$verbSemTransTemplate . 1)
                                             (#$evaluationTypeDirectEvalueeType . 1)
                                             (#$allSubCollectionsAreInstancesOf . 1)
                                             (#$typeDeterminesValueOfArgInReln . 3)
                                             (#$hypothesisLevelStrategyForPredAndArgs . 2)
                                             (#$sourceFixedSemanticValueForRelationArg . 3))
  "[Cyc] An alist, each entry of the form (PRED . ARGNUM) where PRED is the predicate to be cached and ARGNUM specifies the argnum in which to find the indexed terms to be cached. Currently a predicate CANNOT have more than one cached argnum.")

(defglobal *some-pred-assertion-somewhere-cache* :uninitialized
  "[Cyc] Dictionary of PRED -> set where the set is the set of indexed terms that appear in the ARGNUMth position of some assertion with PRED as its predicate. ARGNUM is the value specified in the *somewhere-cached-preds-table* for PRED.")

(defun some-pred-assertion-somewhere? (pred term argnum &optional (initialize-if-uninitialized? t))
  "[Cyc] Return T iff there are any true assertions of the form
  (PRED ... TERM ...)
where TERM appears in the ARGNUMth position of the assertion,
where ARGNUM is specified in the *somewhere-cached-preds-table*.
Does not account for specPreds of PRED.
INITIALIZE-IF-UNINITIALIZED?: if somewhere cache for PRED and ARGNUM has not been initialized yet, should we initialize it? If NIL, we'll error instead."
  (case (some-pred-assertion-somewhere?-internal pred term argnum initialize-if-uninitialized?)
    (:yes t)
    (:no nil)
    (:maybe (missing-larkc 32142))))

(defun clear-all-somewhere-caches ()
  (setf *some-pred-assertion-somewhere-cache* (make-hash-table :test #'eq :size (length *somewhere-cached-preds-table*)))
  0)

(deflexical *somewhere-cache-gaf-after-adding-info* (cons 'recache-some-pred-assertion-somewhere #$UniversalVocabularyMt)
  "[Cyc] For use by get-after-adding and get-after-removing.")

(defun recache-some-pred-assertion-somewhere (argument assertion)
  "[Cyc] 'after-adding' and 'after-removing' for preds in *somewhere-cached-preds-table*.
This is not asserted as an afterAdding and afterRemoving in the KB, it's hard-coded specially in get-after-adding and get-after-removing."
  (declare (ignore argument))
  (let* ((pred (gaf-predicate assertion))
         (argnum (some-pred-assertion-somewhere-argnum pred))
         (arg (gaf-arg assertion argnum)))
    (when (valid-somewhere-cache-item? arg)
      (recache-some-pred-assertion-somewhere-int pred arg))))

(defun somewhere-cached-pred-p (object)
  (alist-has-key? *somewhere-cached-preds-table* object #'eq))

(defun some-pred-assertion-somewhere-argnum (pred)
  (alist-lookup-without-values *somewhere-cached-preds-table* pred #'eq))

(defglobal *inter-arg-result-isa-somewhere-cache* (uninitialized))

(defun some-pred-assertion-somewhere?-internal (pred term argnum initialize-if-uninitialized?)
  (let ((cached-argnum (some-pred-assertion-somewhere-argnum pred)))
    (if (eql argnum cached-argnum)
        (let ((set (lookup-somewhere-set-for-pred pred initialize-if-uninitialized?)))
          (if (set-member? term set) :yes :no))
        :maybe)))

(defun lookup-somewhere-set-for-pred (pred initialize-if-uninitialized?)
  (if-let ((set (gethash pred *some-pred-assertion-somewhere-cache*)))
    set
    (when initialize-if-uninitialized?
      (initialize-somewhere-cache pred))))

(defun initialize-somewhere-cache (pred)
  (let* ((estimated-size (num-predicate-extent-index pred))
         (set (new-set #'eq estimated-size)))
    ;; TODO - mt macro
    (let ((*relevant-mt-function* #'relevant-mt-is-everything)
          (*mt* #$EverythingPSC))
      (kmu-do-index-iteration (gaf predicate-extent (pred) (:gaf :true nil))
        (cache-some-pred-assertion-somewhere set gaf)))
    ;; Returns set as well
    (setf (gethash pred *some-pred-assertion-somewhere-cache*) set)))

(defun recache-some-pred-assertion-somewhere-int (pred term)
  (let ((set (lookup-somewhere-set-for-pred pred t)))
    (set-remove term set)
    (let ((done nil)
          (argnum (some-pred-assertion-somewhere-argnum pred)))
      ;; TODO - mt macro
      (let ((*relevant-mt-function* #'relevant-mt-is-everything)
            (*mt* #$EverythingPSC))
        (kmu-do-index-iteration (gaf gaf-arg (term argnum pred) (:gaf :true nil)
                                     :done-place done)
          (setf done (cache-some-pred-assertion-somewhere set gaf))))
      done)))

(defun cache-some-pred-assertion-somewhere (set gaf)
  (when (assertion-still-there? gaf :true)
    (let ((mt (assertion-mt gaf)))
      (when (hlmt-p mt)
        (let* ((pred (gaf-predicate gaf))
               (argnum (some-pred-assertion-somewhere-argnum pred))
               (arg (gaf-arg gaf argnum)))
          (when (valid-somewhere-cache-item? arg)
            (set-add arg set)
            t))))))

(defun* valid-somewhere-cache-item? (object) (:inline t)
  "[Cyc] Is OBJECT something we can reliably stick in the somewhere cache?"
  (reified-term-p object))

(defun somewhere-cache-unbuilt? ()
  (not (and (hash-table-p *some-pred-assertion-somewhere-cache*)
            (not (hash-table-empty-p *some-pred-assertion-somewhere-cache*)))))

(defun load-somewhere-cache-from-stream (stream)
  (setf *some-pred-assertion-somewhere-cache* (cfasl-input stream))
  (cfasl-input stream)
  (cfasl-input stream)
  (cfasl-input stream))
