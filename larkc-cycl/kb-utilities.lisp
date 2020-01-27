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


(defun kb-statistics (&optional (stream *standard-output*))
  (let* ((constant-count (constant-count))
         (cached-constant-index-count (cached-constant-index-count))
         (nart-count (nart-count))
         (cached-nart-index-count (cached-nart-index-count))
         (cached-nart-hl-formula-count (cached-nart-hl-formula-count))
         (fort-count (fort-count))
         (kb-assertion-count (assertion-count))
         (cached-assertion-count (cached-assertion-count))
         (bookkeeping-assertion-count (bookkeeping-assertion-count))
         (virtual-assertion-count 0)
         (deduction-count (deduction-count))
         (cached-deduction-count (cached-deduction-count))
         (kb-hl-support-count (kb-hl-support-count))
         (cached-kb-hl-support-count (cached-kb-hl-support-count))
         (unrepresented-term-count (kb-unrepresented-term-count))
         (cached-unrepresented-term-index-count (cached-unrepresented-term-index-count))
         (total-assertion-count (+ kb-assertion-count
                                   bookkeeping-assertion-count
                                   virtual-assertion-count)))
    (let ((*read-default-float-format* 'double-float))
      (format stream "~%;;; KB ~s statistics" (kb-loaded))
      (format stream "~%FORTs                   : ~9,' d" fort-count)
      (format stream "~% Constants              : ~9,' d" constant-count)
      (unless (zerop constant-count)
        (format stream "~%  cached indexing       : ~9,' d  (~a%)"
                cached-constant-index-count
                (percent cached-constant-index-count constant-count 3)))
      (format stream "~% NARTs                  : ~9,' d" nart-count)
      (unless (zerop nart-count)
        (format stream "~%  cached indexing       : ~9,' d  (~a%)"
                cached-nart-index-count
                (percent cached-nart-index-count nart-count 3))
        (format stream "~%  cached HL formulas    : ~9,' d  (~a%)"
                cached-nart-hl-formula-count
                (percent cached-nart-hl-formula-count nart-count 3)))
      (format stream "~%Assertions              : ~9,' d" total-assertion-count)
      (format stream "~% KB Assertions          : ~9,' d" kb-assertion-count)
      (unless (zerop kb-assertion-count)
        (format stream "~%  cached                : ~9,' d  (~a%)"
                cached-assertion-count
                (percent cached-assertion-count kb-assertion-count 3)))
      (format stream "~% Bookkeeping Assertions : ~9,' d" bookkeeping-assertion-count)
      (format stream "~%Deductions              : ~9,' d" deduction-count)
      (unless (zerop deduction-count)
        (format stream "~%  cached                : ~9,' d  (~a%)"
                cached-deduction-count
                (percent cached-deduction-count deduction-count 3)))
      (format stream "~%KB HL supports          : ~9,' d" kb-hl-support-count)
      (unless (zerop kb-hl-support-count)
        (format stream "~%  cached                : ~9,' d  (~a%)"
                cached-kb-hl-support-count
                (percent cached-kb-hl-support-count kb-hl-support-count 3)))
      (format stream "~%Unrepresented terms     : ~9,' d" unrepresented-term-count)
      (unless (zerop unrepresented-term-count)
        (format stream "~%  cached indexing       : ~9,' d  (~a%)"
                cached-unrepresented-term-index-count
                (percent cached-unrepresented-term-index-count unrepresented-term-count 3)))
      (terpri stream))))

(deflexical *estimated-assertions-per-constant* 17.1d0)
(deflexical *estimated-constants-per-nart* 1.41d0)
(deflexical *estimated-assertions-per-deduction* 2.67d0)
(deflexical *estimated-assertions-per-clause-struc* 39.3d0)
(deflexical *estimated-assertions-per-meta-assertion* 30.3d0)
(deflexical *estimated-arguments-per-assertion* 1.12d0)
(deflexical *estimated-assertions-per-unrepresented-term* 7.97)
(deflexical *estimated-deductions-per-hl-support* 10)
(deflexical *kb-table-padding-multiplier* 1.05)

(defun setup-kb-tables-int (exact? constant-count nart-count assertion-count deduction-count kb-hl-support-count clause-struc-count kb-unrepresented-term-count)
  (setf constant-count (ceiling (* constant-count *kb-table-padding-multiplier*)))
  (setf nart-count (ceiling (* nart-count *kb-table-padding-multiplier*)))
  (setf assertion-count (ceiling (* assertion-count *kb-table-padding-multiplier*)))
  (setf deduction-count (ceiling (* deduction-count *kb-table-padding-multiplier*)))
  (setf kb-hl-support-count (ceiling (* kb-hl-support-count *kb-table-padding-multiplier*)))
  (setf clause-struc-count (ceiling (* clause-struc-count *kb-table-padding-multiplier*)))
  (setf kb-unrepresented-term-count (ceiling (* kb-unrepresented-term-count *kb-table-padding-multiplier*)))

  (setup-kb-fort-tables constant-count nart-count exact?)
  (setup-kb-assertion-tables assertion-count exact?)
  (setup-kb-deduction-tables deduction-count exact?)
  (setup-kb-hl-support-tables kb-hl-support-count exact?)
  (setup-clause-struc-table clause-struc-count exact?)
  (setup-unrepresented-term-table kb-unrepresented-term-count exact?)
  (setup-variable-table)
  (setup-indexing-tables constant-count)
  (setup-rule-set assertion-count)
  (setup-cardinality-tables constant-count))

(defun setup-kb-fort-tables (constant-count nart-count exact?)
  "[Cyc] Setup the kb fort tables, based on an estimate of CONSTANT-COUNT total constants and NART-COUNT total narts."
  (let ((constant-table-size constant-count)
        (nart-table-size nart-count))
    (setup-constant-tables constant-table-size exact?)
    (setup-nart-table nart-table-size exact?)
    (setup-nart-hl-formula-table nart-table-size exact?)
    (setup-nart-index-table nart-table-size exact?)))

(defun setup-kb-assertion-tables (assertion-table-size exact?)
  (setup-assertion-table assertion-table-size exact?)
  (setup-assertion-content-table assertion-table-size exact?))

(defun setup-kb-deduction-tables (deduction-table-size exact?)
  (setup-deduction-table deduction-table-size exact?)
  (setup-deduction-content-table deduction-table-size exact?))

(defparameter *default-estimated-constant-count* 50000)

(defun clear-kb-state-int ()
  (free-all-clause-strucs)
  (free-all-kb-hl-support)
  (free-all-deductions)
  (free-all-assertions)
  (free-all-narts)
  (free-all-constants)
  (map-constants-in-completions #'init-constant)
  (clear-unrepresented-term-table)
  (clear-current-forward-inference-environment)
  (clear-bookkeeping-binary-gaf-store)
  (clear-kb-state-hashes))

(defun possibly-clear-dumpable-kb-state-hashes ()
  (when (defns-cache-unbuilt?)
    (clear-defns-cache))
  (when (somewhere-cache-unbuilt?)
    (clear-all-somewhere-caches)))

(defun possibly-initialize-dumpable-kb-state-hashes ()
  (when (nart-hl-formulas-unbuilt?)
    (missing-larkc 869))
  (when (non-fort-isa-tables-unbuilt?)
    (missing-larkc 1806))
  (when (tva-cache-unbuilt?)
    (missing-larkc 3822))
  (when (defns-cache-unbuilt?)
    (missing-larkc 10610))
  (when (somewhere-cache-unbuilt?)
    (missing-larkc 32146))
  (when (arity-cache-unbuilt?)
    (missing-larkc 12081)))

(defun clear-kb-state-hashes ()
  "[Cyc] Clear any hashes related to KB state."
  (possibly-clear-dumpable-kb-state-hashes)
  (clear-after-addings)
  (clear-after-removings)
  (clear-some-equality-assertions-somewhere-set)
  (clear-all-arg-type-predicate-caches))

(defun initialize-kb-state-hashes ()
  "[Cyc] Initialize any hashes related to KB state."
  (possibly-initialize-dumpable-kb-state-hashes)
  (rebuild-after-adding-caches)
  (initialize-some-equality-assertions-somewhere-set)
  (initialize-all-arg-type-predicate-caches))

(defun swap-out-all-pristine-kb-objects ()
  (swap-out-all-pristine-assertions)
  (swap-out-all-pristine-deductions)
  (swap-out-all-pristine-constant-indices)
  (swap-out-all-pristine-nart-indices)
  (swap-out-all-pristine-nart-hl-formulas)
  (swap-out-all-pristine-unrepresented-term-indices)
  (swap-out-all-pristine-kb-hl-supports)
  (swap-out-all-pristine-sbhl-module-graph-links))

(defparameter *sort-terms-constants-by-name* t)
(defparameter *sort-terms-ignore-variable-symbols* nil)
(defparameter *sort-terms-by-internal-id?* nil
  "[Cyc] This trumps *SORT-TERMS-CONSTANTS-BY-NAME*.")

(defun sort-terms (list &optional copy? stable? constants-by-name? ignore-variable-symbols? (key #'identity) use-internal-ids?)
  (let ((*sort-terms-constants-by-name* constants-by-name?)
         (*sort-terms-ignore-variable-symbols* ignore-variable-symbols?)
         (*sort-terms-by-internal-id?* use-internal-ids?)
         (seq (if copy?
                  (copy-list list)
                  list))
         (sort-func (if stable?
                        #'stable-sort
                        #'sort)))
    (funcall sort-func seq #'form-sort-pred :key key)))

(defun term-< (term1 term2 &optional constants-by-name? ignore-variable-symbols? use-internal-ids?)
  (let ((*sort-terms-constants-by-name* constants-by-name?)
        (*sort-terms-ignore-variable-symbols* ignore-variable-symbols?)
        (*sort-terms-by-internal-id?* use-internal-ids?))
    (form-sort-pred term1 term2)))

(defun form-sort-pred (form1 form2)
  (unless (eq form1 form2)
    (if (atom form1)
        (if (atom form2)
            (atom-sort-pred form1 form2)
            t)
        (if (atom form2)
            nil
            (cons-sort-pred form1 form2)))))

(defun cons-sort-pred (cons1 cons2)
  (loop
     for curr-cons1 on cons1
     for curr-cons2 on cons2
     for car1 = (car curr-cons1)
     for car2 = (car curr-cons2)
     do (cond
          ((form-sort-pred car1 car2) (return t))
          ((form-sort-pred car2 car1) (return nil))
          (t (let ((cdr1 (cdr curr-cons1))
                   (cdr2 (cdr curr-cons2)))
               (if (atom cdr1)
                   (if (atom cdr2)
                       (return (atom-sort-pred cdr1 cdr2))
                       (return t))
                   (when (atom cdr2)
                     (return nil))))))))

(defun atom-sort-pred (atom1 atom2)
  (unless (eq atom1 atom2)
    ;; TODO - what a horrible mess. Must be a macroexpansion from a sort order spec. At some point, reverse what the spec is, especially since we could add to or modify the supported atom types in the future. Also, somehow ensure that this didn't gain translation errors from the java.
    (or (and (fort-p atom1)
             (or (not (fort-p atom2))
                 (fort-sort-pred atom1 atom2)))
        (and (not (fort-p atom2))
             (or (and (variable-p atom1)
                      (or (not (variable-p atom2))
                          (variable-< atom1 atom2)))
                 (and (not (variable-p atom2))
                      (or (and (symbolp atom1)
                               (or (not (symbolp atom2))
                                   (symbol-sort-pred atom1 atom2)))
                          (and (not (symbolp atom2))
                               (or (and (stringp atom1)
                                        (or (not (stringp atom2))
                                            (string< atom1 atom2)))
                                   (and (not (stringp atom2))
                                        (or (and (numberp atom1)
                                                 (or (not (numberp atom2))
                                                     (< atom1 atom2)))
                                            (and (characterp atom1)
                                                 (not (numberp atom2))
                                                 (or (not (characterp atom2))
                                                     (char< atom1 atom2))))))))))))))

(defun symbol-sort-pred (symbol1 symbol2)
  ;; TODO - ugh, another one. This is rotting my brain. Same note as above, but this has the additional fun of unreachable code from missing-larkc.  But I kept all the structure for sanity's sake.
  (or (and (keywordp symbol1)
           (or (not (keywordp symbol2))
               (string< (symbol-name symbol1)
                        (symbol-name symbol2))))
      (and (not (keywordp symbol2))
           (or (and (missing-larkc 32021)
                    (or (not (missing-larkc 32022))
                        (and (not *sort-terms-ignore-variable-symbols*)
                             (string< (symbol-name symbol1)
                                      (symbol-name symbol2)))))
               (and (not (missing-larkc 32023))
                    (string< (symbol-name symbol1)
                             (symbol-name symbol2)))))))

(defun fort-sort-pred (fort1 fort2)
  (if (nart-p fort1)
      (if (nart-p fort2)
          (missing-larkc 4905)
          nil)
      (if (nart-p fort2)
          t
          (constant-sort-pred fort1 fort2))))

(defun constant-sort-pred (constant1 constant2)
  (if *sort-terms-by-internal-id?*
      (missing-larkc 31624)
      (if *sort-terms-constants-by-name*
          (atom-sort-pred (constant-name constant1)
                          (constant-name constant2))
          (constant-external-id-< constant1 constant2))))

(deflexical *definitional-pred-sort-order* (list #$isa
                                                 #$genls
                                                 #$genlPreds
                                                 #$genlInverse
                                                 #$genlMt
                                                 #$disjointWith
                                                 #$negationPreds
                                                 #$negationInverse
                                                 #$negationMt
                                                 #$defnIff
                                                 #$defnSufficient
                                                 #$defnNecessary
                                                 #$resultIsa
                                                 #$resultIsaArg
                                                 #$resultGenl
                                                 #$resultGenlArg
                                                 #$arity
                                                 #$arityMin
                                                 #$arityMax
                                                 #$argsIsa
                                                 #$argsGenl
                                                 #$arg1Isa
                                                 #$arg1Genl
                                                 #$arg2Isa
                                                 #$arg2Genl
                                                 #$arg3Isa
                                                 #$arg3Genl
                                                 #$arg4Isa
                                                 #$arg4Genl
                                                 #$arg5Isa
                                                 #$arg5Genl
                                                 #$argIsa
                                                 #$argGenl
                                                 #$fanOutArg
                                                 #$evaluationDefn
                                                 #$afterAdding
                                                 #$afterRemoving))

;; TODO DESIGN - removed the term-order methods, as their bodies were all missing-larkc
;; TODO - missing some -caching-state variables, as there's no matching defun

(defparameter *set-to-collection-uses-reformulator?* t
  "[Cyc] Temporary variable. @todo hard-code to T."
  ;; this todo is done?
  )

(deflexical *forbidden-kb-covering-collection-types* (list #$UnderspecifiedCollectionType
                                                           #$CycKBSubsetCollection)
  "[Cyc] Instances of any of these collections are forbidden.")

(deflexical *forbidden-kb-covering-quoted-collection-types* (list #$WorkflowConstant
                                                                  #$TPTP-PLA001-1-ProblemFORT
                                                                  #$PoorlyOntologized
                                                                  #$StubTerm
                                                                  #$IndeterminateTerm)
  "[Cyc] Quoted instances of any of these collections are forbidden.")

;; TODO DESIGN - I wonder if these forbiddings are to ensure private client data doesnt escape?  See if this is referenced in the larkc version.

(deflexical *forbidden-cols* (list #$PotentialCBRNEThreat
                                   #$Y2KThing
                                   #$BPVMilitaryUnit
                                   #$BPVEvent
                                   #$BPVArtifact
                                   #$BPVAgent
                                   #$HPKB-TransnationalAgent)
  "[Cyc] These exact collections are forbidden.")
    
(deflexical *forbidden-specs* nil
  "[Cyc] Specs of any of these collections are forbidden.")

(defparameter *min-each-spec-cardinality* nil
  "[Cyc] Temporary variable for forbidden-kb-covering-collection?.")

(deflexical *predicate-type-arity-table* '((1 . #$UnaryPredicate)
                                           (2 . #$BinaryPredicate)
                                           (3 . #$TernaryPredicate)
                                           (4 . #$QuaternaryPredicate)
                                           (5 . #$QuintaryPredicate)))

(defparameter *coasserted-fort-source* nil)
(defparameter *coasserted-fort-set* nil)

