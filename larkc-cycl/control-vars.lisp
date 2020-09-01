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


;; TODO DESIGN - Nothing in the code ever sets this to NIL, so the #$ can't be used in bootstrap?
;; HACK - setting it to NIL for now
(defparameter *read-require-constant-exists* nil ;;t
  "[Cyc] Does the #$ reader error if the referenced constant does not exist?")
(defglobal *table-area* nil)
(defvar *rkf-mt* nil
  "[Cyc] The mt within which RKF interactions are assumed.")
(defglobal *hl-lock* (bt:make-lock "HL Store Lock")
  "[Cyc] Controls modification of the HL store")
(defparameter *bootstrapping-kb?* nil)
(deflexical *keywords-package* (find-package "KEYWORD"))
(deflexical *sublisp-package* (find-package "SUBLISP")) ;; TODO - clyc? subl?
(deflexical *cyc-package* (find-package "CYC")) ;; TODO - clyc?
(defparameter *cnf-matching-predicate* 'equal
  "[Cyc] predicate used to compare two cnfs when searching for an assertion (or axiom) in the kb")
(defparameter *gaf-matching-predicate* 'equal
  "[Cyc] predicate used to compare two gaf formulas when searching for an assertion (or axiom) in the kb")
(defparameter *nat-matching-predicate* 'equal
  "[Cyc] predicate used to compare two nat formulas when searching for a reified nat in the kb")
(defparameter *candidate-assertion* nil
  "[Cyc] used for robust assertion lookup in find-assertions-*")
(defparameter *variable-names* nil)
(defparameter *assertion-truth* :true)
(defparameter *mapping-answer* nil)
(defparameter *mapping-pred* nil)
(defparameter *mapping-source* nil)
(defparameter *mapping-target* nil)
(defparameter *mapping-target-arg* nil)
(defparameter *mapping-index-arg* nil)
(defparameter *mapping-gather-arg* nil)
(defparameter *mapping-gather-args* nil)
(defparameter *mapping-output-stream* t)
(defparameter *mapping-equality-test* #'eq)
(defparameter *mapping-any-answer?* nil)
(defparameter *mapping-relation* nil)
(defparameter *mapping-finished-fn* nil)
(defparameter *mapping-path* nil)
(defparameter *mapping-data-1* nil)
(defparameter *mapping-data-2* nil)
(defparameter *mapping-pivot-arg* #'identity)
(defparameter *mapping-gather-key-args* nil)
(defparameter *mapping-assertion-selection-fn* nil)
(defparameter *mapping-assertion-bookkeeping-fn* nil)
(defparameter *mapping-fn* *unprovided*
  "[Cyc] function applied in mapping-funcall-arg")
(defparameter *mapping-fn-arg* 1
  "[Cyc] designates non-default argument in mapping-funcall-arg")
(defparameter *mapping-fn-arg1* *unprovided*
  "[Cyc] default arg1 in mapping-funcall-arg")
(defparameter *mapping-fn-arg2* *unprovided*
  "[Cyc] default arg2 in mapping-funcall-arg")
(defparameter *mapping-fn-arg3* *unprovided*
  "[Cyc] default arg3 in mapping-funcall-arg")
(defparameter *mapping-fn-arg4* *unprovided*
  "[Cyc] default arg4 in mapping-funcall-arg")
(defparameter *mapping-fn-arg5* *unprovided*
  "[Cyc] default arg5 in mapping-funcall-arg")
(defparameter *mapping-fn-arg6* *unprovided*
  "[Cyc] default arg6 in mapping-funcall-arg")
(defparameter *mapping-fn-arg7* *unprovided*
  "[Cyc] default arg7 in mapping-funcall-arg")
(defparameter *mapping-fn-arg8* *unprovided*
  "[Cyc] default arg8 in mapping-funcall-arg")
(defparameter *kba-pred* nil)
(defparameter *standard-indent-string* " ")
(defparameter *term-functional-complexity-cutoff* nil
  "[Cyc] The maximum function complexity of CycL allowed by the system. NIL means 'no limit'.")
(defparameter *term-relational-complexiy-cutoff* nil
  "[Cyc] The maximum relational complexity of CycL allowed by the system. NIL means 'no limit'.")
(defparameter *collect-justification-compilations?* nil
  "[Cyc] compile successful inference chains into macro rules?")
(defparameter *justification-compilations* nil
  "[Cyc] candidate macro rules are recorded here")
(defparameter *ebl-trace* 0
  "[Cyc] tracing level for ebl module [0..5]")
(defparameter *allow-forward-skolemization* nil
  "[Cyc] Do we allow skolemization during forward inference?")
(defparameter *prefer-forward-skolemization* nil
  "[Cyc] Do we prefer skolemization during forward inference? This option will make forward inference tend not to unify to existing NARTs so that new NARTs can be created if they would come into existence (see nat-lookup-pos-preference.)")
(defparameter *perform-unification-occurs-check* t
  "[Cyc] Do we check for and reject unifications where a variable appears in its own binding?")
(defparameter *perform-equals-unification* t
  "[Cyc] Do we use #$equals assertions within term unification?")
(defparameter *allow-backward-gafs* t
  "[Cyc] Do we allow backward gafs?")
(defparameter *cached-ask-result-direction* :forward
  "[Cyc] The direction to use for cached ask results.")
(defparameter *check-for-circular-justs* t
  "[Cyc] Do we check for circularly justified assertions?")
(defparameter *filter-deductions-for-trivially-derivable-gafs* nil
  "[Cyc] Do we ignore deductions for gafs which are already trivially derivable?")
(defparameter *inference-debug?* nil
  "[Cyc] Whether the inference engine is to be run in debug mode.")
(defvar *browse-forward-inferences?* nil
  "[Cyc] Whether forward inferences will be browsable. If NIL, they will be destroyed after use, along with their problem stores. If T, problem store descruction may never happen for many problem stores--BE CAREFUL")

(defun browse-forward-inferences? ()
  *browse-forward-inferences?*)

(defparameter *query-properties-inherited-by-recursive-queries* '(:productivity-limit :removal-backtracking-productivity-limit)
  "[Cyc] The query properties that should be inherited by recursive queries.")
(defparameter *proof-checking-enabled* nil
  "[Cyc] Are we using the inference engine as a proof-checker?")
(defparameter *proof-checker-rules* nil
  "[Cyc] allowable rules")
(defparameter *inference-propagate-mt-scope* nil)
(defparameter *inference-current-node-mt-scope* nil)
(defparameter *inference-literal* nil)
(defparameter *inference-sense* nil)
(defparameter *inference-arg* nil)
(defparameter *inference-more-supports* nil)
(defparameter *inference-highly-relevant-assertions* nil
  "[Cyc] Axioms specified by #$highlyRelevantAssertion.")
(defparameter *inference-highly-relevant-mts* nil
  "[Cyc] Microtheories specified by highlyRelevantMt.")
(defparameter *within-hl-failure-backchaining?* nil)
(defparameter *hl-failure-backchaining* nil
  "[Cyc] Do we backchain on HL predicates?")
(defparameter *evaluatable-backchain-enabled* nil
  "[Cyc] Do we backchain on evaluatable predicates?")
(defparameter *negation-by-failure* nil
  "[Cyc] Do we allow the minimization inference methods to fire?")
(defparameter *complete-extent-minimization* t
  "[Cyc] Do we allow use of the 'complete extent' HL inference modules?")
(defparameter *unbound-rule-backchain-enabled* nil
  "[Cyc] Do we allow backchaining using the unbound rule index.")
(deflexical *default-removal-cost-cutoff* 10000)
(defparameter *removal-cost-cutoff* *default-removal-cost-cutoff*
  "[Cyc] How expensive a removal do we allow (NIL for no restriction).")
(defparameter *forward-inference-removal-cost-cutoff* *default-removal-cost-cutoff*
  "[Cyc] How expensive a removal do we allow during forward inference (NIL for no restriction).")
(defparameter *application-filtering-enabled* nil)
(defparameter *marking-doomed-inference-ancestors* nil
  "[Cyc] When a goal node is rejected, do we mark all its semantically invalid ancestors as doomed, thereby cutting off large chunks of search which will fail.")
(defparameter *inference-search-strategy* :heuristic)
(defparameter *unique-inference-result-bindings* t)
(defparameter *inference-answer-handlers* t
  "[Cyc] The handler function to use when generating the results to return from inference searches.")
(defparameter *hl-module-simplification-cost* 0.1
  "[Cyc] The cost value used for performing an HL module simplification step.")
(defparameter *hl-module-check-cost* 0.5
  "[Cyc] The cost value used for performing fully-bound HL module checks.")
(deflexical *cheap-hl-module-check-cost* 0.5
  "[Cyc] The cost value used for performing cheap fully-bound HL module checks.")
(deflexical *typical-hl-module-check-cost* 1.0
  "[Cyc] The cost value used for performing typical fully-bound HL module checks.")
(deflexical *expensive-hl-module-check-cost* 1.5
  "[Cyc] The cost value used for performing expensive fully-bound HL module checks.")
(deflexical *expensive-hl-module-singleton-generate-costs* *expensive-hl-module-check-cost*
  "[Cyc] The cost value used for performing expensive HL module generations.")
(deflexical *maximum-hl-module-check-cost* nil
  "[Cyc] When non-NIL, the maximum cost value allowable for fully-bound HL module checks.")
(defparameter *average-all-isa-count* 38
  "[Cyc] An estimate of the total number of types for the average term.")
(defparameter *average-all-genls-count* 47
  "[Cyc] An estimate of the total number of superclasses for the average collection.")
(defparameter *pgia-active?* nil)
(defparameter *the-term-inference-enabled* nil
  "[Cyc] Global control of whether we ever allow any the-term reasoning at all.")
(defparameter *allow-the-term-unification* nil
  "[Cyc] Controls whether the unifier treats the-terms as variables. Should always be globally NIL and bound to T by the-term inference methods.")
(defparameter *inference-the-term-bindings* nil)
(defparameter *the-term-qua-constant* nil)
(defparameter *external-inference-enabled* nil
  "[Cyc] Determines whether or not External HL module inferencing is enabled.")
(defparameter *suppress-conflict-notices?* nil)
(defparameter *ignore-conflicts?* nil)
(defparameter *conflicts-from-invalid-deductions* nil
  "[Cyc] Do we treat semantically invalid deductions as conflicts?")
(defparameter *record-inconsistent-support-sets* nil
  "[Cyc] When non-NIL, sets of mutually inconsistent HL supports are stored on the variable *INCONSISTENT-SUPPORT-SETS*")
(deflexical *last-agenda-op* nil)
(deflexical *last-agenda-error-message* nil)
(deflexical *last-agenda-error-explanatory-supports* nil
  "[Cyc] A list - containing either one or more assertions or a list of the form (#$equals <term> <term>) - the contents of which accounts for the halting of the agenda.")
(defparameter *agenda-display-fi-warnings* nil)
(defparameter *ignore-remote-errors* t
  "[Cyc] Do we ignore remote errors or handle them the same way as local errors?")
(defglobal *auto-increment-kb* nil
  "[Cyc] This determines whether or not the image will change to the next KB when teh close-kb transcript operation is reached.")
(deflexical *load-submitted-transcripts?* nil
  "[Cyc] Controls whether the running image will load submitted transcripts via MAYBE-LOAD-SUBMITTED-TRANSCRIPT.")
(deflexical *send-submitted-transcript-loading-notices?* nil
  "[Cyc] Controls whether, when a submitted transcript is loaded, the image should notify the submitter that it is being loaded as part of a build.")
(defvar *cyc-image-id* nil
  "[Cyc] A string consisting of '<machine-name>-<universal-time>-<process-id>'.")

;; TODO - referenced from kb-accessors
(missing-function-implementation mapping-funcall-arg)

(defun make-cyc-image-id ()
  "[Cyc] Make a unique identifier for a cyc image: '<machine-name>-<universal-time>-<process-id>"
  (let ((machine-name (string-downcase (machine-instance)))
        (process-id #+sbcl (write-to-string (sb-posix:getpid)) #-sbcl "unknownpid")
        (cyc-universal-time (universal-timestring)))
    (format nil "~a-~a-~a" machine-name cyc-universal-time process-id)))

(defun set-cyc-image-id ()
  (setf *cyc-image-id* (make-cyc-image-id)))

(defun cyc-image-id ()
  "[Cyc] Accessor for *CYC-IMAGE-ID*"
  *cyc-image-id*)

(defglobal *build-kb-loaded* nil)

(defun set-build-kb-loaded (kb)
  (when kb
    (check-type kb 'integerp))
  (setf *build-kb-loaded* kb))

(defglobal *kb-loaded* nil)

(defun kb-loaded ()
  "[Cyc] @return nil or integerp; Return the current KB version."
  *kb-loaded*)

(defun set-kb-loaded (kb)
  (when kb
    (check-type kb 'integerp))
  (setf *kb-loaded* kb))

(defun non-tiny-kb-loaded? ()
  "[Cyc] Does the KB contain a nontrivial amount that is not the core (tiny) KB?"
  ;; TODO - distant forward references from SubL into CycL. These should be moved into a CycL library
  (and (> (funcall 'constant-count) 10000)
       (< 8 (truncate (funcall 'assertion-count) (funcall 'fort-count)))))

(defglobal *kb-pedigree* :unknown)
(defparameter *use-transcript?* t)
(defglobal *run-own-operations?* t)
(defglobal *caught-up-on-master-transcript* nil
  "[Cyc] Boolean: This is used by the agenda to decide whether or not to wait before doing another read.")
(defglobal *communication-mode* :unknown)
(defparameter *unencapsulating-within-agenda* nil)
(defvar *save-asked-queries?* nil
  "[Cyc] Whether to save queries asked into a query transcript.")

(defun save-asked-queries? ()
  (and *save-asked-queries?*
       (non-tiny-kb-loaded?)
       (typep (kb-loaded) '(integer 0))))

(defglobal *init-file-loaded?* nil)
(defparameter *within-assert* nil)
(defparameter *within-unassert* nil)
(defparameter *within-ask* nil)
(defparameter *within-query* nil)

(defun within-ask? ()
  *within-ask*)

(defun within-query? ()
  (or *within-ask*
      *within-query*))

(defun within-assert? ()
  *within-assert*)

(defparameter *compute-inference-results* t)
(defparameter *cache-inference-results* nil
  "[Cyc] Do we cache the results of successful inference in the KB?")
(defparameter *transformation-depth-cutoff* nil)
(defparameter *show-assertions-in-english* nil
  "[Cyc] boolean; Should assertions be displayed in English?")
(defparameter *assume-cyc-cyclist-dialog?* t
  "[Cyc] boolean; Should we assume Cyc is talking to the currently-logged-in Cyclist when generating NL in the CB interface?")
(defparameter *show-fet-edit-buttons?* t
  "[Cyc] boolean; Should terms have links to edit term with FET?")
(defparameter *show-fet-create-instance-buttons?* t
  "[Cyc] boolean; Should collections have links to create instance with FET?")
(defparameter *show-fet-create-spec-buttons?* nil
  "[Cyc] boolean; Should collections have links to create spec with FET?")
(defparameter *cb-literal-query-results-one-per-line?* nil
  "[Cyc] boolean; Should literal query result terms be displayed one per line?")
(defparameter *cb-skolem-applicable-relations?* nil
  "[Cyc] boolean; Should skolem applicable relations be displayed?")
(defparameter *cb-applicable-relations-one-per-line?* nil
  "[Cyc] boolean; Should applicable relations be displayed one per line?")
(defparameter *cb-paraphrase-applicable-relations?* nil
  "[Cyc] boolean; Should applicable relations be paraphrased?")
(defparameter *meta-query-start-string* nil)
(defparameter *current-cache* nil)
(defparameter *dbm-init-file-loaded?* nil
  "[Cyc] Has the db meta query init file successfully loaded, or not?")
(defparameter *dbm-cache-loading-started?* nil)
(defparameter *dbm-cache-loading-finished?* nil)
(defglobal *acip-subkernel-extraction* nil
  "[Cyc] When non-NIL, the ACIP subkernel we are currently extracting.")
(defglobal *acip-subkernel-output-stream* nil
  "[Cyc] When non-NIL, the stream we are using for output of the ACIP subkernel we are extracting.")
(defparameter *janus-tag* nil
  "[Cyc] This tag will be inserted into every Janus operation that is logged.")
(defparameter *janus-new-constants* nil)
(defparameter *janus-test-case-logging?* nil)
(defparameter *janus-operations* nil)
(defparameter *janus-extraction-deduce-specs* nil)
(defparameter *janus-within-something?* nil)
(defparameter *janus-testing-deduce-specs* nil)
(defparameter *janus-test-case-running?* nil)
(defvar *ask-quirk?* nil)
(defvar *curried-kbq-lookup?* t
  "[Cyc] Whether to use #$sentenceParameterValueInSpecification, #$microtheoryParameterValueInSpecification, and #$inferenceModeParameterValueInSpecification for lookup")
(defparameter *kbq-run-query-auto-destroy-enabled?* t
  "[Cyc] When non-NIL, the inferences and problem-stores generated by KBQ-RUN-QUERY are auto-destroyed.")
(defparameter *kbq-run-query-non-continuable-enabled?* t
  "[Cyc] When non-NIL, the inferences generated by KBQ-RUN-QUERY are always run with :CONTINUABLE? NIL since they won't ever be continued.")
