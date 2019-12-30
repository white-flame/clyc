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






;; Concatenated multiple files:
;;  generic_testing - moved up because it has the defstruct
;;  cyc_testing_initialization
;;  cyc_testing


(in-package :clyc)



(defglobal *test-case-table-index* (make-hash-table)
  "[Cyc] An index of test case names (keywords) -> tables (lists) of (args-to-eval . expected-results) tuples.")
(defglobal *ordered-test-cases* nil
  "[Cyc] An ordered list of test case names, in order of definition.")
(deflexical *test-case-tables-by-class* (make-hash-table)
  "[Cyc] All the test-cases sorted by what classes they belong to.")
(deflexical *generic-test-results* '(:success :failure :error :not-run :invalid)
  "[Cyc] The possible statuses for generic tests.")
(deflexical *generic-test-verbosity-levels* '(:silent :terse :verbose :post-build)
  "[Cyc] The possible levels of verbosity for generic tests.")
(deflexical *test-case-table-post-build-token* :tct
  "[Cyc] The token identifying 'test case table' in the space of post-build tests.")

(defstruct (generic-test-case-table (:conc-name #:gtct-))
  name
  tuples
  (test #'equal)
  owner
  classes
  (kb :tiny)
  (working? t))

(defun new-generic-test-case-table (name tuples test owner &optional classes (kb :tiny) (working? t))
  ;; TODO - lots of elided type checks
  (make-generic-test-case-table :name name
                                :tuples tuples
                                :test (or test #'equal)
                                :owner owner
                                :classes classes
                                :kb kb
                                :working? working?))

(defun generic-test-case-table-name (gtct)
  (gtct-name gtct))

  ;; ELIDED define-test-case-table-int (macro-helper to nonexistent macro)


(defconstant *cfasl-wide-opcode-generic-test-case-table* 512)




 ;;;; FILE cyc_testing.cyc_testing_initialization
(file "cyc_testing/cyc_testing_initialization")
(deflexical *cyc-tests-initialized?* nil
  "[Cyc] Set to T after initializations have been performed. IF YOU RECOMPILE THIS (thereby setting it back to nil), IT WILL BREAK CYC-TESTING. If you start getting errors like 'FOO is not a GENERIC-TEST-CASE-TABLE-P', you need to rerun PERFORM-CYC-TESTING-INITIALIZATIONS.")

(defun cyc-tests-initialized? ()
  *cyc-tests-initialized?*)

(defun perform-cyc-testing-initializations ()
  (index-all-cyc-tests-by-name)
  (setf *cyc-tests-initialized?* t))




;;;; FILE cyc_testing.cyc_testing
(file "cyc-testing/cyc-testing")
(defparameter *it-output-format* :standard)
(defparameter *cyc-test-debug?* nil
  "[Cyc] Set this to T if you want to debug the tests (not catch errors)")
(defparameter *run-tiny-kb-tests-in-full-kb?* t
  "[Cyc] Whether to run tests that only require the tiny KB in the full KB. The default is T so that it's easy to run all tests on a full KB, but should be bound to NIL when testing on both a tiny and a full KB.")
(defparameter *test-real-time-pruning?* nil
  "[Cyc] Whether to test real-time while-inference-is-running pruning. This will force :COMPUTE-ANSWER-JUSTIFICATIONS? to NIL and will only run tests where that makes sense.")

(defun testing-real-time-prining? ()
  *test-real-time-pruning?*)

(deflexical *cyc-test-verbosity-levels* (list :silent :terse :verbose)
  "[Cyc] The possible levels of verbosiy for Cyc tests.")
(defparameter *cyc-test-filename* nil
  "[Cyc] Bound to the current file being loaded, so that the tests can know what file they're in")
(defparameter *warn-on-duplicate-cyc-test-names?* nil
  "[Cyc] Whether we should warn if a test has the same name as another test. This often happens when tests are redefined or updated, so we only want to do it when we're loading tests from a clean initial state.")
(deflexical *cyc-test-result-success-values* '(:success :regression-success)
  "[Cyc] Test results that mean that the test succeeded.")
(deflexical *cyc-test-result-failure-values* '(:failure :regression-failure :abnormal :error)
  "[Cyc] Test results that mean that the test failed.")
(deflexical *cyc-test-result-ignore-values* '(:non-regression-success :non-regression-failure :not-run :invalid)
  "[Cyc] Test results that mean that the test was ignored, or that the test results should be ignored, and counted as neither a success nor a failure.")
(deflexical *cyc-test-result-values* (append *cyc-test-result-success-values*
                                             *cyc-test-result-failure-values*
                                             *cyc-test-result-ignore-values*)
  "[Cyc] All possible results for tests.")
(deflexical *cyc-test-type-table* '((:iut "inference unit test")
                                    (:it "inference test")
                                    (:rmt "removal module test")
                                    (:ert "evaluatable relation test")
                                    (:tct "test case table")
                                    (:kct "KB content test"))
  "[Cyc] The table of known Cyc test types. Column 1 is a uniquely identifying keyword. Column 2 is a string description of the test type.")
(defglobal *cyc-tests* nil
  "[Cyc] The master ordered list of all Cyc test objects.")

(defun cyc-tests ()
  *cyc-tests*)

(defglobal *cyc-test-by-name* (make-hash-table :test #'equal)
  "[Cyc] An index from NAME -> Cyc Test object")
(defglobal *cyc-test-by-dwimmed-name* (make-hash-table :test #'equal)
  "[Cyc] An index from DWIMMED-NAME -> list of Cyc Test objects")

(defun index-cyc-test-by-name (ct name)
  (when (and *warn-on-duplicate-cyc-test-names?*
             (gethash name *cyc-test-by-name*))
    (warn "A Cyc test named ~a already existed; overwriting" name))
  (setf (gethash name *cyc-test-by-name*) ct)
  (push ct (gethash name *cyc-test-by-dwimmed-name*))

  (when (consp name)
    (missing-larkc 32431))
  (when (cyc-tests-initialized?)
    (let ((rmt (cyc-test-guts ct)))
      (when (funcall 'removal-module-test-p rmt)
        (missing-larkc 32432))))
  (when (cyc-tests-initialized?)
    (let ((rmct (cyc-test-guts ct)))
      (when (funcall 'removal-module-cost-test-p rmct)
        (missing-larkc 32433))))
  ct)

(defun index-all-cyc-tests-by-name ()
  (mapc #'index-cyc (cyc-tests)))

(defstruct (cyc-test (:conc-name #:ct-))
  file
  guts)

(defun new-cyc-test (file guts)
  (declare ((or null string) file))
  (if (cyc-tests-initialized?)
      (must (cyc-test-guts-p guts)
            "~s is not a CYC-TEST-GUTS-P" guts)
      (check-type guts #'generic-test-case-table-p))
  (let* ((ct (make-cyc-test :file file
                            :guts guts))
         (name (if (cyc-tests-initialized?)
                   (cyc-test-name ct)
                   (generic-test-case-table-name guts)))
         (existing-ct (find-cyc-test-by-exact-name name)))
    (when existing-ct
      (setf *cyc-tests* (delete existing-ct *cyc-tests* :test #'eq))
      (missing-larkc 32458))
    (push-last ct *cyc-tests*)
    (index-cyc-test-by-name ct name)
    ct))

(defun cyc-test-guts (ct)
  (ct-guts ct))

(defun cyc-test-type (ct)
  (or (cyc-test-type-permissive ct)
      (error "Cyc-test of unexpected type ~s" ct)))

(defun cyc-test-type-permissive (ct)
  (cyc-test-guts-type (cyc-test-guts ct)))

(defun cyc-test-guts-type (guts)
  (cond
    ((generic-test-case-table-p guts) :tct)
    ;; TODO - lots of other specific conditions tested, all missing
    (t (missing-larkc 32334))))

(defun cyc-test-name (ct)
  "[Cyc] Names are assumed to be unique, even across type"
  (let ((guts (cyc-test-guts ct)))
    (case (cyc-test-type guts)
      (:it guts)
      (:tct (generic-test-case-table-name guts))
      ;; TODO - these all have their own error codes, but I'm lazy
      ((:iut :rmt :tmt :rmct :ert :kct) (missing-larkc 32325))
      (otherwise (error "Cyc-test of unexpected type ~s" guts)))))

(defun find-cyc-test-by-exact-name (name)
  (gethash name *cyc-test-by-name*))

(defconstant *cfasl-wide-opcode-cyc-test* 514)
(defglobal *cyc-test-files* nil
  "[Cyc] The master ordered list of all Cyc test file objects.")

(defstruct cyc-test-file
  filename
  kb)

(defglobal *most-recent-cyc-test-funs* nil
  "[Cyc] The most recent runs are saved here for the cases where they're not returned directly")
(defglobal *most-recent-cyc-test-file-load-failures* nil
  "[Cyc] The Cyc test files which failed to load the last time LOAD-ALL-CYC-TESTS was evaluated.")
(deflexical *tests-that-dont-work-with-real-time-pruning* (list :canonicalize-inference-answer-justifications :non-explanatory-sentence-supports :non-explanatory-variable-map-supports :true-sentence-not-canonicalization :true-sentence-of-atomic-sentence-reduction :ist-of-atomic-sentence-reduction :relation-all-instance-iterate-2 :relation-instance-all-iterate-2 :reject-previously-proven-proofs :inference-harness-overhead :tactically-unexamined-no-good-implies-strategically-unexamined-no-good :the-set-of-elements-returns-hl-narts :the-collection-of-instances-returns-hl-narts :genlpreds-lookup-generates-correct-supports :kappa-removal-works :dont-reopen-answer-link :removal-true-sentence-universal-disjunction-14a :closed-asent-with-3-children :simple-except-when :simple-except-when-residual-transformation :partial-except-when :variable-map-except-when :true-sentence-implies-var-canonicalization :exception-tms-backward-no-op :multiple-transformation-proofs-for-closed-problem :backchain-to-removal-true-sentence-universal-disjunction-1 :backchain-to-removal-true-sentence-universal-disjunction-2 :backchain-to-removal-true-sentence-universal-disjunction-3 :collection-isa-backchain-required-4 :collection-genls-backchain-required-4 :collection-backchain-required-3 :collection-backchain-required-4 :early-removal-of-8-restricted-problems-requiring-transformation :early-new-root-of-9-restricted-problems-requiring-transformation :forward-indeterminate-result :simple-forward-pragmatic-requirement :simple-forward-pragmatic-requirement-supports :nart-isa-in-right-mt :forward-problem-store-destruction-on-conflict :forward-rule-concluding-consequent-in-wrong-mt :skolemize-forward :forward-inference-with-defns :completeness-in-low-mt-doesnt-hose-forward-inference :hypothetical-mt-completeness-assertion-doesnt-hose-forward-inference :except-mt-in-mid-mt-blocks-high-mt-from-low-mt :except-mt-in-high-mt-hoses-backward-inference :cyc-assert-with-reifiable-monad-mt :forward-rule-concluding-false :skolem-result-arg :unassert-reifiable-nat-mt :unassert-nart-mt-sentence-with-nart :unassert-reifiable-nat-mt-via-tl :canonicalize-nested-mt :function-test :nat-removal :resulttype-change :meta-assertion-removal :arg-type-mt-denoting-function :max-floor-mts-of-nat :contextualized-collection-specpred-of-isa :use-defns-to-check-inference-semantically-valid-dnf :sbhl-trumps-defns :skolemize-forward-naut-genl-mt-wff-exception :one-step :two-step :two-step :two-step-arg-1 :two-step-arg-1 :many-step :cross-mt :disjunctive-syllogism :argumentation :tms-loop :reconsider-deduction :reconsider-deduction :hl-support-mt-handling :there-exists :except-when :except-when :strength-propagation :sequence-variables-inference :inference-answer-template :forward-propagate-mt :forward-propagate-mt-continue :ist-triggers-forward-inference-simple :forward-non-trigger-literal-honored :except-blocks-backward :except-blocks-forward :true-sentence-universal-disjunction-scoping :tms-reconsideration-with-backchain-forbidden :tms-for-hl-supports :assertion-direction :merge-ignores-opaque-references :mt-floors-wrt-isa-paths :min-genl-mts :min-genl-predicates :min-genls-collection :split-no-goodness-propagation :lazily-manifest-non-focals :consider-no-good-after-determining-tactics :removal-all-isa-of-type-2 :avoid-lookup-on-indeterminates :irrelevant-does-not-imply-pending :asserted-instance-of-disjoint-collections :chaining-skolem-straightforward :chaining-skolem-shallow :chaining-skolem-deep :chaining-skolem :except-decontextualized :problem-store-pruning-max-insufficient :restricted-closed-good-problems-stay-unexamined :genls-between :conjunctive-integer-between-1 :conjunctive-integer-between-2 :conjunctive-integer-between-3 :conjunctive-integer-between-4 :conjunctive-integer-between-5 :conjunctive-integer-between-6 :conjunctive-integer-between-7 :conjunctive-integer-between-8 :conjunctive-followup-additional-join-ordered :conjunctive-followup-additional-join-ordered-without-inference :circular-proofs)
  "[Cyc] A list of tests that will fail if :COMPUTE-ANSWER-JUSTIFICATIONS? is forced to NIL and/or if problem store pruning happens while they're running.")




