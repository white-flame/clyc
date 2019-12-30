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


(defparameter *cyc-raw-revision-string* "$Revision: 128948 $")
(defparameter *cyc-major-version-number* 10)
(defparameter *cyc-revision-numbers* (extract-cyc-revision-numbers
                                      (extract-cyc-revision-string *cyc-raw-revision-string*)))
(defparameter *cyc-revision-string* (construct-cyc-revision-string-from-numbers *cyc-revision-numbers*))
(defparameter *cycl-common-revision* "1.269")
(defparameter *cycl-crtl-revision* "1.555")
(defparameter *cycl-translator-revision* "1.69")
(defparameter *cycl-opencyc-revision* "1.391")
(defparameter *cycl-framework-revision* "1.1767")
(defparameter *cycl-sublisp-revision* "1.319")
(defparameter *cycl-tests-revision* "1.907")
(defparameter *cycl-mysentient-revision* "1.437")
(defparameter *cycl-butler-revision* "1.277")
(defparameter *cycl-tool-revision* "1.652")
(defparameter *cycl-=======-revision* "")

(toplevel
  (create-system "cycl")
  (dolist (name '("cyc-cvs-id" "meta-macros" "access-macros" "subl-macro-promotions" "subl-promotions" "subl-macros" "format-nil" "modules" "cyc-revision-extraction" "system-parameters" "system-version" "system-info" "utilities-macros" "timing" "cyc-file-dependencies" "cycml-macros" "control-vars" "sunit-macros" "sunit-external" "cfasl" "sxhash-external" "red-implementation" "red-api" "cyc-testing-initialization" "cyc-testing" "generic-testing" "hash-table-utilities" "keyhash" "set-contents" "reader-writer-locks" "memoization-state" "list-utilities" "transform-list-utilities" "vector-utilities" "string-utilities" "unicode-strings" "unicode-subsets" "unicode-support" "unicode-streams" "defstruct-sequence" "visitation-utilities" "mail-utilities" "mail-message" "number-utilities" "fraction-utilities" "matrix-utilities" "numeric-date-utilities" "iteration" "binary-tree" "stacks" "queues" "deck" "integer-sequence-generator" "semaphores" "process-utilities" "os-process-utilities" "tcp-server-utilities" "pattern-match" "tries" "shelfs" "id-index" "dictionary-contents" "dictionary" "cfasl-compression" "cfasl-utilities" "bijection" "glob" "keyhash-utilities" "set" "set-utilities" "dictionary-utilities" "map-utilities" "bag" "accumulation" "red-infrastructure" "red-infrastructure-macros" "red-utilities" "file-utilities" "stream-buffer" "hierarchical-visitor" "strie" "finite-state-transducer" "cache" "cache-utilities" "special-variable-state" "simple-lru-cache-strategy" "file-hash-table" "file-hash-table-utilities" "file-vector" "file-vector-utilities" "generic-table-utilities" "sdbc-macros" "sdbc" "graph-utilities" "sparse-vector" "sparse-matrix" "remote-image" "cyc-testing-utilities" "heap" "decision-tree" "misc-utilities" "web-utilities" "timing-by-category" "api-control-vars" "eval-in-api" "eval-in-api-registrations" "api-kernel" "cfasl-kernel" "guardian" "hl-interface-infrastructure" "kb-macros" "constant-completion-low" "constant-completion-interface" "constant-completion-high" "constant-completion" "constant-handles" "constant-reader" "enumeration-types" "kb-control-vars" "mt-vars" "graphl-search-vars" "ghl-search-vars" "sbhl-iteration" "sbhl-paranoia" "sbhl-module-vars" "sbhl-link-vars" "sbhl-marking-vars" "sbhl-search-datastructures" "sbhl-search-vars" "sbhl-time-vars" "sbhl-link-iterators" "at-vars" "gt-vars" "czer-vars" "mt-relevance-macros" "pred-relevance-macros" "at-macros" "czer-macros" "wff-macros" "wff-vars" "gt-macros" "hl-macros" "obsolete" "obsolete-macros" "kb-mapping-macros" "kb-access-metering" "kb-object-manager" "hlmt" "hlmt-czer" "constants-interface" "constant-index-manager" "constants-low" "constants-high" "nart-handles" "narts-interface" "nart-index-manager" "nart-hl-formula-manager" "narts-low" "narts-high" "forts" "assertion-handles" "assertions-interface" "assertion-manager" "assertions-low" "assertions-high" "kb-hl-support-manager" "kb-hl-supports" "deduction-handles" "deductions-interface" "deduction-manager" "deductions-low" "deductions-high" "unrepresented-term-index-manager" "unrepresented-terms" "arguments" "clause-strucs" "variables" "format-cycl-expression" "hl-storage-modules" "hl-modifiers" "sxhash-external-kb" "el-macros" "sbhl-macros" "cycl-variables" "el-utilities" "clause-utilities" "cycl-utilities" "cycl-grammar" "el-grammar" "unicode-nauts" "term" "kb-indexing-datastructures" "kb-utilities" "simple-indexing" "kb-indexing-declarations" "kb-indexing-macros" "kb-indexing" "virtual-indexing" "kb-mapping" "kb-mapping-utilities" "kb-gp-mapping" "somewhere-cache" "auxiliary-indexing" "inferred-indexing" "arity" "kb-accessors" "kb-iterators" "function-terms" "relation-evaluation" "assertion-utilities" "parameter-specification-utilities" "clauses" "bindings" "unification" "unification-utilities" "file-backed-cache" "graphl-graph-utilities" "ghl-graph-utilities" "ghl-link-iterators" "ghl-marking-utilities" "ghl-search-utilities" "sbhl-graphs" "sbhl-caching-policies" "sbhl-module-utilities" "sbhl-links" "sbhl-nat-utilities" "sbhl-link-utilities" "sbhl-link-methods" "sbhl-marking-utilities" "sbhl-search-utilities" "sbhl-marking-methods" "sbhl-search-methods" "sbhl-search-what-mts" "sbhl-search-implied-relations" "sbhl-module-declarations" "genls" "disjoint-with" "sdc" "isa" "genl-predicates" "negation-predicate" "genl-mts" "mt-relevance-cache" "predicate-relevance-cache" "negation-mt" "ghl-search-methods" "sbhl-cache" "fort-types-interface" "czer-trampolines" "wff-utilities" "wff-module-datastructures" "wff-modules" "wff" "wff-suggest" "simplifier" "tersifier" "verbosifier" "czer-utilities" "precanonicalizer" "postcanonicalizer" "clausifier" "prop-sentence-clausifier" "czer-graph" "czer-main" "rule-macros" "skolems" "czer-meta" "uncanonicalizer" "canon-tl" "at-routines" "at-utilities" "at-admitted" "at-defns" "defns" "at-var-types" "at-cache" "arg-type" "applicable-relations" "sksi-macros" "janus-macros" "gt-utilities" "gt-search" "gt-methods" "transitivity" "transfers-through" "tva-utilities" "tva-tactic" "tva-strategy" "tva-inference" "tva-cache" "equality-store" "equals" "rewrite-of-propagation" "hl-supports" "conflicts" "ebl" "preserves-genls-in-arg" "formula-pattern-match" "cfasl-kb-methods" "inference-macros" "inference-modules" "search" "tms" "after-adding" "rule-after-adding" "after-adding-modules" "argumentation" "backward" "psc" "inference-trampolines" "inference-completeness-utilities" "backward-utilities" "backward-results" "transformation-heuristics" "inference-pad-data" "preference-modules" "preference-module-declarations" "inference-datastructures-enumerated-types" "inference-datastructures-problem-store" "inference-datastructures-problem-query" "inference-datastructures-problem" "inference-datastructures-problem-link" "inference-datastructures-tactic" "inference-datastructures-proof" "inference-datastructures-strategy" "inference-datastructures-forward-propagate" "inference-datastructures-inference" "inference-czer" "inference-proof-spec" "inference-proof-spec-store" "inference-worker" "inference-worker-answer" "inference-worker-restriction" "inference-worker-removal" "inference-worker-transformation" "inference-worker-residual-transformation" "inference-worker-rewrite" "inference-worker-split" "inference-worker-join-ordered" "inference-worker-join" "inference-worker-union" "inference-tactician-strategic-uninterestingness" "inference-lookahead-productivity" "inference-min-transformation-depth" "inference-tactician" "inference-tactician-utilities" "inference-strategic-heuristics" "inference-balanced-tactician-datastructures" "inference-balanced-tactician-strategic-uninterestingness" "inference-balanced-tactician-motivation" "inference-balanced-tactician-execution" "inference-heuristic-balanced-tactician" "balancing-tactician" "removal-tactician-datastructures" "removal-tactician-uninterestingness" "removal-tactician-motivation" "removal-tactician-execution" "removal-tactician" "transformation-tactician-datastructures" "transformation-tactician-uninterestingness" "transformation-tactician-motivation" "transformation-tactician-execution" "transformation-tactician" "new-root-tactician-datastructures" "new-root-tactician-motivation" "new-root-tactician-execution" "new-root-tactician" "neural-net" "inference-strategist" "inference-kernel" "inference-trivial" "inference-utilities" "inference-parameters" "inference-metrics" "inference-analysis" "inference-serialization" "inference-unit-tests" "removal-module-tests" "transformation-module-tests" "evaluatable-relation-tests" "inference-testing-helpers" "inference-testing" "removal-module-cost-tests" "kb-query" "kbq-query-run" "arete" "janus" "leviathan" "deep-inference-generator" "lilliput" "lilliput-caches" "ask-utilities" "removal-module-utilities" "removal-modules-lookup" "removal-modules-minimization" "removal-modules-evaluation" "removal-modules-symmetry" "removal-modules-transitivity" "removal-modules-reflexivity" "removal-modules-reflexive-on" "removal-modules-genlpreds-lookup" "removal-modules-relation-all" "removal-modules-relation-all-instance" "removal-modules-relation-all-exists" "removal-modules-relation-instance-exists" "removal-modules-term-external-id-string" "removal-modules-backchain-required" "removal-modules-abduction" "meta-removal-modules" "removal-modules-non-wff" "removal-modules-isa" "removal-modules-genls" "removal-modules-denotes" "removal-modules-classification" "removal-modules-subset-of" "removal-modules-nearest-isa" "removal-modules-disjointwith" "removal-modules-termofunit" "removal-modules-natfunction" "removal-modules-formula-arg-n" "removal-modules-equals" "removal-modules-evaluate" "removal-modules-different" "removal-modules-genlmt" "removal-modules-genlpreds" "removal-modules-genlinverse" "removal-modules-negationpreds" "removal-modules-negationinverse" "removal-modules-ist" "removal-modules-consistent" "removal-modules-admitted-formula" "removal-modules-asserted-formula" "removal-modules-asserted-arg1-binary-preds" "removal-modules-function-corresponding-predicate" "removal-modules-start-offset" "removal-modules-true-sentence" "removal-modules-formula-implies" "removal-modules-asserted-more-specifically" "removal-modules-isomorphic-sentences" "removal-modules-conceptually-related" "removal-modules-integer-between" "removal-modules-indexical-referent" "removal-modules-constant-name" "removal-modules-assertion-mt" "removal-modules-term-strings" "removal-modules-perform-subl" "removal-modules-term-chosen" "removal-modules-tva-lookup" "removal-modules-bookkeeping" "removal-modules-rtv" "removal-modules-member-of-list" "removal-modules-assertion-arguments" "removal-modules-deduction-supports" "removal-modules-assertion-deductions" "removal-modules-assertion-hl-asserted-argument-keyword" "removal-modules-inference-reflection" "removal-modules-known-antecedent-rule" "removal-modules-distance-between" "removal-modules-lat-long" "removal-modules-country-of-city" "removal-modules-kappa" "removal-modules-interval-range" "removal-modules-ke-useless-precision-value" "removal-modules-kb-indexing" "removal-modules-concatenate-strings" "removal-modules-query-answers" "removal-modules-set-of-list-with-same-member-in-pos" "removal-modules-conjunctive-pruning" "removal-modules-kb-sentence" "removal-modules-source-sentence" "transformation-modules" "simplification-modules" "rewrite-modules" "forward" "forward-modules" "forward-propagate-assertions" "abnormal" "hl-prototypes" "collection-intersection" "inference-abduction-utilities" "abduction" "fi" "cyc-bookkeeping" "cyc-kernel" "prove" "inference-iterators" "ke" "api-remote-objects" "batch-ke" "inference-viewer" "plot-generation" "cardinality-estimates" "relationship-generality-estimates" "evaluation-defns" "collection-defns" "ke-utilities" "ke-text" "kb-ontology-utilities" "ontology-layers" "system-benchmarks" "object-similarity" "partitions" "convert-partitions" "core" "kbs-utilities" "kbs-identification" "kbs-add-redundant" "kbs-partition" "kbs-cleanup" "kbs-compare" "kb-cleanup" "genls-hierarchy-problems" "encapsulation" "transcript-utilities" "transcript-server" "operation-communication" "operation-queues" "remote-operation-filters" "user-actions" "formula-template-vars" "xml-vars" "xml-macros" "agenda" "subl-identifier" "query-utilities" "kb-compare" "kb-paths" "ke-coherence" "ke-tools" "kb-filtering" "xml-utilities" "misc-kb-utilities" "scientific-numbers" "scientific-number-utilities" "extended-numbers" "arithmetic" "hl-storage-module-declarations" "bookkeeping-store" "dumper" "builder-utilities" "gt-modules" "task-processor" "java-api-kernel" "connection-guarding" "open-cyc-simple-inference-api" "term-classification-tree" "ctest-macros" "ctest-utils" "kct-variables" "kct-utils" "kct-cyc-testing" "kct-thinking" "cycl-query-specification" "new-cycl-query-specification" "formula-templates" "graphic-library-format" "value-tables" "script-instance-editor-api" "similarity" "constrained-term-finder" "constraint-filters" "test-query-suite" "nl-api-datastructures" "morphology" "file-backed-cache-setup" "standard-tokenization" "interval-span" "removal-modules-relevant-similar-queries" "removal-modules-semantically-related" "folification" "thcl" "cycml" "cycml-generator" "concept-filter" "lucene-session" "quirk-trampolines" "nlp-tests" "properties" "webcache" "xref-database" "secure-translation" "form-translation" "file-translation" "system-translation" "common-optimization" "c-name-translation" "c-backend" "optimized-funcall-declarations" "java-name-translation" "java-backend" "translator-utilities"))
    (create-module name "cycl")))