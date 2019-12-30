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
 
This file incorporates work covered by the following copyright
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

(in-package :asdf-user)

(defsystem "clyc"
  :name "clyc"
  :author "White Flame"
  :licence "GNU Affero General Public License, version 3"
  :description "Port of Cyc to Common Lisp"

  :depends-on ("alexandria"
               "bordeaux-threads"
               "flexi-streams"
               "named-readtables"
               "usocket-server")

  :serial t
  :components ((:file "subl-support")
               (:module "larkc-cycl"
                        ;; The list of files is in order of initialization from the original cycl.java
                        ;; 413 non-missing components included in larkc version.
                        ;; Some files only have some vars or structs with no behavior,
                        ;; but we can see tons of commented out functions in
                        ;; its declarations.  These files are elided instead of
                        ;; taking up visual/conceptual space for now.
                        ;; Dependency ordering is still completely bonkered.
                        :components (;; MISSING-LARKC (:file "cyc-cvs-id")
                                     ;; ELIDED (:file "meta-macros") - too low level
                                     ;; ELIDED (:file "access-macros") - too low level
                                     ;; ELIDED (:file "subl-macro-promotions") - too low level
                                     (:file "subl-promotions")
                                     (:file "subl-macros")
                                     ;; ELIDED (:file "format-nil") - cl:format
                                     (:file "modules")
                                     (:file "cyc-revision-extraction")
                                     (:file "system-parameters"
                                            :depends-on ("system-info"))
                                     (:file "system-version")
                                     (:file "system-info"
                                            :depends-on ("system-version"))
                                     (:file "utilities-macros"
                                            :depends-on ("numeric-date-utilities"))
                                     ;; ELIDED (:file "timing") - mostly missing-larkc
                                     ;; MISSING-LARKC (:file "cyc-file-dependencies")
                                     ;; MISSING-LARKC (:file "cycml-macros")
                                     (:file "control-vars")
                                     ;; MISSING-LARKC (:file "sunit-macros")
                                     ;; MISSING-LARKC (:file "sunit-external")
                                     (:file "cfasl"
                                            :depends-on ("cfasl-compression"))
                                     ;; MISSING-LARKC (:file "sxhash-external")
                                     ;; MISSING-LARKC (:file "red-implementation")
                                     ;; ELIDED (:file "red-api") - basically missing-larkc
                                     (:file "cyc-testing")
                                     ;; Concatenation of:
                                     ;; (:file "cyc-testing.cyc-testing-initialization")
                                     ;; (:file "cyc-testing.cyc-testing")
                                     ;; (:file "cyc-testing.generic-testing")
                                     (:file "hash-table-utilities")
                                     ;; ELIDED (:file "keyhash") - use standard hashtables
                                     (:file "set-contents")
                                     ;; MISSING-LARKC (:file "reader-writer-locks")
                                     (:file "memoization-state"
                                            :depends-on ("cache"))
                                     (:file "list-utilities")
                                     (:file "transform-list-utilities")
                                     (:file "vector-utilities")
                                     (:file "string-utilities")
                                     ;; ELIDED (:file "unicode-strings") - CL unicode support
                                     ;; ELIDED (:file "unicode-subsets") - CL unicode support
                                     ;; MISSING-LARKC (:file "unicode-support")
                                     ;; ELIDED (:file "unicode-streams") - CL unicode support
                                     ;; MISSING-LARKC (:file "defstruct-sequence")
                                     ;; MISSING-LARKC (:file "visitation-utilities")
                                     ;; MISSING-LARKC (:file "mail-utilities")
                                     ;; ELIDED (:file "mail-message") - only structs
                                     (:file "number-utilities")
                                     ;; ELIDED (:file "fraction-utilities") - CL rationals
                                     ;; MISSING-LARKC (:file "matrix-utilities")
                                     (:file "numeric-date-utilities")
                                     (:file "iteration")
                                     (:file "binary-tree")
                                     (:file "stacks")
                                     (:file "queues")
                                     (:file "deck")
                                     (:file "integer-sequence-generator")
                                     ;; MISSING-LARKC (:file "semaphores")
                                     (:file "process-utilities") ;; possibly missing-larkc
                                     ;; ELIDED (:file "os-process-utilities") - make-os-process is missing-larkc
                                     (:file "tcp") ;; added, was part of native JRTL
                                     (:file "tcp-server-utilities")
                                     (:file "pattern-match")
                                     (:file "tries")
                                     ;; ELIDED (:file "shelfs") - basically missing-larkc
                                     (:file "id-index")
                                     ;; ELIDED (:file "dictionary-contents") - use hashtables
                                     ;; ELIDED (:file "dictionary") - use hashtables
                                     (:file "cfasl-compression")
                                     (:file "cfasl-utilities")
                                     ;; ELIDED (:file "bijection") ;; basically missing-larkc
                                     ;; ELIDED (:file "glob") - basically missing-larkc
                                     ;; ELIDED (:file "keyhash-utilities") - since keyhash is
                                     (:file "set")
                                     (:file "set-utilities")
                                     (:file "dictionary-utilities")
                                     (:file "map-utilities")
                                     ;; ELIDED (:file "bag") - basically missing-larkc
                                     ;; ELIDED (:file "accumulation") - basically missing-larkc
                                     ;; ELIDED (:file "red-infrastructure") - dependencies are missing-larkc
                                     ;; ELIDED (:file "red-infrastructure-macros") - dependencies are missing-larkc
                                     ;; MISSING-LARKC (:file "red-utilities")
                                     (:file "file-utilities")
                                     ;; ELIDED (:file "stream-buffer") - only structs, no funcs
                                     ;; ELIDED (:file "hierarchical-visitor") - only structs
                                     ;; MISSING-LARKC (:file "strie")
                                     ;; ELIDED (:file "finite-state-transducer") - only structs
                                     (:file "cache")
                                     (:file "cache-utilities")
                                     (:file "special-variable-state")
                                     ;; ELIDED (:file "simple-lru-cache-strategy") - mostly missing-larkc
                                     ;; ELIDED (:file "file-hash-table") - behavior missing-larkc. contained read-fht-uint4, replaced by read-32bit-be
                                     ;; MISSING-LARKC (:file "file-hash-table-utilities")
                                     (:file "file-vector")
                                     (:file "file-vector-utilities")
                                     ;; MISSING-LARKC (:file "generic-table-utilities")
                                     ;; MISSING-LARKC (:file "sdbc-macros")
                                     ;; ELIDED (:file "sdbc") - just vars & structss
                                     (:file "graph-utilities")
                                     ;; ELIDED (:file "sparse-vector") - just struct
                                     ;; ELIDED (:file "sparse-matrix") - just struct
                                     ;; ELIDED (:file "remote-image") - just struct
                                     ;; MISSING-LARKC (:file "cyc-testing.cyc-testing-utilities")
                                     ;; ELIDED (:file "heap") - just struct
                                     ;; MISSING-LARKC (:file "decision-tree")
                                     (:file "misc-utilities")
                                     ;; ELIDED (:file "web-utilities") - just vars & structs
                                     ;; MISSING-LARKC (:file "timing-by-category")
                                     

                                     ;; ---- Start of CYC itself.  Above here is generally the basic SubL definition & stdlib
                                     
                                     (:file "api-control-vars")
                                     (:file "eval-in-api")
                                     ;;(:file "eval-in-api-registrations")
                                     ;;(:file "api-kernel")
                                     ;;(:file "cfasl-kernel")
                                     ;;(:file "guardian")

                                     (:file "hl-interface-infrastructure")
                                     (:file "kb-macros")

                                     (:file "constant-handles") ;; moved up before completion tools

                                     (:file "constant-completion-low")
                                     (:file "constant-completion-interface")
                                     (:file "constant-completion-high")
                                     (:file "constant-completion")

                                     (:file "constant-reader")
                                     (:file "enumeration-types")
                                     (:file "kb-control-vars")
                                     (:file "mt-vars")
                                     ;;(:file "graphl-search-vars")
                                     ;;(:file "ghl-search-vars")
                                     ;;(:file "sbhl.sbhl-iteration")
                                     ;;(:file "sbhl.sbhl-paranoia")
                                     ;;(:file "sbhl.sbhl-module-vars")
                                     ;;(:file "sbhl.sbhl-link-vars")
                                     ;;(:file "sbhl.sbhl-marking-vars")
                                     ;;(:file "sbhl.sbhl-search-datastructures")
                                     ;;(:file "sbhl.sbhl-search-vars")
                                     ;; MISSING-LARKC (:file "sbhl.sbhl-time-vars")
                                     ;;(:file "sbhl.sbhl-link-iterators")
                                     ;;(:file "at-vars")
                                     ;;(:file "gt-vars")
                                     ;;(:file "czer-vars")
                                     (:file "mt-relevance-macros"
                                            :depends-on (;; TODO "hlmt"
                                                         "mt-vars"
                                                         ;; TODO "mt-relevance-cache"
                                                         "psc"
                                                         "el-utilities"))
                                     ;;(:file "pred-relevance-macros")
                                     ;;(:file "at-macros")
                                     ;; MISSING-LARKC (:file "czer-macros")
                                     ;;(:file "wff-macros")
                                     ;;(:file "wff-vars")
                                     ;; MISSING-LARKC (:file "gt-macros")
                                     ;; MISSING-LARKC (:file "hl-macros")
                                     ;;(:file "obsolete")
                                     ;; MISSING-LARKC (:file "obsolete-macros")
                                     ;;(:file "kb-mapping-macros")
                                     ;;(:file "kb-access-metering")

                                     (:file "kb-object-manager")
                                     ;;(:file "hlmt")
                                     ;;(:file "hlmt-czer")

                                     (:file "constants-interface"
                                            :depends-on ("hl-interface-infrastructure"
                                                         "control-vars"
                                                         "constants-low"
                                                         "constants-high"
                                                         "constant-completion-low"))
                                     ;;(:file "constant-index-manager")

                                     (:file "constants-low"
                                            :depends-on ("id-index"
                                                         "constants-high"
                                                         "constant-completion-low"
                                                         "constant-handles"))
                                     (:file "constants-high"
                                            :depends-on (;; CIRCULAR "constants-interface"
                                                         "kb-macros"
                                                         "term"
                                                         "narts-high"
                                                         ;; TODO "bookkeeping-store"
                                                         ;; TODO "kb-indexing"
                                                         ;; TODO "kb-hl-supports"
                                                         ;; TODO "cardinality-estimates"
                                                         "constant-handles"))
                                     (:file "nart-handles")
                                     ;; MISSING-LARKC (:file "narts-interface")
                                     ;;(:file "nart-index-manager")
                                     ;;(:file "nart-hl-formula-manager")
                                     ;; MISSING-LARKC (:file "narts-low")
                                     (:file "narts-high")
                                     (:file "forts"
                                            :depends-on ("constant-handles"
                                                         "constants-low"
                                                         "nart-handles"
                                                         "id-index"
                                                         "cfasl"))
                                     ;;(:file "assertion-handles")
                                     ;;(:file "assertions-interface")
                                     ;;(:file "assertion-manager")
                                     ;;(:file "assertions-low")
                                     ;;(:file "assertions-high")
                                     ;;(:file "kb-hl-support-manager")
                                     ;;(:file "kb-hl-supports")
                                     ;;(:file "deduction-handles")
                                     ;;(:file "deductions-interface")
                                     ;;(:file "deduction-manager")
                                     ;;(:file "deductions-low")
                                     ;;(:file "deductions-high")
                                     ;;(:file "unrepresented-term-index-manager")
                                     ;;(:file "unrepresented-terms")
                                     ;;(:file "arguments")
                                     ;;(:file "clause-strucs")
                                     ;;(:file "variables")
                                     ;; MISSING-LARKC (:file "format-cycl-expression")
                                     ;;(:file "hl-storage-modules")
                                     ;;(:file "hl-modifiers")
                                     ;; MISSING-LARKC (:file "sxhash-external-kb")
                                     ;; MISSING-LARKC (:file "el-macros")
                                     ;;(:file "sbhl.sbhl-macros")
                                     ;;(:file "cycl-variables")
                                     (:file "el-utilities"
                                            :depends-on (;; TODO "cycl-utilities"
                                                         "list-utilities"))
                                     ;;(:file "clause-utilities")
                                     ;;(:file "cycl-utilities")
                                     ;;(:file "cycl-grammar")
                                     ;;(:file "el-grammar")
                                     ;; MISSING-LARKC (:file "unicode-nauts")
                                     (:file "term"
                                            :depends-on ("el-utilities"
                                                         ;; TODO "assertion-handles"
                                                         ;; CIRCULAR "forts"
                                                         ;; CIRCULAR "fort-types-interface"
                                                         ;; TODO "kb-accessors"
                                                         "el-utilities"
                                                         ;; TODO "kb-mapping-utilities"
                                                         "constant-handles"
                                                         ;; CIRCULAR "constants-high"
                                                         "string-utilities"
                                                         "nart-handles"
                                                         ;; TODO "cycl-utilities"
                                                         "narts-high"
                                                         "list-utilities"
                                                         ;; TODO "cycl-variables"
                                                         ;; TODO "czer-vars"
                                                         "mt-relevance-macros"))
                                     ;;(:file "kb-indexing-datastructures")
                                     ;;(:file "kb-utilities")
                                     ;;(:file "simple-indexing")
                                     ;;(:file "kb-indexing-declarations")
                                     ;;(:file "kb-indexing-macros")
                                     ;;(:file "kb-indexing")
                                     ;;(:file "virtual-indexing")
                                     ;;(:file "kb-mapping")
                                     ;;(:file "kb-mapping-utilities")
                                     ;;(:file "kb-gp-mapping")
                                     ;;(:file "somewhere-cache")
                                     ;;(:file "auxiliary-indexing")
                                     ;; MISSING-LARKC (:file "inferred-indexing")
                                     ;;(:file "arity")
                                     ;;(:file "kb-accessors")
                                     ;; MISSING-LARKC (:file "kb-iterators")
                                     ;;(:file "function-terms")
                                     ;;(:file "relation-evaluation")
                                     ;;(:file "assertion-utilities")
                                     ;; MISSING-LARKC (:file "parameter-specification-utilities")
                                     ;;(:file "clauses")
                                     ;;(:file "bindings")
                                     ;;(:file "unification")
                                     ;;(:file "unification-utilities")
                                     ;;(:file "file-backed-cache")
                                     ;;(:file "graphl-graph-utilities")
                                     ;; MISSING-LARKC (:file "ghl-graph-utilities")
                                     ;;(:file "ghl-link-iterators")
                                     ;;(:file "ghl-marking-utilities")
                                     ;;(:file "ghl-search-utilities")
                                     ;;(:file "sbhl.sbhl-graphs")
                                     ;;(:file "sbhl.sbhl-caching-policies")
                                     ;;(:file "sbhl.sbhl-module-utilities")
                                     ;;(:file "sbhl.sbhl-links")
                                     ;; MISSING-LARKC (:file "sbhl.sbhl-nat-utilities")
                                     ;;(:file "sbhl.sbhl-link-utilities")
                                     ;;(:file "sbhl.sbhl-link-methods")
                                     ;;(:file "sbhl.sbhl-marking-utilities")
                                     ;;(:file "sbhl.sbhl-search-utilities")
                                     ;;(:file "sbhl.sbhl-marking-methods")
                                     ;;(:file "sbhl.sbhl-search-methods")
                                     ;;(:file "sbhl.sbhl-search-what-mts")
                                     ;;(:file "sbhl.sbhl-search-implied-relations")
                                     ;;(:file "sbhl.sbhl-module-declarations")
                                     ;;(:file "genls")
                                     ;;(:file "disjoint-with")
                                     ;;(:file "sdc")
                                     ;;(:file "isa")
                                     ;;(:file "genl-predicates")
                                     ;;(:file "negation-predicate")
                                     ;;(:file "genl-mts")
                                     ;;(:file "mt-relevance-cache")
                                     ;;(:file "predicate-relevance-cache")
                                     ;; MISSING-LARKC (:file "negation-mt")
                                     ;;(:file "ghl-search-methods")
                                     ;;(:file "sbhl.sbhl-cache")
                                     (:file "fort-types-interface"
                                            :depends-on ("forts"
                                                         "mt-relevance-macros"
                                                         ;; TODO "subhl-module-vars"
                                                         ;; TODO "subhl-search-vars"
                                                         ;; TODO "subhl-search-methods"
                                                         ;; TODO "subhl-paranoia"
                                                         ;; TODO "isa"
                                                         ;; TODO "at-defns"
                                                         ))
                                     ;;(:file "czer-trampolines")
                                     ;;(:file "wff-utilities")
                                     ;;(:file "wff-module-datastructures")
                                     ;; MISSING-LARKC (:file "wff-modules")
                                     ;;(:file "wff")
                                     ;; MISSING-LARKC (:file "wff-suggest")
                                     ;;(:file "simplifier")
                                     ;; MISSING-LARKC (:file "tersifier")
                                     ;;(:file "verbosifier")
                                     ;;(:file "czer-utilities")
                                     ;;(:file "precanonicalizer")
                                     ;;(:file "postcanonicalizer")
                                     ;;(:file "clausifier")
                                     ;; MISSING-LARKC (:file "prop-sentence-clausifier")
                                     ;;(:file "czer-graph")
                                     ;;(:file "czer-main")
                                     ;;(:file "rule-macros")
                                     ;;(:file "skolems")
                                     ;;(:file "czer-meta")
                                     ;;(:file "uncanonicalizer")
                                     ;;(:file "canon-tl")
                                     ;;(:file "at-routines")
                                     ;;(:file "at-utilities")
                                     ;;(:file "at-admitted")
                                     ;;(:file "at-defns")
                                     ;;(:file "defns")
                                     ;;(:file "at-var-types")
                                     ;;(:file "at-cache")
                                     ;;(:file "arg-type")
                                     ;; MISSING-LARKC (:file "applicable-relations")
                                     ;;(:file "sksi.sksi-infrastructure.sksi-macros")
                                     ;; MISSING-LARKC (:file "inference.janus-macros")
                                     ;;(:file "gt-utilities")
                                     ;;(:file "gt-search")
                                     ;;(:file "gt-methods")
                                     ;;(:file "transitivity")
                                     ;; MISSING-LARKC (:file "transfers-through")
                                     ;;(:file "tva-utilities")
                                     ;;(:file "tva-tactic")
                                     ;;(:file "tva-strategy")
                                     ;;(:file "tva-inference")
                                     ;;(:file "tva-cache")
                                     ;;(:file "equality-store")
                                     ;;(:file "equals")
                                     ;;(:file "rewrite-of-propagation")
                                     ;;(:file "hl-supports")
                                     ;; MISSING-LARKC (:file "conflicts")
                                     ;; MISSING-LARKC (:file "ebl")
                                     ;;(:file "preserves-genls-in-arg")
                                     ;;(:file "formula-pattern-match")
                                     ;;(:file "cfasl-kb-methods")
                                     ;;(:file "inference.harness.inference-macros")
                                     ;;(:file "inference.harness.inference-modules")
                                     ;;(:file "search")
                                     ;;(:file "tms")
                                     ;;(:file "inference.harness.after-adding")
                                     ;;(:file "inference.harness.rule-after-adding")
                                     ;;(:file "inference.modules.after-adding-modules")
                                     ;;(:file "inference.harness.argumentation")
                                     ;;(:file "backward")
                                     (:file "psc")
                                     ;;(:file "inference.inference-trampolines")
                                     ;;(:file "inference.inference-completeness-utilities")
                                     ;;(:file "backward-utilities")
                                     ;;(:file "backward-results")
                                     ;; MISSING-LARKC (:file "transformation-heuristics")
                                     ;;(:file "inference.inference-pad-data")
                                     ;;(:file "inference.modules.preference-modules")
                                     ;; MISSING-LARKC (:file "inference.modules.preference-module-declarations")
                                     ;;(:file "inference.harness.inference-datastructures-enumerated-types")
                                     ;;(:file "inference.harness.inference-datastructures-problem-store")
                                     ;;(:file "inference.harness.inference-datastructures-problem-query")
                                     ;;(:file "inference.harness.inference-datastructures-problem")
                                     ;;(:file "inference.harness.inference-datastructures-problem-link")
                                     ;;(:file "inference.harness.inference-datastructures-tactic")
                                     ;;(:file "inference.harness.inference-datastructures-proof")
                                     ;;(:file "inference.harness.inference-datastructures-strategy")
                                     ;;(:file "inference.harness.inference-datastructures-forward-propagate")
                                     ;;(:file "inference.harness.inference-datastructures-inference")
                                     ;;(:file "inference.harness.inference-czer")
                                     ;; MISSING-LARKC (:file "inference.harness.inference-proof-spec")
                                     ;; MISSING-LARKC (:file "inference.harness.inference-proof-spec-store")
                                     ;;(:file "inference.harness.inference-worker")
                                     ;;(:file "inference.harness.inference-worker-answer")
                                     ;;(:file "inference.harness.inference-worker-restriction")
                                     ;;(:file "inference.harness.inference-worker-removal")
                                     ;;(:file "inference.harness.inference-worker-transformation")
                                     ;;(:file "inference.harness.inference-worker-residual-transformation")
                                     ;;(:file "inference.harness.inference-worker-rewrite")
                                     ;;(:file "inference.harness.inference-worker-split")
                                     ;;(:file "inference.harness.inference-worker-join-ordered")
                                     ;;(:file "inference.harness.inference-worker-join")
                                     ;;(:file "inference.harness.inference-worker-union")
                                     ;;(:file "inference.harness.inference-tactician-strategic-uninterestingness")
                                     ;;(:file "inference.harness.inference-lookahead-productivity")
                                     ;;(:file "inference.harness.inference-min-transformation-depth")
                                     ;;(:file "inference.harness.inference-tactician")
                                     ;;(:file "inference.harness.inference-tactician-utilities")
                                     ;;(:file "inference.harness.inference-strategic-heuristics")
                                     ;;(:file "inference.harness.inference-balanced-tactician-datastructures")
                                     ;;(:file "inference.harness.inference-balanced-tactician-strategic-uninterestingness")
                                     ;;(:file "inference.harness.inference-balanced-tactician-motivation")
                                     ;;(:file "inference.harness.inference-balanced-tactician-execution")
                                     ;;(:file "inference.harness.inference-heuristic-balanced-tactician")
                                     ;;(:file "inference.harness.balancing-tactician")
                                     ;;(:file "inference.harness.removal-tactician-datastructures")
                                     ;;(:file "inference.harness.removal-tactician-uninterestingness")
                                     ;;(:file "inference.harness.removal-tactician-motivation")
                                     ;;(:file "inference.harness.removal-tactician-execution")
                                     ;;(:file "inference.harness.removal-tactician")
                                     ;;(:file "inference.harness.transformation-tactician-datastructures")
                                     ;; MISSING-LARKC (:file "inference.harness.transformation-tactician-uninterestingness")
                                     ;; MISSING-LARKC (:file "inference.harness.transformation-tactician-motivation")
                                     ;; MISSING-LARKC (:file "inference.harness.transformation-tactician-execution")
                                     ;; MISSING-LARKC (:file "inference.harness.transformation-tactician")
                                     ;;(:file "inference.harness.new-root-tactician-datastructures")
                                     ;; MISSING-LARKC (:file "inference.harness.new-root-tactician-motivation")
                                     ;; MISSING-LARKC (:file "inference.harness.new-root-tactician-execution")
                                     ;; MISSING-LARKC (:file "inference.harness.new-root-tactician")
                                     ;;(:file "neural-net")
                                     ;;(:file "inference.harness.inference-strategist")
                                     ;;(:file "inference.harness.inference-kernel")
                                     ;;(:file "inference.harness.inference-trivial")
                                     ;; MISSING-LARKC (:file "inference.harness.inference-utilities")
                                     ;;(:file "inference.harness.inference-parameters")
                                     ;;(:file "inference.harness.inference-metrics")
                                     ;;(:file "inference.harness.inference-analysis")
                                     ;; MISSING-LARKC (:file "inference.harness.inference-serialization")
                                     ;;(:file "cyc-testing.inference-unit-tests")
                                     ;;(:file "cyc-testing.removal-module-tests")
                                     ;;(:file "cyc-testing.transformation-module-tests")
                                     ;;(:file "cyc-testing.evaluatable-relation-tests")
                                     ;; MISSING-LARKC (:file "cyc-testing.inference-testing-helpers")
                                     ;; MISSING-LARKC (:file "cyc-testing.inference-testing")
                                     ;;(:file "cyc-testing.removal-module-cost-tests")
                                     ;; MISSING-LARKC (:file "inference.kb-query")
                                     ;;(:file "inference.kbq-query-run")
                                     ;;(:file "inference.arete")
                                     ;;(:file "inference.janus")
                                     ;;(:file "inference.leviathan")
                                     ;; MISSING-LARKC (:file "inference.deep-inference-generator")
                                     ;; MISSING-LARKC (:file "inference.lilliput")
                                     ;; MISSING-LARKC (:file "inference.lilliput-caches")
                                     ;;(:file "inference.ask-utilities")
                                     ;;(:file "inference.harness.removal-module-utilities")
                                     ;;(:file "inference.modules.removal.removal-modules-lookup")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-minimization")
                                     ;;(:file "inference.modules.removal.removal-modules-evaluation")
                                     ;;(:file "inference.modules.removal.removal-modules-symmetry")
                                     ;;(:file "inference.modules.removal.removal-modules-transitivity")
                                     ;;(:file "inference.modules.removal.removal-modules-reflexivity")
                                     ;;(:file "inference.modules.removal.removal-modules-reflexive-on")
                                     ;;(:file "inference.modules.removal.removal-modules-genlpreds-lookup")
                                     ;;(:file "inference.modules.removal.removal-modules-relation-all")
                                     ;;(:file "inference.modules.removal.removal-modules-relation-all-instance")
                                     ;;(:file "inference.modules.removal.removal-modules-relation-all-exists")
                                     ;;(:file "inference.modules.removal.removal-modules-relation-instance-exists")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-term-external-id-string")
                                     ;;(:file "inference.modules.removal.removal-modules-backchain-required")
                                     ;;(:file "inference.modules.removal.removal-modules-abduction")
                                     ;;(:file "inference.modules.removal.meta-removal-modules")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-non-wff")
                                     ;;(:file "inference.modules.removal.removal-modules-isa")
                                     ;;(:file "inference.modules.removal.removal-modules-genls")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-denotes")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-classification")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-subset-of")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-nearest-isa")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-disjointwith")
                                     ;;(:file "inference.modules.removal.removal-modules-termofunit")
                                     ;;(:file "inference.modules.removal.removal-modules-natfunction")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-formula-arg-n")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-equals")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-evaluate")
                                     ;;(:file "inference.modules.removal.removal-modules-different")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-genlmt")
                                     ;;(:file "inference.modules.removal.removal-modules-genlpreds")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-genlinverse")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-negationpreds")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-negationinverse")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-ist")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-consistent")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-admitted-formula")
                                     ;;(:file "inference.modules.removal.removal-modules-asserted-formula")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-asserted-arg1-binary-preds")
                                     ;;(:file "inference.modules.removal.removal-modules-function-corresponding-predicate")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-start-offset")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-true-sentence")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-formula-implies")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-asserted-more-specifically")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-isomorphic-sentences")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-conceptually-related")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-integer-between")
                                     ;;(:file "inference.modules.removal.removal-modules-indexical-referent")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-constant-name")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-assertion-mt")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-term-strings")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-perform-subl")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-term-chosen")
                                     ;;(:file "inference.modules.removal.removal-modules-tva-lookup")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-bookkeeping")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-rtv")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-member-of-list")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-assertion-arguments")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-deduction-supports")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-assertion-deductions")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-assertion-hl-asserted-argument-keyword")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-inference-reflection")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-known-antecedent-rule")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-distance-between")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-lat-long")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-country-of-city")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-kappa")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-interval-range")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-ke-useless-precision-value")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-kb-indexing")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-concatenate-strings")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-query-answers")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-set-of-list-with-same-member-in-pos")
                                     ;;(:file "inference.modules.removal.removal-modules-conjunctive-pruning")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-kb-sentence")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-source-sentence")
                                     ;;(:file "inference.modules.transformation-modules")
                                     ;;(:file "inference.modules.simplification-modules")
                                     ;;(:file "inference.modules.rewrite-modules")
                                     ;;(:file "inference.harness.forward")
                                     ;;(:file "inference.modules.forward-modules")
                                     ;; MISSING-LARKC (:file "inference.forward-propagate-assertions")
                                     ;;(:file "inference.harness.abnormal")
                                     ;;(:file "inference.harness.hl-prototypes")
                                     ;;(:file "inference.collection-intersection")
                                     ;;(:file "inference.harness.inference-abduction-utilities")
                                     ;; MISSING-LARKC (:file "abduction")
                                     ;;(:file "fi")
                                     ;;(:file "cyc-bookkeeping")
                                     ;;(:file "cyc-kernel")
                                     ;; MISSING-LARKC (:file "inference.harness.prove")
                                     ;; MISSING-LARKC (:file "inference.inference-iterators")
                                     ;;(:file "ke")
                                     ;; MISSING-LARKC (:file "api-remote-objects")
                                     ;; MISSING-LARKC (:file "batch-ke")
                                     ;; MISSING-LARKC (:file "inference.inference-viewer")
                                     ;; MISSING-LARKC (:file "plot-generation")
                                     ;;(:file "cardinality-estimates")
                                     ;; MISSING-LARKC (:file "relationship-generality-estimates")
                                     ;;(:file "evaluation-defns")
                                     ;;(:file "collection-defns")
                                     ;; MISSING-LARKC (:file "ke-utilities")
                                     ;; MISSING-LARKC (:file "ke-text")
                                     ;; MISSING-LARKC (:file "kb-ontology-utilities")
                                     ;; MISSING-LARKC (:file "ontology-layers")
                                     ;;(:file "system-benchmarks")
                                     ;; MISSING-LARKC (:file "object-similarity")
                                     ;; MISSING-LARKC (:file "partitions")
                                     ;; MISSING-LARKC (:file "convert-partitions")
                                     ;; MISSING-LARKC (:file "core")
                                     ;; MISSING-LARKC (:file "kbs-utilities")
                                     ;; MISSING-LARKC (:file "kbs-identification")
                                     ;; MISSING-LARKC (:file "kbs-add-redundant")
                                     ;; MISSING-LARKC (:file "kbs-partition")
                                     ;; MISSING-LARKC (:file "kbs-cleanup")
                                     ;; MISSING-LARKC (:file "kbs-compare")
                                     ;; MISSING-LARKC (:file "kb-cleanup")
                                     ;; MISSING-LARKC (:file "genls-hierarchy-problems")
                                     ;;(:file "encapsulation")
                                     ;;(:file "transcript-utilities")
                                     ;;(:file "transcript-server")
                                     ;;(:file "operation-communication")
                                     ;;(:file "operation-queues")
                                     ;; MISSING-LARKC (:file "remote-operation-filters")
                                     ;;(:file "user-actions")
                                     ;; MISSING-LARKC (:file "formula-template-vars")
                                     ;; MISSING-LARKC (:file "xml-vars")
                                     ;; MISSING-LARKC (:file "xml-macros")
                                     ;;(:file "agenda")
                                     ;;(:file "subl-identifier")
                                     ;; MISSING-LARKC (:file "query-utilities")
                                     ;;(:file "kb-compare")
                                     ;;(:file "kb-paths")
                                     ;; MISSING-LARKC (:file "ke-coherence")
                                     ;; MISSING-LARKC (:file "ke-tools")
                                     ;; MISSING-LARKC (:file "kb-filtering")
                                     ;;(:file "xml-utilities")
                                     ;;(:file "misc-kb-utilities")
                                     ;;(:file "scientific-numbers")
                                     ;; MISSING-LARKC (:file "scientific-number-utilities")
                                     ;; MISSING-LARKC (:file "extended-numbers")
                                     ;; MISSING-LARKC (:file "arithmetic")
                                     ;;(:file "hl-storage-module-declarations")
                                     ;;(:file "bookkeeping-store")
                                     ;;(:file "dumper")
                                     ;;(:file "builder-utilities")
                                     ;; MISSING-LARKC (:file "gt-modules")
                                     ;;(:file "task-processor")
                                     ;;(:file "java-api-kernel")
                                     ;; MISSING-LARKC (:file "connection-guarding")
                                     ;; MISSING-LARKC (:file "inference.open-cyc-simple-inference-api")
                                     ;; MISSING-LARKC (:file "term-classification-tree")
                                     ;; MISSING-LARKC (:file "cyc-testing.ctest-macros")
                                     ;;(:file "cyc-testing.ctest-utils")
                                     ;; MISSING-LARKC (:file "cyc-testing.kb-content-test.kct-variables")
                                     ;;(:file "cyc-testing.kb-content-test.kct-utils")
                                     ;; MISSING-LARKC (:file "cyc-testing.kb-content-test.kct-cyc-testing")
                                     ;; MISSING-LARKC (:file "cyc-testing.kb-content-test.kct-thinking")
                                     ;;(:file "cycl-query-specification")
                                     ;;(:file "new-cycl-query-specification")
                                     ;;(:file "formula-templates")
                                     ;;(:file "graphic-library-format")
                                     ;;(:file "value-tables")
                                     ;; MISSING-LARKC (:file "script-instance-editor-api")
                                     ;; MISSING-LARKC (:file "similarity")
                                     ;; MISSING-LARKC (:file "constrained-term-finder")
                                     ;; MISSING-LARKC (:file "constraint-filters")
                                     ;;(:file "test-query-suite")
                                     ;; MISSING-LARKC (:file "nl-api-datastructures")
                                     ;;(:file "morphology")
                                     ;;(:file "file-backed-cache-setup")
                                     ;;(:file "standard-tokenization")
                                     ;;(:file "interval-span")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-relevant-similar-queries")
                                     ;; MISSING-LARKC (:file "inference.modules.removal.removal-modules-semantically-related")
                                     ;;(:file "folification")
                                     ;; MISSING-LARKC (:file "thcl")
                                     ;; MISSING-LARKC (:file "cycml")
                                     ;;(:file "cycml-generator")
                                     ;;(:file "concept-filter")
                                     ;;(:file "lucene-session")
                                     ;; MISSING-LARKC (:file "quirk.quirk-trampolines")
                                     ;; MISSING-LARKC (:file "nlp-tests")
                                     ;; MISSING-LARKC (:file "properties")
                                     ;; MISSING-LARKC (:file "webcache")
                                     ;;(:file "xref-database")
                                     ;;(:file "secure-translation")
                                     ;; MISSING-LARKC (:file "form-translation")
                                     ;;(:file "file-translation")
                                     ;;(:file "system-translation")
                                     ;; MISSING-LARKC (:file "common-optimization")
                                     ;;(:file "c-name-translation")
                                     ;;(:file "c-backend")
                                     ;; MISSING-LARKC (:file "optimized-funcall-declarations")
                                     ;;(:file "java-name-translation")
                                     ;; MISSING-LARKC (:file "java-backend")
                                     ;; MISSING-LARKC (:file "translator-utilities")
                                     ;;(:file "eu.larkc.core.LarkcInit")
                                     ;; MISSING-LARKC (:file "eu.larkc.core.endpoint.sparql.LarKCHttpServer")
                                     ))))
