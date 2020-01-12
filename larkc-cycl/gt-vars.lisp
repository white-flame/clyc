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

;; Building off the macro from at-vars.lisp
(defmacro def-gt-state-var (name val &body (&optional docstring (definer 'defparameter)))
  `(def-state-var ,name ,val *at-state-variables* ,docstring ,definer))


(def-gt-state-var *gt-dispatch-table*
    '((:superiors gtm-superiors (fort &optional mt))
      (:inferiors gtm-inferiors (fort &optional mt))
      (:min-superiors gtm-min-superiors (fort &optional mt))
      (:max-inferiors gtm-max-inferiors (fort &optional mt))
      (:co-inferiors gtm-co-inferiors (fort &optional mt))
      (:co-superiors gtm-co-superiors (fort &optional mt))
      (:redundant-superiors gtm-redundant-superiors (fort &optional mt))
      (:redundant-inferiors gtm-redundant-inferiors (fort &optional mt))
      (:all-superiors gtm-all-superiors (fort &optional mt))
      (:all-inferiors gtm-all-inferiors (fort &optional mt))
      (:all-accessible gtm-all-accessible (fort &optional mt))
      (:union-all-inferiors gtm-union-all-inferiors (fort &optional mt))
      (:compose-fn-superiors gtm-compose-fn-all-superiors (fort fn &optional (combine-fn 'nconc) mt))
      (:compose-fn-inferiors gtm-compose-fn-all-inferiors (fort fn &optional (combine-fn 'nconc) mt))
      (:all-dependent-inferiors gtm-all-dependent-inferiors (fort &optional mt))
      (:compose-pred-superiors gtm-compose-pred-all-superiors (fort pred &optional (index 1) (gather 2) mt))
      (:compose-pred-inferiors gtm-compose-pred-all-inferiors (fort pred &optional (index 1) (gather 2) mt))
      (:roots gtm-roots (fort &optional mt))
      (:leaves gtm-leaves (fort &optional mt))
      (:boolean? gtm-boolean? (arg1 arg2 &optional mt))
      (:superior? gtm-superior? (superior inferior &optional mt))
      (:inferior? gtm-inferior? (inferior superior &optional mt))
      (:has-superior? gtm-has-superior? (inferior superior &optional mt))
      (:has-inferior? gtm-has-inferior? (superior inferior &optional mt))
      (:gather-superior gtm-gather-superior (spec gather-fn &optional mt))
      (:gather-inferior gtm-gather-inferior (genl gather-fn &optional mt))
      (:cycles? gtm-cycles? (fort &optional mt))
      (:completes-cycle? gtm-completes-cycle? (fort-1 fort-2 &optional mt))
      (:min-nodes gtm-min-nodes (forts &optional mt))
      (:max-nodes gtm-max-nodes (forts &optional mt))
      (:min-ceilings gtm-min-ceilings (forts &optional candidates mt))
      (:max-floors gtm-max-floors (forts &optional candidates mt))
      (:min-superiors-excluding gtm-min-superiors-excluding (inferior superior &optional mt))
      (:max-inferiors-excluding gtm-max-inferiors-excluding (superior inferior &optional mt))
      (:any-superior-path gtm-any-superior-path (inferior superior &optional mt))
      (:why-superior? gtm-why-superior? (superior inferior &optional mt))
      (:why-completes-cycle? gtm-why-completes-cycle? (fort1 fort2 &optional mt))
      (:all-superior-edges gtm-all-superior-edges (inferior superior &optional mt))
      (:all-inferior-edges gtm-all-inferior-edges (inferior superior &optional mt)) 
      (:all-paths gtm-all-paths (inferior superior &optional mt))
      (:inferiors-with-mts gtm-all-inferiors-with-mts (superior))
      (:superior-in-what-mts gtm-in-what-mts (inferior superior key-method))
      (:inferior-in-what-mts gtm-in-what-mts (superior inferior key-method))
      (:accessible-in-what-mts gtm-accessible-in-what-mts (inferior superior key-method)))
  "[Cyc] Table used to dispatch gt methods as actual calls on gt executables."
  deflexical)

(def-gt-state-var *gt-methods* (mapcar #'car *gt-dispatch-table*)
  "[Cyc] Candidate methods for gt modules.")
(def-gt-state-var *gt-parameters* (append '(:predicate
                                            :index-arg
                                            :gather-arg
                                            :accessors
                                            :mt)
                                          *gt-methods*)
  "[Cyc] Candidate parameters for gt methods.")
(def-gt-state-var *transitivity-modules* nil 
  "[Cyc] All cyc modules defined using the general transitivity module.")
(def-gt-state-var *gt-pred* nil
  "[Cyc] Transitive predicate used for current gt query.")
(def-gt-state-var *gt-index* nil
  "[Cyc] Arg used as initial index for current gt query.")
(def-gt-state-var *gt-gather* nil
  "[Cyc] Arg used as initial gather (e.g., search target) for current gt query.")
(def-gt-state-var *gt-index-arg* 1
  "[Cyc] Indexing arg position used for current gt query.")
(def-gt-state-var *gt-gather-arg* 2
  "[Cyc] Gathering arg position used for current gt query.")
(def-gt-state-var *gt-accessors* nil
  "[Cyc] Accessors used for current gt query.")
(def-gt-state-var *gt-link-type* :assertion
  "[Cyc] Type of links used in gt module.")
(def-gt-state-var *gt-mode* :superior
  "[Cyc] Mode (e.g., direction) of search during gt query.")
(def-gt-state-var *gt-modes* (list :superior
                                   :inferior
                                   :accessible
                                   :directed
                                   :inverse)
  "[Cyc] Candidate modes for gt search.")
(def-gt-state-var *gt-truth* :true
  "[Cyc] Truth relevant to the current gt query [:true :false].")
(def-gt-state-var *gt-query* nil
  "[Cyc] Literal denoting query formula of current gt query.")
(def-gt-state-var *gt-done?* nil
  "[Cyc] Terminate the current gt search?")
(def-gt-state-var *gt-searched?* nil
  "[Cyc] Current gt search path encountered searched nodes.")
(def-gt-state-var *gt-target* nil
  "[Cyc] Target of current gt search.")
(def-gt-state-var *gt-result* nil
  "[Cyc] Accumlates results of current gt query.")
(def-gt-state-var *gt-searcher* nil
  "[Cyc] Current searcher during multiple-searcher gt search.")
(def-gt-state-var *gt-base-fn* nil
  "[Cyc] Fn applied to each candidate node during gt search.")
(def-gt-state-var *gt-step-fn* nil
  "[Cyc] Fn used to step from one node to another during gt closure search.")
(def-gt-state-var *gt-compose-fn* nil
  "[Cyc] Fn applied to each accessed node during composing gt search.")
(def-gt-state-var *gt-gather-fn* nil
  "[Cyc] Fn applied to each accessed node durung a gather gt search.")
(def-gt-state-var *gt-combine-fn* #'nconc
  "[Cyc] Fn used to combine results of composing fn applied to each accessed node.")
(def-gt-state-var *gt-compare-fn* nil
  "[Cyc] Test used to compare each accessed node with target during gt search.")
(def-gt-state-var *gt-equality-fn* #'equal
  "[Cyc] Equality test used to remove duplicates from non-fort results during gt search.")
(def-gt-state-var *gt-compose-pred* nil
  "[Cyc] Composing predicate used for current composing gt query.")
(def-gt-state-var *gt-compose-index-arg* 1
  "[Cyc] Indexing arg position used for composing pred in current gt query.")
(def-gt-state-var *gt-compose-gather-arg* 2
  "[Cyc] Gathering arg position used for composing pred in current gt query.")
(def-gt-state-var *gt-max-nodes-direction* :down
  "[Cyc] Default search direction when computing gt-max-nodes.")
(def-gt-state-var *gt-use-spec-preds?* t
  "[Cyc] Use spec preds during gt searches?")
(def-gt-state-var *gt-handle-non-transitive-predicate?* nil
  "[Cyc] Make gt modules applicable to non-transitive predicates?")
;; This one is plain without the state var registration
(defparameter *gt-link-support* nil
  "[Cyc] The current link assertion or hl support.")
(def-gt-state-var *gt-what-mts-result* nil
  "[Cyc] Result holder for what-mts.")
(def-gt-state-var *gt-what-mts-goal-node* nil
  "[Cyc] Goal node of in-what-mt searches.")
(def-gt-state-var *gt-path-list-of-mts* nil
  "[Cyc] List of mts along current path.")
(def-gt-state-var *gt-path-list-of-nodes* nil
  "[Cyc] Path list of nodes, accumulated to guard against cycles.")
(def-gt-state-var *gt-path-length* 0
  "[Cyc] Length of current mt path.")
(def-gt-state-var *gt-list-of-path-lengths* nil
  "[Cyc] List of path lengths in order.")
(def-gt-state-var *gt-marking-table* nil
  "[Cyc] The hash table where we do the marking (usually dynamic).")
(def-gt-state-var *gt-target-marking-table* nil
  "[Cyc] Used for the occasional nested search.")
(def-gt-state-var *gt-depth-cutoff* 1
  "[Cyc] Depth cutoff for the search.")
(def-gt-state-var *gt-depth-cutoff?* nil
  "[Cyc] Flag for whether or not to use depth cutoff.")
(def-gt-state-var *gt-prev-depth-cutoff* 1
  "[Cyc] What the previous depth cutoff was, used for iterive deepening.")
(def-gt-state-var *gt-time-cutoff?* nil
  "[Cyc] Flag for whether or not to use time cutoff.")
(def-gt-state-var *gt-time-cutoff* 10
  "[Cyc] Time cutoff, in seconds, for the search.")
(def-gt-state-var *gt-initial-time* 0
  "[Cyc] When did the timing begin?")
(def-gt-state-var *gt-answers-cutoff?* nil
  "[Cyc] Flag for whether or not to use number of answers cutoff.")
(def-gt-state-var *gt-answers-cutoff* 1
  "[Cyc] Number of answers after which we are done searching.")
(def-gt-state-var *gt-answers-so-far* 0
  "[Cyc] Accumulator for number of answers so far.")
(def-gt-state-var *gt-goal-node* nil
  "[Cyc] Goal node.")
(def-gt-state-var *gt-edge-list* nil
  "[Cyc] List of edges along search, for graphing.")
(def-gt-state-var *gt-edge-list-return?* nil
  "[Cyc] Are we gather edge lists?")
(def-gt-state-var *gt-path-list-of-assertions* nil
  "[Cyc] List of assertions along search. for graphing.")
(def-gt-state-var *gt-cyclical-edges* nil
  "[Cyc] List of pairs (a b), where a is the node in the search upon which the cycle was discovered, and b is the edge list of a cycle it belongs to.")
(def-gt-state-var *gt-trace-level* 1 
  "[Cyc] Controls extent of tracing, warnings, etc., for the general transitivity module [0 .. 5].")
(def-gt-state-var *gt-test-level* 3 
  "[Cyc] Controls extent of testing for the general transitivity module [0 .. 5].")
(def-gt-state-var *suspend-gt-type-checking?* nil
  "[Cyc] Skip type checking during gt queries?")
(def-gt-state-var *gt-warnings* nil
  "[Cyc] Warnings from transitiviy module; possible values: :invalid-module :invalid-method.")
(def-gt-state-var *tt-dispatch-table* '((:all-targets ttm-all-targets (fort &optional mt))
                                        (:all-sources ttm-all-sources (fort &optional mt))
                                        (:boolean? ttm-boolean? (fort term &optional mt))
                                        (:accesses-in-what-mts ttm-accesses-in-what-mts (fort term)))
  "[Cyc] Table used to dispatch tt methods as actual calls on tt executables.")
(def-gt-state-var *tt-methods* (mapcar #'car *tt-dispatch-table*)
  "[Cyc] Candidate methods for gt modules.")
(def-gt-state-var *transfers-through-modules* nil 
  "[Cyc] All cyc modules defined using the general transfers-through module.")
(def-gt-state-var *tt-parameters* (append (list :predicate
                                                :conduit-pred
                                                :tt-index-arg
                                                :tt-gather-arg
                                                :gt-index-arg
                                                :gt-gather-arg
                                                :mt)
                                          *tt-methods*)
  "[Cyc] Candidate parameters for tt methods.")
(def-gt-state-var *tt-pred* nil
  "[Cyc] Transitive predicate used for current tt query.")
(def-gt-state-var *tt-index* nil
  "[Cyc] Arg used as initial index for current tt query.")
(def-gt-state-var *tt-gather* nil
  "[Cyc] Arg used as initial gather (e.g., search target) for current tt query.")
(def-gt-state-var *tt-index-arg* 1
  "[Cyc] Indexing arg position used for current tt query.")
(def-gt-state-var *tt-gather-arg* 2
  "[Cyc] Gathering arg position used for current tt query.")
(def-gt-state-var *tt-truth* :true
  "[Cyc] Truth relevant to the current gt query [:true :false].")
(def-gt-state-var *tt-step-fn* nil
  "[Cyc] Fn used to step from one node to another during tt search.")
(def-gt-state-var *tt-transitive-conduit?* nil
  "[Cyc] Is conduit-arg transitive in current tt search?")
(def-gt-state-var *gt-transitive-via-arg-active?* t
  "[Cyc] Is the gt-transitive-via-arg module active?")
(def-gt-state-var *gt-within-transitive-via-arg?* nil
  "[Cyc] Currently within scope of a gt-transitive-via-arg module?")
