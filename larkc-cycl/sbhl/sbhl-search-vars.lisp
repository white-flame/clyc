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


(defparameter *sbhl-search-types* (list :closure
                                        :boolean
                                        :what-mts)
  "[Cyc] Different types of search, leading to different behaviors.")
               
(defparameter *sbhl-search-type* nil
  "[Cyc] The current type of search. Governs how each search node is used during search.")

(defun* get-sbhl-search-type () (:inline t)
  *sbhl-search-type*)

(defun* sbhl-boolean-search-p () (:inline t)
  "[Cyc] Whether search type indicates boolean search."
  (eq (get-sbhl-search-type) :boolean))

(defparameter *sbhl-justification-search-p* nil
  "[Cyc] Does the current boolean search show the path that allowed success to be concluded?")

(defun* sbhl-justification-search-p () (:inline t)
  "[Cyc] Whether current search is one that gathers justifications."
  *sbhl-justification-search-p*)

(defparameter *sbhl-search-behavior* nil
  "[Cyc] The current search's entry function.")

(defun* get-sbhl-search-behavior () (:inline t)
  *sbhl-search-behavior*)


(defparameter *sbhl-justification-behavior* :old
  "[Cyc] The current behavior used for assembling sbhl-justifications, and determining how their returns will appear.")

(defun* get-sbhl-just-behavior () (:inline t)
  *sbhl-justification-behavior*)

(defparameter *sbhl-justification-defaulted-old* nil
  "[Cyc] Parameter used to indicate when justification was coerced and requires old justification behavior.")

(defparameter *sbhl-justification-assembled-p* nil
  "[Cyc] Has the justification path already been assembled?")

(defun* sbhl-justification-assembled-p () (:inline t)
  *sbhl-justification-assembled-p*)

(defparameter *sbhl-unmarking-search-p* nil
  "[Cyc] Is current search an unmarking search?")

(defun* sbhl-unmarking-search-p () (:inline t)
  *sbhl-unmarking-search-p*)

(defparameter *sbhl-search-module* nil
  "[Cyc] The module initiating the current search.")

(defun* get-sbhl-search-module () (:inline t)
  (let ((val *sbhl-search-module*))
    (unless val
      (warn "GET-SBHL-SEARCH-MODULE: *SBHL-SEARCH-MODULE* is unexpectedly null."))
    val))

(defparameter *sbhl-search-module-type* nil
  "[Cyc] The module type of the current search module.")

(defun* get-sbhl-search-module-type () (:inline t)
  *sbhl-search-module-type*)

(defparameter *sbhl-add-node-to-result-test* nil
  "[Cyc] The function used to govern adding nodes to the result during search.")

(defun* get-sbhl-search-add-node-test () (:inline t)
  "[Cyc] The function applied to a node's marking before pushing correctly marked nodes onto result. Determined by current *SBHL-SEARCH-MODULE*."
  *sbhl-add-node-to-result-test*)

(defparameter *sbhl-add-unmarked-node-to-result-test* nil
  "[Cyc] The function used to govern adding nodes to the result of an unmarking search.")

(defun* get-sbhl-search-add-unmarked-node-test () (:inline t)
  "[Cyc] The function applied to a node's marking before pushing unmarked nodes onto result. Determined by current *SBHL-SEARCH-MODULE*."
  *sbhl-add-unmarked-node-to-result-test*)

(defparameter *genl-inverse-mode-p* nil
  "[Cyc] Whether current search state has argument order flipped from search's initial order.")

(defun* genl-inverse-mode-p () (:inline t)
  *genl-inverse-mode-p*)

(defun* not-genl-inverse-mode-p () (:inline t)
  "[Cyc] Opposite of *sbhl-genl-inverse-mode-p*"
  (not *genl-inverse-mode-p*))

(defun sbhl-module-flips-inverse-mode-p (&optional (sbhl-module (get-sbhl-module)))
  (let ((pred (get-sbhl-link-pred sbhl-module)))
    (or (eq pred #$genlInverse)
        (eq pred #$negationInverse))))

(defun flip-genl-inverse-mode? (&optional (sbhl-module (get-sbhl-module))
                                  (sbhl-search-module (get-sbhl-search-module)))
  (and (sbhl-module-indicates-predicate-search-p sbhl-search-module)
       (sbhl-module-flips-inverse-mode-p sbhl-module)))

;; TODO - not defconstant?
(deflexical *sbhl-forward-search-direction* :forward
  "[Cyc] The keyword specifying forward search")

(defun* sbhl-forward-search-direction-p (direction) (:inline t)
  "[Cyc] Whether SBHL search direction is forward"
  (eq direction *sbhl-forward-search-direction*))

(defun* get-sbhl-forward-search-direction () (:inline t)
  *sbhl-forward-search-direction*)

;; TODO - not defconstant?
(deflexical *sbhl-backward-search-direction* :backward
  "[Cyc] The keyword specifying backward search.")

(defun* sbhl-backward-search-direction-p (direction) (:inline t)
  "[Cyc] Whether SBHL search direction is backward"
  (eq direction *sbhl-backward-search-direction*))

(defun* get-sbhl-backward-search-direction () (:inline t)
  *sbhl-backward-search-direction*)

(defparameter *sbhl-search-direction* nil
  "[Cyc] The direction of current search")

(defun* get-sbhl-search-direction () (:inline t)
  *sbhl-search-direction*)

(defun* sbhl-forward-search-p () (:inline t)
  "[Cyc] Whether the *SBHL-SEARCH-DIRECTION* is forward."
  (sbhl-forward-search-direction-p *sbhl-search-direction*))

(defun* sbhl-backward-search-p () (:inline t)
  "[Cyc] Whether the *SBHL-SEARCH-DIRECTION* is backward."
  (sbhl-backward-search-direction-p *sbhl-search-direction*))

;; TODO - deprecate
(defparameter *sbhl-index-arg* nil
  "[Cyc] Obsolete.")

(defparameter *sbhl-search-index-arg* nil
  "[Cyc] The index arg for current search.")

(defparameter *sbhl-map-function* nil
  "[Cyc] The recursive winding function of current search.")

(defun* get-sbhl-map-function () (:inline t)
  *sbhl-map-function*)

(defparameter *sbhl-unwind-function* nil
  "[Cyc] The function applied during recursive unwind.")

(defun* get-sbhl-unwind-function () (:inline t)
   *sbhl-unwind-function*)

(defparameter *sbhl-apply-unwind-function-p* nil
  "[Cyc] Toggle determining whether to apply *SBHL-UNWIND-FUNCTION*.")

(defun* sbhl-apply-unwind-function-p () (:inline t)
  *sbhl-apply-unwind-function-p*)

(defun* sbhl-toggle-unwind-function-on () (:inline t)
  "[Cyc] Sets to T *SBHL-APPLY-UNWIND-FUNCTION-P*."
  (setf *sbhl-apply-unwind-function-p* t))

(defparameter *sbhl-search-parent-marking* nil
  "[Cyc] The current node's parent marking.")
                        
(defun* set-sbhl-search-parent-marking (marking) (:inline t)
  "[Cyc] Sets *SBHL-SEARCH-PARENT-MARKING* to MARKING."
  (setf *sbhl-search-parent-marking* marking))

(defparameter *sbhl-nodes-previous-marking* nil
  "[Cyc] Previous marking of node.")

(defparameter *sbhl-finished?* nil
  "[Cyc] Stores whether sbhl search is finished and if so, how.")

(defun* sbhl-finished-with-goal () (:inline t)
  "[Cyc] Sets *SBHL-FINISHED?* to :GOAL. Used to specify that an SBHL search terminated upon reaching its goal condition."
  (setf *sbhl-finished?* :goal))

(defparameter *sbhl-stop-search-path?* nil
  "[Cyc] Stop mark and sweep from going further.")

(defun* sbhl-stop-search-path-p () (:inline t)
  "[Cyc] Whether to stop the current search path."
  *sbhl-stop-search-path?*)
  
(defun* sbhl-stop-search-path () (:inline t)
  (setf *sbhl-stop-search-path?* t))

(defparameter *sbhl-target-node* nil
  "[Cyc] The target node for current search.")

(defparameter *sbhl-goal-node* nil
  "[Cyc] The goal node for current search.")

(defun* sbhl-goal-node-p (node) (:inline t)
  "[Cyc] Whether NODE is equal to *sbhl-goal-node*"
  (eq node *sbhl-goal-node*))

(defun* get-sbhl-goal-node () (:inline t)
  *sbhl-goal-node*)

(defparameter *sbhl-goal-nodes* nil
  "[Cyc] The goal nodes for current search for any goal.")

(defun* get-sbhl-goal-nodes () (:inline t)
  "[Cyc] The current goal nodes."
  *sbhl-goal-nodes*)

(defparameter *sbhl-isa?-goal* nil
  "[Cyc] The goal node for current search.")

(defparameter *sbhl-result* nil
  "[Cyc] The result in search.")

(defparameter *sbhl-justification-result* nil
  "[Cyc] The result in justification searches.")

(defparameter *sbhl-consider-node-fn* nil
  "[Cyc] Function applied to each node during closure searches.")

(defun* get-sbhl-consider-node-fn () (:inline t)
  *sbhl-consider-node-fn*)

(defparameter *sbhl-compose-fn* nil
  "[Cyc] Function applied to each node during closure searches.")

(defun* get-sbhl-compose-fn () (:inline t)
  *sbhl-compose-fn*)

(defparameter *sbhl-combine-fn* #'nconc
  "[Cyc] Function applied to each node during closure searches.")

(defparameter *sbhl-map-test-fn* nil
  "[Cyc] Function applied to each node during closure searches.")

(defparameter *sbhl-accessed-gather-nodes* nil
  "[Cyc] The nodes upon which a gather has already been performed.")

(defparameter *sbhl-dependents-cut-node* nil
  "[Cyc] The source node in dependent searches.")

(defparameter *sbhl-access-arg* nil
  "[Cyc] The arg accessed in current SBHL mapping. Used for checking validity of terms within an arg position of a predicate.")

(defparameter *sbhl-referent* nil
  "[Cyc] Variable that can be used for comparison during SBHL searched.")

(defparameter *sbhl-current-leaf-queue* nil
  "[Cyc] Current search path during sample leaves search. Path is saved if successful.")

(defparameter *sbhl-sample-leaf-queues* nil
  "[Cyc] The list of queues guiding sample leaf searches.")

(defun* sbhl-leaf-sample-search-p () (:inline t)
  "[Cyc] Uses *sbhl-sample-leaf-queues* to answer whether current search is one that uses the leaf queues."
  *sbhl-sample-leaf-queues*)

(defparameter *sbhl-extremal-test-fn* nil
  "[Cyc] Function used to test extremal nodes.")

(defparameter *sbhl-check-for-goal-marking-p* nil
  "[Cyc] Whether to check whether a node is marked as :goal before checking other marking in search path termination.")

(defun* sbhl-check-for-goal-marking-p () (:inline t)
  *sbhl-check-for-goal-marking-p*)

(defparameter *sbhl-precomputed-goal-space* nil
  "[Cyc] The space bound for marking successful paths in repeated boolean searches.")

(defparameter *infer-nat-sbhl?* t
  "[Cyc] Permit isa module to infer isa/genls of reified nats from result-type of functor?")

;; TODO - deprecate
(defparameter *sbhl-mapping-pred* nil
  "[Cyc] Obsolete.")

;; TODO - deprecate
(defparameter *maximize-sbhl-result?* nil
  "[Cyc] Obsolete.")

;; TODO - deprecate
(defparameter *minimize-sbhl-result?* t
  "Obsolete.")

(defparameter *sbhl-search-truth* nil
  "[Cyc] Used to determine whether the search is true or false.")

(defun* sbhl-true-search-p () (:inline t)
  "[Cyc] Whether *SBHL-TV* generalizes to #$True-JustificationTruth. See SBHL-TV-GENERALIZES-TO-GENERAL-TV?"
  (sbhl-true-tv-p *sbhl-search-truth*))

(deflexical *sbhl-search-truth-values* (list #$MonotonicallyTrue
                                             #$DefaultTrue
                                             #$MonotonicallyFalse
                                             #$DefaultFalse
                                             #$True-JustificationTruth
                                             #$False-JustificationTruth
                                             #$ArbitraryTruth-JustificationTruth)
  "[Cyc] Valid SBHL truth values for searching.")

(defun sbhl-search-truth-value-p (truth)
  "[Cyc] Whether TRUTH is a member of *SBHL-SEARCH-TRUTH-VALUES*."
  (member? truth *sbhl-search-truth-values*))

(deflexical *sbhl-true-tv* #$True-JustificationTruth
  "[Cyc] The encompassing true truth for searches.")

(deflexical *sbhl-false-tv* #$False-JustificationTruth
  "[Cyc] The encompassing false truth for searches.")

(defun* get-sbhl-true-tv () (:inline t)
  "[Cyc] The encompassing true truth."
  *sbhl-true-tv*)

(defun* get-sbhl-false-tv () (:inline t)
  "[Cyc] The encompassing false truth."
  *sbhl-false-tv*)

(defparameter *sbhl-tv* #$True-JustificationTruth
  "[Cyc] Search parameter.")

(defun* get-sbhl-tv () (:inline t)
  "[Cyc] The truth relevance for the current search."
  *sbhl-tv*)

(defparameter *relevant-sbhl-tv-function* nil
  "[Cyc] The truth relevance function for the current search.")

(defun sbhl-tv-generalizes-to-general-tv? (tv general-tv)
  "[Cyc] Whether TV is subsumed by GENERAL-TV."
  (or (eq tv general-tv)
      ;; TODO READER - #$ returns an expression to be evaluated, so can't use it in CASE!
      (case tv
        (#$MonotonicallyTrue
         (or (eq general-tv #$DefaultTrue)
             (eq general-tv #$True-JustificationTruth)
             (eq general-tv #$ArbitraryTruth-JustifactionTruth)))
        
        (#$DefaultTrue
         (or (eq general-tv #$True-JustificationTruth)
             (eq general-tv #$ArbitraryTruth-JustificationTruth)))
        
        (#$True-JustificationTruth
         (eq general-tv #$ArbitraryTruth-JustificationTruth))
        
        (#$MonotonicallyFalse
         (or (eq general-tv #$DefaultFalse)
             (eq general-tv #$False-JustificationTruth)
             (eq general-tv #$JustifactionTruth)))
        
        (#$DefaultFalse
         (or (eq general-tv #$False-JustificationTruth)
             (eq general-tv #$ArbitraryTruth-JustificationTruth)))
        
        (#$False-JustificationTruth
         (eq general-tv #$ArbitraryTruth-JustificationTruth))
        
        (#$Unknown-JustificationTruth
         (eq general-tv #$ArbitraryTruth-JustificationTruth)))))
     
(defun* relevant-sbhl-tv-is-general-tv (tv) (:inline t)
  "[Cyc] Whether TV generalizes to *sbhl-tv*"
  (sbhl-tv-generalizes-to-general-tv? tv *sbhl-tv*))

(defun relevant-sbhl-tv? (tv)
  "[Cyc] Applies *RELEVANT-SBHL-TV-FUNCTION* to TV, or if no function is bound as default applies RELEVANT-SBHL-TV-IS-GENERAL-TV."
  (if *relevant-sbhl-tv-function*
      (funcall *relevant-sbhl-tv-function* tv)
      (relevant-sbhl-tv-is-general-tv tv)))

(defun* sbhl-true-tv-p (tv) (:inline t)
  "[Cyc] Whether TV generalizes to #$True-JustificationTruth."
  (sbhl-tv-generalizes-to-general-tv? tv #$True-JustificationTruth))

(defun* sbhl-false-tv-p (tv) (:inline t)
  "[Cyc] Whether TV generalizes to #$False-JustificationTruth."
  (sbhl-tv-generalizes-to-general-tv? tv #$False-JustificationTruth))

(defun sbhl-opposite-tv (tv)
  "[Cyc] The truth value with same strength and opposite truth from TV."
  ;; TODO READER - #$ returns an expression to be evaluated, so can't use it in CASE!
  (case tv
    (#$MonotonicallyTrue #$MonotonicallyFalse)
    (#$DefaultTrue #$DefaultFalse)
    (#$MonotonicallyFalse #$MonotonicallyTrue)
    (#$DefaultFalse #$DefaultTrue)
    (#$True-JustificationTruth #$False-JustificationTruth)
    (#$False-JustificationTruth #$True-JustificationTruth)
    (#$ArbitraryTruth-JustificationTruth #$ArbitraryTruth-JustificationTruth)
    (otherwise (sbhl-check-type tv sbhl-search-truth-value-p)
               nil)))

(defun sbhl-true-tv (tv)
  "[Cyc] The true truth value with same strength as TV."
  (cond 
    ((sbhl-true-tv-p tv) tv)
    ((sbhl-false-tv-p tv) (sbhl-opposite-tv tv))
    (t (sbhl-error 1 "Expected true or false sbhl truth value. tv: ~a~%" tv))))

(defun sbhl-false-tv (tv)
  "[Cyc] The false truth value with same strength as TV."
  (cond 
    ((sbhl-false-tv-p tv) tv)
    ((sbhl-true-tv-p tv) (sbhl-opposite-tv tv))
    (t (sbhl-error 1 "Expected true or false sbhl truth value. tv: ~a~%" tv))))

(defun* sbhl-search-true-tv () (:inline t)
  "[Cyc] The true truth value with same strength as *SBHL-TV*."
  (sbhl-true-tv *sbhl-tv*))
  
(defun* sbhl-search-false-tv () (:inline t)
  "[Cyc] The false truth value with same strength as *SBHL-TV*."
  (sbhl-false-tv *sbhl-tv*))

(defun sbhl-translate-to-old-tv (tv)
  "[Cyc] The keyword associated with TV, either :TRUE or :FALSE."
  (cond
    ((sbhl-true-tv-p tv) :true)
    ((sbhl-false-tv-p tv) :false)
    (t (sbhl-error 1 "Unknown truth value for translation: ~a" tv))))

(defun sbhl-possibly-translate-tv (tv)
  (if (or (eq tv :true)
          (eq tv :false))
      tv
      (sbhl-translate-to-old-tv tv)))
