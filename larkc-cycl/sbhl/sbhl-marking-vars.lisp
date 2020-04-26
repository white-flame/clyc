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


(defparameter *debug-sbhl-marking-spaces?* nil)

(defun* average-genl-cardinality (&optional module) (:inline t)
  (declare (ignore module))
  100)

(defun* average-spec-cardinality (&optional module) (:inline t)
  (declare (ignore module))
  3000)

(defun get-sbhl-marking-space ()
  (if (resourcing-sbhl-marking-spaces-p)
      (get-sbhl-resourced-marking-space)
      (instantiate-sbhl-marking-space)))

(defun free-sbhl-marking-space (space)
  (when (resourcing-sbhl-marking-spaces-p)
    (update-sbhl-resourced-spaces space)))

(defun instantiate-sbhl-marking-space ()
  "[Cyc] Creates an SBHL-SPACE-P."
  (cond
   ((sbhl-forward-search-p)
    (make-hash-table :size (average-genl-cardinality)
                     :test #'equal))
   ((sbhl-backward-search-p)
    (make-hash-table :size (average-spec-cardinality)
                     :test #'equal))
   (t (make-hash-table :size (average-genl-cardinality)
                       :test #'equal))))

(defparameter *resourcing-sbhl-marking-spaces-p* nil
  "[Cyc] Flag parameter to determine whether to resource SBHL spaces.")
(defparameter *resourced-sbhl-marking-space-limit* nil
  "[Cyc] The maximal number of spaces to resource.")
(defparameter *resourced-sbhl-marking-spaces* nil
  "[Cyc] The store of spaces that are being resourced.")
(deflexical *default-resourced-sbhl-space-limit* 4
  "[Cyc] The default number of spaces to resource.")

(defun* resourcing-sbhl-marking-spaces-p () (:inline t)
  "[Cyc] Determines whether to resource SBHL marking spaces."
  *resourcing-sbhl-marking-spaces-p*)

(defun* possibly-new-marking-resource (resourcing-p) (:inline t)
  (and resourcing-p
       *resourced-sbhl-marking-spaces*))

(defparameter *within-new-sbhl-marking-space-resource* nil)

(defun new-sbhl-marking-space-resource (&optional num)
  "[Cyc] Returns a resource of SBHL marking spaces."
  (let ((default-num (or num 0))
        (result nil))
    (let ((*within-new-sbhl-marking-space-resource* t))
      (dotimes (i default-num)
        (push (instantiate-sbhl-marking-space) result)))
    (nreverse result)))

(defun determine-marking-space-limit (marking-resource)
  (let ((length (length marking-resource)))
    (if (eq length 0)
        *default-resourced-sbhl-space-limit*
        length)))

(defun get-sbhl-resourced-marking-space ()
  "[Cyc] Gets an available resourced SBHL-MARKING-SPACE, if one exists. If not, it creates one."
  (let ((result (first *resourced-sbhl-marking-spaces*)))
    (if result
        (progn
          (when *debug-sbhl-marking-spaces?*
            (unless (hash-table-empty-p result)
              (warn "Dirty resourced marking space being reused ~a!" result)))
          (pop *resourced-sbhl-marking-spaces*))
        (setf result (instantiate-sbhl-marking-space)))
    result))

(defun update-sbhl-resourced-spaces (space)
  "[Cyc] Modifier. Pushes SPACE onto resourced spaces, unless they are over the limit."
  (if (length>= *resourced-sbhl-marking-spaces* *resourced-sbhl-marking-space-limit*)
      (when *debug-sbhl-marking-spaces?*
        (warn "Need more than the limit of ~a marking spaces."
              *resourced-sbhl-marking-space-limit*))
      (progn 
        (clear-sbhl-space space)
        (push space *resourced-sbhl-marking-spaces*))))

(defparameter *sbhl-space* nil
  "[Cyc] The current context for marking.")

(defparameter *sbhl-gather-space* nil
  "[Cyc] The context used for gathering valid results in 3 part searches.")

(defparameter *sbhl-disjoins-space* nil
  "[Cyc] A context name used for marking disjoins.")

(defparameter *sbhl-space-0* nil
  "[Cyc] A variable to use for SBHL spaces.")
(defparameter *sbhl-space-1* nil
  "[Cyc] A variable to use for SBHL spaces.")
(defparameter *sbhl-space-2* nil
  "[Cyc] A variable to use for SBHL spaces.")
(defparameter *sbhl-space-3* nil
  "[Cyc] A variable to use for SBHL spaces.")
(defparameter *sbhl-space-4* nil
  "[Cyc] A variable to use for SBHL spaces.")
(defparameter *sbhl-space-5* nil
  "[Cyc] A variable to use for SBHL spaces.")

(defun  sbhl-new-space-source ()
  "[Cyc] Determine whether and where to get new spaces from."
  (cond
   ((sbhl-suspend-new-spaces?) :old)
   ((resourcing-sbhl-marking-spaces-p) :resource)
   (t :new)))

(defun sbhl-get-new-space (source)
  "[Cyc] Get a new SBHL marking space from SOURCE."
  (case source
    (:resource (get-sbhl-resourced-marking-space))
    (otherwise (instantiate-sbhl-marking-space))))

(defparameter *sbhl-suspend-new-spaces?* nil
  "[CYc] Toggle variable to suspend creation of new spaces during search.")

(defun* sbhl-suspend-new-spaces? () (:inline t)
  "[Cyc] Whether to suspend creation of new spaces during search."
  *sbhl-suspend-new-spaces?*)

(defparameter *sbhl-goal-space* nil
  "[Cyc] The space var used when goal nodes are premarked.")

(defun* get-sbhl-goal-space () (:inline t)
  *sbhl-goal-space*)

(defparameter *sbhl-target-space* nil
  "[Cyc] The space used when we want independent searches to make cross references.")

(defparameter *sbhl-target-gather-space* nil
  "[CYc] The space used when we want independent searches to make cross references.")

(defparameter *sbhl-terminating-marking-space* nil
  "[Cyc] The space var used to indicate where the marking of result nodes occurs.")

(defparameter *sbhl-apply-marking-space* nil)

(deflexical *sbhl-genl-preds-marking* (list #$genlPreds)
  "[Cyc] The marking for nodes only accessed in predicate mode during search.")

(deflexical *sbhl-genl-inverse-marking* (list #$genlInverse)
  "[Cyc] The marking for nodes only accessed in argument-flipped mode during search.")

(deflexical *sbhl-genl-preds-and-genl-inverse-marking* (list #$genlPreds
                                                             #$genlInverse)
  "[Cyc] The marking for nodes accessed in both predicate argument-flipped modes and during search.")

(deflexical *sbhl-genl-inverse-and-genl-preds-marking* (list #$genlInverse
                                                             #$genlPreds)
  "[Cyc] The marking for nodes accessed in both predicate argument-flipped modes and during search.")

(defun* genl-preds-marking-p (marking)  (:inline t)
  "[Cyc] Whether MARKING indicates access only in predicate mode."
  (eq marking *sbhl-genl-preds-marking*))

(defun* genl-inverse-marking-p (marking) (:inline t)
  "[Cyc] Whether MARKING indicates access only in argument-flipped mode."
  (eq marking *sbhl-genl-inverse-marking*))

(defun* genl-preds-and-genl-inverse-marking-p (marking) (:inline t)
  "[Cyc] Whether MARKING indicates access in both predicate and argument-flipped mode."
  (or (eq marking *sbhl-genl-preds-and-genl-inverse-marking*)
      (eq marking *sbhl-genl-inverse-and-genl-preds-marking*)))

(defparameter *sbhl-marking-generation* nil
  "[Cyc] The current marking generation.")
(defparameter *sbhl-suspend-new-spaces-during-mapping?* nil
  "[Cyc] Toggle parameter for whether to suspend the creation of new search spaces during mapping.")

(defun sbhl-suspend-new-spaces-during-mapping? ()
  "[Cyc] Whether to suspend the creation of new search spaces during mapping."
  *sbhl-suspend-new-spaces-during-mapping?*)

(defparameter *sbhl-mapping-marking-space* nil
  "[Cyc] Space nameholder for marking during mapping.")

(defparameter *sbhl-mapping-gather-marking-space* nil
  "[Cyc] Space nameholder for marking during mapping.")

(defparameter *sbhl-temporality-search-checks-nodes-in-target-space-p* nil
  "[Cyc] Parameter determining whether a time search checks markings in target space (for searching marked nodes).")

;; TODO - this value string from the java probably was a docstring in the lisp. Setting it to NIL here instead, and moving the string value to the docstring position.
(defparameter *sbhl-table* nil
  "[Cyc] Table used for public marking interface.")

(defparameter *sbhl-goal-table* nil
  "[Cyc] The table var used when goal nodes are premarked.")
