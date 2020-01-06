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

(deflexical *ghl-search-property-defaults* '((:direction . :forward)
                                             (:tv . :true-def)
                                             (:order . :breadth-first)))

(defun ghl-search-property-default (property)
  (cdr (assoc property *ghl-search-property-defaults* :test #'eq)))

(defstruct ghl-search
  graphl-search
  predicates
  tv)

(defun new-ghl-search (plist)
  (let ((ghl-search (make-ghl-search))
        (graphl-search (new-graphl-search nil)))
    (set-ghl-graphl-search ghl-search graphl-search)
    (set-ghl-search-properties ghl-search plist)
    ghl-search))

(defun destroy-ghl-search (graph-search)
  (destroy-graphl-search (ghl-search-graphl-search graph-search))
  (setf (ghl-search-graphl-search graph-search) :free)
  (setf (ghl-search-predicates graph-search) :free)
  (setf (ghl-search-tv graph-search) :free))

;; Renamed getters
(symbol-mapping ghl-graphl-search ghl-search-graphl-search
                ghl-relevant-predicates ghl-search-predicates
                ghl-tv ghl-search-tv)

;; Getters that read through the graphl-search reference

(defun ghl-result (search)
  (graphl-result (ghl-graphl-search search)))

(defun ghl-space (search)
  (graphl-space (ghl-graphl-search search)))

(defun ghl-direction (search)
  (graphl-direction (ghl-graphl-search search)))

(defun ghl-compute-justify? (search)
  (graphl-compute-justify? (ghl-graphl-search search)))

(defun ghl-goal (search)
  (graphl-search-goal (ghl-graphl-search search)))

(defun ghl-goal-fn (search)
  (graphl-search-goal-fn (ghl-graphl-search search)))

(defun ghl-depth-first-search-p (search)
  (graphl-depth-first-search-p (ghl-graphl-search search)))

(defun set-ghl-search-properties (search plist)
  (dolist (cons *ghl-search-property-defaults*)
    (let ((prop (car cons))
          (value (cdr cons)))
      (when value
        (set-ghl-search-property search prop value))))
  (do-plist (prop val plist)
    (set-ghl-search-property search prop val)))

(defun set-ghl-search-property (search property value)
  (unless value
    (setf value (ghl-search-property-default property)))
  (let ((graphl-search (ghl-graphl-search search)))
    (case property
      (:predicates (set-ghl-search-predicates search value))
      (:direction (set-graphl-search-direction graphl-search value))
      (:tv (set-ghl-search-tv search value))
      (:type (set-graphl-search-type graphl-search value))
      (:order (set-graphl-search-order graphl-search value))
      (:cutoff (missing-larkc 31965))
      (:marking (set-graphl-search-marking graphl-search value))
      (:marking-space (set-graphl-search-marking-space graphl-search value))
      (:goal (set-graphl-search-goal graphl-search value))
      (:goal-fn (missing-larkc 31966))
      (:goal-space (missing-larkc 31967))
      (:satisfy-fn (missing-larkc 31970))
      (:map-fn (missing-larkc 31968))
      (:justify? (set-graphl-search-justify? graphl-search value))
      (:add-to-result? (missing-larkc 31964)))))

;; Setters, called from set-ghl-search-property. TODO - are these use elsewhere?  Could wrap them into the above to shorten this up.

;; TODO DESIGN - this also smells like structure inheritance.  Rethink this API.

(defun set-ghl-graphl-search (search graphl-search)
  (setf (ghl-search-graphl-search search) graphl-search))

(defun set-ghl-search-predicates (search predicates)
  (setf (ghl-search-predicates search) predicates))

(defun set-ghl-search-tv (search tv)
  (setf (ghl-search-tv search) tv))

(defun set-ghl-search-result (search result)
  (set-graphl-search-result (ghl-graphl-search search) result))

(defun set-ghl-goal-found-p (search found-p)
  (set-graphl-search-goal-found-p (ghl-graphl-search search) found-p))

(defun ghl-set-result (search result)
  (set-ghl-search-result search result))

(defun ghl-add-to-result (search addition &optional (test #'eq))
  (graphl-add-to-result (ghl-graphl-search search) addition test))

(defun ghl-forward-direction-p (direction)
  (graphl-forward-direction-p direction))
                
(defun ghl-direction-for-predicate-relation (pred)
  "[Cyc] Direction to search when determining the predicate relation (PRED A B).
:FORWARD corresponds to searching from A to B, e.g. (genls Dog Thing)
:BACKWARD corresponds to searching from B to A, e.g. (geoSubRegion USA Austin)"
  (if (eq 1 (fan-out-arg pred))
      :forward
      :backward))

(defparameter *sksi-gt-search-pred* nil)
(defparameter *ghl-uses-spec-preds-p* t)

(defun ghl-uses-spec-preds-p ()
  *ghl-uses-spec-preds-p*)

(defparameter *gt-args-swapped-p* t)

(defun gt-args-swapped-p ()
  *gt-args-swapped-p*)

(defparameter *ghl-trace-level* 1)
