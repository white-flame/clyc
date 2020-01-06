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

;; TODO DESIGN - it seems like modules used to be dictionaries in the past, as comments make reference to that.  Since the get/set property functions cherry pick those slots listed in the struct, and defer the rest to a hashtable, the whole thing was probably in a hashtable and the comments are out of date. However, should still verify that there are no hashtable-based functions around.

(defstruct (sbhl-module (:conc-name "SBHL-MOD-"))
  link-pred
  accessible-link-preds
  graph
  link-style
  index-arg
  module-type
  type-test
  path-terminating-mark-fn
  marking-fn
  unmarking-fn
  var-bindings
  misc-properties)

(defun new-sbhl-module (pred)
  (let ((module (make-sbhl-module)))
    (setf (sbhl-mod-link-pred module) pred)
    (setf (sbhl-mod-misc-properties module) (make-hash-table :test #'eq))))

(defun set-sbhl-module-property (module property value)
  (sbhl-check-type property sbhl-module-property-p)
  (sbhl-check-type module sbhl-module-p)
  (case property
    (:link-pred (setf (sbhl-mod-link-pred module) value))
    (:accessible-link-preds (setf (sbhl-mod-accessible-link-preds module) value))
    (:graph (setf (sbhl-mod-graph module) value))
    (:link-style (setf (sbhl-mod-link-style module) value))
    (:index-arg (setf (sbhl-mod-index-arg module) value))
    (:module-type (setf (sbhl-mod-module-type module) value))
    (:type-test (setf (sbhl-mod-type-test module) value))
    ;; TODO DESIGN - the question mark here is inconsistent with the slot name, but is present throughout the keyword slot name interface.  Change this to the same name as the slot.
    (:path-terminating-mark?-fn (setf (sbhl-mod-path-terminating-mark-fn module) value))
    (:marking-fn (setf (sbhl-mod-marking-fn module) value))
    (:unmarking-fn (setf (sbhl-mod-unmarking-fn module) value))
    (otherwise (setf (gethash property (sbhl-mod-misc-properties module)) value))))

(defun get-sbhl-module-property (module property)
  (sbhl-check-type module sbhl-module-p)
  (case property
    (:link-pred (sbhl-mod-link-pred module))
    (:accessible-link-preds (sbhl-mod-accessible-link-preds module))
    (:graph (sbhl-mod-graph module))
    (:link-style (sbhl-mod-link-style module))
    (:index-arg (sbhl-mod-index-arg module))
    (:module-type (sbhl-mod-module-type module))
    (:type-test (sbhl-mod-type-test module))
    (:path-terminating-mark?-fn (sbhl-mod-path-terminating-mark-fn module))
    (:marking-fn (sbhl-mod-marking-fn module))
    (:unmarking-fn (sbhl-mod-unmarking-fn module))
    (otherwise (gethash property (sbhl-mod-misc-properties module)))))

;; TODO - the java had broken comments that failed to describe the actual slots

(symbol-mapping get-sbhl-module-link-pred sbhl-mod-link-pred
                get-sbhl-module-accessible-link-preds sbhl-mod-accessible-link-preds
                get-sbhl-module-graph sbhl-mod-graph
                get-sbhl-module-link-style sbhl-mod-link-style
                get-sbhl-module-index-arg sbhl-mod-index-arg
                get-sbhl-module-module-type sbhl-mod-module-type
                get-sbhl-module-type-test sbhl-mod-type-test
                ;; TODO - this is a slight inconsistency in naming
                get-sbhl-module-terminating-mark sbhl-mod-terminating-mark-fn
                get-sbhl-module-marking-fn sbhl-mod-marking-fn
                get-sbhl-madule-unmarking-fn sbhl-mod-unmarking-fn)

(defconstant *sbhl-module-key-test* #'eq)
(defglobal *sbhl-modules* (make-hash-table :test *sbhl-module-key-test*)
  "[Cyc] Dictionary of SBHL modules, built up by module declaration.")

(defun-inline sbhl-module-object-p (object)
  "[Cyc] Whether OBJECT is a DICTIONARY-P."
  ;; TODO - hashtable comment
  (sbhl-module-p object))

(defun reset-sbhl-modules ()
  (setf *sbhl-modules* (make-hash-table :test *sbhl-module-key-test*)))

(defun-inline get-sbhl-modules ()
  "[Cyc] Return a hashtable of the defined SBHL modules, which each correspond directly to a link table."
  *sbhl-modules*)

(defun add-sbhl-module (predicate module)
  "[Cyc] Enters MODULE into *SBHL-MODULES*. Assumes *SBHL-MODULES* is a hashtable. Checks that MODULE-KEY is a fort-p, and MODULE_DATA is a hashtable-p."
  ;; TODO - hashtable comment
  (sbhl-check-type predicate sbhl-predicate-object-p)
  (sbhl-check-type module sbhl-module-object-p)
  (setf (gethash predicate *sbhl-modules*) module))

(defun-inline get-sbhl-predicates-int ()
  "[Cyc] Returns a list of the defined sbhl predicates."
  (hash-table-keys (get-sbhl-modules)))

(defun-inline get-sbhl-module-list ()
  "[Cyc] Return what the SBHL module structures that the predicates point to."
  ;; TODO - bad comment grammar
  (hash-table-values (get-sbhl-modules)))

(defun-inline sbhl-predicate-object-p (object)
  "[Cyc] Type test for candidate sbhl-predicates."
  (fort-p object))

(deflexical *sbhl-module-types* '(:simple-reflexive
                                  :simple-non-reflexive
                                  :transfers-through
                                  :disjoins
                                  :time)
  "[Cyc] Roles that SBHL modules play in the grand SBHL scheme.")

(defun-inline sbhl-simple-reflexive-module-type-p (module-type)
  "[Cyc] Returns whether MODULE-TYPE is of the simple transitive and reflexive variety."
  (eq module-type :simple-reflexive))

(defun-inline sbhl-simple-non-reflexive-module-type-p (module-type)
  "[Cyc] Returns whether MODULE-TYPE is of the simple transitive but irreflexive variety."
  (eq module-type :simple-non-reflexive))

(defun-inline sbhl-transfers-through-module-type-p (module-type)
  "[Cyc] Returns whether MODULE-TYPE is the keyword for transfers-through sbhl modules."
  (eq module-type :transfers-through))

(defun-inline sbhl-disjoins-module-type-p (module-type)
  "[Cyc] Returns whether MODULE-TYPE is the keyword for disjoins sbhl modules."
  (eq module-type :disjoines))

(defun-inline sbhl-time-module-type-p (module-type)
  "[Cyc] Returns whether MODULE-TYPE is the keyword for sbhl time modules."
  (eq module-type :time))

(defun-inline sbhl-transitive-module-type-p (module-type)
  "[Cyc] Returns whether MODULE-TYPE is the keyword for simple sbhl modules, or for sbhl time modules."
  (case module-type
    ((:simple-reflexive :simple-non-reflexive :time) t)))

(defglobal *sbhl-module-properties* (make-hash-table :test #'eq)
  "[Cyc] The list of properties available for each of the *SBHL-MODULES*. Each key is a keyword. Each value field should be a functionp, corresponding for the test function for the sbhl module property associated with the key.")

(defun init-sbhl-module-properties (property-list)
  "[Cyc] Modifier. Used to store initial values fo rthe *SBHL-MODULE-PROPERTIES*."
  ;; TODO DESIGN - this is a ((key val) (key val) ..) list, not a plist
  (dolist (property-test-pair property-list)
    (setf (gethash (first property-test-pair) *sbhl-module-properties*)
          (second property-test-pair))))

(defun sbhl-module-property-p (property)
  "[Cyc] Returns whether PROPERTY is a member of *SBHL-MODULE-PROPERTIES*."
  (gethash property *sbhl-module-properties*))

(deflexical *sbhl-module-required-properties* (list :link-pred
                                                    :link-style
                                                    :module-type
                                                    ;; TODO DESIGN - another question mark
                                                    :path-terminating-mark?-fn
                                                    :marking-fn
                                                    :unmarking-fn
                                                    :index-arg
                                                    :graph)
  "[Cyc] The list of required properties for each of the *SBHL-MODULES*.")

(defparameter *sbhl-module* nil
  "[Cyc] The current sbhl-module in use for link traversal.")

(defun get-sbhl-module (&optional predicate)
  "[Cyc] Return the SBHL module for PREDICATE. Defaults to *SBHL-MODULE*."
  (cond
    ((not predicate) *sbhl-module*)
    ((and (sbhl-module-p *sbhl-module*)
          (eq predicate (get-sbhl-link-pred *sbhl-module*)))
     *sbhl-module*)
    (t (let ((module (gethash predicate (get-sbhl-modules))))
         (or module
             (sbhl-warn 0 "~a is not a valid SBHL-PREDICATE-P" predicate))))))

(defvar *sbhl-module-vars* nil
  "[Cyc] The parameters bound with each SBHL module.")
(deflexical *fort-denoting-sbhl-directed-graph* #$DirectedMultigraph
  "[Cyc] The fort which is used to determine whether a predicate has directed links.")
(deflexical *fort-denoting-sbhl-undirected-graph* #$Multigraph
  "[Cyc] The fort which is used to determine whether a predicate has undirected links.")

(defun fort-denotes-sbhl-directed-graph-p (fort)
  (cond
    ((eq fort *fort-denoting-sbhl-directed-graph*) t)
    ((eq fort *fort-denoting-sbhl-undirected-graph*) nil)
    ;; The original had a weird usage where the error was in the test field
    (t (sbhl-error 1 "Term, ~a, is not used to specify directed nor undirected graphs."))))

(defparameter *assume-sbhl-extensions-nonempty* t
  "[Cyc] Assumption made for a collection, predicate, etc. that has no known extent.
The two possible values are T (assume nonempty) and NIL (assume nothing).")

(defun clean-sbhl-modules ()
  (dohash (key module (get-sbhl-modules))
    (let ((predicate (get-sbhl-link-pred module)))
      (unless (valid-fort? predicate)
        (missing-larkc 2755)))))

;; Elided all the dictionary optimization stuff, as we don't use alists anymore
