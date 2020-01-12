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

;; TODO - convert this to (var . default) instead?
(defvar *wff-properties-table* (make-hash-table :test #'eq)
  "[Cyc] A dictionary mapping WFF property keywords to a list of the form (<corresponding variable> <default value>). Initialized via DEFPARAMETER-WFF.")

(defun-inline wff-properties-table ()
  *wff-properties-table*)

(defun-inline wff-expansion-formula ()
  *wff-expansion-formula*)

(defun-inline wff-debug? ()
  *wff-debug?*)

(defun-inline wff-violation-data-terse? ()
  *wff-violation-data-terse?*)

(defun-inline wff-original-formula ()
  *wff-original-formula*)

(defun-inline note-wff-property (keyword variable default)
  (setf (gethash keyword (wff-properties-table)) (list variable default)))

(defparameter *wff-properties* nil
  "[Cyc] Dynamically bound to the user-specified WFF properties.")

(defparameter *wff-mode* :strict
  "[Cyc] The primary way WFF will handle constraints.
:STRICT -- WFF rejects sentences if they do not provably meet arg constraints. Colloquially known as 'bitchy gatekeeper' mode.
:LENIENT -- WFF rejects sentences if they are disjoint with arg constaints, but does not add the provable consequences to the KB.")

(defparameter *wff-debug?* nil
  "[Cyc] Should WFF print out debugging messages during its computation? Setting or binding this to T can be useful when trying to figure out why WFF is rejecting something and WHY-NOT-WFF is not yielding a useful result.")

(defparameter *validate-constants?* t
  "[Cyc] Require that constants referenced by a formula be valid for the formula to be valid?")

(defparameter *recognize-variables?* t
  "[Cyc] Do not impose arg-type constraints on variables?")

(defparameter *reject-sbhl-conflicts?* t
  "[Cyc] Should asserted GAFs that introduce SBHL conflicts be rejected?")

(defparameter *inhibit-skolem-asserts?* t
  "[Cyc] Restrict assertions involving skolems?")

(defparameter *simplify-evaluatable-expressions?* nil
  "[Cyc] Canonicalize evaluatable expressions to their result when possible?")

(defparameter *enforce-evaluatable-satisfiability?* t
  "[Cyc] Must evaluatable literals be satisfiable to be deemed WFFs?")

(defparameter *enforce-only-definitional-gafs-in-vocab-mt?* nil
  ;; TODO - grammar?
  "[Cyc] Must predicates of GAFs asserted to vocab MTs must be definitional predicates?")

(defparameter *inhibit-cyclic-commutative-in-args?* t
  "[Cyc] Whether to inhibit asserting semantically dubious #$commutativeInArgs or #$commutativeInArgsAndRest GAFs, based on what is already in the KB, as determined by OKK-WRT-PARTIAL-COMMUTATIVITY?.")

(defparameter *accumulating-wff-violations?* nil
  "[Cyc] Whether we note more than one WFF violation, and continue at WFF checks even after failure.")

(defparameter *noting-wff-violations?* nil
  "[Cyc] Should WFF violations be recorded for presentation?")

(defparameter *include-suf-defn-violations?* t
  "[Cyc] Should sufficient defn violations be included in WFF violations?")

(defparameter *enforce-literal-wff-idiosyncrasies?* t
  "[Cyc] Require WFF literals to pass idiosyncratic constraints?")

(defparameter *wff-violation-data-terse?* nil
  "[Cyc] Is WFF module only gathering terse violation data?")

(defparameter nil*permit-keyword-variables?*
  "[Cyc] Transient state variable; is T during the execution of KWT-WFF?.")

(defparameter *permit-generic-arg-variables?* nil
  ;; TODO - not a very useful comment.
  "[Cyc] Transient state variable; is T sometimes and NIL other times.")

(defparameter *validate-expansions?* nil
  "[Cyc] Should WFF tests be applied to expansions (in addition to given form)?")

(defun check-wff-properties (properties)
  (do-plist (property value properties)
    (check-type property 'wff-property-p))
  (let ((val (getf properties :wff-mode)))
    (when val
      (check-type val 'wff-mode-p))))

(defparameter *within-wff?* nil
  "[Cyc] Transient state variable; is T during the execution of WFF?.")

(deflexical *wff-modes* (list :strict
                              :lenient)
  "[Cyc] See *wff-mode*")

(defun-inline wff-mode ()
  *wff-mode*)

(defun-inline wff-lenient? ()
  (eq :lenient (wff-mode)))

(defparameter *wff-formula* nil
  "[Cyc] Formula being appraised by WFF module.")

(defun-inline wff-formula ()
  *wff-formula*)

(defparameter *wff-original-formula* nil
  "[Cyc] Original formula being appraised by WFF module.")

(defparameter *wff-expansion-formula* nil
  "[Cyc] Expansion formula being appraised by WFF module.")

(defparameter *coherence-violations* nil
  "[Cyc] Descriptions of how the current argument to WFF? is incoherent.")

(defparameter *wff-violations* nil
  "[Cyc] Descriptions of how the current argument to WFF? is invalid.")

(defparameter *arity-violations* nil
  "[Cyc] Descriptions of how a relational expression is not valid wrt arity constraints.")

(defparameter *provide-wff-suggestions?* nil
  "[Cyc] Suggestions for making a non-WFF formula WFF will be attempted when this is non-NIL.")

(defparameter *wff-suggestions* nil
  "[Cyc] Descriptiosn of how the current invalid argument to WFF? can be fixed.")

(defparameter *wff-memoization-state* nil
  "[Cyc] Transient state variable; contains the current memoization state during execution of WFF?.")

(defparameter *validating-expansion?* nil
  "[Cyc] Within WFF tests applied to expansion (in addition to given form)?")

(defun-inline validating-expansion? ()
  *validating-expansion?*)

(defparameter *unexpanded-formula* nil
  "[Cyc] Original formula whose expansion is being considered?")

(defun-inline unexpanded-formula ()
  *unexpanded-formula*)
