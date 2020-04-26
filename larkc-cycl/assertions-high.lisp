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


(defmacro define-valid-assertion-func (name &body (docstring &optional (internal-func (symbolicate "KB-" name))))
  `(defun ,name (assertion)
     ,docstring
     (and (assertion-valid? assertion)
          (,internal-func assertion))))

(define-valid-assertion-func assertion-cnf
  "[Cyc] Return the cnf of ASSERTION.
Note: If you know the assertion is a gaf, consider using gaf-formula instead, if you do not explicitly need a CNF.")

(define-valid-assertion-func possibly-assertion-cnf
  "[Cyc] Return the CNF of ASSERTION, or NIL if none can be found.")

(define-valid-assertion-func assertion-mt
  "[Cyc] Return the MT of ASSERTION.")

(define-valid-assertion-func assertion-gaf-hl-formula
  nil)

(define-valid-assertion-func assertion-cons
    "[Cyc] Return a cons list representing ASSERTION's formula in some form, maybe a CNF, maybe a GAF formula, or NIL if it's invalid.
Note: Result is not destructible.")

(define-valid-assertion-func gaf-assertion?
  "[Cyc] Return T iff ASSERTION is a ground atomic formula (gaf).")

(define-valid-assertion-func assertion-direction
  "[Cyc] Return the direction of ASSERTION (either :backward, :forward, or :code).")

(define-valid-assertion-func assertion-truth
  "[Cyc] Return the current truth of ASSERTION -- either :true :false or :unknown.")

(define-valid-assertion-func assertion-strength
  "[Cyc] Return the current argumentation strength of ASSERTION -- either :monotic, :default, or :unknown.")

(define-valid-assertion-func assertion-variable-names
  "[Cyc] Return the variable names for ASSERTION.")

;; TODO - names that don't fit the pattern
(define-valid-assertion-func asserted-by
  "[Cyc] Returns the cyclist who asserted ASSERTION."
  kb-assertion-asserted-by)

(define-valid-assertion-func asserted-when
  "[Cyc] Returns teh day when ASSERTION was asserted."
  kb-assertion-asserted-when)

(define-valid-assertion-func asserted-why
  "[Cyc] Returns the reason why ASSERTION was asserted."
  kb-assertion-asserted-why)

(define-valid-assertion-func asserted-second
  "[Cyc] Returns the second of the day when ASSERTION was asserted."
  kb-assertion-asserted-second)

(define-valid-assertion-func assertion-arguments
  "[Cyc] Return a list of the arguments for ASSERTION.")

(define-valid-assertion-func assertion-dependents
  "[Cyc] Return a list of the dependents of ASSERTION.")



(defun cyc-assertion-tv (assertion)
  "[Cyc] Cyc has its own notion of tv (truth + strength) as a legacy of when the Cyc and HL sides were entangled."
  (tv-from-truth-strength (assertion-truth assertion)
                          (assertion-strength assertion)))

(defun assertion-formula (assertion)
  "[Cyc] Return a formula for ASSERTION."
  (if (gaf-assertion? assertion)
      (gaf-el-formula assertion)
      (when-let ((cnf (assertion-cnf assertion)))
        (and (cnf-p cnf)
             (cnf-formula cnf (assertion-truth assertion))))))

(defun rule-assertion? (assertion)
  "[Cyc] Return T iff ASSERTION is a rule, i.e. not a ground atomic formula (gaf)."
  (and (assertion-p assertion)
       (not (gaf-assertion? assertion))))

(defun forward-rule? (assertion)
  (and (rule-assertion? assertion)
       (forward-assertion? assertion)))

(defun* assertion-type (assertion) (:inline t)
  "[Cyc] Return the current type of ASSERTION -- either :GAF or :RULE."
  (if (gaf-assertion? assertion)
      :gaf
      :rule))

(defun* assertion-has-type? (assertion type) (:inline t)
  "[Cyc] Return T iff ASSERTION's current type is TYPE."
  (eq type (assertion-type assertion)))

(defun* gaf-formula (assertion) (:inline t)
  "[Cyc] Return the formula for ASSERTION, which must be a gaf.
Does not put a #$not around negated gafs."
  (gaf-hl-formula assertion))

(defun* gaf-hl-formula (assertion) (:inline t)
  "[Cyc] Return the formula for ASSERTION, which must be a gaf.
Does not put a #$not around negated gafs."
  (assertion-gaf-hl-formula assertion))

(defun* gaf-el-formula (assertion) (:inline t)
  "[Cyc] Return the formula for ASSERTION, which must be a gaf.
Puts a #$not around negated gafs.
Does not do any uncanonicalization or conversion of HL terms in args to EL."
  (assertion-gaf-el-formula assertion))

(defun assertion-gaf-el-formula (assertion)
  "[Cyc] Returns the EL formula of ASSERTION if it's a gaf, otherwise returns NIL.
This will return (#$not <blah>) for negated gafs."
  (when-let ((formula (assertion-gaf-hl-formula assertion)))
    (if (eq :false (assertion-truth assertion))
        (negate formula)
        formula)))

(defun gaf-args (assertion)
  "[Cyc] Returnargs of the gaf ASSERTION."
  (formula-args (gaf-formula assertion)))

(defun* gaf-arg (assertion n) (:inline t)
  "[Cyc] Return arg N of the gaf ASSERTION."
  (nth n (gaf-formula assertion)))

(defun* gaf-predicate (assertion) (:inline t)
  "[Cyc] Return the predicate of gaf ASSERTION."
  (formula-arg0 (gaf-hl-formula assertion)))

(defun* gaf-arg1 (assertion) (:inline t)
  "[Cyc] Return arg 1 of the gaf ASSERTION."
  (gaf-arg assertion 1))

(defun* gaf-arg2 (assertion) (:inline t)
  "[Cyc] Return arg 2 of the gaf ASSERTION."
  (gaf-arg assertion 2))

(defun* gaf-arg3 (assertion) (:inline t)
  "[Cyc] Return arg 3 of the gaf ASSERTION."
  (gaf-arg assertion 3))

(defun* assertion-has-direction? (assertion direction) (:inline t)
  "[Cyc] Return T iff ASSERTION has DIRECTION as its direction."
  (eq direction (assertion-direction assertion)))

;; TODO - deprecate
(defun* assertion-has-direction (assertion direction) (:inline t)
  (assertion-has-direction? assertion direction))

(defun forward-assertion? (assertion)
  "[Cyc] Predicate returns T iff ASSERTION's direction is :FORWARD."
  (and (assertion-p assertion)
       (eq :forward (assertion-direction assertion))))

(defun* assertion-has-truth? (assertion truth) (:inline t)
  "[Cyc] Return T iff ASSERTION's current truth is TRUTH."
  (eq (assertion-truth assertion) truth))

;; TODO - deprecate
(defun* assertion-has-truth (assertion truth) (:inline t)
  (assertion-has-truth? assertion truth))

(defun* assertion-el-variables (assertion) (:inline t)
  "[Cyc] Return a list of the EL variables, for ASSERTION."
  (mapcar #'intern-el-var (assertion-variable-names assertion)))

(defun timestamp-asserted-assertion (assertion &optional who when why second)
  (when *recording-hl-transcript-operations?*
    (missing-larkc 32158))
  (timestamp-asserted-assertion-int assertion who when why second))

(defglobal *tl-assertion-lookaside-table* nil
    "[Cyc] A lookaside cache for efficiency of tl-timestamp-asserted-assertion.
TL assertion -> HL assertion.")

(deflexical *tl-assertion-capacity* 5)

(defun timestamp-asserted-assertion-int (assertion &optional who when why second)
  (when (asserted-assertion? assertion)
    (kb-set-assertion-asserted-by assertion who)
    (kb-set-assertion-asserted-when assertion when)
    (kb-set-assertion-asserted-why assertion why)
    (kb-set-assertion-asserted-second assertion second)))

(defun invalid-assertion? (assertion &optional robust?)
  (declare (ignore robust?))
  (and (assertion-p assertion)
       (not (valid-assertion? assertion))))

;; TODO - deprecate
(defun* valid-assertion (assertion &optional robust?) (:inline t)
  (declare (ignore robust?))
  (valid-assertion? assertion))

(defun* create-assertion (cnf mt &optional var-names (direction :backward)) (:inline t)
  "[Cyc] Create a new assertion with CNF in MT."
  (create-assertion-int cnf mt var-names direction))

(defun find-or-create-assertion (cnf mt &optional var-names (direction :backward))
  "[Cyc] Return assertion in MT with CNF, if it exists -- else create it."
  (or (find-assertion cnf mt)
      (create-assertion cnf mt var-names direction)))

(defun create-assertion-int (cnf mt &optional var-names (direction :backward))
  (let ((assertion (kb-create-assertion cnf mt)))
    (when assertion
      (kb-set-assertion-variable-names assertion var-names)
      (kb-set-assertion-direction assertion direction))
    assertion))

(defun* remove-assertion (assertion) (:inline t)
  "[Cyc] Remove ASSERTION."
  (kb-remove-assertion assertion))

(defun only-argment-of-assertion-p (assertion argument)
  "[Cyc] Returns T if ARGUMENT is the sole argument for ASSERTION; NIL if there are other, different arguments."
  (not (member? argument (assertion-arguments assertion) :test #'not-eq)))

(defun asserted-assertion? (assertion)
  "[Cyc] Return non-NIL iff ASSERTION has an asserted argument."
  (find-if #'asserted-argument-p (assertion-arguments assertion)))

(defun get-asserted-argument (assertion)
  "[Cyc] Return the asserted argument for ASSERTION, or NIL if none present."
  (find-if #'asserted-argument-p (assertion-arguments assertion)))

(defun assertion-dependent-count (assertion)
  "[Cyc] Return the number of arguments depending on ASSERTION."
  (length (assertion-dependents assertion)))

(defparameter *assertion-dump-id-table* nil)

(defun* find-assertion-by-dump-id (dump-id) (:inline t)
  "[Cyc] Return the assertion with DUMP-ID during a KB load."
  (find-assertion-by-id dump-id))
