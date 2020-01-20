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

;; TODO - all these hl-modify-remote? checks are kinda crappy.  The dispatch mechanism should be more centralized, and maybe use a function naming protocol to automatically generate the tests as to which function to call.

(define-hl-creator kb-create-assertion (cnf mt)
    "[Cyc] Create a new assertion with CNF in MT."
    nil
  (if (hl-modify-remote?)
      (missing-larkc 32157)
      (kb-create-assertion-local cnf mt)))

(defun kb-create-assertion-local (cnf mt)
  (let ((internal-id (kb-create-assertion-kb-store cnf mt)))
    (find-assertion-by-id internal-id)))

(define-hl-modifier kb-remove-assertion (assertion)
    "[Cyc] Remove ASSERTION from the KB."
    nil
  (kb-remove-assertion-internal assertion))




;; TODO DESIGN - instead of all these remote checks everywhere, can't we manifest a simpler struct cached locally and use the normal local accessors on it?

(defmacro define-kb-non-remote (name params &body (docstring &optional (internal-func (symbolicate name "-INTERNAL"))))
  "Helper for these annoyingly repetetive functions. Maps the function KB-<name> to <name>-INTERNAL with the same parameters, after a hl-access-remote? check."
  `(defun ,(symbolicate "KB-" name) ,params
     ,docstring
     (if (hl-access-remote?)
         ;; These were different per instantiation, but whatever.
         (missing-larkc 29511)
         (,internal-func ,@params))))

(define-kb-non-remote assertion-cnf (assertion)
  "[Cyc] Return the CNF for ASSERTION.")

(define-kb-non-remote possibly-assertion-cnf (assertion)
  "[Cyc] Return the CNF for ASSERTION or NIL.")

(define-kb-non-remote assertion-mt (assertion)
  "[Cyc] Return the MT for ASSERTION.")

(define-kb-non-remote lookup-assertion (cnf mt)
  "[Cyc] Return the assertion with CNF and MT, if it exists. Return NIL otherwise."
  find-assertion-internal)

;; TODO - ? vs -P disconnect
(define-kb-non-remote gaf-assertion? (assertion)
  "[Cyc} Return T iff ASSERTION is a ground atomic formula (gaf)."
  assertion-gaf-p)

(define-kb-non-remote assertion-gaf-hl-formula (asssertion)
  "[Cyc] Returns the HL clause of ASSERTION if it's a gaf, otherwise returns NIL.
Ignores the truth - i.e. returns <blah> instead of (#$not <blah>) for negated gafs."
  assertion-gaf-formula-internal)

(define-kb-non-remote assertion-cons (assertion)
  "[Cyc] Returns a CNF or GAF HL formula.")

(define-kb-non-remote assertion-direction (assertion)
  "[Cyc] Return the direction of ASSERTION (either :BACKWARD, :FORWARD, or :CODE).")

(define-kb-non-remote assertion-truth (assertion)
  "[Cyc] Return the current truth of ASSERTION -- either :TRUE :FALSE or :UNKNOWN")

(define-kb-non-remote assertion-strength (assertion)
  "[Cyc] Return the current argumentation strength of ASSERTION -- either :MONOTONIC, :DEFAULT, or :UNKNOWN.")

(define-kb-non-remote assertion-variable-names (assertion)
  "[Cyc] Return the variable names for ASSERTION.")

(define-kb-non-remote assertion-asserted-by (assertion)
  "[Cyc] Return the asserted-by bookkeeping info for ASSERTION."
  asserted-by-internal)

(define-kb-non-remote assertion-asserted-when (assertion)
  "[Cyc] Return the asserted-when bookkeeping info for ASSERTION."
  asserted-when-internal)

(define-kb-non-remote assertion-asserted-why (assertion)
  "[Cyc] Return the asserted-why bookkeeping info for ASSERTION."
  asserted-why-internal)

(define-kb-non-remote assertion-asserted-second (assertion)
  "[Cyc] Return the asserted-second bookkeeping info for ASSERTION."
  asserted-second-internal)


(defmacro define-kb-hl-modifier (name params &body (docstring &optional (new-func (symbolicate name "-INTERNAL"))))
  `(define-hl-modifier ,(symbolicate "KB-" name) ,params
       ,docstring
       nil
     ;; TODO - original code had old-* variables that were unused, which read the overwritten value. Skipping since I don't believe it has side effects.
     (,new-func ,@params)))

(define-kb-hl-modifier set-assertion-direction (assertion new-direction)
  "[Cyc] Change direction of ASSERTION to NEW-DIRECTION.")

(define-kb-hl-modifier set-assertion-truth (assertion new-truth)
  "[Cyc] Change the truth of ASSERTION to NEW-TRUTH."
  reset-assertion-truth)

(define-kb-hl-modifier set-assertion-strength (assertion new-strength)
  "[Cyc] Change the strength of ASSERTION to NEW-STRENGTH."
  reset-assertion-strength)

(define-kb-hl-modifier set-assertion-variable-names (assertion new-variable-names)
  "[Cyc] Change the variable names for ASSERTION to NEW-VARIABLE-NAMES."
  reset-assertion-variable-names)

(define-kb-hl-modifier set-assertion-asserted-by (assertion assertor)
  "[Cyc] Set the asserted-by-bookkeeping info for ASSERTION to ASSERTOR."
  set-assertion-asserted-by)

(define-kb-hl-modifier set-assertion-asserted-when (assertion universal-date)
  "[Cyc] Set the aserted-when bookkeeping info for ASSERTION to UNIVERSAL-DATE."
  set-assertion-asserted-when)

(define-kb-hl-modifier set-assertion-asserted-why (assertion reason)
  "[Cyc] Set the asserted-why bookkeeping info for ASSERTION to REASON."
  set-assertion-asserted-why)

(define-kb-hl-modifier set-assertion-asserted-second (assertion universal-second)
  "[Cyc] Set the asserted-second bookkeeping info for ASSERTION to UNIVERSAL-SECOND."
  set-assertion-asserted-second)



;; TODO - Not sure why these aren't with the batch above the hl-modifiers. Figure out the grouping from the java declareFunction list.

(define-kb-non-remote assertion-arguments (assertion)
  "[Cyc] Return the arguments for ASSERTION.")

(define-kb-non-remote assertion-dependents (assertion)
  "[Cyc] Return the dependents of ASSERTION.")

