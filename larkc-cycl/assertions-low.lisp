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


(defstruct (assertion-content (:conc-name "AS-CONTENT-"))
  formula-data
  mt
  (flags 0 :type (or null fixnum))
  arguments
  plist)

(deflexical *default-assertion-flags* 0)

(defun create-assertion-content (mt)
  (make-assertion-content :formula-data nil
                          :mt mt
                          :flags *default-assertion-flags*
                          :arguments nil
                          :plist nil))

(defun destroy-assertion-content (id)
  (when-let ((assertion-content (lookup-assertion-content id)))
    (deregister-assertion-content id)
    ;; The flags field is omitted as as a fixnum it's not a pointer.
    (setf (as-content-formula-data assertion-content)
          (setf (as-content-mt assertion-content)
                (setf (as-content-arguments assertion-content)
                      (setf (as-content-plist assertion-content)
                            nil))))))

;; TODO - all these readers dereference the ID
(macrolet ((define-reader (field)
             `(defun-inline ,(symbolicate 'lookup-assertion- field) (id)
                (when-let ((contents (lookup-assertion id)))
                  (,(symbolicate 'as-content- field) contents)))))

  (define-reader formula-data)
  (define-reader mt)
  (define-reader flags)
  (define-reader arguments)
  (define-reader plist))

(macrolet ((define-writer (field)
             (let ((data-var (symbolicate 'new- field)))
               `(defun-inline ,(symbolicate 'set-assertion- field) (id ,data-var)
                  (setf (,(symbolicate 'as-content- field) (lookup-assertion-content id))
                        ,data-var)
                  (mark-assertion-content-as-muted id)))))
  (define-writer formula-data)
  (define-writer mt)
  (define-writer flags)
  (define-writer arguments)
  (define-writer plist))

(defun load-assertion-content (assertion stream)
  ;; Relying on left-to-right parameter evaluation order, guaranteed by 3.1.2.1.2.3
  (load-assertion-content-int (assertion-id assertion)
                              (cfasl-input stream)
                              (cfasl-input stream)
                              (cfasl-input stream)
                              (cfasl-input stream)
                              (cfasl-input stream)))

(defun load-assertion-content-int (id formula-data mt flags arguments plist)
  ;; TODO - would have liked this to bypass create-assertion-content to use make- keywords easier
  (let ((assertion-content (create-assertion-content mt)))
    (setf (as-content-formula-data assertion-content) formula-data)
    (setf (as-content-flags assertion-content) flags)
    (setf (as-content-arguments assertion-content) arguments)
    (setf (as-content-plist assertion-content) plist)
    (register-assertion-content id assertion-content)))

(defun assertion-cnf-internal (assertion)
  (let ((hl-cnf (assertion-hl-cnf assertion)))
    (if (clause-struc-p hl-cnf)
        (clause-struc-cnf hl-cnf)
        hl-cnf)))

(defun possibly-assertion-cnf-internal (assertion)
  (and (valid-assertion-with-content? assertion)
       (assertion-cnf-internal assertion)))

;; TODO - and this is where we look up info by assertion ID for each field accessor.  That seems kind of nuts.  Cache the assertion-content struct on the assertion handle itself.

(defun-inline assertion-mt-internal (assertion)
  (lookup-assertion-mt (assertion-id assertion)))

(defun assertion-gaf-hl-formula-internal (assertion)
  (when (assertion-gaf-p assertion)
    (let ((formula-data (assertion-formula-data assertion)))
      (if (clause-struc-p formula-data)
          (cnf-to-gaf-formula (clause-struc-cnf formula-data))
          formula-data))))

(defun assertion-cons-internal (assertion)
  (if (assertion-gaf-p assertion)
      (assertion-gaf-hl-formula assertion)
      (assertion-cnf-internal assertion)))

(defun assertion-direction-internal (assertion)
  (decode-direction (assertion-flags-direction-code (assertion-flags assertion))))

(defun assertion-truth-internal (assertion)
  (tv-strength (assertion-tv assertion)))

(defun assertion-tv (assertion)
  "[Cyc] Return the HL TV of ASSERTION."
  (decode-tv (assertion-flags-tv-decode (assertion-flags assertion))))

(defun-inline assertion-variable-names-internal (assertion)
  "[Cyc] Return the list of names for the variables in ASSERTION."
  (get-assertion-prop assertion :variable-names))

(defun asserted-by-internal (assertion)
  (and (asserted-assertion? assertion)
       (assert-info-who (assertion-assert-info assertion))))

(defun asserted-when-internal (assertion)
  (and (asserted-assertion? assertion)
       (assert-info-when (assertion-assert-info assertion))))

(defun asserted-why-internal (assertion)
  (and (asserted-assertion? assertion)
       (assert-info-why (assertion-assert-info assertion))))

(defun asserted-second-internal (assertion)
  (and (asserted-assertion? assertion)
       (assert-info-second (assertion-assert-info assertion))))

;; TODO - assertion lookup by id
(defun-inline assertion-arguments-internal (assertion)
  (lookup-assertion-arguments (assertion-id assertion)))

(defun-inline assertion-dependents-internal (assertion)
  (get-assertion-prop assertion :dependents))

;; TODO - assertion lookup by id
(defun assertion-formula-data (assertion)
  "[Cyc] Return the HL structure used to implement the formula for ASSERTION.
This will either be a clause struc containing a cnf, a cnf, or a gaf formula." ;; Or an ATM machine.
  (lookup-assertion-formula-data (assertion-id assertion)))

;; TODO - assertion lookup by id
(defun reset-assertion-formula-data (assertion new-formula-data)
  "[Cyc] Primitively sets the HL structure used to implement the formula for ASSERTION.
This should either be a clause struc containing a cnf, a cnf, or a gaf formula."
  (set-assertion-formula-data (assertion-id assertion) new-formula-data))

(defun assertion-hl-cnf (assertion)
  "[Cyc] Return the HL structure used to implement the CNF clause for ASSERTION.
This will either be a clause struc containing a cnf, or a cnf. GAF formulas are expanded into CNFs."
  (let ((formula-data (assertion-formula-data assertion)))
    (if (or (clause-struc-p formula-data)
              (not formula-data)
              (not (assertion-gaf-p assertion)))
        formula-data
        (gaf-formula-to-cnf formula-data))))

(defun update-assertion-formula-data (assertion new-formula-data)
  "[Cyc] Primitively change the formula data of ASSERTION to NEW-FORMULA-DATA, and update the GAF flag. Assumes that NEW-FORMULA-DATA is either a CNF clause, a gaf formula, a clause-struc, or NIL."
  (cond
    ((clause-struc-p new-formula-data) (missing-larkc 32000))
    ((not new-formula-data) (annihilate-assertion-formula-data assertion))
    ((cnf-p new-formula-data) (reset-assertion-cnf assertion new-formula-data))
    ((el-formula-p new-formula-data) (reset-assertion-gaf-formula assertion new-formula-data))
    (t (error "Unexpected formula-data type: ~s" new-formula-data))))

(defun assertion-clause-struc (assertion)
  "[Cyc] If ASSERTION has a clause struc as its HL CNF implementation, return it. Otherwise, return NIL."
  (let ((formula-data (assertion-formula-data assertion)))
    (when (clause-struc-p formula-data)
      formula-data)))

(defun reset-assertion-cnf (assertion new-cnf)
  "[Cyc] Primitively change the formula data of ASSERTION to NEW-CNF, and update the GAF flag. Shrinks NEW-CNF to a gaf formula if possible."
  (let ((gaf? (determine-cnf-gaf-p new-cnf)))
    (reset-assertion-formula-data assertion
                                  (if gaf?
                                      (cnf-to-gaf-formula new-cnf)
                                      new-cnf))
    (set-assertion-gaf-p assertion gaf?)))

(defun reset-assertion-gaf-formula (assertion new-gaf-formula)
  "[Cyc] Primitively change the formula data of ASSERTION to NEW-GAF-FORMULA, and set the GAF flag to T. Assumes the NEW-GAF-FORMULA is a valid gaf formula."
  (reset-assertion-formula-data assertion new-gaf-formula)
  (set-assertion-gaf-p assertion t))

(defun annihilate-assertion-formula-data (assertion)
  "[Cyc] Primitivly change the formula data of ASSERTION to NIL, and update the GAF flag to T (why not?)."
  (reset-assertion-formula assertion nil)
  (set-assertion-gaf-p assertion t))

;; TODO - assertion lookup by id
(defun-inline assertion-flags (assertion)
  "[Cyc] Return the bit-flags for ASSERTION."
  (lookup-assertion-flags (assertion-id assertion)))

;; TODO - assertion lookup by id
(defun reset-assertion-flags (assertion new-flags)
  ;; TODO - equality check before write.  It could be faster in RAM-only scenarios to write always, but cache effects might dominate.
  (let ((flags (assertion-flags assertion)))
    (unless (eql flags new-flags)
      (set-assertion-flags (assertion-id assertion) new-flags))))



;;  All this is gaf flag stuff.


;; TODO - fixnum declarations for speedup

(defun-inline set-assertion-flags-gaf-code (flags code)
  (dpb code (byte 1 0) flags))

(defun-inline assertion-flags-direction-code (flags)
  (ldb (byte 2 1) flags))

(defun-inline assertion-flags-tv-code (flags)
  (ldb (byte 3 3) flags))

(defun-inline set-assertion-flags-tv-code (flags code)
  (dpb code (byte 3 3) flags))

(defun-inline assertion-flags-gaf-p (assertion)
  "[Cyc] Return T iff ASSERTION is a GAF according to its internal flag bits."
  (oddp (assertion-flags assertion)))

(defun set-assertion-flags-gaf-p (assertion gaf-p)
  "[Cyc] Primitively set the gaf flag of ASSERTION."
  ;; TODO - this checked the return value of ENCODE-BOOLEAN for non-NILness which makes no sense
  (reset-assertion-flags assertion (set-assertion-flags-gaf-code (assertion-flags assertion)
                                                                 (encode-boolean gaf-p))))

(defglobal *rule-set* nil
    "[Cyc] When non-NIL, a cache of all the rule assertions in the KB.")

(defglobal *prefer-rule-set-over-flags?* nil
    "[Cyc] When non-NIL, the rule-set cache is used to compute GAF vs Rule rather than using the bit in the flags.")

;; TODO - eliminate hash table size estimates
(deflexical *estimated-assertions-per-rule* 60)

(defun setup-rule-set (estimated-assertion-size)
  (let ((estimated-rule-count (ceiling (/ estimated-assertion-size
                                          *estimated-assertions-per-rule*))))
    (setf *rule-set* (new-set #'eq estimated-rule-count))))

(defun assertion-gaf-p (assertion)
  (if *prefer-rule-set-over-flags?*
      (when *rule-set*
        ;; TODO - why use this instead of the flags?
        (not (set-member? assertion *rule-set*)))
      (assertion-flags-gaf-p assertion)))

(defun set-assertion-gaf-p (assertion gaf?)
  "[Cyc] Primitively set the gaf flag of ASSERTION."
  (when *rule-set*
    (if gaf?
        (set-remove assertion *rule-set*)
        (set-add assertion *rule-set*)))
  (set-assertion-flags-gaf-p assertion gaf?))

(defun-inline determine-cnf-gaf-p (cnf)
  "[Cyc] Return the recomputed value for the gaf flag of ASSERTION."
  (gaf-cnf? cnf))






(defun load-rule-set-from-stream (stream)
  (setf *rule-set* (cfasl-input stream))
  (set-size *rule-set*))

(defun-inline gaf-formula-to-cnf (gaf)
  "[Cyc] Converts a gaf formula to a CNF clause."
  (make-gaf-cnf gaf))

(defun-inline cnf-to-gaf-formula (cnf)
  "[Cyc] Converts a CNF representation of a gaf formula to a gaf formula."
  (gaf-cnf-literal cnf))

(defun kb-set-assertion-direction-internal (assertion new-direction)
  (if (gaf-assertion? assertion)
      (reset-assertion-direction assertion new-direction)
      (progn
        (remove-assertion-indices assertion)
        (reset-assertion-direction assertion new-direction)
        (add-assertion-indices assertion))))

(defun reset-assertion-tv (assertion new-tv)
  "[Cyc] Primitively change the HL TV of ASSERTION to NEW-TV."
  (when-let ((tv-code (encode-tv new-tv)))
    (reset-assertion assertion (set-assertion-flags-tv-code (assertion-flags assertion) tv-code))))

(defun reset-assertion-truth (assertion new-truth)
  (let* ((existing-strength (assertion-strength assertion))
         (new-tv (tv-from-truth-strength new-truth existing-strength)))
    (reset-assertion-tv assertion new-tv)))

(defun reset-assertion-strength (assertion new-strength)
  (let* ((existing-truth (assertion-truth assertion))
         (new-tv (tv-from-truth-strength existing-truth new-strength)))
    (reset-assertion-tv assertion new-tv)))

;; TODO - assertion lookup by id
(defun assertion-plist (assertion)
  (lookup-assertion-plist (assertion-id assertion)))

;; TODO - assertion lookup by id
(defun-inline reset-assertion-plist (assertion plist)
  (set-assertion-plist (assertion-id assertion) plist))

(defun-inline get-assertion-prop (assertion indicator &optional default)
  (getf (assertion-plist assertion) indicator default))

;; TODO - these actually mutate the returned plist form assertion-plist.  Is this correct?
(defun set-assertion-prop (assertion indicator value)
  (reset-assertion-plist assertion
                         (setf (getf (assertion-plist assertion) indicator)
                               value)))

(defun rem-assertion-prop (assertion indicator)
  (let ((old-plist (assertion-plist assertion)))
    (reset-assertion-plist assertion
                           (remf old-plist indicator))))

(defun reset-assertion-variable-names (assertion new-variable-names)
  "[Cyc] Primitively change the variable names for ASSERTION to NEW-VARIABLE-NAMES."
  ;; TODO - check that NEW-VARIABLE-NAMES is a proper list of all strings, but type checks are diabled
  (if new-variable-names
      (set-assertion-prop assertion :variable-names new-variable-names)
      (rem-assertion-prop assertion :variable-names)))

(defun-inline assertion-index (assertion)
  "[Cyc] Return the indexing structure for ASSERTION."
  (assertion-indexing-store-get assertion))

(defun reset-assertion-index (assertion new-index)
  "[Cyc] Primitively change the indexing structure for ASSERTION to NEW-INDEX."
  (if (eq new-index (new-simple-index))
      ;; TODO - this is probably an optimization that we can ignore and safely continue.
      (missing-larkc 31913)
      (assertion-indexing-store-set assertion new-index)))

(defun-inline asertion-assert-info (assertion)
  "[Cyc] Return the assert timestamping info for ASSERTION."
  (get-assertion-prop assertion :assert-info))

(defun reset-assertion-assert-info (assertion new-info)
  (if new-info
      (set-assertion-prop assertion :assert-info new-info)
      (rem-assertion-prop assertion :assert-info)))

(defun asserted-assertion-timestamped? (assertion)
  (when (asserted-assertion? assertion)
    (assertion-assert-info assertion)))

;; Cheap way to make accessors
(defstruct (assert-info (:type list)
                        (:constructor nil))
  who
  when
  why
  second)

(defun make-assert-info (&optional who when why second)
  ;; TODO - this might be more effective as a macro, unless NILs are passed in.
  (cond
    (second (list who when why second))
    (why (list who when why))
    (when (list who when))
    (who (list who))))

;; HACK - faster, simpler versions, but they assume the list format.
(defun set-assertion-asserted-by (assertion assertor)
  (list* assertor (cdr (assertion-assert-info assertion))))

(defun set-assertion-asserted-when (assertion universal-date)
  (let ((ai (copy-list (assertion-assert-info assertion))))
    (setf (assert-info-when ai) universal-date)
    ai))

(defun set-assertion-asserted-why (assertion reason)
  (let ((ai (copy-list (assertion-assert-info assertion))))
    (setf (assert-info-why ai) reason)
    ai))

(defun set-assertion-asserted-second (assertion universal-second)
  (let ((ai (copy-list (assertion-assert-info assertion))))
    (setf (assert-info-second ai) universal-second)))

(defun valid-assertion-with-content? (assertion)
  "[Cyc] Does ASSERTION have content?"
  (let ((id (assertion-id assertion)))
    (ignore-errors
      (lookup-assertion-content id))))

(defun kb-create-assertion-kb-store (cnf mt)
  (let ((assertion (find-assertion-internal cnf mt)))
    (if assertion
        (assertion-id assertion)
        (let ((internal-id (make-assertion-id)))
          (kb-create-assertion-int (make-assertion-shell internal-id)
                                   internal-id cnf mt)
          internal-id))))

(defun kb-create-assertion-int (assertion internal-id cnf mt)
  (let ((assertion-content (create-assertion-content mt)))
    (register-assertion-content internal-id assertion-content)
    (reset-assertion-tv assertion :unknown)
    (connect-assertion assertion (find-cnf-formula-data-hook cnf))))

(defun find-cnf-formula-data-hook (cnf)
  (if (gaf-cnf? cnf)
      (find-gaf-formula-hook (gaf-cnf-literal cnf))
      (find-hl-cnf-hook cnf)))

(defun find-hl-cnf-hook (cnf)
  (let ((assertion (find-assertion-any-mt cnf)))
    (if assertion
        (or (assertion-clause-struc assertion)
            assertion)
        cnf)))

(defun find-gaf-formula-hook (gaf)
  (let ((assertion (find-gaf-any-mt gaf)))
    (if assertion
        (or (assertion-clause-struc assertion)
            assertion)
        gaf)))

(defun connect-assertion (assertion formula-data-hook)
  "[Cyc] Connect ASSERTION to FORMULA-DATA-HOOK and all its relevant indexes."
  (connect-assertion-formula-data assertion formula-data-hook)
  (add-assertion-indices assertion))

(defun connect-assertion-formula-data (assertion formula-data-hook)
  (let ((formula-data formula-data-hook))
    (cond
      ((clause-struc-p formula-data-hook) (missing-larkc 11315))
      ((assertion-p formula-data-hook) (missing-larkc 11343))
      ((or (cnf-p formula-data-hook)
           (el-formula-p formula-data-hook) nil))
      (t (error "Unexpected formula data hook: ~s" formula-data-hook)))
    (update-assertion-formula-data assertion formula-data)))

(defun kb-remove-assertion-internal (assertion)
  (let ((id (assertion-id assertion)))
    (disconnect-assertion assertion)
    (destroy-assertion-content id)
    (deregister-assertion-id id))
  (free-assertion assertion))

(defun disconnect-assertion (assertion)
  "[Cyc] Disconnect ASSERTION from all its connections."
  (remove-assertion-indices assertion)
  (disconnect-assertion-formula-data assertion))

(defun disconnect-assertion-formula-data (assertion)
  (when (assertion-clause-struc assertion)
    (missing-larkc 11355))
  (annihilate-assertion-formula-data assertion))

(defun add-new-assertion-argument (assertion new-argument)
  (set-assertion-arguments (assertion-id assertion)
                           (cons new-argument
                                 (assertion-arguments assertion))))

(defun remove-assertion-argument (assertion argument)
  (setf (assertion-arguments (assertion-id assertion))
       (delete-first argument (assertion-arguments assertion))))

(defun reset-assertion-dependents (assertion new-dependents)
  "[Cyc] Primitively set the dependent arguments of ASSERTION to NEW-DEPENDENTS."
  (if new-dependents
      (set-assertion-prop assertion :dependents new-dependents)
      (rem-assertion-prop assertion :dependents)))

(defun add-assertion-dependent (assertion argument)
  "[Cyc] Add ARGUMENT as an argument depending on ASSERTION. Return ASSERTION."
  (reset-assertion-dependents assertion (cons argument (assertion-dependents assertion)))
  assertion)

(defun remove-assertion-dependent (assertion argument)
  "[Cyc] Remove ARGUMENT as an argument depending on ASSERTION. Return ASSERTION."
  (reset-assertion-dependents assertion (delete-first argument (assertion-dependents assertion)))
  assertion)

(defparameter *dependent-deduction-table* nil)
(defparameter *dependent-assertion-table* nil)

