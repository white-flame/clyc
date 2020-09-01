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


;; TODO - local stopgap for iteration macro, for kb-mapping-utilities usage
(defmacro kmu-do-index-iteration ((assertion-var type params inner-params &key done-place) &body body)
  (with-gensyms (iterator done token
                          final-index-spec valid
                          final-index-iterator
                          inner-done inner-token inner-valid)
    `(when (,(symbolicate 'do- type '-index-key-validator) ,@params)
       (let ((,iterator (,(symbolicate 'new- type '-final-index-spec-iterator) ,@params))
             (,done ,done-place)
             (,token nil))
         (until ,done
           (let* ((,final-index-spec (iteration-next-without-values-macro-helper ,iterator ,token))
                  (,valid (not (eq ,token ,final-index-spec))))
             (when ,valid
               (let ((,final-index-iterator (new-final-index-iterator ,final-index-spec ,@inner-params)))
                 (unwind-protect (let ((,inner-done ,done-place)
                                       (,inner-token nil))
                                   (until ,inner-done
                                     (let* ((,assertion-var (iteration-next-without-values-macro-helper ,final-index-iterator ,inner-token))
                                            (,inner-valid (not (eq ,inner-token ,assertion-var))))
                                       (when ,inner-valid
                                         ,@body)
                                       ;; TODO - all from done-place?
                                       (setf ,inner-done (or (not ,inner-valid)
                                                             ,done-place)))))
                   (destroy-final-index-iterator ,final-index-iterator))))
             (setf ,done (or (not ,valid)
                             ,done-place))))))))

(defun some-pred-value (term pred &optional (index-arg 1) (truth :true))
  "[Cyc] Find the first gaf assertion such that:
a) the assertion is in a relevant microtheory (relevance is established outside)
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) TERM is the term in the INDEX-ARG position.
Return T if such an assertion exists, otherwise return NIL."
  (let ((answer nil))
    (kmu-do-index-iteration (assertion gaf-arg (term index-arg pred) (:gaf truth nil)
                                       :done-place answer)
      ;; TODO - bookkeeping macro
      (when *mapping-assertion-bookkeeping-fn*
        (funcall *mapping-assertion-bookkeeping-fn* assertion))
      ;; TODO - this iteration needs to become a non-local return instead of the stupid flag management.
      (setf answer t))
    answer))

(defun some-pred-value-in-any-mt (term pred &optional (index-arg 1) (truth :true))
  "[Cyc] Find the first gaf assertion such that:
a) the assertion is allowed to be in any microtheory
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) TERM is the term in the INDEX-ARG position.
Return T if such an assertion exists, otherwise return NIL."
  ;; TODO - mt macro
  (let ((*relevant-mt-function* #'relevant-mt-is-everything)
        (*mt* #$EverythingPSC))
    (some-pred-value term pred index-arg truth)))

(defun some-pred-value-in-relevant-mts (term pred &optional mt (index-arg 1) (truth :true))
  "[Cyc] If MT is NIL, behaves like SOME-PRED-VALUE.  Otherwise, behaves like SOME-PRED-VALUE-IN-MT."
  (possibly-in-mt (mt)
    (some-pred-value term pred index-arg truth)))

(defun some-pred-value-if (term pred test &optional (index-arg 1) (truth :true))
  "[Cyc] Find teh first gaf assertion such that:
a) the assertion is in a relevant microtheory (relevance is established outside)
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) TERM is the term in the INDEX-ARG position.
e) TEST returns non-nil when applied to assertion.
Return T if such an assertion exists, otherwise return NIL."
  (let ((answer nil))
    (kmu-do-index-iteration (assertion gaf-arg (term index-arg pred) (:gaf truth nil)
                                       :done-place answer)
      (when (funcall test assertion)
        ;; TODO bookeeping macro
        (when *mapping-assertion-bookkeeping-fn*
          (funcall *mapping-assertion-bookkeeping-fn* assertion)))
      (setf answer t))
    answer))

(defun fpred-value-gaf (term pred &optional (index-argnum 1) (truth :true))
  "[Cyc] Find the first gaf assertion that:
a) the assertion si in a relevant microtheory (relevance is established outside)
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) TERM is the term in the INDEX-ARGNUM position.
Return the gaf if it exists. Otherwise, return NIL."
  (let ((answer nil))
    (kmu-do-index-iteration (assertion gaf-arg (term index-argnum pred) (:gaf truth nil)
                                       :done-place answer)
      ;; TODO - bookkeeping macro
      (when-let ((fn *mapping-assertion-bookkeeping-fn*))
        (funcall fn assertion))
      ;; TODO - use nonlocal return instead
      (setf answer assertion))
    answer))

(defun fpred-value-gaf-in-relevant-mts (term pred &optional mt (index-argnum 1) (truth :true))
  "[Cyc] If MT is NIL, behaves like FPRED-VALUE-GAF.  Otherwise, looks in all genlMts of MT."
  (possibly-in-mt (mt)
    (fpred-value-gaf term pred index-argnum truth)))

(defun fpred-value (term pred &optional (index-arg 1) (gather-arg 2) (truth :true))
  "[Cyc] Find the first gaf assertion such that:
a) the assertion is in a relevant microtheory (relevance is established outside)
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) TERM is the term in the INDEX-ARG position.
Return the term in the GATHER-ARG position if such an assertion exists. Otherwise, return NIL."
  (when-let ((assertion (fpred-value-gaf term pred index-arg truth)))
    (gaf-arg assertion gather-arg)))

(defun fpred-value-in-any-mt (term pred &optional (index-arg 1) (gather-arg 2) (truth :true))
  "[Cyc] Find the first gaf assertion such that:
a) the assertion is allowed to be in any microtheory
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) TERM is the term in the INDEX-ARG position.
Return the term in the GATHER-ARG position if such an assertion exists. Otherwise, return NIL."
  ;; TODO - mt macro
  (let ((*relevant-mt-function* #'relevant-mt-is-everything)
        (*mt* #$EverythingPSC))
    (fpred-value term pred index-arg gather-arg truth)))

(defun fpred-value-in-relevant-mts (term pred &optional mt (index-arg 1) (gather-arg 2) (truth :true))
  "[Cyc] If MT is NIL, behaves like FPRED-VALUE.  Otherwise, looks in all genlMts of MT."
  (possibly-in-mt (mt)
    (fpred-value term pred index-arg gather-arg truth)))

(defun pred-values (term pred &optional (index-arg 1) (gather-arg 2) (truth :true))
  "[Cyc] Find all gaf assertions such that:
a) the assertion is in a relevant microtheory (relevance is established outside)
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) TERM is the term in the INDEX-ARG position.
Return a list of the terms in the GATHER-ARG position of all such assertions."
  (let ((values nil))
    (kmu-do-index-iteration (assertion gaf-arg (term index-arg pred) (:gaf truth nil))
      ;; TODO - bookkeeping macro
      (when-let ((fn *mapping-assertion-bookkeeping-fn*))
        (funcall fn assertion))
      (let ((value (gaf-arg assertion gather-arg)))
        (if *mapping-equality-test*
            (pushnew value values :test *mapping-equality-test*)
            (push value values))))
    values))

(defun pred-values-in-relevant-mts (term pred &optional mt (index-arg 1) (gather-arg 2) (truth :true))
  "[Cyc] If MT is NIL, behaves like PRED-VALUES.  Otherwise, behaves like PRED-VALUES-IN-MT."
  (possibly-in-mt (mt)
    (pred-values term pred index-arg gather-arg truth)))

(defun pred-u-v-holds (pred u v &optional (u-arg 1) (v-arg 2) (truth :true))
  "[Cyc] Find the first gaf assertion such that:
a) the assertion is in a relevant microtheory (relevance is established outside)
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) U is the term in the U-ARG position.
e) V is the term in the V-ARG position.
Return T if such an assertion exists, otherwise return NIL."
  (let ((answer nil))
    (kmu-do-index-iteration (assertion gaf-arg (u u-arg pred) (:gaf truth nil)
                                       :done-place answer)
      (when (funcall *mapping-equality-test* (gaf-arg assertion v-arg) v)
        ;; TODO - bookkeeping macro
        (when-let ((fn *mapping-assertion-bookkeeping-fn*))
          (funcall fn assertion))
        ;; TODO - nonlocal return instead
        (setf answer t)))
    answer))

(defun pred-u-v-holds-in-any-mt (pred u v &optional (u-arg 1) (v-arg 2) (truth :true))
  "[Cyc] Find the first gaf assertion such that:
a) the assertion is allowed to be in any microtheory
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) U is the term in the U-ARG position.
e) V is the term in the V-ARG position.
Return T if such an assertion exists, otherwise return NIL."
  ;; TODO - mt macro
  (let ((*relevant-mt-function* #'relevant-mt-is-everything)
        (*mt* #$EverythingPSC))
    (pred-u-v-holds pred u v u-arg v-arg truth)))

(defun pred-arg-values (term pred arg &optional (term-psn 1) (arg-psn 2) (gather-psn 3) (truth :true))
  (let ((answer nil))
    (kmu-do-index-iteration (assertion gaf-arg (term term-psn pred) (:gaf truth nil))
      (when (equalp arg (gaf-arg assertion arg-psn))
        ;; TODO - bookkeeping macro
        (when-let ((fn *mapping-assertion-bookkeeping-fn*))
          (funcall fn assertion))
        (if-let ((test *mapping-equality-test*))
          (pushnew (gaf-arg assertion gather-psn) answer :test test)
          (push (gaf-arg assertion gather-psn) answer))))
    answer))

;; TODO - referenced in kb-accessors
(missing-function-implementation pred-arg-values-in-relevant-mts)

(defun pred-value-tuples (term pred index-arg gather-args &optional (truth :true))
  "[Cyc] Find all gaf assertions such that:
a) the assertion is in a relevant microtheory (relevance is established outside)
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED Is the predicate used.
d) TERM is the term in the INDEX-ARG position.
Return a list of tuples formed from the GATHER-ARGS position of all such assertions."
  (must (every #'integerp gather-args) "~s is not a valid arg-position-list" gather-args)
  (let ((answer nil))
    (kmu-do-index-iteration (assertion gaf-arg (term index-arg pred) (:gaf truth nil))
      (let ((tuple (mapcar (lambda (arg)
                             (gaf-arg assertion arg))
                           gather-args)))
        ;; TODO - bookkeeping macro
        (when-let ((fn *mapping-assertion-bookkeeping-fn*))
          (funcall fn assertion))
        (if-let ((test *mapping-equality-test*))
          (pushnew tuple answer :test test)
          (push tuple answer))))
    answer))

(defun pred-value-tuples-in-any-mt (term pred index-arg gather-args &optional (truth :true))
  "[Cyc] Find all gaf assertions such that:
a) the assertion is allowed to be from any microtheory
b) if TRUTH is non-nil, the assertion has TRUTH as its truth value
c) PRED is the predicate used.
d) TERM is the term in the INDEX-ARG position.
Return a list of tuples formed from the GATHER-ARGS positions of all such assertions."
  ;; TODO - mt macro
  (let ((*relevant-mt-function* #'relevant-mt-is-everything)
        (*mt* #$EverythingPSC))
    (pred-value-tuples term pred index-arg gather-args truth)))

