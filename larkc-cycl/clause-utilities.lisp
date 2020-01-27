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


(defun nmake-clause (neg-lits pos-lits clause)
  "[Cyc] Destructively modify CLAUSE to have NEG-LITS and POS-LITS."
  ;; TODO - test eq before write
  (unless (eq neg-lits (neg-lits clause))
    (setf (nth 0 clause) neg-lits))
  (unless (eq pos-lits (pos-lits clause))
    (setf (nth 1 clause) pos-lits))
  clause)

(defun-inline make-gaf-cnf (asent)
  "[Cyc] Return a new cnf constructed from the true gaf ASENT."
  (make-cnf nil (list asent)))

(defun-inline nmake-dnf (neg-lits pos-lits dnf)
  "[Cyc] Destructively modify DNF to have NEG-LITS and POS-LITS, and return DNF itself."
  (nmake-clause neg-lits pos-lits dnf))

(defun clause-with-lit-counts-p (clause neg-lit-count pos-lit-count)
  "[Cyc] Return T iff CLAUSE is a clause with exactly NEG-LIT-COUNT neglits and exactly POS-LIT-COUNT poslits."
  (and (clause-p clause)
       (length= (neg-lits clause) neg-lit-count)
       (length= (pos-lits clause) pos-lit-count)))
  
(defun pos-atomic-cnf-p (cnf)
  "[Cyc] Return T iff CNF is a cnf representation of an atomic formula with exactly one poslit and no neglits. This is much quicker to check than GAF-CNF?."
  (and (cnf-p cnf)
       (clause-with-lit-counts-p cnf 0 1)))

(defun-inline pos-atomic-clause-p (clause)
  "[Cyc] Return T iff CLAUSE is a clause representation of an atomic formula with exactly one poslit and no neglits."
  (clause-with-lit-counts-p clause 0 1))

(defun-inline neg-atomic-clause-p (clause)
  "[Cyc] Return T iff CLAUSE is a clause representation of an atomic formula with exactly one neglit and no poslits."
  (clause-with-lit-counts-p clause 1 0))

(defun atomic-clause-with-all-var-args? (clause)
  "[Cyc] Return T iff CLAUSE is an atomic clause, and all of the arguments to its predicate are variables."
  (when (atomic-clause-p clause)
    (let* ((asent (atomic-clause-asent clause))
           (asent-args (atomic-sentence-args asent)))
      (every-in-list #'cyc-var? asent-args))))

(defun-inline gaf-cnf-literal (cnf)
  (first (pos-lits cnf)))

(defun atomic-cnf-asent (atomic-clause)
  "[Cyc] Returns the single pos-lit if it's a positive gaf cnf, or the single neg-lit if it's a negated gaf cnf."
  (if (pos-atomic-cnf-p atomic-clause)
      (first (pos-lits atomic-clause))
      (first (neg-lits atomic-clause))))

(defun atomic-clause-asent (atomic-clause)
  "[Cyc] Returns the single pos-lit if it's a positive gaf clause, or the single neg-lit if it's a negated gaf clause."
  (if (pos-atomic-clause-p atomic-clause)
      (first (pos-lits atomic-clause))
      (first (neg-lits atomic-clause))))

(defun atomic-cnf-predicate (atomic-clause)
  (atomic-sentence-predicate (atomic-cnf-asent atomic-clause)))

(defun atomic-clauses-p (object)
  "[Cyc] Return T iff OBJECT is a singleton list containing one atomic-clause-p."
  (and (consp object)
       (singleton? object)
       (atomic-clause-p (first object))))

(defun pos-atomic-clauses-p (object)
  "[Cyc] Return T iff OBJECT is a singleton list containing one pos-atomic-clause-p."
  (and (consp object)
       (singleton? object)
       (pos-atomic-clause-p (first object))))

(defun-inline unmake-clause (clause)
  "[Cyc] Return 0: a list of the negative literals (neg-lits) in CLAUSE.
Return 1: a list of the positive literals (pos-lits) in CLAUSE."
  (values (neg-lits clause)
          (pos-lits clause)))

(defun-inline clause-number-of-literals (clause)
  "[Cyc] Returns the number of literals (both positive and negative) in CLAUSE."
  (clause-literal-count clause))

(defun clause-literal-count (clause)
  (+ (length (neg-lits clause))
     (length (pos-lits clause))))

(defun binary-clause-p (clause)
  (= 2 (clause-number-of-literals clause)))

(defun all-literals-as-asents (clause)
  (append (neg-lits clause) (pos-lits clause)))

(defun clause-free-variables (clause &optional (var? #'cyc-var?) (include-sequence-vars? t))
  (destructuring-bind (neg-lits pos-lits) clause
    (let ((bound nil))
      (if (and (atomic-clause-p clause)
               (tou-lit? (first pos-lits)))
          (let ((*within-tou-gaf?* t))
            (ordered-union (literals-free-variables neg-lits bound var? include-sequence-vars?)
                           (literals-free-variables pos-lits bound var? include-sequence-vars?)))
          (ordered-union (literals-free-variables neg-lits bound var? include-sequence-vars?)
                         (literals-free-variables pos-lits bound var? include-sequence-vars?))))))

(defun new-subclause-spec (negative-indices positive-indices)
  "[Cyc] Note: this could be memoized"
  (list (canonicalize-literal-indices negative-indices)
        (canonicalize-literal-indices positive-indices)))

(defun new-single-literal-subclause-spec (sense index)
  (new-subclause-spec (when (eq sense :neg)
                        (list index))
                      (when (eq sense :pos)
                        (list index))))

(defun-inline ncanonicalize-literal-indices (indices)
  (sort indices #'<))

(defun-inline canonicalize-literal-indices (indices)
  (ncanonicalize-literal-indices (copy-list indices)))

(defun new-complement-subclause-spec (subclause-spec sample-clause)
  (let ((neg-lit-count (length (neg-lits sample-clause)))
        (pos-lit-count (length (pos-lits sample-clause))))
    (destructuring-bind (neg-indices pos-indices) subclause-spec
      (let ((complement-neg-indices nil)
            (complement-pos-indices nil))
        (dotimes (neg-index neg-lit-count)
          (unless (member? neg-index neg-indices)
            (push neg-index complement-neg-indices)))
        (dotimes (pos-index pos-lit-count)
          (unless (member? pos-index pos-indices)
            (push pos-index complement-pos-indices)))
        (new-subclause-spec complement-neg-indices complement-pos-indices)))))

(defstruct (subclause-spec (:type list))
  negative-indices
  positive-indices)

;; Inlining since it seems sense is often given literally
(declaim (notinline index-and-sense-match-subclause-spec?))
(defun index-and-sense-match-subclause-spec? (index sense subclause-spec)
  (member-eq? index (if (eq :neg sense)
                        (subclause-spec-negative-indices subclause-spec)
                        (subclause-spec-positive-indices subclause-spec))))

;; DESIGN - Changed params to select between matching & not matching
;; Switched to neg-form and pos-form to be more direct, and avoid unreachable code warnings due to the testing that often happens.
(defmacro do-subclause-spec* ((asent sense clause subclause-spec &optional invert?)
                              &body (neg-form pos-form))
  ;; Implementation taken from subclause-specified-by-spec
  ;; Iterates both the pos & neg lits (indicated by SENSE being :pos or :neg),
  ;; giving the INDEX into each list.
  ;; Clause & subclause-spec are values passed in.
  (let ((test (if invert? 'unless 'when)))
    (alexandria:with-gensyms (index)
      (alexandria:once-only (clause subclause-spec)
        `(progn
           (let ((,sense :neg))
             (declare (ignorable sense))
             (dolistn (,index ,asent (neg-lits ,clause))
               ;; This is the slow dynamic part
               (,test (index-and-sense-match-subclause-spec? ,index :neg ,subclause-spec)
                      ,neg-form)))
           (let ((,sense :pos))
             (declare (ignorable sense))
             (dolistn (,index ,asent (pos-lits ,clause))
               ;; Again, slow dynamic part
               (,test (index-and-sense-match-subclause-spec? ,index :pos ,subclause-spec)
                      ,pos-form))))))))

(defun subclause-specified-by-spec (clause subclause-spec)
  (let ((neg-lits nil)
        (pos-lits nil))
    (do-subclause-spec* (asent sense clause subclause-spec)
      (push asent neg-lits)
      (push asent pos-lits))
    (make-clause (nreverse neg-lits)
                 (nreverse pos-lits))))

(defun complement-of-subclause-specified-by-spec (clause subclause-spec)
  (let ((neg-lits nil)
        (pos-lits nil))
    (do-subclause-spec* (asent sense clause subclause-spec t)
      (push asent neg-lits)
      (push asent pos-lits))
    (make-clause (nreverse neg-lits)
                 (nreverse pos-lits))))

(defun subclause-spec-from-clauses (big-clause little-clause)
  (new-subclause-spec (literal-indices-from-literals (neg-lits big-clause)
                                                     (neg-lits little-clause))
                      (literal-indices-from-literals (pos-lits big-clause)
                                                    (pos-lits little-clause))))

(defun literal-indices-from-literals (big-lits little-lits)
  (loop for lit in little-lits
       collect (position lit big-lits :test #'equal)))

(defun subclause-spec-literal-count (subclause-spec)
  (+ (length (subclause-spec-positive-indices subclause-spec))
     (length (subclause-spec-negative-indices subclause-spec))))

(defun single-literal-subclause-spec? (subclause-spec)
  "[Cyc] Return T iff SUBCLAUSE-SPEC specifies a single literal."
  (= 1 (subclause-spec-literal-count subclause-spec)))
