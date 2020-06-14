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

;; TODO - all the gather-* functions cons up lists & deduplicate them.  A faster, less gc pressure way of iterating?

(defparameter *mapping-function* nil)
(defparameter *mapping-truth* nil)
(defparameter *mapping-direction* nil)

(defun map-nart-arg-index (subl-function term &optional argnum cycl-function)
  "Apply FUNCTION to each #$termOfUnit assertion whose arg2 is a naut which mentions TERM in position ARGNUM."
  (catch :mapping-done
    (cond
      ((and argnum cycl-function)
       ;; TODO - iteration macro
       (when (do-nart-arg-index-key-validator term argnum cycl-function)
         (let ((iterator-var (new-nart-arg-final-index-spec-iterator term argnum cycl-function))
               (done-var nil)
               (token-var nil))
           (until done-var
             (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                    (valid (not (eq token-var final-index-spec))))
               (when valid
                 (let ((final-index-iterator nil))
                   (unwind-protect (progn
                                     (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf nil nil))
                                     (let ((done-var-25 nil)
                                           (token-var-26 nil))
                                       (until done-var-25
                                         (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-26))
                                                (valid-27 (not (eq token-var-26 ass))))
                                           (when valid-27
                                             (funcall subl-function ass))
                                           (setf done-var-25 (not valid-27))))))
                     (when final-index-iterator
                       (destroy-final-index-iterator final-index-iterator)))))
               (setf done-var (not valid)))))))

      ((and argnum (not cycl-function))
       (when (do-nart-arg-index-key-validator term argnum nil)
         (let ((iterator-var (new-nart-arg-final-index-spec-iterator term argnum nil))
               (done-var nil)
               (token-var nil))
           (until done-var
             (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                    (valid (not (eq token-var final-index-spec))))
               (when valid
                 (let ((final-index-iterator nil))
                   (unwind-protect (progn
                                     (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf nil nil))
                                     (let ((done-var-28 nil)
                                           (token-var-29 nil))
                                       (until done-var-28
                                         (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-29))
                                                (valid-30 (not (eq token-var-29 ass))))
                                           (when valid-30
                                             (funcall subl-function ass))
                                           (setf done-var-28 (not valid-30))))))
                     (when final-index-iterator
                       (destroy-final-index-iterator final-index-iterator)))))
               (setf done-var (not valid)))))))

      ((and (not argnum) cycl-function)
       (when (do-nart-arg-index-key-validator term nil cycl-function)
         (let ((iterator-var (new-nart-arg-final-index-spec-iterator term nil cycl-function))
               (done-var nil)
               (token-var nil))
           (until done-var
             (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                    (valid (not (eq token-var final-index-spec))))
               (when valid
                 (let ((final-index-iterator (new-final-index-iterator final-index-spec :gaf nil nil)))
                   (unwind-protect (let ((done-var-31 nil)
                                         (token-var-32 nil))
                                     (until done-var-31
                                       (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-32))
                                              (valid-33 (not (eq token-var-32 ass))))
                                         (when valid-33
                                           (funcall subl-function ass))
                                         (setf done-var-31 (not valid-33)))))
                     (destroy-final-index-iterator final-index-iterator))))
               (setf done-var (not valid)))))))

      ((and (not argnum)
            (not cycl-function))
       (when (do-nart-arg-index-key-validator term nil nil)
         (let ((iterator-var (new-nart-arg-final-index-spec-iterator term nil nil))
               (done-var nil)
               (token-var nil))
           (until done-var
             (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                    (valid (not (eq token-var final-index-spec))))
               (when valid
                 (let ((final-index-iterator (new-final-index-iterator final-index-spec :gaf nil nil)))
                   (unwind-protect (let ((done-var-34 nil)
                                         (token-var-35 nil))
                                     (until done-var-34
                                       (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-35))
                                              (valid-36 (not (eq token-var-35 ass))))
                                         (when valid-36
                                           (funcall subl-function ass))
                                         (setf done-var-34 (not valid-36)))))
                     (destroy-final-index-iterator final-index-iterator))))
               (setf done-var (not valid))))))))))

(defun map-predicate-rule-index (function pred sense &optional direction mt)
  (catch :mapping-done
    (possibly-with-just-mt (mt)
      (if direction
          ;; TODO - macro helper instance
          (when (do-predicate-rule-index-key-validator pred sense direction)
            (let ((iterator-var (new-predicate-rule-final-index-spec-iterator pred sense direction))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator nil))
                      (unwind-protect (progn
                                        (setf final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction))
                                        (let ((done-var-37 nil)
                                              (token-var-38 nil))
                                          (until done-var-37
                                            ;; macro var
                                            (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-38))
                                                   (valid-39 (not (eq token-var-38 ass))))
                                              (when valid-39
                                                ;; macro body
                                                (funcall function ass))
                                              (setf done-var-37 (not valid-39))))))
                        (when final-index-iterator                          
                          (destroy-final-index-iterator final-index-iterator)))))
                  (setf done-var (not valid))))))
          ;; TODO - another macro helper instance, different iteration via direction decision above
          (when (do-predicate-rule-index-key-validator pred sense nil)
            (let ((iterator-var (new-predicate-rule-final-index-spec-iterator pred sense nil))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator nil))
                      (unwind-protect (progn
                                        (setf final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil))
                                        (let ((done-var-41 nil)
                                              (token-var-42 nil))
                                          (until done-var-41
                                            (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-42))
                                                   (valid-43 (not (eq token-var-42 ass))))
                                              (when valid-43
                                                (funcall function ass))
                                              (setf done-var-41 (not valid-43))))))
                        (when final-index-iterator
                          (destroy-final-index-iterator final-index-iterator)))))
                  (setf done-var (not valid))))))))))

(defparameter *map-term-selective-test* nil)
(defparameter *map-term-selective-action* nil)

(defun map-mt-contents (function term &optional truth gafs-only)
  "[Cyc] Apply FUNCTION to each assertion with TRUTH in MT TERM.
If TRUTH is NIL, all assertions are mapped.
If GAFS-ONLY, then only gafs are mapped."

  (when (fort-p term)
    (if (broad-mt? term)
        (when (relevant-mt? term)
          (let ((*mapping-truth* truth))
            (catch :mapping-done
              ;; TODO - more macro expansion that isn't part of noting-percent-progress?  the totl & sofar seem to be part of calculating percentage?
              (let* ((idx (do-assertions-table))
                     (total (id-index-count idx))
                     (sofar 0))
                (noting-percent-progress ("mapping broad mt index")
                  ;; TODO - more macroexpansions, re gensym'd variables, but this is missing-larkc anyway
                  (let ((idx-120 idx))
                    (unless (id-index-objects-empty-p idx-120 :skip)
                      ;; peer 1 code, old objects?  this is an id-index structure
                      (let ((idx-121 idx-120))
                        (unless (id-index-old-objects-empty-p idx-121 :skip)
                          ;; likely gensyms as well
                          (let* ((vector-var (id-index-old-objects idx-121))
                                 (backward?-var nil)
                                 (length (length vector-var)))
                            (loop for iteration from 0 below length
                               ;; TODO - slower than doing a different iteration, or precalculating a +1/-1 delta
                               do (let* ((id (if backward?-var
                                                 ;; backward?-var is likely a macro-generated constant
                                                 (- length iteration 1)
                                                 iteration))
                                         (assertion (aref vector-var id)))
                                    (unless (and (id-index-tombstone-p assertion)
                                                 (id-index-skip-tombstones-p :skip))
                                      (when (id-index-tombstone-p assertion)
                                        (setf assertion :skip))
                                      (note-percent-progress sofar total)
                                      (incf sofar)
                                      (missing-larkc 9466)))))))
                      ;; peer 2 code, new objects?
                      (let ((idx-122 idx-120))
                        (unless (and (id-index-new-objects-empty-p idx-122)
                                     (id-index-skip-tombstones-p :skip))
                          (let* ((new (id-index-new-objects idx-122))
                                 (id (id-index-new-id-threshold idx-122))
                                 (end-id (id-index-next-id idx-122))
                                 (default (if (id-index-skip-tombstones-p :skip)
                                              nil
                                              :skip)))
                            (while (< id end-id)
                              (let ((assertion (gethash id new default)))
                                (unless (and (id-index-skip-tombstones-p :skip)
                                             (id-index-tombstone-p assertion))
                                  (note-percent-progress sofar total)
                                  (incf sofar)
                                  (missing-larkc 9467)))
                              (incf id))))))))))))
        (map-mt-index function term truth gafs-only))))

(defun map-mt-index (function mt &optional truth gafs-only)
  "[Cyc] Apply FUNCTION to each assertion with TRUTH at mt index MT.
If TRUTH is nil, all assertions are mapped.
If GAFS-ONLY, then only gafs are mapped."
  (when (fort-p mt)
    (let ((type (if gafs-only :gaf nil)))
      ;; TODO - iteration macro
      (when (do-mt-index-key-validator mt type)
        (let ((final-index-spec (mt-final-index-spec mt))
              (final-index-iterator nil))
          (unwind-protect (progn
                            (setf final-index-iterator (new-final-index-iterator final-index-spec type truth nil))
                            (let ((done-var nil)
                                  (token-var nil))
                              (until done-var
                                (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var))
                                       (valid (not (eq token-var ass))))
                                  (when valid
                                    (funcall function ass))
                                  (setf done-var (not valid))))))
            (when final-index-iterator
              (destroy-final-index-iterator final-index-iterator))))))))

(defun map-other-index (function term &optional truth gafs-only)
  "[Cyc] Apply FUNCTION to each assertion with TRUTH at other index TERM.
If TRUTH is nil, all assertionsa re mapped.
If GAFS-ONLY, then only gafs are mapped."
  (let ((type (if gafs-only :gaf nil)))
    ;; TODO - iteration macro
    (when (do-other-index-key-validator term type)
      (let ((final-index-spec (other-final-index-spec term))
            (final-index-iterator nil))
        (unwind-protect (progn
                          (setf final-index-iterator (new-final-index-iterator final-index-spec type truth nil))
                          (let ((done-var nil)
                                (token-var nil))
                            (until done-var
                              (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var))
                                     (valid (not (eq token-var ass))))
                                (when valid
                                  (when valid
                                    (when (missing-larkc 30389)
                                      (funcall function ass))))
                                (setf done-var (not valid))))))
          (when final-index-iterator
            (destroy-final-index-iterator final-index-iterator)))))))

(defun gather-index (term &optional remove-duplicates?)
  "[Cyc] Return a list of all mt-relevant assertions indexed via TERM.
If REMOVE-DUPLICATES? is non-nil, assertions are guaranteed to only be listed once."
  (let ((result nil))
    (if (auxiliary-index-p term)
        (if (eq term (unbound-rule-index))
            (missing-larkc 30393)
            ;; TODO - why a ~% at the end of an error message?
            (cerror "So don't!" "Can't gather unknown auxilliar index ~s~%" term))
        ;; TODO - iteration macro
        (when (do-term-index-key-validator term nil)
          (let ((iterator-var (new-term-final-index-spec-iterator term nil))
                (done-var nil)
                (token-var nil))
            (until done-var
              (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                     (valid (not (eq token-var final-index-spec))))
                (when valid
                  (let ((final-index-iterator nil))
                    (unwind-protect (progn
                                      (setf final-index-iterator (new-final-index-iterator final-index-spec nil nil nil))
                                      (let ((done-var-126 nil)
                                            (token-var-127 nil))
                                        (until done-var-126
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-127))
                                                 (valid-128 (not (eq token-var-127 ass))))
                                            (when (and valid-128
                                                       (do-term-index-assertion-match-p ass final-index-spec))
                                              (push ass result))
                                            (setf done-var-126 (not valid-128))))))
                      (when final-index-iterator
                        (destroy-final-index-iterator final-index-iterator)))))
                (setf done-var (not valid)))))))
    (if remove-duplicates?
        (fast-delete-duplicates result #'eq)
        result)))

(defun gather-index-in-any-mt (term &optional remove-duplicates?)
  "[Cyc] Return a list of all assertions indexed via TERM.
If REMOVE-DUPLICATES? is non-nil, assertions are guaranteed to only be listed once."
  ;; TODO - mt binding macro
  (let ((*relevant-mt-function* #'relevant-mt-is-everything)
        (*mt* #$EverythingPSC))
    (gather-index term remove-duplicates?)))

(defun gather-gaf-arg-index (term argnum &optional pred mt (truth :true))
  "[Cyc] Return a list of all gaf assertions such that:
a) TERM is its ARGNUMth argument
b) if TRUTH is non-nil, then TRUTH is its truth value
c) if PRED is non-nil, then PRED must be its predicate
d) if MT is non-nil, then MT must be its microtheory (and PRED must be non-nil)."
  (let ((result nil))
    (possibly-with-just-mt (mt)
      (if pred
          ;; TODO - iteration macro, contains pred as a parameter
          (let ((pred-var pred))
            (when (do-gaf-arg-index-key-validator term argnum pred-var)
              (let ((iterator-var (new-gaf-arg-final-index-spec-iterator term argnum pred-var))
                    (done-var nil)
                    (token-var nil))
                (until done-var
                  (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                         (valid (not (eq token-var final-index-spec))))
                    (when valid
                      (let ((final-index-iterator nil))
                        (unwind-protect (progn
                                          (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf truth nil))
                                          (let ((done-var-129 nil)
                                                (token-var-130 nil))
                                            (until done-var-129
                                              (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-130))
                                                     (valid-131 (not (eq token-var-130 ass))))
                                                (when valid-131
                                                  (push ass result))
                                                (setf done-var-129 (not valid-131))))))
                          (when final-index-iterator
                            (destroy-final-index-iterator final-index-iterator)))))
                    (setf done-var (not valid)))))))
          ;; TODO - iteration macro, again with pred
          (let ((pred-var nil))
            (when (do-gaf-arg-index-key-validator term argnum pred-var)
              (let ((iterator-var (new-gaf-arg-final-index-spec-iterator term argnum pred-var))
                    (done-var nil)
                    (token-var nil))
                (until done-var
                  (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                         (valid (not (eq token-var final-index-spec))))
                    (when valid
                      (let ((final-index-iterator nil))
                        (unwind-protect (progn
                                          (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf truth nil))
                                          (let ((done-var-133 nil)
                                                (token-var-134 nil))
                                            (until done-var-133
                                              (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-134))
                                                     (valid-135 (not (eq token-var-134 ass))))
                                                (when valid-135
                                                  (push ass result))
                                                (setf done-var-133 (not valid-135))))))
                          (when final-index-iterator
                            (destroy-final-index-iterator final-index-iterator)))))
                    (setf done-var (not valid)))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-predicate-extent-index (pred &optional mt (truth :true))
  "[Cyc] Return a list of all gaf assertions such that:
a) PRED is its predicate
b) if TRUTH is non-nil, then TRUTH is its truth value
c) if MT is non-nil, then MT must be its microtheory."
  (let ((result nil))
    (possibly-with-just-mt (mt)
      (let ((pred-var pred))
        (when (do-predicate-extent-index-key-validator pred-var)
          (let ((iterator-var (new-predicate-extent-final-index-spec-iterator pred-var))
                (done-var nil)
                (token-var nil))
            (until done-var
              (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                     (valid (not (eq token-var final-index-spec))))
                (when valid
                  (let ((final-index-iterator nil))
                    (unwind-protect (progn
                                      (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf truth nil))
                                      (let ((done-var-143 nil)
                                            (token-var-144 nil))
                                        (until done-var-143
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-144))
                                                 (valid-145 (not (eq token-var-144 ass))))
                                            (when valid-145
                                              (push ass result))
                                            (setf done-var-143 (not valid-145))))))
                      (when final-index-iterator
                        (destroy-final-index-iterator final-index-iterator)))))
                (setf done-var (not valid))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-function-extent-index (func)
  "[Cyc] Return a list of all #$termOfUnit assertions such that:
FUNC is the functor of the naut arg 2."
  (let ((result nil))
    ;; TODO - iteration macro
    (when (do-function-extent-index-key-validator func)
      (let ((final-index-spec (function-extent-final-index-spec func))
            (final-index-iterator nil))
        (unwind-protect (progn
                          (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf nil nil))
                          (let ((done-var nil)
                                (token-var nil))
                            (until done-var
                              (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var))
                                     (valid (not (eq token-var ass))))
                                (when valid
                                  (push ass result))
                                (setf done-var (not valid))))))
          (when final-index-iterator
            (destroy-final-index-iterator final-index-iterator)))))
    (fast-delete-duplicates result #'eq)))

(defun gather-predicate-rule-index (pred sense &optional mt direction)
  "[Cyc] Returna  list of all non-gaf assertions (rules) such that:
a) if SENSE is :pos, it has PRED as a predicate in a positive literal
b) if SENSE is :neg, it has PRED as a predicate in a negative literal
c) if MT is non-nil, then MT must be its microtheory
d) if DIRECTION is non-nil, then DIRECTION must be its direciton."
  (let ((result nil))
    (possibly-with-just-mt (mt)
      (if direction
          ;; TODO - iteration macro
          (when (do-predicate-rule-index-key-validator pred sense direction)
            (let ((iterator-var (new-predicate-rule-final-index-spec-iterator pred sense direction))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator nil))
                      (unwind-protect (progn
                                        (setf final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction))
                                        (let ((done-var-147 nil)
                                              (token-var-148 nil))
                                          (until done-var-147
                                            (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-148))
                                                   (valid-149 (not (eq token-var-148 ass))))
                                              (when valid-149
                                                (push ass result))
                                              (setf done-var-147 (not valid-149))))))
                        (when final-index-iterator
                          (destroy-final-index-iterator final-index-iterator)))))
                  (setf done-var (not valid))))))
          ;; TODO - iteration macro
          (when (do-predicate-rule-index-key-validator pred sense nil)
            (let ((iterator-var (new-predicate-rule-final-index-spec-iterator pred sense nil))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil)))
                      (unwind-protect (let ((done-var-151 nil)
                                            (token-var-152 nil))
                                        (until done-var-151
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-152))
                                                 (valid-153 (not (eq token-var-152 ass))))
                                            (when valid-153
                                              (push ass result))
                                            (setf done-var-151 (not valid-153)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-decontextualized-ist-predicate-rule-index (pred sense &optional direction)
  "[Cyc] Returna  list of all non-gaf assertions (rules) such that:
a) if SENSE is :pos, it has PRED as a predicate in a positive literal wrapped in #$ist
b) if SENSE is :neg, it has PRED as a predicate in a negative literal wrapped in #$ist
c) if DIRECTION is non-nil, then DIRECTION must be its direction."
  (let ((result nil))
    (if direction
        ;; TODO - iteration macro
        (when (do-decontextualized-ist-predicate-rule-index-key-validator pred sense direction)
          (let ((iterator-var (new-decontextualized-ist-predicate-rule-final-index-spec-iterator pred sense direction))
                (done-var nil)
                (token-var nil))
            (until done-var
              (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                     (valid (not (eq token-var final-index-spec))))
                (when valid
                  (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction)))
                    (unwind-protect (let ((done-var-155 nil)
                                          (token-var-156 nil))
                                      (until done-var-155
                                        (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-156))
                                               (valid-157 (not (eq token-var-156 ass))))
                                          (when valid-157
                                            (push ass result))
                                          (setf done-var-155 (not valid-157)))))
                      (destroy-final-index-iterator final-index-iterator))))
                (setf done-var (not valid))))))
        ;; TODO - iteration macro
        (when (do-decontextualized-ist-predicate-rule-index-key-validator pred sense nil)
          (let ((iterator-var (new-decontextualized-ist-predicate-rule-final-index-spec-iterator pred sense nil))
                (done-var nil)
                (token-var nil))
            (until done-var
              (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                     (valid (not (eq token-var final-index-spec))))
                (when valid
                  (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil)))
                    (unwind-protect (let ((done-var-158 nil)
                                          (token-var-159 nil))
                                      (until done-var-158
                                        (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-159))
                                               (valid-160 (not (eq token-var-159 ass))))
                                          (when valid-160
                                            (push ass result))
                                          (setf done-var-158 (not valid-160)))))
                      (destroy-final-index-iterator final-index-iterator))))
                (setf done-var (not valid)))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-isa-rule-index (collection sense &optional mt direction)
  "[Cyc] Return a list of all non-gaf assertions (rules) such that:
a) if SENSE is :pos, it has a positive literal of the form (isa <whatever> COLLECTION)
b) if SENSE is :neg, it has a negative literal of the form (isa <whatever> COLLECTION)
c) if MT is non-nil, then MT must be its microtheory
d) if DIRECTION is non-nil, then DIRECTION must be its direction."
  (let ((result nil))
    (possibly-with-just-mt (mt)
      (if direction
          ;; TODO - iteration macro
          (when (do-isa-rule-index-key-validator collection sense direction)
            (let ((iterator-var (new-isa-rule-final-index-spec-iterator collection sense direction))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction)))
                      (unwind-protect (let ((done-var-161 nil)
                                            (token-var-162 nil))
                                        (until done-var-161
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-162))
                                                 (valid-163 (not (eq token-var-162 ass))))
                                            (when valid-163
                                              (push ass result))
                                            (setf done-var-161 (not valid-163)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))
          ;; TODO - iteration macro
          (when (do-isa-rule-index-key-validator collection sense nil)
            (let ((iterator-var (new-isa-rule-final-index-spec-iterator collection sense nil))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil)))
                      (unwind-protect (let ((done-var-165 nil)
                                            (token-var-166 nil))
                                        (until done-var-165
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-166))
                                                 (valid-167 (not (eq token-var-166 ass))))
                                            (when valid-167
                                              (push ass result))
                                            (setf done-var-165 (not valid-167)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-quoted-isa-rule-index (collection sense &optional mt direction)
  "[Cyc] Return a list of all non-gaf assertions (rules) such that:
a) if SENSE is :pos, it has a positive literal of the form (quotedIsa <whatever> COLLECTION)
b) if SENSE is :neg, it has a negative literal of the form (quotedIsa <whatever> COLLECTION)
c) if MT is non-nil, then MT must be its microtheory
d) if DIRECTION is non-nil, then DIRECTION must be its direction."
  (let ((result nil))
    (possibly-with-just-mt (mt)
      (if direction
          ;; TODO - iteration macro
          (when (do-quoted-isa-rule-index-key-validator collection sense direction)
            (let ((iterator-var (new-quoted-isa-rule-final-index-spec-iterator collection sense direction))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction)))
                      (unwind-protect (let ((done-var-169 nil)
                                            (token-var-170 nil))
                                        (until done-var-169
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-170))
                                                 (valid-171 (not (eq token-var-170 ass))))
                                            (when valid-171
                                              (push ass result))
                                            (setf done-var-169 (not valid-171)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))
          ;; TODO - iteration macro
          (when (do-quoted-isa-rule-index-key-validator collection sense nil)
            (let ((iterator-var (new-quoted-isa-rule-final-index-spec-iterator collection sense nil))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil)))
                      (unwind-protect (let ((done-var-173 nil)
                                            (token-var-174 nil))
                                        (until done-var-173
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-174))
                                                 (valid-175 (not (eq token-var-174 ass))))
                                            (when valid-175
                                              (push ass result))
                                            (setf done-var-173 (not valid-175)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-genls-rule-index (collection sense &optional mt direction)
  "[Cyc] Return a list of all non-gaf assertions (rules) such that:
a) if SENSE is :pos, it has a positive literal of the form (genls <whatever> COLLECTION)
b) if SENSE is :neg, it has a negative literal of the form (genls <whatever> COLLECTION)
c) if MT is non-nil, then MT must be its microtheory
d) if DIRECTION is non-nil, then DIRECTION must be its direction."

  (let ((result nil))
    (possibly-with-just-mt (mt)
      (if direction
          ;; TODO - iteration macro
          (when (do-genls-rule-index-key-validator collection sense direction)
            (let ((iterator-var (new-genls-rule-final-index-spec-iterator collection sense direction))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction)))
                      (unwind-protect (let ((done-var-177 nil)
                                            (token-var-178 nil))
                                        (until done-var-177
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-178))
                                                 (valid-179 (not (eq token-var-178 ass))))
                                            (when valid-179
                                              (push ass result))
                                            (setf done-var-177 (not valid-179)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))
          ;; TODO - iteration macro
          (when (do-genls-rule-index-key-validator collection sense nil)
            (let ((iterator-var (new-genls-rule-final-index-spec-iterator collection sense nil))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil)))
                      (unwind-protect (let ((done-var-181 nil)
                                            (token-var-182 nil))
                                        (until done-var-181
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-182))
                                                 (valid-183 (not (eq token-var-182 ass))))
                                            (when valid-183
                                              (push ass result))
                                            (setf done-var-181 (not valid-183)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-genl-mt-rule-index (genl-mt sense &optional rule-mt direction)
  "[Cyc] Returns  alist of all non-gaf assertions (rules) such that:
a) if SENSE is :pos, it has a positive literal of the form (genlMt <whatever> GENL-MT)
b) if SENSE is :neg, it has a negative literal of the form (genlMt <whatever> GENL-MT)
c) if RULE-MT is non-nil, then RULE-MT must be its microtheory
d) if DIRECTION is non-nil, then DIRECTION must be its direction."
  (let ((result nil))
    (possibly-with-just-mt (rule-mt)
      (if direction
          ;; TODO - iteration macro
          (when (do-genl-mt-rule-index-key-validator genl-mt sense direction)
            (let ((iterator-var (new-genl-mt-rule-final-index-spec-iterator genl-mt sense direction))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction)))
                      (unwind-protect (let ((done-var-185 nil)
                                            (token-var-186 nil))
                                        (until done-var-185
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-186))
                                                 (valid-187 (not (eq token-var-186 ass))))
                                            (when valid-187
                                              (push ass result))
                                            (setf done-var-185 (not valid-187)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))
          ;; TODO - iteration macro
          (when (do-genl-mt-rule-index-key-validator genl-mt sense nil)
            (let ((iterator-var (new-genl-mt-rule-final-index-spec-iterator genl-mt sense nil))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil)))
                      (unwind-protect (let ((done-var-189 nil)
                                            (token-var-190 nil))
                                        (until done-var-189
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-190))
                                                 (valid-191 (not (eq token-var-190 ass))))
                                            (when valid-191
                                              (push ass result))
                                            (setf done-var-189 (not valid-191)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-function-rule-index (func &optional mt direction)
  "[Cyc] Return a list of all non-gaf assertions (rules) such that:
a) it has a negative literal of the form (termOfUnit <whatever> (FUNC . <whatever>))
b) if MT is non-nil, then MT must be its microtheory
c) if DIRECTION is non-nil, then DIRECTION must be its direction."
  (let ((result nil))
    (possibly-with-just-mt (mt)
      (if direction
          ;; TODO - iteration macro
          (when (do-function-rule-final-index-key-validator func direction)
            (let ((iterator-var (new-function-rule-final-index-spec-iterator func direction))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction)))
                      (unwind-protect (let ((done-var-193 nil)
                                            (token-var-194 nil))
                                        (until done-var-193
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-194))
                                                 (valid-195 (not (eq token-var-194 ass))))
                                            (when valid-195
                                              (push ass result))
                                            (setf done-var-193 (not valid-195)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))
          ;; TODO - iteration macro
          (when (do-function-rule-final-index-key-validator func nil)
            (let ((iterator-var (new-function-rule-final-index-spec-iterator func nil))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil)))
                      (unwind-protect (let ((done-var-197 nil)
                                            (token-var-198 nil))
                                        (until done-var-197
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-198))
                                                 (valid-199 (not (eq token-var-198 ass))))
                                            (when valid-199
                                              (push ass result))
                                            (setf done-var-197 (not valid-199)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-exception-rule-index (rule &optional mt direction)
  "[Cyc] Return a list of all non-gaf assertions (rules) such that:
a) it has a positive literal of the form (abnormal <whatever> RULE)
b) if MT is non-nil, then MT must be its microtheory
c) if DIRECTION is non-nil, then DIRECTION must be its direction."
  (let ((result nil))
    (possibly-with-just-mt (mt)
      (if direction
          ;; TODO - iteration macro
          (when (do-exception-rule-index-key-validator rule direction)
            (let ((iterator-var (new-exception-rule-final-index-spec-iterator rule direction))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction)))
                      (unwind-protect (let ((done-var-201 nil)
                                            (token-var-202 nil))
                                        (until done-var-201
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-202))
                                                 (valid-203 (not (eq token-var-202 ass))))
                                            (when valid-203
                                              (push ass result))
                                            (setf done-var-201 (not valid-203)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))
          ;; TODO - iteration macro
          (when (do-exception-rule-index-key-validator rule nil)
            (let ((iterator-var (new-exception-rule-final-index-spec-iterator rule nil))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil)))
                      (unwind-protect (let ((done-var-205 nil)
                                            (token-var-206 nil))
                                        (until done-var-205
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-206))
                                                 (valid-207 (not (eq token-var-206 ass))))
                                            (when valid-207
                                              (push ass result))
                                            (setf done-var-205 (not valid-207)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-pragma-rule-index (rule &optional mt direction)
  "[Cyc] Return a list of all non-gaf assertions (rules) such that:
a) it has a positive literal of the form (meetsPragmaticRequirement <whatever> RULE)
b) if MT is non-nil, then MT must be its microtheory
c) if DIRECTION is non-nil, then DIRECTION must be its direction."
  (let ((result nil))
    (possibly-with-just-mt (mt)
      (if direction
          ;; TODO - iteration macro
          (when (do-pragma-rule-index-key-validator rule direction)
            (let ((iterator-var (new-pragma-rule-final-index-spec-iterator rule direction))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil direction)))
                      (unwind-protect (let ((done-var-209 nil)
                                            (token-var-210 nil))
                                        (until done-var-209
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-210))
                                                 (valid-211 (not (eq token-var-210 ass))))
                                            (when valid-211
                                              (push ass result))
                                            (setf done-var-209 (not valid-211)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))
          ;; TODO - iteration macro
          (when (do-pragma-rule-index-key-validator rule nil)
            (let ((iterator-var (new-pragma-rule-final-index-spec-iterator rule nil))
                  (done-var nil)
                  (token-var nil))
              (until done-var
                (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                       (valid (not (eq token-var final-index-spec))))
                  (when valid
                    (let ((final-index-iterator (new-final-index-iterator final-index-spec :rule nil nil)))
                      (unwind-protect (let ((done-var-213 nil)
                                            (token-var-214 nil))
                                        (until done-var-213
                                          (let* ((ass (iteration-next-without-values-macro-helper final-index-iterator token-var-214))
                                                 (valid-215 (not (eq token-var-214 ass))))
                                            (when valid-215
                                              (push ass result))
                                            (setf done-var-213 (not valid-215)))))
                        (destroy-final-index-iterator final-index-iterator))))
                  (setf done-var (not valid))))))))
    (fast-delete-duplicates result #'eq)))

(defun gather-mt-index (term)
  "[Cyc] Return a list of all assertions such that TERM is its microtheory."
  (if (or (simple-indexed-term-p term)
          (and (hlmt-p term)
               (broad-mt? (hlmt-monad-mt term))))
      (let ((*mapping-answer* nil))
        ;; TODO - mt macro
        (let ((*relevant-mt-function* #'relevant-mt-is-eq)
              (*mt* term))
          (map-mt-contents #'gather-assertions (hlmt-monad-mt term))
          *mapping-answer*))
      (missing-larkc 12735)))

(defun gather-other-index (term)
  "[Cyc] Return a list of other assertions mentioning TERM but not indexed in any other more useful manner."
  (if (simple-indexed-term-p term)
      (let ((*mapping-answer* nil))
        (map-other-index #'gather-assertions term)
        *mapping-answer*)
      (when-let ((final-index (get-other-subindex term)))
        (missing-larkc 31921))))

(defun gather-assertions (assertion)
  (when (or (not *mapping-assertion-selection-fn*)
            (funcall *mapping-assertion-selection-fn* assertion))
    (push assertion *mapping-answer*)))

