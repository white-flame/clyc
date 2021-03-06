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

;; TODO - massive macro extraction and inlining of utility functions required here!

;; TODO - desparately needs testing.

(defun get-subindex (term keys)
  (let ((subindex (term-index term)))
    ;; TODO - this should early exit if subindex ever goes to NIL?
    (dolist (key keys)
      (when (and key subindex)
        (setf subindex (intermediate-index-lookup subindex key))))
    subindex))

(defun term-add-indexing-leaf (term keys leaf)
  "[Cyc] Walks down the indexing for TERM by following successive elements of KEYS, and once it gets to the bottom, inserts LEAF."
  (mark-term-index-as-muted term)
  (if (simple-indexed-term-p term)
      (add-simple-index term leaf)
      (intermediate-index-insert (term-index term) keys leaf)))

(defun term-rem-indexing-leaf (term keys leaf)
  "[Cyc] Walks down the indexing for TERM by following successive elements of KEYS, and once it gets to the bottom, deletes LEAF."
  (mark-term-index-as-muted term)
  (if (simple-indexed-term-p term)
      (rem-simple-index term leaf)
      (progn
        (intermediate-index-delete (term-index term) keys leaf)
        (possibly-toggle-term-index-mode term)
        ;; TODO - return value?
        leaf)))

(defun* all-mt-subindex-keys-relevant-p () (:inline t)
  (any-or-all-mts-are-relevant?))

(defun relevant-mt-subindex-count-with-cutoff (mt-subindex cutoff)
  (cond
   ((all-mt-subindex-keys-relevant-p) (min cutoff (subindex-leaf-count mt-subindex)))
   ((only-specified-mt-is-relevant?) (let* ((mt (current-mt-relevance-mt))
                                           (subindex (subindex-lookup mt-subindex mt)))
                                       (if subindex
                                           (min cutoff (subindex-leaf-count subindex))
                                           0)))
    (t (let ((count 0))
         (do-intermediate-index (mt subindex mt-subindex)
           (if (number-has-reached-cutoff? count cutoff)
               (return-from do-intermediate-index)
               (when (relevant-mt? mt)
                 (incf count (subindex-leaf-count subindex)))))
         (min cutoff count)))))

(defun mark-term-index-as-muted (term)
  (cond
    ((constant-p term) (when-let ((id (constant-suid term)))
                         (mark-constant-index-as-muted id)))
    ((nart-p term) (missing-larkc 30874))
    ((kb-unrepresented-term-p term) (when-let ((id (unrepresented-term-suid term)))
                                      (mark-unrepresented-term-index-as-muted id)))))




;; GAF arg index

(defun num-gaf-arg-index (term &optional argnum pred mt)
  "[Cyc] Return the number of gafs indexed off of TERM ARGNUM PRED MT."
  (let ((num 0))
    ;; TODO - this pattern is clearly a macro that should be passing in NUM as a place directly
    (if (simple-indexed-term-p term)
        (let ((count 0))
          (dolist (ass (do-simple-index-term-assertion-list term))
            (when (matches-gaf-arg-index ass term argnum pred mt)
              (incf count)))
          (setf num count))
        (setf num
              (if-let ((subindex (get-gaf-arg-subindex term argnum pred mt)))
                (subindex-leaf-count subindex)
                0)))
    num))

(defun relevant-num-gaf-arg-index (term &optional argnum pred)
  "[Cyc] Return the assertion count at relevant mts under TERM ARGNUM PRED."
  (let ((num 0))
    ;; TODO - might be the same as num-gaf-arg-index, but uses NUM as a place directly, instead of just returning straight from the COND
    (cond
      ((all-mt-subindex-keys-relevant-p)
       (setf num (num-gaf-arg-index term argnum pred)))
      
      ((simple-indexed-term-p term)
       (dolist (ass (do-simple-index-term-assertion-list term))
         (when (and (matches-gaf-arg-index ass term argnum pred)
                    (relevant-mt? (assertion-mt ass)))
           (incf num))))

      (t (let ((good-key-count (number-of-non-null-args-in-order argnum pred)))
           ;; TODO - macro usage, subtracting constants
           (if (= good-key-count (- 3 1))
               (when-let ((mt-subindex (get-gaf-arg-subindex term argnum pred)))
                 (missing-larkc 12807))
               (missing-larkc 4718)))))
    num))

(defun relevant-num-gaf-arg-index-with-cutoff (term cutoff &optional argnum pred)
  "[Cyc] Return the assertion count at relevant mts under TERM ARGNUM PRED.
CUTOFF: non-negative-integer-p; a number beyond which to stop counting relevant assertions and just return CUTOFF."
  (let ((num 0))
    ;; TODO - suspicion of macro like num-gaf-arg-index & relevant-num-gaf-arg-index
    (cond
      ((all-mt-subindex-keys-relevant-p)
       ;; TODO - optimize check to a single write, but probably reveals structure of macro
       (setf num (num-gaf-arg-index term argnum pred))
       (when (number-has-reached-cutoff? num cutoff)
         (setf num cutoff)))

      ((simple-indexed-term-p term)
       (dolist (ass (do-simple-index-term-assertion-list term))
         ;; TODO - should abort the dolist if this is true instead
         (unless (number-has-reached-cutoff? num cutoff)
           (when (and (matches-gaf-arg-index ass term argnum pred)
                      (relevant-mt? (assertion-mt ass)))
             (incf num)))))

      (t (let ((good-key-count (number-of-non-null-args-in-order argnum pred)))
           ;; TODO - macro substracting constants
           (if (= good-key-count (- 3 1))
               (when-let ((mt-subindex (get-gaf-arg-subindex term argnum pred)))
                 (setf num (relevant-mt-subindex-count-with-cutoff mt-subindex cutoff)))
               (missing-larkc 4719)))))
    num))

(defun clear-key-gaf-arg-index-cached ()
  (when-let ((cs *key-gaf-arg-index-cached-caching-state*))
    (caching-state-clear cs)))

;; TODO HACK - this requires the last 2 args to be optional, but I don't think we can currently express that!
(defun-memoized key-gaf-arg-index-cached (term #|&optional|#argnum pred)
    (:capacity 5000 :test eq :clear-when :hl-store-modified)
  "[Cyc] Return a list of the keys to the next index level below TERM ARGNUM PRED."
  (key-gaf-arg-index term argnum pred))

(defun key-gaf-arg-index (term &optional argnum pred)
  "[Cyc] Return a list of the keys to the next index level below TERM ARGNUM PRED.
@note destructible"
  (let ((keys nil))
    ;; TODO - definintely a macro, passing keys-accum to keys
    (if (simple-indexed-term-p term)
        (let ((keys-accum nil))
          (dolist (ass (do-simple-index-term-assertion-list term))
            (setf keys-accum (simple-key-gaf-arg-index ass keys-accum term argnum pred)))
          (setf keys keys-accum))
        (let ((next-level-subindex (get-gaf-arg-subindex term argnum pred)))
          (setf keys (if (intermediate-index-p next-level-subindex)
                         (intermediate-index-keys next-level-subindex)
                         nil))))
    keys))

(defun gaf-arg-index-key-validator (term &optional argnum predicate mt)
  "[Cyc] Return T iff TERM, ARGNUM, PREDICATE, and MT are valid keys for the :GAF-ARG INDEX."
  (and (indexed-term-p term)
       (or (not argnum) (positive-integer-p argnum))
       (or (not predicate) (fort-p predicate))
       (or (not mt) (hlmt-p mt))))

(defun get-gaf-arg-subindex (term &optional argnum pred mt)
  "[Cyc] Return the subindex at TERM ARGNUM PRED MT. Return NIL if none present."
  (get-subindex term (list :gaf-arg argnum pred mt)))

(defun add-gaf-arg-index (term argnum pred mt assertion)
  (term-add-indexing-leaf term (list :gaf-arg argnum pred mt) assertion))

(defun rem-gaf-arg-index (term argnum pred mt assertion)
  (term-rem-indexing-leaf term (list :gaf-arg argnum pred mt) assertion))




;; NART arg index

(defun num-nart-arg-index (term &optional argnum func)
  "[Cyc] Return the number of #$termOfUnit gafs indexed off of TERM ARGNUM FUNC."
  (let ((num 0))
    ;; TODO - macro, similar to num-gaf-arg-index but different inc tests. Doesn't refer to macro helpers, so not sure exactly what the input here is supposed to be, and which of these usages share a macro.
    (if (simple-indexed-term-p term)
        (let ((count 0))
          (dolist (ass (do-simple-index-term-assertion-list term))
            (when (matches-nart-arg-index ass term argnum func)
              (incf count)))
          (setf num count))
        (setf num (if-let ((subindex (get-nart-arg-subindex term argnum func)))
                    (subindex-leaf-count subindex)
                    0)))
    num))

(defun key-nart-arg-index (term &optional argnum func)
  "[Cyc] Return a list of the keys to the next index level below TERM ARGNUM FUNC."
  (let ((keys nil))
    ;; TODO - macro
    (if (simple-indexed-term-p term)
        (let ((keys-accum nil))
          (dolist (ass (do-simple-index-term-assertion-list term))
            (setf keys-accum (simple-key-nart-arg-index ass keys-accum term argnum func)))
          (setf keys keys-accum))
        (setf keys (let ((next-level-subindex (get-nart-arg-subindex term argnum func)))
                     (if (intermediate-index-p next-level-subindex)
                         (intermediate-index-keys next-level-subindex)
                         nil))))
    keys))

(defun get-nart-arg-subindex (term &optional argnum func)
  "[Cyc] Return the subindex at TERM ARGNUM FUNC MT. Return NIL if none present."
  (get-subindex term (list :nart-arg argnum func)))




;; Predicate extent index

(defun num-predicate-extent-index (pred &optional mt)
  "[Cyc] Return the assertion count at PRED MT."
  (let ((num 0))
    ;; TODO - macro, as above
    (if (simple-indexed-term-p pred)
        (let ((count 0))
          (dolist (ass (do-simple-index-term-assertion-list pred))
            (when (matches-predicate-extent-index ass pred mt)
              (incf count)))
          (setf num count))
        (setf num (if-let ((subindex (get-predicate-extent-subindex pred mt)))
                    (subindex-leaf-count subindex)
                    0)))
    num))

(defun relevant-num-predicate-extent-index-with-cutoff (pred cutoff)
  "[Cyc] Compute the assertion count at relevant mts under PRED.
CUTOFF: non-negative-integer-p; a number beyond which to stop counting relevant assertions and just return CUTOFF."
  (let ((num 0))
    ;; TODO - macro, as above
    (cond
      ((all-mt-subindex-keys-relevant-p)
       (setf num (num-predicate-extent-index pred))
       (when (number-has-reached-cutoff? num cutoff)
         (setf num cutoff)))

      ((simple-indexed-term-p pred)
       (dolist (ass (do-simple-index-term-assertion-list pred))
         (unless (number-has-reached-cutoff? num cutoff)
           (when (and (matches-predicate-extent-index ass pred)
                      (relevant-mt? (assertion-mt ass)))
             (incf num)))))

      ;; TODO - degenerate constructs from macrogen
      (t (let ((good-key-count (number-of-non-null-args-in-order)))
           (if (= good-key-count (- 1 1))
               (when-let ((mt-subindex (get-predicate-extent-subindex pred)))
                 (setf num (relevant-mt-subindex-count-with-cutoff mt-subindex cutoff)))
               (missing-larkc 4722)))))
    num))

(defun key-predicate-extent-index (pred)
  "[Cyc] Return a list of the keys to the next predicate-extent index level below PRED."
  (let ((keys nil))
    ;; TODO - macro as above
    (if (simple-indexed-term-p pred)
        (let ((keys-accum nil))
          (dolist (ass (do-simple-index-term-assertion-list pred))
            (declare (ignore ass))
            (missing-larkc 30241))
          (setf keys keys-accum))
        (setf keys (let ((next-level-subindex (get-predicate-extent-subindex pred)))
                     (if (intermediate-index-p next-level-subindex)
                         (intermediate-index-keys next-level-subindex)
                         nil))))
    keys))

(defun* predicate-extent-top-level-key () (:inline t)
  :predicate-extent)

(defun add-predicate-extent-index (pred mt assertion)
  (term-add-indexing-leaf pred (list (predicate-extent-top-level-key) mt) assertion))

(defun rem-predicate-extent-index (pred mt assertion)
  (term-rem-indexing-leaf pred (list (predicate-extent-top-level-key) mt) assertion))

(defun get-predicate-extent-subindex (pred &optional mt)
  "[Cyc] Return the subindex at PRED MT, or NIL if none present."
  (get-subindex pred (list (predicate-extent-top-level-key) mt)))



;; Predicate rule index

(defun num-predicate-rule-index (pred &optional sense mt direction)
  "[Cyc] Return the raw assertion count at PRED SENSE MT DIRECTION."
  (let ((num 0))
    ;; TODO - macro as above. Starting to simplify, though, because it's annoying
    (if (simple-indexed-term-p pred)
        (dolist (ass (do-simple-index-term-assertion-list pred))
          (when (matches-predicate-rule-index ass pred sense mt direction)
            (incf num)))
        (when-let ((subindex (get-predicate-rule-subindex pred sense mt direction)))
          (setf num (subindex-leaf-count subindex))))
    num))

(defun relevant-num-predicate-rule-index (pred &optional sense)
  "[Cyc] Return the raw assertion count at relevant mts under PRED SENSE."
  (let ((num 0))
    ;; TODO - macro, simplified
    (cond
      ((all-mt-subindex-keys-relevant-p)
       (setf num (num-predicate-rule-index pred sense)))

      ((simple-indexed-term-p pred)
       (dolist (ass (do-simple-index-term-assertion-list pred))
         (when (and (matches-predicate-rule-index ass pred sense)
                    (relevant-mt? (assertion-mt ass)))
           (incf num))))

      (t (let ((good-key-count (number-of-non-null-args-in-order sense)))
           (if (= good-key-count (- 2 1))
               (when-let ((mt-subindex (get-predicate-rule-subindex pred sense)))
                 (missing-larkc 12809))
               (missing-larkc 4724)))))
    num))

(defun key-predicate-rule-index (pred &optional sense mt)
  "[Cyc] Return a list of the keys to the next index level below PRED SENSE MT."
  (let ((keys nil))
    ;; TODO - macro
    (if (simple-indexed-term-p pred)
        (dolist (ass (do-simple-index-term-assertion-list pred))
          (declare (ignore ass))
          (missing-larkc 30242))
        (let ((next-level-subindex (get-predicate-rule-subindex pred sense mt)))
          (when (intermediate-index-p next-level-subindex)
            (setf keys (intermediate-index-keys next-level-subindex)))))
    keys))

(defun get-predicate-rule-subindex (pred &optional sense mt direction)
  "[Cyc] Return NIL or subindex-p"
  (get-subindex pred (list :predicate-rule sense mt direction)))

(defun add-predicate-rule-index (pred sense mt direction assertion)
  (term-add-indexing-leaf pred (list :predicate-rule sense mt direction) assertion))

(defun rem-predicate-rule-index (pred sense mt direction assertion)
  (term-rem-indexing-leaf pred (list :predicate-rule sense mt direction) assertion))




;; Leftover pieces of others

(defun key-decontextualized-ist-predicate-rule-index (pred &optional sense)
  "[Cyc] Return a list of the keys to the next index level below PRED SENSE."
  (let ((keys nil))
    ;; TODO - macro
    (if (simple-indexed-term-p pred)
        (dolist (ass (do-simple-index-term-assertion-list pred))
          (declare (ignore ass))
          (missing-larkc 30235))
        (let ((next-level-subindex (get-decontextualized-ist-predicate-rule-subindex pred sense)))
          (when (intermediate-index-p next-level-subindex)
            (setf keys (intermediate-index-keys next-level-subindex)))))
    keys))

(defun get-decontextualized-ist-predicate-rule-subindex (pred &optional sense direction)
  "[Cyc] Return NIL or subindex-p"
  (get-subindex pred (list :decontextualized-ist-predicate-rule sense direction)))

(defun key-isa-rule-index (col &optional sense mt)
  "[Cyc] Return a list of the keys to the next index level below COL SENSE MT."
  (let ((keys nil))
    ;; TODO - macro
    (if (simple-indexed-term-p col)
        (dolist (ass (do-simple-index-term-assertion-list col))
          (declare (ignore ass))
          (missing-larkc 30239))
        (let ((next-level-subindex (get-isa-rule-subindex col sense mt)))
          (when (intermediate-index-p next-level-subindex)
            (setf keys (intermediate-index-keys next-level-subindex)))))
    keys))

(defun get-isa-rule-subindex (col &optional sense mt direction)
  "[Cyc] Return NIL or SUBINDEX-P"
  (get-subindex col (list :isa-rule sense mt direction)))

(defun num-quoted-isa-rule-index (col &optional sense mt direction)
  "[Cyc] Return the raw assertion count at COL SENSE MT DIRECTION."
  (let ((num 0))
    ;; TODO - macro
    (if (simple-indexed-term-p col)
        (dolist (ass (do-simple-index-term-assertion-list col))
          (when (matches-quoted-isa-rule-index ass col sense mt direction)
            (incf num)))
        (missing-larkc 12641))
    num))

(defun num-genls-rule-index (col &optional sense mt direction)
  "[Cyc] Return the raw assertion count at COL SENSE MT DIRECTION."
  (let ((num 0))
    ;; TODO - macro
    (if (simple-indexed-term-p col)
        (dolist (ass (do-simple-index-term-assertion-list col))
          (when (matches-genls-rule-index ass col sense mt direction)
            (incf num)))
        (when-let ((subindex (get-genls-rule-subindex col sense mt direction)))
          (setf num (subindex-leaf-count subindex))))
    num))

(defun key-genls-rule-index (col &optional sense mt)
  "[Cyc] Return a list of the keys to the next index level below COL SENSE MT."
  (let ((keys nil))
    ;; TODO - macro
    (if (simple-indexed-term-p col)
        (dolist (ass (do-simple-index-term-assertion-list col))
          (declare (ignore ass))
          (missing-larkc 30238))
        (let ((next-level-subindex (get-genls-rule-subindex col sense mt)))
          (when (intermediate-index-p next-level-subindex)
            (setf keys (intermediate-index-keys next-level-subindex)))))
    keys))

(defun get-genls-rule-subindex (col &optional sense mt direction)
  "[Cyc] Returns NIL or SUBINDEX-P"
  (get-subindex col (list :genls-rule sense mt direction)))

(defun add-genls-rule-index (col sense mt direction assertion)
  (term-add-indexing-leaf col (list :genls-rule sense mt direction) assertion))

(defun rem-genls-rule-index (col sense mt direction assertion)
  (term-rem-indexing-leaf col (list :genls-rule sense mt direction) assertion))

(defun key-genl-mt-rule-index (col &optional sense mt)
  "[Cyc] Return a list of the keys to the next index level below COL SENSE MT."
  (let ((keys nil))
    ;; TODO - macro
    (if (simple-indexed-term-p col)
        (dolist (ass (do-simple-index-term-assertion-list col))
          (declare (ignore ass))
          (missing-larkc 30237))
        (let ((next-level-subindex (get-genl-mt-rule-subindex col sense mt)))
          (when (intermediate-index-p next-level-subindex)
            (setf keys (intermediate-index-keys next-level-subindex)))))
    keys))

(defun get-genl-mt-rule-subindex (col &optional sense mt direction)
  "[Cyc] Returns NIL or SUBINDEX-P."
  (get-subindex col (list :genl-mt-rule sense mt direction)))

(defun key-function-rule-index (func &optional mt)
  "[Cyc] Return a list of the keys to the inext index level below FUNC MT."
  (let ((keys nil))
    ;; TODO - macro
    (if (simple-indexed-term-p func)
        (dolist (ass (do-simple-index-term-assertion-list func))
          (declare (ignore ass))
          (missing-larkc 30236))
        (let ((next-level-subindex (get-function-rule-subindex func mt)))
          (when (intermediate-index-p next-level-subindex)
            (setf keys (intermediate-index-keys next-level-subindex)))))
    keys))

(defun* function-rule-top-level-key () (:inline t)
  :function-rule)

(defun get-function-rule-subindex (func &optional mt direction)
  "[Cyc] Return NIL or SUBINDEX-P."
  (get-subindex func (list (function-rule-top-level-key) mt direction)))

(defun relevant-num-pragma-rule-index (rule)
  "[Cyc] Return the raw assertion count at relevant mts under RULE."
  (let ((num 0))
    ;; TODO - macro
    (cond
      ((all-mt-subindex-keys-relevant-p) (missing-larkc 12801))

      ((simple-indexed-term-p rule)
       (dolist (ass (do-simple-index-term-assertion-list rule))
         (when (and (matches-pragma-rule-index ass rule)
                    (relevant-mt? (assertion-mt ass)))
           (incf num))))

      (t (let ((good-key-count (number-of-non-null-args-in-order)))
           (if (= good-key-count (- 1 1))
               (missing-larkc 12737)
               (missing-larkc 4745)))))
    num))

(defun add-mt-index (term assertion)
  (unless (broad-mt? term)
    (add-mt-index-internal term assertion))
  ;; TODO - necessary return value?
  assertion)

(defun rem-mt-index (term assertion)
  (unless (broad-mt? term)
    (rem-mt-index-internal term assertion))
  ;; TODO - return value?
  assertion)

(defun* mt-top-level-key () (:inline t)
  :ist)

(defun add-mt-index-internal (term assertion)
  (term-add-indexing-leaf term (list (mt-top-level-key)) assertion))

(defun rem-mt-index-internal (term assertion)
  (term-rem-indexing-leaf term (list (mt-top-level-key)) assertion))

(defun broad-mt? (mt)
  (let ((monad (hlmt-monad-mt mt)))
    (when (fort-p monad)
      (broad-microtheory-p monad))))

(defun num-other-index (term)
  "[Cyc] Return the number of assertions at the other index for TERM."
  (let ((num 0))
    ;; TODO - macro
    (if (simple-indexed-term-p term)
        (dolist (ass (do-simple-index-term-assertion-list term))
          (when (matches-other-index ass term)
            (incf num)))
        (when-let ((subindex (get-other-subindex term)))
          (setf num (subindex-leaf-count subindex))))
    num))

(defun* other-top-level-key () (:inline t)
  :other)

(defun get-other-subindex (term)
  (term-complex-index-lookup term (other-top-level-key)))

(defun add-other-index (term assertion)
  (term-add-indexing-leaf term (list (other-top-level-key)) assertion))

(defun rem-other-index (term assertion)
  (term-rem-indexing-leaf term (list (other-top-level-key)) assertion))







;; Higher level interface?

(defun num-index (term)
  "[Cyc] The total number of assertions indexed from TERM."
  (if (simple-indexed-term-p term)
      (simple-num-index term)
      (complex-index-leaf-count (term-index term))))

(defun add-assertion-indices (assertion &optional term)
  (noting-terms-to-toggle-indexing-mode
    (when (valid-assertion-handle? assertion)
      (if (kb-gaf-assertion? assertion)
          (add-gaf-indices assertion term)
          (add-rule-indices assertion term)))))

(defun remove-assertion-indices (assertion &optional term)
  (noting-terms-to-toggle-indexing-mode
    (if (gaf-assertion? assertion)
        (remove-gaf-indices assertion term)
        (remove-rule-indices assertion term)))
  ;; TODO - return value?
  assertion)

;; TODO - this gathers lists and then calls tms-remove-assertion-list, instead of iterating without consing up that intermediate list. Look into it.
(defun remove-term-indices (term)
  "[Cyc] Remove all assertions about TERM from the KB. Return the TERM."
  (let ((*relevant-mt-function* #'relevant-mt-is-everything)
        (*mt* #$EverythingPSC)
        ;;(*ignore-warns?* t) ;; TODO - need this?
        )
    (tms-remove-assertion-list (gather-other-index term))
    (when (hlmt-p term)
      (tms-remove-assertion-list (gather-mt-index term)))
    (macrolet ((prefix (&body clauses)
                 `(progn
                    ,@(mapcar (lambda (clause) `(tms-remove-assertion-list ,clause)) clauses))))
      (prefix
       (gather-predicate-rule-index term :pos)
       (gather-predicate-rule-index term :neg)
       (gather-decontextualized-ist-predicate-rule-index term :pos)
       (gather-decontextualized-ist-predicate-rule-index term :neg)
       (gather-isa-rule-index term :neg)
       (gather-isa-rule-index term :pos)
       (gather-quoted-isa-rule-index term :neg)
       (gather-quoted-isa-rule-index term :pos)
       (gather-genls-rule-index term :neg)
       (gather-genls-rule-index term :pos)
       (gather-genl-mt-rule-index term :neg)
       (gather-genl-mt-rule-index term :pos)
       (gather-function-rule-index term)
       (gather-exception-rule-index term)
       (gather-pragma-rule-index term)))
    (when (fort-p term)
      (tms-remove-assertion-list (gather-predicate-extent-index term))
      (tms-remove-assertion-list (gather-function-extent-index term)))
    (dolist (argnum (sort (key-nart-arg-index term) #'>))
      (declare (ignore argnum))
      (missing-larkc 9445))
    (dolist (argnum (sort (key-gaf-arg-index term) #'>))
      (when (/= 1 argnum)
        (tms-remove-assertion-list (gather-gaf-arg-index term argnum nil nil nil))))
    (let ((isa-assertions (gather-gaf-arg-index term 1 #$isa nil nil))
          (genls-assertions (gather-gaf-arg-index term 1 #$genls nil nil))
          (tou-assertions (gather-gaf-arg-index term 1 #$termOfUnit nil nil))
          (arg1-assertions (gather-gaf-arg-index term 1 nil nil nil)))
      (dolist (assertion arg1-assertions)
        (when (and (valid-assertion assertion)
                   (not (or (member? assertion isa-assertions)
                            (member assertion genls-assertions)
                            (member assertion tou-assertions))))
          (tms-remove-assertion assertion)))
      (tms-remove-assertion-list genls-assertions)
      (tms-remove-assertion-list isa-assertions)
      (tms-remove-assertion-list tou-assertions))
    (let ((remaining-assertions (all-term-assertions term t)))
      (when remaining-assertions
        (warn "Indexing problem while removing ~s" term))
      (tms-remove-assertion-list remaining-assertions)))
  term)

(defun determine-formula-indices (formula)
  "[Cyc] Return 0: alist-p, a list of (argnum . term) pairs
Return 1: listp, a list of terms not indexed by any other argnum."
  (setf formula (ignore-sequence-vars formula))
  (let ((others nil)
        (pairs nil)
        (terms (formula-terms formula :ignore)))
    (dolistn (argnum arg terms)
      (if (valid-indexed-term? arg)
          (push (cons argnum arg) pairs)
          (setf others (nunion (tree-gather arg #'valid-fully-indexed-term-p) others))))
    (when others
      (setf others (loop for other in (fast-delete-duplicates others #'equal)
                      unless (member? other pairs #'equal #'cdr)
                      collect other)))
    (values (nreverse pairs) others)))

(defun determine-gaf-indices (formula mt)
  "[Cyc] Return 0: alist-p, a list of (argnum . term) pairs
Return 1: listp, a listof terms not indexed by any other argnum"
  (multiple-value-bind (argnum-pairs others) (determine-formula-indices formula)
    (unless (fort-p mt)
      (setf others (nunion (formula-gather mt #'fully-indexed-hlmt-term-p) others)))
    (values argnum-pairs others)))

(defun add-gaf-indices (assertion &optional term)
  (let ((literal (gaf-formula assertion))
        (mt (assertion-mt assertion)))
    (multiple-value-bind (alist others) (determine-gaf-indices literal mt)
      (let ((pred (cdr (assoc 0 alist))))
        (unless (and (hlmt-p mt)
                     (fort-p pred))
          (cerror "So don't!" "Don't know how to index ~s" assertion)
          (return-from add-gaf-indices nil))
        (when (or (not term)
                  (hlmt-equal term mt))
          (add-mt-index mt assertion))
        (when (or (not term)
                  (eq term pred))
          (add-predicate-extent-index pred mt assertion))
        (do-alist (argnum arg alist)
          (when (and (plusp argnum)
                     arg
                     (or (not term)
                         (equal term arg)))
            (add-gaf-arg-index arg argnum pred mt assertion)))
        (cond
          ((eq pred #$termOfUnit) (missing-larkc 12694))
          (t (dolist (fort others)
               (when (and (fully-indexed-term-p fort)
                          (or (not term)
                              (equal term fort)))
                 (add-other-index fort assertion)))))))))

(defun remove-gaf-indices (assertion &optional term)
  (let ((literal (gaf-formula assertion))
        (mt (assertion-mt assertion)))
    (multiple-value-bind (alist others) (determine-gaf-indices literal mt)
      (let ((pred (cdr (assoc 0 alist))))
        (unless (and (hlmt-p mt)
                     (fort-p pred))
          (cerror "So don't!" "Don't know how to index ~s" assertion)
          (return-from remove-gaf-indices nil))
        (when (or (not term)
                  (hlmt-equal term mt))
          (rem-mt-index mt assertion))
        (when (or (not term)
                  (eq term pred))
          (rem-predicate-extent-index pred mt assertion))
        (do-alist (argnum arg alist)
          (when (and (plusp argnum)
                     arg
                     (or (not term)
                         (equal term arg)))
            (rem-gaf-arg-index arg argnum pred mt assertion)))
        (if (eq pred #$termOfUnit)
            (missing-larkc 12831)
            (dolist (fort others)
              (when (and (fully-indexed-term-p fort)
                         (or (not term)
                             (equal term fort)))
                (rem-other-index fort assertion))))))))

(defun determine-rule-indices-int (asents sense)
  "[Cyc] Return 0: A list of pairs. The first element of each pair is the type of indexing (:pred, :ist-pred, :func, :isa, :genls, :genl-mt, :exception, or :pragma) and the second element of each pair is the term to be indexed with that type of indexing.
Return 1: A list of terms to be potentically indexed via 'other' indexing."
  (let ((pairs nil)
        (other nil))
    (macrolet ((do-push (test true-prefix &key (accessor '(sentence-arg2 asent)) no-else)
                 `(let ((term ,accessor))
                    (pushnew (if (,test term)
                                 (list ,true-prefix term)
                                 ,(when no-else
                                    `(list :pred pred)))
                             pairs :test #'equal))))
      (dolist (asent asents)
        (let ((pred (atomic-sentence-predicate asent)))
          (cond
            ((eq pred #$isa) (do-push fort-p :isa))
            ((eq pred #$quotedIsa) (do-push fort-p :quoted-isa))
            ((eq pred #$genls) (do-push fort-p :genls))
            ((eq pred #$genlMt) (do-push hlmt-p :genl-mt))
            ((and (eq sense :neg)
                  (eq pred #$termOfUnit))
             (let ((naut (sentence-arg2 asent)))
               (if (possibly-naut-p naut)
                   (do-push fort-p :func :accessor (nat-functor naut))
                   (pushnew (list :pred pred) pairs :test #'equal))))
            ((and (eq sense :pos)
                  (eq pred #$abnormal))
             (do-push assertion-p :exception))
            ((and (eq sense :pos)
                  (eq pred #$meetsPragmaticRequirement))
             (do-push assertion-p :pragma))
            ((eq pred #$ist) (do-push fort-p :ist-pred
                                      :no-else t
                                      :accessor (literal-predicate (sentence-arg2 asent))))
            ((fort-p pred) (pushnew (list :pred pred) pairs :test #'equal))))
        (setf other (nunion other
                            (tree-gather (sentence-args asent) #'fully-indexed-term-p))))
      (values pairs other))))

(defun determine-rule-indices (cnf)
  "[Cyc] Return 0: A list of pairs. The first element of each pair is the type of indexing (:pred, :func, :isa, :genls, :genl-mt, :exception, or :pragma), and the second element of each pair is the term to be indexed with that type of indexing. All these pairs occurred as neg-lits in CNF.
Return 1: A list of pairs that occurred as pos-lits in CNF.
Return 2: A list of terms to be indexed via 'other' indexing."
  (multiple-value-bind (neg-pairs neg-other) (determine-rule-indices-int (neg-lits cnf) :neg)
    (multiple-value-bind (pos-pairs pos-other) (determine-rule-indices-int (pos-lits cnf) :pos)
      (let* ((neg-terms (mapcar #'second neg-pairs))
             (pos-terms (mapcar #'second pos-pairs))
             (other (nset-difference (nset-difference (fast-delete-duplicates (nunion neg-other pos-other))
                                                      neg-terms)
                                     pos-terms)))
        (values neg-pairs pos-pairs other)))))

;; TODO - all this stuff might be better handled via defmethod?

(defun add-rule-indices (assertion &optional term)
  (let ((cnf (assertion-cnf assertion))
        (mt (assertion-mt assertion))
        (dir (assertion-direction assertion)))
    (multiple-value-bind (neg-pairs pos-pairs other) (determine-rule-indices cnf)
      (dolist (neg-pair neg-pairs)
        (destructuring-bind (neg-indexing-type neg-term) neg-pair
          (when (and (fully-indexed-term-p neg-term)
                     (or (not term)
                         (equal neg-term term)))
            (case neg-indexing-type
              ;; Shuffled the non-missing-larkc ones to the top
              (:pred (add-predicate-rule-index term :neg mt dir assertion))
              (:genls (add-genls-rule-index neg-term :neg mt dir assertion))
              (:ist-pred (missing-larkc 12690))
              (:func (missing-larkc 12695))
              (:isa (missing-larkc 12698))
              (:quoted-isa (missing-larkc 12701))
              (:genl-mt (missing-larkc 12696))
              (:pragma (cerror "So don't!" "Can't index a pragmatic requirement as a neg-lit ~s" assertion))
              (:exception (cerror "So don't!" "Can't index an exception as a neg-lit ~s" assertion))
              (otherwise (cerror "So don't!" "Don't know how to handle indexing type ~s" neg-indexing-type))))))
      (dolist (pos-pair pos-pairs)
        (destructuring-bind (pos-indexing-type pos-term) pos-pair
          (when (and (fully-indexed-term-p pos-term)
                     (or (not term)
                         (equal pos-term term)))
            (case pos-indexing-type
              ;; Shuffled the non-missing-larkc ones to the top
              (:pred (add-predicate-rule-index term :pos mt dir assertion))
              (:genls (add-genls-rule-index pos-term :pos mt dir assertion))
              (:ist-pred (missing-larkc 12691))
              (:isa (missing-larkc 12699))
              (:quoted-isa (missing-larkc 12702))
              (:genl-mt (missing-larkc 12697))
              (:pragma (missing-larkc 12700))
              (:exception (missing-larkc 12692))
              (:func (cerror "So don't!" "Can't index a function rule as a pos-lit ~s" assertion))
              (otherwise (cerror "So don't!" "Don't know how to handle indexing type ~s" pos-indexing-type))))))
      (dolist (other-term other)
        (when (and (fully-indexed-term-p other-term)
                   (or (not term)
                       (equal other-term term)))
          (add-other-index other-term assertion)))
      (when (and (hlmt-p mt)
                 (or (not term)
                     (hlmt-equal mt term)))
        (add-mt-index mt assertion)))
    (unless term
      (add-unbound-rule-indices assertion))))

(defun remove-rule-indices (assertion &optional term)
  (let ((cnf (assertion-cnf assertion))
        (mt (assertion-mt assertion))
        (dir (assertion-direction assertion)))
    (multiple-value-bind (neg-pairs pos-pairs other) (determine-rule-indices cnf)
      (dolist (neg-pair neg-pairs)
        (destructuring-bind (neg-indexing-type neg-term) neg-pair
          (when (and (fully-indexed-term-p neg-term)
                     (or (not term)
                         (equal neg-term term)))
            (case neg-indexing-type
              ;; Shuffled the non-missing-larkc ones to the top
              (:pred (rem-predicate-rule-index neg-term :neg mt dir assertion))
              (:genls (rem-genls-rule-index neg-term :neg mt dir assertion))
              (:ist-pred (missing-larkc 12827))
              (:isa (missing-larkc 12835))
              (:quoted-isa (missing-larkc 12838))
              (:genl-mt (missing-larkc 12833))
              (:func (missing-larkc 12832))
              (:pragma (cerror "So don't!" "Can't remove the index of a pragmatic requirement as a neg-lit ~s" assertion))
              (:exception (cerror "So don't!" "Can't remove the index of an exception as a neg-lit ~s" assertion))
              (otherwise (cerror "So don't!" "Don't know how to handle indexing type ~s" neg-indexing-type))))))
      (dolist (pos-pair pos-pairs)
        (destructuring-bind (pos-indexing-type pos-term) pos-pair
          (when (and (fully-indexed-term-p pos-term)
                     (or (not term)
                         (equal pos-term term)))
            (case pos-indexing-type
              (:pred (rem-predicate-rule-index pos-term :pos mt dir assertion))
              (:genls (rem-genls-rule-index pos-term :pos mt dir assertion))
              (:ist-pred (missing-larkc 12828))
              (:isa (missing-larkc 12836))
              (:quoted-isa (missing-larkc 12839))
              (:genl-mt (missing-larkc 12834))
              (:exception (missing-larkc 12829))
              (:pragma (missing-larkc 12837))
              (:func (cerror "So don't!" "Can't remove the index of a function rule as a pos-lit ~s" assertion))
              (otherwise (cerror "So don't!" "Don't know how to handle indexing type ~s" pos-indexing-type))))))
      (dolist (other-term other)
        (when (and (fully-indexed-term-p other-term)
                   (or (not term)
                       (equal other-term term)))
          (rem-other-index other-term assertion)))
      (when (and (hlmt-p mt)
                 (or (not term)
                     (hlmt-equal mt term)))
        (rem-mt-index mt assertion)))
    (unless term
      (rem-unbound-rule-indices assertion))))

(defun dependent-narts (fort)
  "[Cyc] Return a list of all current NARTs which are functions of FORT< or which have FORT as their functor."
  (let ((answer nil))
    (do-nart-arg-index (assertion fort)
      (push (gaf-arg1 assertion) answer))
    (do-function-extent-index (assertion fort)
      (push (gaf-arg1 assertion) answer))
    (do-other-index (assertion fort)
      (when (and (tou-assertion? assertion)
                 (expression-find fort (gaf-arg2 assertion) t))
        (push (gaf-arg1 assertion) answer)))
    (fast-delete-duplicates answer)))

(defun decent-rule-index (rule-cnf)
  "[Cyc] Return 0: The type of indexing: :pred-neg, :pred-pos, :ist-pred-neg, :ist-pred-pos, :func, :isa-neg, :isa-pos, :genls-neg, :genls-pos, :genl-mt-neg, :genl-mt-pos, :exception, :pragma, or :other.
Return 1: The term to be indexed with that type of indexing."
  (let ((best-type nil)
        (best-term nil)
        (best-total most-positive-fixnum))
    (multiple-value-bind (neg-pairs pos-pairs other) (determine-rule-indices rule-cnf)
      (dolist (pos-pair pos-pairs)
        ;; TODO - really, the above instances should do this as well. Grabbing first/second is faster than destructuring-bind
        (let ((pos-indexing-type (first pos-pair))
              (pos-term (second pos-pair)))
          (when (indexed-term-p pos-term)
            (let ((total (case pos-indexing-type
                           ;; Reordered
                           (:pred (num-predicate-rule-index pos-term :pos))
                           (:quoted-isa (num-quoted-isa-rule-index pos-term :pos))
                           (:genls (num-genls-rule-index pos-term :pos))
                           (:ist-pred (missing-larkc 12773))
                           (:isa (missing-larkc 12795))
                           (:genl-mt (missing-larkc 12791))
                           (:pragma (missing-larkc 12803))
                           (:exception (missing-larkc 12779))
                           (otherwise (cerror "So don't!" "Don't know how to handle indexing type ~s" pos-indexing-type)
                                      most-positive-fixnum))))
              (when (< total best-total)
                (setf best-total total
                      best-term pos-term
                      best-type (case pos-indexing-type
                                  (:pred :pred-pos)
                                  (:ist-pred :ist-pred-pos)
                                  (:isa :isa-pos)
                                  (:quoted-isa :quoted-isa-pos)
                                  (:genls :genls-pos)
                                  (:genl-mt :genl-mt-pos)
                                  (otherwise pos-indexing-type))))))))
      (dolist (neg-pair neg-pairs)
        (let ((neg-indexing-type (first neg-pair))
              (neg-term (second neg-pair)))
          (when (indexed-term-p neg-term)
            (let ((total (case neg-indexing-type
                           ;; Reordered
                           (:pred (num-predicate-rule-index neg-term :neg))
                           (:quoted-isa (num-quoted-isa-rule-index neg-term :neg))
                           (:genls (num-genls-rule-index neg-term :neg))
                           (:ist-pred (missing-larkc 12774))
                           (:isa (missing-larkc 12796))
                           (:genl-mt (missing-larkc 12792))
                           (:func (missing-larkc 12787))
                           (otherwise (cerror "So don't!" "Don't know how to handle indexing type ~s" neg-indexing-type)
			              most-positive-fixnum))))
              (when (< total best-total)
                (setf best-total total)
                (setf best-term neg-term)
                (setf best-type (case neg-indexing-type
                                  (:pred :pred-neg)
                                  (:ist-pred :ist-pred-neg)
                                  (:isa :isa-neg)
                                  (:quoted-isa :quoted-isa-neg)
                                  (:genls :genls-neg)
                                  (:genl-mt :genl-mt-neg)
                                  (otherwise neg-indexing-type))))))))
      (dolist (other-term other)
        (when (indexed-term-p other-term)
          (let ((total (num-other-index other-term)))
            (when (< total best-total)
              (setf best-total total
                    best-term other-term
                    best-type :other))))))
    (values best-type best-term)))

(defun* lookup-index-get-property (lookup-index indicator &optional default) (:inline t)
  (getf lookup-index indicator default))

(defun* lookup-index-set-property (lookup-index indicator value) (:inline t)
  "[Cyc] Usage: (csetq li (lookup-index-set-property li :foo 212)) ."
  (putf lookup-index indicator value))

(defun* lookup-index-get-type (lookup-index) (:inline t)
  (lookup-index-get-property lookup-index :index-type))

(defun lookup-index-gaf-arg-values (lookup-index)
  "[Cyc] Assumes LOOKUP-INDEX is of type :gaf-arg.
Return 0: term
Return 1: argnum
Return 2: predicate"
  (values (lookup-index-get-property lookup-index :term)
          (lookup-index-get-property lookup-index :argnum)
          (lookup-index-get-property lookup-index :predicate)))

;; TODO - probably a macro to expand these lookup-index-set-property chains, but there's only 2 uses so far, so whatever.

(defun lookup-index-for-predicate-extent (predicate)
  (let* ((lookup-index (lookup-index-set-property nil :index-type :predicate-extent))
         (lookup-index (lookup-index-set-property lookup-index :predicate predicate)))
    lookup-index))

(defun lookup-index-for-gaf-arg (best-term best-index-argnum index-pred)
  (let* ((lookup-index (lookup-index-set-property nil :index-type :gaf-arg))
         (lookup-index (lookup-index-set-property lookup-index :term best-term))
         (lookup-index (lookup-index-set-property lookup-index :argnum best-index-argnum))
         (lookup-index (lookup-index-set-property lookup-index :predicate index-pred)))
    lookup-index))

(defun lookup-methods-include? (index-type methods)
  "[Cyc] Return T iff INDEX-TYPE is allowable, according to METHODS."
  (or (not methods)
      ;; TODO - probably :test #'eq since they're all keywords?
      (member index-type methods)))

(defun best-gaf-lookup-index (asent truth &optional methods)
  "[Cyc] Returns a property list containing the property :index-type, which identifies which type of index is best for lookup of ASENT with TRUTH. The remaining elements on the plist are additional information pertaining to that type of index. A nil return value means that no possible index was found using the allowable methods.
METHODS: The allowable methods (index-types) that the function can return. If NIL, all methods are allowed."
  (cond
    ((or (lookup-methods-include? :predicate-extent methods)
         (lookup-methods-include? :gaf-arg methods))
     (best-gaf-lookup-index-try-all-allowed asent truth methods))
    
    ((missing-larkc 12767))
    ;;(missing-larkc 12754)
    ))

(defun num-best-gaf-lookup-index (asent truth &optional methods)
  (cond
    ((or (lookup-methods-include? :predicate-extent methods)
         (lookup-methods-include? :gaf-arg methods))
     (num-best-gaf-lookup-index-try-all-allowed asent truth methods))

    ((missing-larkc 12768))
    ;;(missing-larkc 5114)
    ;;(t 0)
    ))

(defun best-gaf-lookup-index-try-all-allowed (asent truth methods)
  (multiple-value-bind (best-fort
                        best-index-argnum
                        index-pred
                        best-count)
      (best-gaf-lookup-index-wrt-methods asent truth methods)
    (cond
      ((and (lookup-methods-include? :overlap methods)
            (lookup-should-use-index-overlap? asent best-count))
       (missing-larkc 12755))
      ((and (not best-fort)
            (not best-index-argnum)
            (not index-pred))
       nil)
      ((and (lookup-methods-include? :predicate-extent methods)
            (zerop best-index-argnum))
       (lookup-index-for-predicate-extent best-fort))
      ((and (lookup-methods-include? :gaf-arg methods)
            (positive-integer-p best-index-argnum))
       (lookup-index-for-gaf-arg best-fort best-index-argnum index-pred))
      (t nil))))

(defun num-best-gaf-lookup-index-try-all-allowed (asent truth methods)
  (multiple-value-bind (best-fort
                        best-index-argnum
                        index-pred
                        best-count)
      (best-gaf-lookup-index-wrt-methods asent truth methods)
    (declare (ignore best-fort
                     best-index-argnum
                     index-pred))
    (cond
      ((and (lookup-methods-include? :overlap methods)
            (lookup-should-use-index-overlap? asent best-count))
       (missing-larkc 5115))
      (t best-count))))

(defun best-gaf-lookup-index-wrt-methods (asent truth methods)
  (let ((tweaked-asent (if (and (lookup-methods-include? :predicate-extent methods)
                                (not (lookup-methods-include? :gaf-arg methods)))
                           (make-formula (atomic-sentence-predicate asent) nil)
                           asent)))
    (multiple-value-bind (best-term
                          best-index-argnum
                          index-pred
                          best-count)
        (best-gaf-lookup-index-int tweaked-asent truth)
      (if (and (not (and (lookup-methods-include? :gaf-arg methods)
                         (positive-integer-p best-index-argnum)))
               (not (and (lookup-methods-include? :predicate-extend methods)
                         (zerop best-index-argnum)))
               (and (lookup-methods-include? :gaf-arg methods)
                    (zerop best-index-argnum)))
          (values nil nil nil 0)
          (values best-term best-index-argnum index-pred best-count)))))

(defun best-gaf-lookup-index-int (asent truth)
  "[Cyc] Determine the best gaf lookup index of ASENT with truth value TRUTH.
First look for mt-insensitive counts, then, if not all mts are relevant, try to do better by finding a smaller mt-sensitive count, but use the min of the mt-insensitive counts as a cutoff so it won't waste time computing the relevance of things that aren't going to be any better."
  (declare (ignore truth))
  (multiple-value-bind (argnum-pairs others) (determine-formula-indices asent)
    (declare (ignore others))
    (let ((best-count nil)
          (best-fort nil)
          (best-argnum nil)
          (pred (cdr (assoc 0 argnum-pairs))))
      (if (fort-p pred)
          (setf best-fort pred
                best-count (num-predicate-extent-index pred)
                best-argnum 0)
          (setf pred nil))
      (do-alist (argnum arg argnum-pairs)
        (when (plusp argnum)
          (let ((num nil))
            (when (indexed-term-p arg)
              (setf num (if pred
                            (num-gaf-arg-index arg argnum pred)
                            (num-gaf-arg-index arg argnum)))
              (when (or (not best-fort)
                        (< num best-count))
                (setf best-count num)
                (setf best-fort arg)
                (setf best-argnum argnum))))))
      (unless (any-or-all-mts-are-relevant?)
        (when pred
          (do-alist (argnum arg argnum-pairs)
            (when (and (plusp argnum)
                       (indexed-term-p arg))
              (let ((arg-count (relevant-num-gaf-arg-index-with-cutoff arg best-count argnum pred)))
                (when (< arg-count best-count)
                  (setf best-count arg-count)
                  (setf best-fort arg)
                  (setf best-argnum argnum))))))
        (when (fort-p pred)
          (let ((pred-count (relevant-num-predicate-extent-index-with-cutoff pred best-count)))
            (when (< pred-count best-count)
              (setf best-fort pred
                    best-count pred-count
                    best-argnum 0)))))
      (values best-fort best-argnum pred best-count))))

;; TODO - probably unused in clyc
(deflexical *reindex-all-assertions-full-gc-threshold-constant-count* 10000
  "[Cyc] When there are more than 10k constant, call (gc-full) to remove old indexes from static space.")

(defparameter *warn-on-assertion-reindexing-errors?* nil
  "[Cyc] Controls whether the reindexing process complains about the indexing errors or just discards them silently.")

(defun* find-assertion (cnf mt) (:inline t)
  "[Cyc] Find the assertion in MT with CNF. Return NIL if not present."
  (kb-lookup-assertion cnf mt))

(defun find-assertion-internal (cnf mt)
  ;; TODO - MT macro? find all occurrences where *mt* is bound and compare to the java macro names
  (let ((*relevant-mt-function* #'relevant-mt-is-eq)
        (*mt* mt))
    (find-cnf cnf)))

(defun find-assertion-any-mt (cnf)
  "[Cyc] Find any assertion in any mt with CNF. Return NIL if none are present."
  ;; TODO - mt macro
  (let ((*relevant-mt-function* #'relevant-mt-is-everything)
        (*mt* #$EverythingPSC))
    (find-cnf cnf)))

(defun find-gaf (gaf-formula mt)
  "[Cyc] Find the assertion in MT with GAF-FORMULA as its formula. Return NIL if not present."
  ;; TODO - mt macro
  (let ((*relevant-mt-function* #'relevant-mt-is-eq)
        (*mt* mt))
    (find-gaf-formula gaf-formula)))

(defun find-gaf-any-mt (gaf-formula)
  "[Cyc] Find any assertion in any mt with GAF-FORMULA as its formula. Return NIL if not present."
  ;; TODO - mt macro
  (let ((*relevant-mt-function* #'relevant-mt-is-everything)
        (*mt* #$EverythingPSC))
    (find-gaf-formula gaf-formula)))

(defun* find-gaf-in-relevant-mt (gaf-formula) (:inline t)
  "[Cyc] Find any assertion in any currently relevant with GAF-FORMULA as its formula. Return NIL if not present."
  ;; TODO - supposed to be "in any currently relevant MT"?
  (find-gaf-formula gaf-formula))

(defun find-cnf (cnf)
  "[Cyc] Return an assertion which has CNF as its cnf or NIL if none present. Relevant mts are assumed scoped from the outside."
  (if (gaf-cnf? cnf)
      (find-gaf-cnf cnf)
      (find-rule-cnf cnf)))

(defun find-gaf-cnf (cnf)
  "[Cyc] Use the gaf indexing to find any assertion whose hl-cnf is CNF."
  (find-gaf-formula (cnf-to-gaf-formula cnf)))

(defun find-rule-cnf (cnf)
  "[Cyc] Use the rule indexing to find any assertion whose hl-cnf is CNF."
  (multiple-value-bind (index term) (decent-rule-index cnf)
    (find-rule-cnf-via-index-int cnf index term)))

(defun find-rule-cnf-via-index-int (cnf index term)
  (when (indexed-term-p term)
    ;; TODO - must be a better way than this weird catch stuff & dynamic bindings
    (let ((*mapping-target* cnf)
          (*mapping-answer* nil))
      ;; TODO - catch_var value from java is ignored?
      (catch :mapping-done
        (case index
          (:other (map-other-index #'find-cnf-internal term nil nil))
          (:pred-neg (map-predicate-rule-index #'find-cnf-internal term :neg))
          (:pred-pos (map-predicate-rule-index #'find-cnf-internal term :pos))
          (:ist-pred-neg (missing-larkc 9446))
          (:ist-pred-pos (missing-larkc 9447))
          (:isa-neg (missing-larkc 9463))
          (:isa-pos (missing-larkc 9464))
          (:genls-neg (missing-larkc 9460))
          (:genls-pos (missing-larkc 9461))
          (:genl-mt-neg (missing-larkc 9458))
          (:genl-mt-pos (missing-larkc 9459))
          (:func (missing-larkc 9451))
          (:exception (missing-larkc 9448))
          (:pragma (missing-larkc 9468))))
      *mapping-answer*)))

(defun find-cnf-internal (assertion)
  (when (and *mapping-target*
             (valid-assertion assertion))
    (let ((cnf (assertion-cnf assertion))
          (*candidate-assertion* assertion))
      (when (funcall *cnf-matching-predicate* cnf *mapping-target*)
        (setf *mapping-answer* assertion)
        ;; TODO - macro from utilities-macros?
        (mapping-finished)))))

(defun find-gaf-formula (gaf-formula)
  "[Cyc] Use the gaf indexing to find any assertion whose gaf formula is GAF-FORMULA, regardless of truth. If there are more than one, it will return an arbitrary one."
  ;; TODO - this is a bunch of nested macro helpers, which expands to quite a large java tree.  grepping seems to indicate it's used often enough to go through and break it all out.  just blindly transposed for now.
  (let* ((result nil)
         (l-index (best-gaf-lookup-index gaf-formula nil nil))
         (method (do-gli-extract-method l-index)))
    (case method
      (:gaf-arg (multiple-value-bind (term argnum predicate) (do-gli-vga-extract-keys l-index)
                  (cond
                    (argnum (if predicate
                                ;; TODO - iteration macro
                                (let ((pred-var predicate))
                                  (when (do-gaf-arg-index-key-validator term argnum pred-var)
                                    (let ((iterator-var (new-gaf-arg-final-index-spec-iterator term argnum pred-var))
                                          (done-var result)
                                          (token-var nil))
                                      (until done-var
                                        (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                                               (valid (not (eq token-var final-index-spec))))
                                          (when valid
                                            (let ((final-index-iterator nil))
                                              (unwind-protect (let ((done-var-28 result)
                                                                    (token-var-29 nil))
                                                                (setf final-index-iterator
                                                                      (new-final-index-iterator final-index-spec :gaf nil nil))
                                                                (until done-var-28
                                                                  (let* ((assertion (iteration-next-without-values-macro-helper
                                                                                     final-index-iterator token-var-29))
                                                                         (valid-30 (not (eq token-var-29 assertion))))
                                                                    (when valid-30
                                                                      (when-let ((possible-result (find-gaf-internal assertion gaf-formula)))
                                                                        (setf result possible-result)))
                                                                    (setf done-var-28 (or (not valid-30)
                                                                                          result)))))
                                                (when final-index-iterator
                                                  (destroy-final-index-iterator final-index-iterator)))))
                                          (setf done-var (or (not valid)
                                                             result)))))))
                                ;; not predicate
                                (let ((pred-var nil))
                                  (when (do-gaf-arg-index-key-validator term argnum pred-var)
                                    (let ((iterator-var (new-gaf-arg-final-index-spec-iterator term argnum pred-var))
                                          (done-var result)
                                          (token-var nil))
                                      (until done-var
                                        (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                                               (valid (not (eq token-var final-index-spec))))
                                          (when valid
                                            (let ((final-index-iterator nil))
                                              (unwind-protect (let ((done-var-31 result)
                                                                    (token-var-32 nil))
                                                                (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf nil nil))
                                                                (until done-var-31
                                                                  (let* ((assertion (iteration-next-without-values-macro-helper
                                                                                     final-index-iterator token-var-32))
                                                                         (valid-33 (not (eq token-var-32 assertion))))
                                                                    (when valid-33
                                                                      (when-let ((possible-result (find-gaf-internal assertion gaf-formula)))
                                                                        (setf result possible-result)))
                                                                    (setf done-var-31 (or (not valid-33)
                                                                                          result)))))
                                                (when final-index-iterator
                                                  (destroy-final-index-iterator final-index-iterator)))))
                                          (setf done-var (or (not valid)
                                                             result)))))))))
                    (predicate (let ((pred-var predicate))
                                 (when (do-gaf-arg-index-key-validator term nil pred-var)
                                   (let ((iterator-var (new-gaf-arg-final-index-spec-iterator term nil pred-var))
                                         (done-var result)
                                         (token-var nil))
                                     (until done-var
                                       (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                                              (valid (not (eq token-var final-index-spec))))
                                         (when valid
                                           (let ((final-index-iterator nil))
                                             (unwind-protect (let ((done-var-34 result)
                                                                   (token-var-35 nil))
                                                               (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf nil nil))
                                                               (until done-var-34
                                                                 (let* ((assertion (iteration-next-without-values-macro-helper final-index-iterator token-var-35))
                                                                        (valid-36 (not (eq token-var-35 assertion))))
                                                                   (when valid-36
                                                                     (when-let ((possible-result (find-gaf-internal assertion gaf-formula)))
                                                                       (setf result possible-result)))
                                                                   (setf done-var-34 (or (not valid-36)
                                                                                         result)))))
                                               (when final-index-iterator
                                                 (destroy-final-index-iterator final-index-iterator)))))
                                         (setf done-var (or (not valid)
                                                            result))))))))
                    (t (let ((pred-var nil))
                         (when (do-gaf-arg-index-key-validator term nil pred-var)
                           (let ((iterator-var (new-gaf-arg-final-index-spec-iterator term nil pred-var))
                                 (done-var result)
                                 (token-var nil))
                             (until done-var
                               (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                                      (valid (not (eq token-var final-index-spec))))
                                 (when valid
                                   (let ((final-index-iterator nil))
                                     (unwind-protect (let ((done-var-37 result)
                                                           (token-var-38 nil))
                                                       (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf nil nil))
                                                       (until done-var-37
                                                         (let* ((assertion (iteration-next-without-values-macro-helper final-index-iterator token-var-38))
                                                                (valid-39 (not (eq token-var-38 assertion))))
                                                           (when valid-39
                                                             (when-let ((possible-result (find-gaf-internal assertion gaf-formula)))
                                                               (setf result possible-result)))
                                                           (setf done-var-37 (or (not valid-39)
                                                                                 result)))))
                                       (when final-index-iterator
                                         (destroy-final-index-iterator final-index-iterator)))))
                                 (setf done-var (or (not valid)
                                                    result)))))))))))
      (:predicate-extent (let ((pred-var (do-gli-vpe-extract-key l-index)))
                           (when (do-predicate-extent-index-key-validator pred-var)
                             (let ((iterator-var (new-predicate-extent-final-index-spec-iterator pred-var))
                                   (done-var result)
                                   (token-var nil))
                               (until done-var
                                 (let* ((final-index-spec (iteration-next-without-values-macro-helper iterator-var token-var))
                                        (valid (not (eq token-var final-index-spec))))
                                   (when valid
                                     (let ((final-index-iterator nil))
                                       (unwind-protect (let ((done-var-40 result)
                                                             (token-var-41 nil))
                                                         (setf final-index-iterator (new-final-index-iterator final-index-spec :gaf nil nil))
                                                         (until done-var-40
                                                           (let* ((assertion (iteration-next-without-values-macro-helper final-index-iterator token-var-41))
                                                                  (valid-42 (not (eq token-var-41 assertion))))
                                                             (when valid-42
                                                               (when-let ((possible-result (find-gaf-internal assertion gaf-formula)))
                                                                 (setf result possible-result)))
                                                             (setf done-var-40 (or (not valid-42)
                                                                                   result)))))
                                         (when final-index-iterator
                                           (destroy-final-index-iterator final-index-iterator)))))
                                   (setf done-var (or (not valid)
                                                      result))))))))
      (:overlap (missing-larkc 5149))
      (otherwise (missing-larkc 30381)))
    result))

(defun find-gaf-internal (assertion sentence)
  (when (and sentence
             (valid-assertion assertion)
             (gaf-assertion? assertion))
    (let ((gaf (assertion-gaf-hl-formula assertion))
          (*candidate-assertion* assertion))
      (when (funcall *gaf-matching-predicate* gaf sentence)
        assertion))))

(defparameter *gathered-rule-assertions* nil
  "[Cyc] The list of gathered rule assertions.")
