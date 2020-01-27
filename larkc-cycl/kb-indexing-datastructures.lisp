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


(defun setup-indexing-tables (estimated-size)
  "[Cyc] Sets up all tables needed for the KB indexing. ESTIMATED-SIZE is the estimated # of constants."
  (let ((estimated-assertion-count (* 10 estimated-size)))
    (assertion-indexing-store-initialize estimated-assertion-count)))

(defglobal *assertion-indexing-store* nil
  "[Cyc] The mapping between assertions and their indices.")

(defun-inline assertion-indexing-store ()
  *assertion-indexing-store*)

(defun-inline assertion-indexing-store-reset (store)
  (setf *assertion-indexing-store* store))

(deflexical *meta-assertion-frequency* 0.015
  "[Cyc] The estimated percentage of assertions that have meta-assertions.")

(defun assertion-indexing-store-initial-size (&optional estimated-assertion-count)
  (if estimated-assertion-count
      (round (* estimated-assertion-count *meta-assertion-frequency*))
      (if (kb-loaded)
          (round (* (assertion-count) *meta-assertion-frequency*))
          64)))

(defun assertion-indexing-store-initialize (&optional estimated-assertion-count)
  (let ((initial-size (assertion-indexing-store-initial-size estimated-assertion-count)))
    (assertion-indexing-store-reset (make-hash-table initial-size #'eq)))
  ;; TODO - meaningful return value?
  *assertion-indexing-store*)

(defun-inline assertion-indexing-store-get (assertion)
  (gethash assertion *assertion-indexing-store* (new-simple-index)))

(defun assertion-indexing-store-set (assertion index)
  (if (eq index (new-simple-index))
      (missing-larkc 31914)
      (setf (gethash assertion *assertion-indexing-store*) index)))

(deflexical *unindexed-syntax-constants* (list #$implies
                                               #$and
                                               #$or
                                               #$not)
  "[Cyc] Constants which are part of the syntax and which therefore are not fully indexed.")

(defun unindexed-syntax-constant-p (object)
  "[Cyc] Return T iff OBJECT is a constants which is part of the syntax and therefore not fully indexed."
  (member-eq? object *unindexed-syntax-constants*))

(defun indexed-term-p (object)
  "[Cyc] Returns T iff OBJECT is an indexed CycL term, e.g. a fort or assertion."
  (or (reified-term-p object)
      (indexed-unrepresented-term-p object)))

(defun-inline indexed-unrepresented-term-p (object)
  "[Cyc] Returns T iff OBJECT is an indexed unrepresented CycL term, e.g., a string or number."
  (cycl-unrepresented-term-p object))

(defun valid-indexed-term? (object)
  "[Cyc] Returns T iff OBJECT is a valid indexed CycL term, i.e. a fort or an assertion."
  (cond
    ((fort-p object) (valid-fort? object))
    ((assertion-p object) (valid-assertion? object))
    ((indexed-unrepresented-term-p object) t)))

(defun fully-indexed-term-p (object)
  "[Cyc] Return T iff OBJECT is the type which will be indexed in the other index, if necessary."
  (and (indexed-term-p object)
       (unindexed-syntax-constant-p object)))

(defun valid-fully-indexed-term-p (object)
  "[Cyc] Return T iff OBJECT is the type which will be indexed in the other index, if necessary, and is valid."
  (and (valid-indexed-term? object)
       (not (unindexed-syntax-constant-p object))))

(defun term-index (term)
  (cond
    ((constant-p term) (when (valid-constant? term)
                         (constant-index term)))
   ((nart-p term) (missing-larkc 30881))
   ((assertion-p term) (assertion-index term))
   ((indexed-unrepresented-term-p term) (unrepresented-term-index term))
   ((auxiliary-index-p term) (get-auxiliary-index term))))

(defun reset-term-index (term index)
  "[Cyc] Primitively replaces TERM's index with INDEX."
  (cond
    ((fort-p term) (reset-fort-index term index))
   ((hlmt-p term) nil)
   ((assertion-p term) (reset-assertion-index term index))
   ((indexed-unrepresented-term-p term) (reset-unrepresented-term-index term index t))
   ((auxiliary-index-p term) (reset-auxiliary-index term index))
   (t (error "~s is not indexed" term)))
  ;; TODO - useful return value?
  term)

(defun free-index (index)
  "[Cyc] Frees all resources consumed by INDEX."
  (cond
   ((simple-index-p index) (missing-larkc 31925))
   ((complex-index-p index) (free-complex-index index))))

(defun free-term-index (term)
  "[Cyc] Frees all resources consumed by the index for TERM."
  (free-index (term-index term))
  (reset-term-index term (new-simple-index)))

(defun simple-index-p (object)
  "[Cyc] Return T iff OBJECT is a simple index."
  (and (listp object)
       (not (complex-index-p object))))

(defun-inline simple-indexed-term-p (term)
  (simple-index-p (term-index term)))

(defun-inline new-simple-index ()
  "[Cyc] Returns a new empty simple index."
  nil)

(defun-inline simple-num-index (term)
  (length (term-index term)))

(defun-inline simple-term-assertion-list (term)
  "[Cyc] Returns the list of all assertions referencing TERM.
Note: result is NOT destructible!"
  (term-index term))

(defun-inline do-simple-index-term-assertion-list (term)
  (simple-term-assertion-list term))

(defun-inline reset-term-simple-index (term simple-index)
  (reset-term-index term simple-index))

(defun-inline complex-index-p (object)
  (subindex-p object))

(defun-inline complex-index-leaf-count (complex-index)
  (subindex-leaf-count complex-index))

(defun-inline complex-index-lookup (complex-index key)
  "[Cyc] Returns NIL or subindex-p or indexing-leaf-p."
  (subindex-lookup complex-index key))

(defun term-complex-index-lookup (term key)
  "[Cyc] Returns NIL or subindex-p."
  (when-let ((index (term-index term)))
    (complex-index-lookup index key)))

(defun-inline initialize-term-complex-index (term)
  "[Cyc] Initializes a complex index for TERM. Clobbers any existing indexing for TERM."
  (initialize-term-subindex term))

(defun free-complex-index (complex-index)
  "[Cyc] Frees all resources consumed by COMPLEX-INDEX."
  (free-subindex complex-index))

(defun subindex-p (object)
  (or (intermediate-index-p object)
      (final-index-p object)))

(defun subindex-lookup (subindex key)
  "[Cyc] Returns NIL or subindex-p or indexing-leaf-p."
  (cond
    ((intermediate-index-p subindex) (intermediate-index-lookup subindex key))
    (t (missing-larkc 31922))))

(defun subindex-leaf-count (subindex)
  "[Cyc] Returns the number of indexing leaves anywhere below SUBINDEX."
  (if (intermediate-index-p subindex)
      (intermediate-index-leaf-count subindex)
      (final-index-leaf-count subindex)))

(defun-inline initialize-term-subindex (term)
  "[Cyc]Initializes a subindex for TERM. Clobbers any existing indexing for TERM."
  (initialize-term-intermediate-index term))

(defun free-subindex (subindex)
  "[Cyc] Frees all resources consumed by SUBINDEX."
  (cond
    ((intermediate-index-p subindex) (free-intermediate-index subindex))
    ((final-index-p subindex) (missing-larkc 31924))))

(defun intermediate-index-p (object)
  (and (consp object)
       (integerp (car object))
       (hash-table-p (cdr object))))

(defun-inline new-intermediate-index (test-function)
  (cons 0 (make-hash-table :test test-function)))

(defun-inline do-intermediate-index-valid-index-p (object)
  ;; TODO - Tests for non-NILness, which should be okay to just pass through.
  object)

(defun-inline intermediate-index-lookup (intermediate-index key)
  "[Cyc] Returns NIL or subindex-p."
  (gethash key (intermediate-index-dictionary intermediate-index)))

(defun-inline intermediate-index-keys (intermediate-index)
  "[Cyc] Returns a list of keys for INTERMEDIATE-INDEX."
  (hash-table-keys (intermediate-index-dictionary intermediate-index)))

(defun intermediate-index-leaf-count (intermediate-index)
  "[Cyc] Returns the number of indexing leaves anywhere below INTERMEDIATE-INDEX."
  (car intermediate-index))

(defun intermediate-index-set (intermediate-index key value)
  "[Cyc] Does not reset the counts."
  (intermediate-index-dictionary-set intermediate-index key value)
  ;; TODO - useful return value?
  intermediate-index)

(defun-inline intermediate-index-insert (intermediate-index keys leaf)
  "[Cyc] Returns whether it actually inserted (NIL if it was already there)."
  (intermediate-index-insert-int intermediate-index keys leaf nil))

(defun intermediate-index-insert-int (intermediate-index keys leaf key-history)
  "[Cyc] Insert LEAF at KEYS, having already gone down the keys in KEY-HISTORY."
  (destructuring-bind (key &rest rest-keys) keys
    (if rest-keys
        (let* ((new-key-history (nconc key-history (list key)))
               (subindex (intermediate-index-lookup-or-create-intermediate
                          intermediate-index key new-key-history)))
          (when (intermediate-index-insert-int subindex rest-keys leaf new-key-history)
            ;; TODO - could be a tail call if this always returns non-NIL
            (intermediate-index-leaf-count-inc intermediate-index 1)
            t))
        (let* ((subindex (intermediate-index-lookup-or-create-final intermediate-index key))
               (old-count (final-index-leaf-count subindex)))
          (final-index-insert subindex leaf)
          (let ((new-count (final-index-leaf-count subindex)))
            (unless (= old-count new-count)
              ;; TODO - could be a tail call if this always returns non-NIL
              (intermediate-index-leaf-count-inc intermediate-index 1)
              t))))))

(defun intermediate-index-delete (intermediate-index keys leaf)
  "[Cyc] Returns whether it actually deleted (nil if it was already gone)."
  (destructuring-bind (key &rest rest-keys) keys
    (let ((subindex (intermediate-index-lookup intermediate-index key)))
      (when subindex
        (prog1 (if rest-keys
                   (prog1-when (intermediate-index-delete subindex rest-keys leaf)
                     (intermediate-index-leaf-count-inc intermediate-index -1))
                   (let ((old-count (final-index-leaf-count subindex)))
                     (final-index-delete subindex leaf)
                     (let ((new-count (final-index-leaf-count subindex)))
                       (unless (= old-count new-count)
                         ;; TODO - could be a tail call if this always returns non-NIL
                         (intermediate-index-leaf-count-inc intermediate-index -1)
                         t))))
          (when (zerop (subindex-leaf-count subindex))
            (intermediate-index-delete-key intermediate-index key)))))))

(defun-inline intermediate-index-delete-key (intermediate-index key)
  "[Cyc] Delete any mapping from KEY to a subindex in INTERMEDIATE-INDEX."
  (intermediate-index-dictionary-delete-key intermediate-index key))

(defun-inline initialize-term-intermediate-index (term)
  "[Cyc] Initializes a top-level intermediate index for TERM. Clobbers any existing indexing for TERM."
  (reset-term-index term (new-intermediate-index #'eq)))

(defun-inline free-intermediate-index (intermediate-index)
  "[Cyc] Frees all resources consumed by INTERMEDIATE-INDEX."
  (clrhash (intermediate-index-dictionary intermediate-index)))

(defun-inline intermediate-index-leaf-count-reset (intermediate-index new-count)
  (rplaca intermediate-index new-count))

(defun intermediate-index-leaf-count-inc (intermediate-index delta)
  (let* ((old-count (intermediate-index-leaf-count intermediate-index))
         (new-count (+ old-count delta)))
    (intermediate-index-leaf-count-reset intermediate-index new-count)))

(defun intermediate-index-lookup-or-create-intermediate (intermediate-index key key-history)
  "[Cyc] Having already gone down the keys in KEY-HISTORY, look up KEY in INTERMEDIATE-INDEX.
If not found, create a new intermediate index for KEY, with an equality test determined from KEY-HISTORY."
  (or (intermediate-index-lookup intermediate-index key)
      (let* ((equality-test (index-equality-test-for-keys key-history))
             (subindex (new-intermediate-index equality-test)))
        (intermediate-index-set intermediate-index key subindex)
        subindex)))

(defun intermediate-index-lookup-or-create-final (intermediate-index key)
  (or (intermediate-index-lookup intermediate-index key)
      (let ((subindex (new-final-index)))
        (intermediate-index-set intermediate-index key subindex)
        subindex)))

(defun-inline intermediate-index-dictionary (intermediate-index)
  "[Cyc] Assumes INTERMEDIATE-INDEX is dictionary-style."
  (cdr intermediate-index))

(defun-inline intermediate-index-dictionary-set (intermediate-index key value)
  (setf (gethash key (intermediate-index-dictionary intermediate-index)) value))

(defun-inline intermediate-index-dictionary-delete-key (intermediate-index key)
  (remhash key (intermediate-index-dictionary intermediate-index)))

(defun-inline final-index-p (object)
  (set-p object))

(defun-inline new-final-index ()
  (new-set #'eq))

(defun-inline final-index-leaf-count (final-index)
  "[Cyc] Returns the number of indexing leaves in FINAL-INDEX."
  (set-size (final-index-set final-index)))

(defun-inline final-index-insert (final-index leaf)
  "[Cyc] Is not required to check for membership before insertion."
  (set-add leaf (final-index-set final-index)))

(defun-inline final-index-delete (final-index leaf)
  "[Cyc] Is not required to check for multiple elements to delete."
  (set-remove leaf (final-index-set final-index)))

(defun-inline final-index-set (final-index)
  "[Cyc] Returns the set datastructure in FINAL-INDEX. Currently a final index _is_ a set, so this is the identity function."
  final-index)
