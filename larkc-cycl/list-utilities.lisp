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

;; TODO - this file contains many macro & defun declarations that can be easily implemented from name.

;; TODO - This is used for cartesian sizing of duplicate detection.  Initially 80, this should probably be smaller.  Need to benchmark it.
(defparameter *magic-hashing-cutoff* 80
  "[Cyc] the cutoff beyond which it's more efficient to use a hashtable than a N^2 non-consing algorithm, used for the fast-* functions.")

  ;; ELIDED sublisp-boolean (deprecated in cycl)
  ;; ELIDED not-eq (deprecated in cycl)
  ;; ELIDED cdddr & cadar (native in cl)


(declaim (inline proper-subsetp)) ;; inlining so compiler can bake in test & key
(defun proper-subsetp (list1 list2 &optional (test #'eql) (key #'identity))
  "Standard SUBSETP returns true if the lists are similar. This ensures that list2 has additional elements."
  (and (subsetp list1 list2 :test test :key key)
       (not (subsetp list2 list1 :test test :key key))))

(defmacro scan-list-cells-with-index ((cell-var list index-var) &key cons end dotted)
  "Iterate a list, with a variable for the current cons cell and the 0-indexed index. The CONS form runs for cons cells, and RETURN can early exit it. The END form runs for the final NIL. The DOTTED form runs with a cell value of a final non-NIL cdr."
  (alexandria:with-gensyms (runner)
    `(block nil
       (labels ((,runner (,cell-var ,index-var)
                  (declare (fixnum ,index-var))
                  (cond
                    ((null ,cell-var) ,end)
                    ((consp ,cell-var) (progn
                                         ,cons
                                         (,runner (cdr ,cell-var) (1+ ,index-var))))
                    (t ,dotted))))
         (,runner ,list 0)))))

;; TODO - using a declared fixnum for sequence lengths, for performance.  Need to propagate this


(defun length< (seq n &optional count-dotted-list?)
  "[Cyc] Returns whether the length of SEQ is stricly less than N.  count-dotted-list? whether (1 2 . 3) counts as length 2 o r 3. If it is T, it counts as length 3, otherwise it counts as length 2."
  (declare (fixnum n))
  (if (listp seq)
      ;; Only bother traversing up to tested length
      (scan-list-cells-with-index (tail seq i)
                                  :cons (when (>= i n)
                                          (return nil))
                                  :end (< i n)
                                  :dotted (if count-dotted-list?
                                              (< (1+ i) n)
                                              t))
      (< (length seq) n)))

(declaim (inline length<=))
(defun length<= (seq n &optional count-dotted-list?)
  "[Cyc] Return whether the length of SEQ is less than or equal to N. count-dotted-list? whether (1 2 . 3) counts as length 2 or 3. If it is T, it counts as length 3, otherwise it counts as length 2."
  (length< seq (1+ n) count-dotted-list?))

(defun length= (seq n &optional count-dotted-list?)
  "[Cyc] Return whether the length of SEQ is exactly N. count-dotted-list? whether (1 2 . 3) counts as length 2 or 3. If it is T, it counts as length 3, otherwise it counts as length 2."
  (declare (fixnum n))
  (if (listp seq)
      (scan-list-cells-with-index (tail seq i)
                                  :end (= i n)
                                  :cons (when (= i n)
                                          (return nil))
                                  :dotted (if count-dotted-list?
                                              (= i (1+ n))
                                              (= i n)))
      (= (length seq) n)))

(declaim (inline length>))
(defun length> (seq n &optional count-dotted-list?)
  "[Cyc] Return whether the length of SEQ is strictly greater than N. count-dotted-list? whether (1 2 . 3) counts as length 2 or 3. If it is T, it counts as length 3, otherwise it counts as length 2."
  (not (length<= seq n count-dotted-list?)))

(declaim (inline length>=))
(defun length>= (seq n &optional count-dotted-list?)
  "[Cyc] Return whether the length of SEQ is greater than or equal to N. count-dotted-list? whether (1 2 . 3) counts as length 2 or 3. If it is T, it counts as length 3, otherwise it counts as length 2."
  (not (length< seq n count-dotted-list?)))

(defun greater-length-p (seq1 seq2)
  "[Cyc] Returns whether the first sequence is of greater length than the second sequence."
  (cond
    ((null seq1) nil)
    ((and (consp seq1)
          (consp seq2)) (progn
                          (mapl (lambda (c1 c2)
                                  (when (and (cdr c1)
                                             (not (cdr c2)))
                                    (return-from greater-length-p t)))
                                seq1 seq2)
                          ;; If we reached here, c1 finished without being longer than c2
                          nil))
    (t (> (length seq1) (length seq2)))))

(defun greater-or-same-length-p (seq1 seq2)
  "[Cyc] Returns whether the first sequence is of greater or equal length than the second sequence."
  (if (and (consp seq1)
           (consp seq2))
      (loop
         for c1 = seq1 then (cdr c1)
         for c2 = seq2 then (cdr c2)
         ;; Do a cheap test for the common continuing case, more expensive checking when this fails.
         while (and c1 c2)
         ;; The 2 valid true conditions
         finally (return (or (eq c1 c2)
                             (and c1 (not c2)))))
      (>= (length seq1) (length seq2))))

(defun same-length-p (seq1 seq2)
  ;; added
  (if (and (consp seq1)
           (consp seq2))
      (loop
         for c1 = seq1 then (cdr c1)
         for c2 = seq1 then (cdr c2)
         do (cond
              ((null c1) (return (null c2)))
              ((null c2) (return t))))
      (= (length seq1) (length seq2))))

(defun proper-list-p (obj)
  "Test for a non-NIL proper list."
  (and (consp obj)
       (null (cdr (last obj)))))

(defun dotted-list-p (obj)
  (and (consp obj)
       (cdr (last obj))))

(declaim (inline non-dotted-list-p))
(defun non-dotted-list-p (obj)
  "If this is true, it may be a proper list or not a list at all."
  ;; This doesn't need NIL checking as the original did. The logic works.
  (not (dotted-list-p obj)))

(defun dotted-length (cons)
  (scan-list-cells-with-index (cell cons index)
                              :end index
                              :dotted (1+ index)))

  ;; ELIDED *negated-test-func* and negated-test-func (local utils for below)
  ;; ELIDED remove-if-not, delete-if-not, find-if-not (standard CL)

  ;; ELIDED *position-if-binary-langda-func* *position-if-binary-lambda-arg2* (since position-if-binary is not included)
  ;; TODO - maybe implement this?  is this a binary search on a vector?


(defun recons (car cdr cons)
  "[Cyc] Cons a new pair only if the CAR or CDR of CONS are not EQ to CAR and CDR. See ncons for a destructive version of this."
  (if (and (eq car (car cons))
           (eq cdr (cdr cons)))
      cons
      (cons car cdr)))

(defun ncons (car cdr cons)
  "[Cyc] Return CONS after replacing its CAR and CDR. See recons for a non-destructive version of this."
  ;; Faster to just do it than test for eql first, although it might have minor GC implications for boxed values.
  (setf (car cons) car
        (cdr cons) cdr)
  cons)

(declaim (inline delete-first))
(defun delete-first (obj sequence &optional (test #'eql))
  (delete obj sequence :test test :count 1))

(declaim (inline nmapcar))
(defun nmapcar (function list)
  "[Cyc] A destructive version of MAPCAR. WARNING: this will produce really funky behavior if the elements of LIST share lsit structure."
  (mapl (lambda (cell)
          (rplaca cell (funcall function (car cell))))
        list))

(declaim (inline mapappend))
(defun mapappend (function list)
  "Appends together the results of the function on lists's elements."
  ;; TODO - pretty sure this is the intent?
  (mapcan function list))

(declaim (inline mapunion)) ;; compile the test in

(defun mapunion (function list &optional (test #'eql))
  "FUNCTION is applied to each element of LIST to get a new element X (which is also a list). Then we return the unique elements of X, concatenating over all elements of LIST."
  (let ((retval nil))
    (dolist (item list)
      (pushnew (funcall function item) retval :test test))
    retval))

(declaim (inline mapnunion)) ;; compile the test in

(defun mapnunion (function list &optional (test #'eql))
  (let ((retval nil))
    (dolist (item list)
      (setf retval (nunion retval (funcall function item) :test test)))
    retval))

(declaim (inline mapcar-product)) ;; chance to compile the function in

(defun mapcar-product (function list1 list2)
  "Returns a list of evaluated (function from-list-1 from-list-2) for the cartesian product of elements."
  (declare (list list1 list2))
  (let ((retval nil))
    (dolist (e1 list1)
      (dolist (e2 list2)
        (push (funcall function e1 e2) retval)))
    (nreverse retval)))

(declaim (inline last1))
(defun last1 (list)
  "[Cyc] Returns a list consisting of the last item in LIST."
  ;; Seems equivalent, even for dotted lists and NIL
  (last list))

(defun nadd-to-end (item list)
  "[Cyc] Returns the LIST with ITEM as the new last element. LIST may be destructively modified."
  (let ((new-cons (cons item nil)))
    (if list
        (progn
          (setf (cdr (last list)) new-cons)
          list)
        new-cons)))

(declaim (inline partition-list)) ;; chance to compile the function in

(defun partition-list (list func)
  "[Cyc] Return value 0 is a list of all elements of LIST which pass FUNC. Value 1 is a list of all elements of LIST which do not pass FUNC. Otherwise, order is preserved."
  (let ((passes nil)
        (fails nil))
    (dolist (item list)
      (if (funcall func item)
          (push item passes)
          (push item fails)))
    (values (nreverse passes)
            (nreverse fails))))

(defun find-all-if-not (test seq &optional (key #'identity))
  ;; TODO
  (declare (ignore test seq key))
  (missing-larkc 4774))

(defun only-one (list)
  "If there's only 1 element in the list, return it. Else, error."
  (must-not (cdr list) "~s was not a singleton" list)
  (car list))

(defun flatten (tree)
  "[Cyc] Non-recursive function which returns a list of the non-NIL atoms in TREE."
  ;; TODO - profile this and see how deep it gets. would this blow a stack or is consing faster than function call overhead?
  ;; in any case, this version conses half as much, as it passes the car of a list straight back without pushing it.
  ;; Using a vector should cons almost never, but as it needs to be adjustable the array operations don't inline
  (let (;; Enqueued items to processs
        (stack nil)
        ;; Off-stack item we're considering
        (current tree)
        (retval nil))
    (loop (progn
            ;; Find the next non-nil item to scan
            (until current
              ;; Bail if the stack is empty and we need another one
              (unless stack
                (return-from flatten (nreverse retval)))
              (setf current (pop stack)))
            
            (if (atom current)
                ;; Skip NILs
                (progn
                  (when current
                    (push current retval))
                  ;; Grab the next element
                  (setf current (pop stack)))
                ;; Sublist element
                (progn
                  (when (cdr current)
                    (push (cdr current) stack))
                  ;; Deal with the car without pushing it
                  (setf current (car current))))))))

(declaim (inline first-n))
(defun first-n (n list)
  "[Cyc] Return the first N elements of LIST."
  (declare (list list))
  (subseq list 0 n))

(defun nreplace-nth (n new list)
  "[Cyc] Replace the Nth item of LIST with NEW (destructive). This is a safer version of set-nth"
  (declare (integer n)
           (list list))
  (when (>= n 0)
    (let ((sublist (nthcdr n list)))
      (when sublist
        (rplaca sublist new))))
  list)

(defun replace-nth (n new list)
  "[Cyc] Replace the Nth item of LIST with NEW (nondestructive)"
  ;; TODO - can we do tail sharing here?  no reason to copy the entire list, unless that's the actual assumption
  (nreplace-nth n new (copy-list list)))

(defun num-list (num &optional (start 0))
  "[Cyc] Returns a list of length NUM containing the integers START to NUM-1+START. Note the list returned by this function is cached, so unless you're sure it won't be modified, use NEW-NUM-LIST instead. Note if you're getting a list of numbers just to iterate over it, use DO-NUMBERS instead."
  ;; TODO - monitor how large NUM gets, if it's sensible to just let it cons, because the verification check is quite expensive anyway; there is no fast path.
  (let ((candidate (num-list-cached num start)))
    (if (verify-num-list candidate num start)
        candidate
        ;; TODO - certainly we can figure out what to fill in here, to repopulate the cache for the mutated candidate
        (progn
          (missing-larkc 9349)
          ;;(num-list-cached num start)
          ))))

(defun new-num-list (num &optional (start 0))
  "[Cyc] Returns a list of length NUM containing the integers START to NUM-1+START. Note the list returned by this function is NOT cached, so if you're sure it won't be modified and you're going to be calling this a lot with the same arguments, use NUM-LIST or DO-NUMBERS instead."
  (declare (fixnum num start))
  (loop for i fixnum from start below (the fixnum (+ num start))
     collect i))

(defun verify-num-list (num-list length start)
  "[Cyc] Returns boolean: Is NUM-LIST a list of length LENGTH containing the integers START to LENGTH-1+START?"
  (and (listp num-list)
       (integerp length)
       (integerp start)
       ;; Might as well combine length check with the value checking
       (do ((len-count length (1- len-count))
            (val-count start (1+ val-count))
            (cell num-list (cdr cell)))
           (nil)
         (declare (fixnum len-count val-count))
         (cond
           ((null cell) (return (= 0 len-count)))
           ((= 0 len-count) (return nil))
           ((/= val-count (car cell)) (return nil))))))

  ;; TODO - see where these are actually used.  If fixnums are simply iterated, then do that instead of this mess.

(defun-memoized num-list-cached (num start)
    (:doc "[Cyc] Returns a list of length NUM containing the integers START to NUM-1+START. Note we cache this to save space, not time.")
  (new-num-list num start))

(declaim (inline numlist))
(defun numlist (length &optional (start 0))
  "[Cyc] Returns a list of length LENGTH containing the numbers 0 + START to LENGTH-1 + START."
  (num-list length start))

(declaim (inline member-eq?))
(defun member-eq? (item list)
  "[Cyc] An optimized form of (member? item list #'eq)"
  (member item list :test #'eq))

(defun member-equal (item list)
  "[Cyc] An optimized form of (member? ITEM LIST #'equal)"
  (member item list :test #'equal))

(defun singleton? (list)
  (and (consp list)
       (null (cdr list))))

(defun doubleton? (list)
  (and (consp list)
       (consp (cdr list))
       (null (cddr list))))

(defun triple? (list)
  (and (consp list)
       (consp (cddr list))
       (null (cdddr list))))

(declaim (inline duplicates?))
(defun duplicates? (list &optional (test #'eql) (key #'identity))
  "Boolean for if the list contains duplicates."
  (do ((cell list (cdr cell)))
      ((null cell))
    (when (member (car cell) (cdr cell) :test test :key key)
      (return t))))

(declaim (inline duplicates))
(defun duplicates (list &optional (test #'eql) (key #'identity))
  "Returns a list of any unique duplicates found"
  (let ((retval nil))
    (do* ((cell list (cdr cell))
          (car (car cell) (car cell)))
         ((null cell))
      (when (member car (cdr cell) :test test :key key)
        (pushnew car retval :test test :key key)))
    retval))

(declaim (inline sets-equal?))
(defun sets-equal? (set1 set2 &optional (test #'eql))
  (or (equal set1 set2)
      (and (subsetp set1 set2 :test test)
           (subsetp set2 set1 :test test))))

(declaim (inline multisets-equal?))
(defun multisets-equal? (set1 set2 &optional (test #'eql))
  (and (same-length-p set1 set2)
       ;; Ensure the count of each element from set1 matches set2
       (not (dolist (item set1)
              (unless (= (count item set1 :test test)
                         (count item set2 :test test))
                (return nil))))
       ;; And ensure the set of unique elements are the same
       (sets-equal? set1 set2 test)))

(defun fast-sets-equal? (set1 set2 &optional (test #'eql))
  (and (fast-subset? set1 set2 test)
       (fast-subset? set2 set1 test)))

(defun list-to-hashset (list &optional (test #'eql))
  "Returns a hashtable of element->T for all elements in the list."
  ;; new, seems related to the larkc missing bodies
  (let ((ht (make-hash-table :test test)))
    (dolist (element list)
      (setf (gethash element ht) t))
    ht))

(defun fast-subset? (list1 list2 &optional (test #'eql))
  "This version builds a hashtable instead of member to perform the search, but only if the lists aren't short"
  ;; Only checking if list2 is short, as it constructs the reference hashtable
  (if (length< list2 *magic-hashing-cutoff*)
      (subsetp list1 list2 :test test)
      (when (greater-or-same-length-p list2 list1)
        (let ((hash2 (list-to-hashset list2 test)))
          (every (lambda (e1) (gethash e1 hash2)) list1)))))

(declaim (inline ordered-union))
(defun ordered-union (set1 set2 &optional (test #'eql) (key #'identity))
  "[Cyc] Like UNION only the result preserves the order of elements in the input sets."
  ;; ordering is set1, then all new elements of set2 in their order
  ;; if set2 is a multiset, then all its elements not in set1 also pass through
  (let ((tail nil))
    (dolist (element set2)
      (unless (member element set1 :test test :key key)
        (push element tail)))
    (append set1 (nreverse tail))))

(declaim (inline ordered-set-difference))
(defun ordered-set-difference (list1 list2 &optional (test #'eql) (key #'identity))
  "[Cyc] Like SET-DIFFERENCE except the order of returned items is the same order as in LIST1."
  (let ((result nil))
    (dolist (element list1)
      (unless (member element list2 :test test :key key)
        (push element result)))
    (nreverse result)))

  ;; This is starting to get pretty big, but needed for the shorter list fastpath TEST optimization

(declaim (inline fast-set-difference))
(defun fast-set-difference (list1 list2 &optional (test #'eql))
  "[Cyc] Like SET-DIFFERENCE except not slow."
  (if (length< list2 *magic-hashing-cutoff*)
      (set-difference list1 list2 :test test)
      ;; This was missing-larkc, but easy enough to infer what it does
      (let ((hash2 (list-to-hashset list2))
            (result nil))
        (dolist (e1 list1)
          (unless (gethash e1 hash2)
            (push e1 result)))
        (nreverse result))))

(defun flip-cons (cons)
  (cons (car cons) (cdr cons)))

(defun flip-alist (alist)
  ;; Couldn't get a reference to flip-cons to inline
  (mapcar (lambda (cons)
            (cons (car cons) (cdr cons)))
          alist))

(defun self-evaluating-form (object)
  "[Cyc] Return T iff evaluation of OBJECT necessarily returns OBJECT."
  ;; TODO - not sure if this is 100% congruent with common lisp's evaluation.  Find where else it's used.
  (and (atom object)
       (or (eq nil object)
           (eq t object)
           (keywordp object)
           (not (symbolp object)))))

(defun quotify (object)
  "[Cyc] Return an expression which, if evaluated, would return OBJECT."
  (if (self-evaluating-form object)
      object
      (list 'quote object)))

(defun splice-into-sorted-list (object sorted-list predicate &optional (key #'identity))
  "[Cyc] Splice OBJECT into SORTED-LIST sorted by PREDICATE."
  (declare (list sorted-list))
  (let ((obj-key (funcall key object)))
    (do ((cell sorted-list (cdr cell))
         (prev nil cell))
        ((cond
           ;; Insert cell at the end of the list, if it wasn't found
           ((null cell) (return (if prev
                                    (progn
                                      (rplacd prev (cons object cell))
                                      sorted-list)
                                    (cons object sorted-list))))
           ;; Insert cell before the predicate-matched object
           ((funcall predicate obj-key (funcall key (car cell)))
            (if prev
                (progn
                  (rplacd prev (cons object cell))
                  (return sorted-list))
                (return (cons object sorted-list)))))))))

(defun tree-funcall-if (test fn object &optional (key #'identity))
  (if (funcall test (funcall key object))
      (funcall fn object)
      (when (consp object)
        (tree-funcall-if test fn (car object) key)
        (tree-funcall-if test fn (cdr object) key))))

  ;; TODO - I think these aren't for fast-delete, or else they'd be called delete-duplicates-*?

(deflexical *remove-duplicates-eq-table* (make-hash-table :test #'eq))
(deflexical *remove-duplicates-eql-table* (make-hash-table :test #'eql))
(deflexical *remove-duplicates-equal-table* (make-hash-table :test #'equal))
(deflexical *remove-duplicates-equalp-table* (make-hash-table :test #'equalp))
(deflexical *remove-duplicates-eq-table-lock* (bt:make-lock "remove-duplicates-eq-table-lock"))
(deflexical *remove-duplicates-eql-table-lock* (bt:make-lock "remove-duplicates-eql-table-lock"))
(deflexical *remove-duplicates-equal-table-lock* (bt:make-lock "remove-duplicates-equal-table-lock"))
(deflexical *remove-duplicates-equalp-table-lock* (bt:make-lock "remove-duplicates-equalp-table-lock"))

(defun fast-delete-duplicates (sequence &optional (test #'equal) (key #'identity) hashtable (start 0) end)
  (cond
    ((length<= sequence 1) sequence)
    ;; TODO - there was other branching that did the plain delete-duplicates, avoiding the hashtable version.
    ;;   suspecting it was something like this start/end detection, but there could be different stuff, too.
    ((or end
         (eq 0 start)
         (length<= sequence *magic-hashing-cutoff*))
     (delete-duplicates sequence :test test :key key :start start :end end))
    ;; TODO - Assuming this is what we do with the hashtable parameter
    (t (let ((ht (or (and hashtable (clrhash hashtable))
                     (make-hash-table :test test)))
             ;; TODO - this isn't actually destructive.  but the comparisons should be faster for long lists.
             (result nil))
         (map nil (lambda (element)
                    (let ((e-key (funcall key element)))
                      (unless (gethash e-key ht)
                        (setf (gethash e-key ht) t)
                        (push element result))))
              sequence)
         (nreverse result)))))

(defun remove-duplicate-forts (forts)
  "[Cyc] Return the list FORTS with all duplicates removes"
  (fast-delete-duplicates forts #'eq))

(defun delete-duplicate-forts (forts)
  "[Cyc] Return the list FORTS with all duplicates descructively removed"
  ;; TODO - if we implement a hash-based fast-delete-duplicates, then use that
  (fast-delete-duplicates forts #'eq))

(defun delete-duplciates-sorted (sorted-list &optional (test #'eql))
  "[Cyc] Deletes duplicates from SORTED-LIST."
  ;; This works on the assumption that similar elements will be consecutive,
  ;; so it only compares a element to the one prior.
  (do* ((last-cons sorted-list)
        (this-cons (cdr sorted-list) (cdr this-cons)))
       ((null this-cons))
    (if (funcall test (car this-cons) (car last-cons))
        (rplacd last-cons (cdr this-cons))
        (setf last-cons this-cons)))
  sorted-list)

(declaim (inline alist-p))
(defun alist-p (object)
  "[Cyc] Return T iff OBJECT is an association list."
  (listp object))

(declaim (inline alist-lookup))
(defun alist-lookup (alist key &optional (test #'eql) default)
  "[Cyc] Return the value associated with KEY in ALIST (using TEST for key equality).
   Return DEFAULT if KEY is not present.
   Return a second value of T iff KEY was found."
  (let ((pair (assoc key alist :test test)))
    (if pair
        (values (cdr pair) t)
        (values default nil))))

(declaim (inline alist-lookup-without-values))
(defun alist-lookup-without-values (alist key &optional (test #'eql) default)
  "[Cyc] Return the value assocaited with KEY in ALIST (using TEST for key equality)
   Return DEFAULT if KEY is not present.
   Unlike ALIST-LOOKUP, only 1 value is returned."
  (let ((pair (assoc key alist :test test)))
    (if pair
        (cdr pair)
        default)))

(declaim (inline alist-has-key-p))
(defun alist-has-key-p (alist key &optional (test #'eql))
  "[Cyc] Returns wheher KEY is a key in the association list ALIST."
  (member alist key :test test))

(defun alist-enter (alist key value &optional (test #'eql))
  "[Cyc] Note that VALUE is associated with KEY in ALIST (using TEST for key equality).
   Return the resulting alist.
   Return a second value of T iff KEY was found."
  (declare (list alist))
  (let ((pair (assoc key alist :test test)))
    (if pair
        (rplacd pair value)
        (setf alist (acons key value alist)))
    (values alist pair)))

(defun alist-enter-without-values (alist key value &optional (test #'eql))
  "[Cyc] Note that VALUE is assocaited with KEY in ALIST (using TEST for key equality).
   Return the resulting alist.
   Unlike ALIST-ENTER, only 1 value is returned."
  (let ((pair (assoc key alist :test test)))
    (if pair
        (progn
          (rplacd pair value)
          alist)
        (acons key value alist))))

(defun alist-delete (alist key &optional (test #'eql))
  "[Cyc] Delete any association for KEY in ALIST (using TEST for key equality).
   Return the resulting alist.
   Return a second value of T iff KEY was found."
  ;; TODO - this can be done in 1 pass, current code does 2
  (let ((pair (assoc key alist :test test)))
    (if pair
        (values (delete-first pair alist #'eq) t)
        (values alist nil))))

(defun alist-push (alist key value &optional (test #'eql))
  "[Cyc] Note that VLUE is in a list associated with KEY in ALIST (using TEST for key equality).
   Return the resulting alist.
   Return a second value of T iff KEY was found."
  (let ((pair (assoc key alist :test test)))
    (if pair
        (rplacd pair (cons value (rest pair)))
        (setf alist (acons key value alist)))
    (values alist pair)))

(declaim (inline alist-keys))
(defun alist-keys (alist)
  "[Cyc] Return a list of all the keys of ALIST."
  (mapcar #'car alist))

(declaim (inline alist-values))
(defun alist-values (alist)
  "[Cyc] Return a list of all the values of ALIST."
  (mapcar #'cdr alist))

(defun alist-optimize (alist predicate)
  "[Cyc] Return a copy of ALIST where the order of the keys have been optimized (sorted) via the preference PREDICATE."
  ;; TODO - the original made a copy of the return value.
  ;;  No clue why, as we're sorting a copy of the list anyway.
  (stable-sort (copy-list alist) predicate :key #'car))

(defun alist-to-hash-table (alist &optional (test #'eql))
  "[Cyc] Return a hashtable fo all the (key . value) entries in ALIST.
   TEST is the equality test for keys in ALIST."
  (let ((hashtable (make-hash-table :test test :size (length alist))))
    (dolist (cons alist)
      (setf (gethash (car cons) hashtable) (cdr cons)))
    hashtable))

(defun alist-to-reverse-hash-table (alist &optional (test #'eql))
  "[Cyc] Return a hashtable of all the (key . value) entries in ALIST using the value as the key and the key as the value.
   TEST is the equality test for values in ALIST."
  (let ((hashtable (make-hash-table :test test :size (length alist))))
    (dolist (cons alist)
      (setf (gethash (cdr cons) hashtable) (car cons)))))

(defun filter-plist (plist pred)
  "[Cyc] Creates a new plist based on PLIST, but only including properties which pass PRED."
  ;; PRED is called just on the key, not the value
  (let ((new-plist nil))
    (do-plist (key value plist)
      (when (funcall pred key)
        (setf new-plist (cons key (cons value new-plist)))))
    (nreverse new-plist)))

(defun nmerge-plist (plist-a plist-b)
  "[Cyc] Place all of the values of list B onto list A destructively and return the resulting PLIST."
  (cond
    ((not plist-b) plist-a)
    ((not plist-a) (copy-list plist-b))
    (t (do-plist (key value plist-b)
         (setf (getf plist-a key) value))
       plist-a)))

(defun merge-plist (plist-a plist-b)
  "[Cyc] Place all of the values of list B onto a copy of list A and return the resulting PLIST."
  (nmerge-plist (copy-list plist-a) plist-b))

(defparameter *plistlist-sort-indicator* nil)

;; TODO - what's the difference between this and SOME?



(declaim (inline any-in-list)) ;; should collapse into being pretty small
(defun any-in-list (predicate list &optional (key #'identity))
  (let ((ans nil))
    (if (eq key #'identity)
        (csome (item list ans)
          (setf ans (funcall predicate item)))
        (csome (item list ans)
          (setf ans (funcall predicate (funcall key item)))))
    ans))

(declaim (inline every-in-list))
(defun every-in-list (predicate list &optional (key #'identity))
  (let ((ans nil))
    (if (eq key #'identity)
        (csome (item list ans)
          (setf ans (not (funcall predicate item))))
        (csome (item list ans)
          (setf ans (not (funcall predicate (funcall key item))))))
    (not ans)))

(defparameter *subseq-subst-recursive-answers* nil)

(defun-inline extremal (list test &optional (key #'identity))
  "[Cyc] Return the first item in LIST which maximizes TEST."
  (when list
    (let ((best (first list)))
      (if (eq key #'identity)
          (dolist (item (rest list))
            (when (funcall test item best)
              (setf best item)))
          (dolist (item (rest list))
            (when (funcall test (funcall key item) (funcall key best))
              (setf best item))))
      best)))

(defun position-< (item1 item2 guide-seq &optional (test #'eql) (key #'identity))
  "[Cyc] Return T iff the position of ITEM1 in GUIDE-SEQ is less than that of ITEM2.
All objects not in the GUIDE-SEQ are considered to be after all those that are,
and are themselves considered equivalent by this test."
  (let ((position1 (position item1 guide-seq :test test :key key))
        (position2 (position item2 guide-seq :test test :key key)))
    (if position1
        (if position2
            (< position1 position2)
            t)
        nil)))

(defparameter *sort-via-position-guide* nil)
(defparameter *sort-via-position-test* nil)

(defun sort-via-position (seq guide-seq &optional (test #'eql) (key #'identity))
  "[Cyc] Sort SEQ using GUIDE-SEQ as a positional guide.
Objects in GUIDE-SEQ appear in the order they are in GUIDE-SEQ.
Objects not in GUIDE-SEQ all appear after those that do."
  (let ((*sort-via-position-guide* guide-seq)
        (*sort-via-position-test* test))
    (sort seq #'sort-via-position-earlier :key key)))

(defun stable-sort-via-position (seq guide-seq &optional (test #'eql) (key #'identity))
  (let ((*sort-via-position-guide* guide-seq)
        (*sort-via-position-test* test))
    (stable-sort seq #'sort-via-position-earlier :key key)))

(defun sort-via-position-earlier (item1 item2)
  (position-< item1 item2 *sort-via-position-guide* *sort-via-position-test*))

(defparameter *sort-via-test-function* nil)

(defun safe-= (object1 object2)
  (when (and (numberp object1)
             (numberp object2))
    (= object1 object2)))

(defun parameterized-median (list sort-pred)
  "[Cyc] Returns the median of a list, after sorting by SORT-PRED.
   It picks the larger if the median falls in the middle.
   Example: (parameterized-median '(1 2 3 4 5) #'<) 3
   Example: (parameterized-median '(4 1 2 5 3) #'<) 3
   Example: (parameterized-median '(4 1 2 5)   #'<) 4"
  (let ((sorted-list (sort (copy-list list) sort-pred)))
    (nth (floor (length sorted-list) 2) sorted-list)))

(defun tree-find (item object &optional (test #'eql) (key #'identity))
  "[Cyc] For the first sub-object in OBJECT (including OBJECT) that satisfies TEST applied to KEY of it and ITEM, return the sub-object.
Return 0: the found sub-object
Return 1: T if a sub-object is found."
  ;; TODO - optimized version with test & key known
  (cond
    ((funcall test item (funcall key object)) (values object t))
    ((consp object) (do* ((list object (cdr list))
                          (sub (car list) (car list)))
                         ((not (consp (cdr list)))
                          ;; Ending forms
                          (multiple-value-bind (ans found?)
                              (tree-find item (car list) test key)
                            (when found?
                              (return (values ans found?))))
                          (alexandria:when-let ((cdr (cdr list)))
                            (multiple-value-bind (ans found?)
                                (tree-find item cdr test key)
                              (when found?
                                (return (values ans found?)))))
                          (values nil nil))
                      ;; Iteration
                      (multiple-value-bind (ans found?) (tree-find item sub test key)
                        (when found?
                          (return (values ans found?))))))
    (t (values nil nil))))

(defun simple-tree-find? (item object)
  "[Cyc] Return T iff the non-nil ITEM is found in OBJECT (via EQ)."
  (cond
    ((eq item object) t)
    ((consp object) (do* ((list object (cdr list))
                          (sub (car list) (car list)))
                         ((not (consp (cdr list)))
                          ;; Ending forms, searches the final cons cell's car & cdr
                          (or (simple-tree-find? item (car list))
                              (and (cdr list)
                                   (simple-tree-find? item (cdr list)))))
                      ;; Iteration
                      ;; TODO - unwrap the equality test & use a LABELS to avoid leaf recursion, both here and in the -equal version below.
                      (when (simple-tree-find? item sub)
                        (return t))))))

(defun simple-tree-find-via-equal? (item object)
  "[Cyc] Return T iff the non-nil ITEM is found in OBJECT (via EQUAL)"
  (cond
    ((equal item object) t)
    ((consp object) (do* ((list object (cdr list))
                          (sub (car list) (car list)))
                         ((not (consp (cdr list)))
                          ;; Ending forms
                          (or (simple-tree-find-via-equal? item (car list))
                              (and (cdr list)
                                   (simple-tree-find-via-equal? item (cdr list)))))
                      ;; Iteration
                      (when (simple-tree-find-via-equal? item sub)
                        (return t))))))

(defun tree-find-any (items tree &optional (test #'eql) (key #'identity))
  "[Cyc] Look for any of ITEMS in the tree OBJECT. Return the first item found, or NIL if none found."
  (dolist (item items)
    (when (tree-find item tree test key)
      (return item))))

(defun cons-tree-find-if (test object &optional (key #'identity))
  "[Cyc] Obsolete -- use tree-find-if"
  ;; TODO - mark obsolete
  (tree-find-if test object key))

(defun tree-find-if (test object &optional (key #'identity))
  (cond
    ((funcall test (funcall key object)) object)
    ((consp object) (do* ((list object (cdr list))
                          (sub (car list) (car list)))
                         ((not (consp (cdr list)))
                          (or (tree-find-if test (car list) key)
                              (and (cdr list)
                                   (tree-find-if test (cdr list) key))))
                      (alexandria:when-let ((ans (tree-find-if test sub key)))
                        (return ans))))))

(defun tree-count-if (test object &optional (key #'identity))
  (cond
    ((funcall test (funcall key object)) 1)
    ((consp object) (let ((total 0))
                      (do* ((list object (cdr list))
                            (sub (car list) (car list)))
                           ((not (consp (cdr list)))
                            (incf total (tree-count-if test (car list) key))
                            (when (cdr list)
                              (incf total (tree-count-if test (cdr list) key)))
                            total)
                        (incf total (tree-count-if test sub key)))))
    (t 0)))

(defun tree-gather (object predicate &optional (test #'eql) (key #'identity) (subs-too? t))
  (nreverse (tree-gather-internal object predicate test key subs-too? nil)))

(defun tree-gather-internal (object predicate test key subs-too? so-far)
  (let ((result so-far))
    (when (funcall predicate (funcall key object))
      (if test
          (pushnew object result :test test)
          (push object result))
      (unless subs-too?
        (return-from tree-gather-internal result)))
    (when (consp object)
      (do* ((list object (cdr list))
            (sub (car list) (car list)))
           ((not (consp (cdr list)))
            (setf result (tree-gather-internal (car list) predicate test key subs-too? result))
            (when (cdr list)
              (setf result (tree-gather-internal (cdr list) predicate test key subs-too? result))))
        (setf result (tree-gather-internal sub predicate test key subs-too? result))))
    result))

(defun delete-subsumed-items (list test &optional (key #'identity))
  "[Cyc] If a and b are in LIST, and (TEST (KEY a) (KEY b)) is true, don't include b in the result"
  (declare (ignore test key))
  (when list
    (missing-larkc 9081)))

(defun permute-list (elements &optional (test #'equal))
  "[Cyc] Return a list of all possible distinct ordered lists of the elements in ELEMENTS.
By convention, (permute-list NIL) -> NIL.
By convention, if TEST is NIL, the check for duplicates is skipped."
  (when elements
    (let ((number-of-elements (length elements))
          (result nil))
      (cond
        ((null test)
         (dolist (permutation (all-permutations (length elements)))
           (push (permute elements permutation) result)))
        
        ((< number-of-elements 5)
         (dolist (permutation (all-permutations (length elements)))
           (pushnew (permute elements permutation) result :test test)))

        (t (dolist (permutation (all-permutations (length elements)))
             (push (permute elements permutation) result))
           (setf result (fast-delete-duplicates result test))))
      result)))

(defun permute-list-int (elements &optional (test #'eq))
  "[Cyc] Given a list of elements, return all permutations of the elements.
Assumes no two elements are equal with respect to TEST."
  (cond
   ((not elements) nil)
   ((atom elements) (list elements))
   ((not (cdr elements)) (list elements))
   ((not (cddr elements)) (list elements (reverse elements)))
   (t (let ((perms nil))
        (dolist (elem elements)
          (dolist (perm (permute-list-int (remove elem elements :test test)))
            (push (cons elem perm) perms)))
        perms))))

(defun all-permutations (n)
  "[Cyc] Returns all permutations of the numbers from 0 to N-1.
By convention, (all-permutations 0) -> (NIL)."
  (if (zerop n)
      (list nil)
      (permute-list-int (num-list n))))

(defun permute (list permutation)
  "[Cyc] e.g. (permute '(a b c) '(2 0 1)) -> (c a b)
By convention, (permute X NIL) -> X."
  (if permutation
      (let (result)
        (dolist (elem permutation)
          (push (nth elem list) result))
        (nreverse result))
      list))

(defun cartesian-product (l &optional (fun #'cons) (start '()) test)
  "[Cyc] Returns a list of the cartesian product of the elements of l.
FUN: an optional function to build the product.
START: an optional seed for building the product."
  (let ((accum (list start)))
    (if (fboundp test)
        (dolist (this-one l)
          (declare (ignore this-one))
          (setf accum (missing-larkc 9053)))
        (dolist (this-one l)
          (setf accum (cartesian-helper this-one accum fun))))
    (nmapcar #'reverse accum)))

(defun cartesian-helper (a b fun)
  "[Cyc] Takes two lists and returns the cartesian product. FUN generally should be #'CONS."
  (let (accum)
    (dolist (b-er b)
      (dolist (a-er a)
        (push (funcall fun a-er b-er) accum)))
    (nreverse accum)))

(defun list-of-type-p (pred object)
  "[Cyc] Returns T if OBJECT is a non-dotted list, and PRED returns non-NIL when applied to any item in OBJECT. Otherwise, returns NIL."
  (when (non-dotted-list-p object)
    (some pred object)))










