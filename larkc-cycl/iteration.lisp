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


;; TODO DESIGN - this will likely run faster if done/next/finalize are 3 functions returned from a single closure around STATE, although loses decoupling.

;; TODO DESIGN - finalizers seem to be missing-larkc even though they're referenced by name.  Finalization is probably never called?

(defstruct (iterator (:conc-name "IT-"))
  state
  done
  next
  finalize)

(defun new-alist-iterator (alist)
  "[Cyc] Returns an iterator for ALIST.
Values returned are tuples of the form (<key> <value>)."
  ;; cons-to-tuple is missing-larkc, so inlined something here.
  (new-indirect-iterator (new-list-iterator alist)
                         (lambda (cons)
                           (list (car cons) (cdr cons)))))

(defun new-indirect-iterator (iterator &optional
                                         (transform #'identity)
                                         ;; TODO - missing-larkc function reference
                                         (finalize 'iterate-indirect-finalize))
  "[Cyc] Return an iterator that transforms the values from another ITERATOR.
TRANSFORM is funcalled on each result from ITERATOR."
  (new-iterator (make-iterator-indirect-state iterator transform)
                #'default-iterator-done-p #'iterate-indirect-next finalize))

(defun make-iterator-indirect-state (iterator transform)
  (list iterator transform))

(defun iterate-indirect-next (state)
  (destructuring-bind (current transform) state
    (multiple-value-bind (next valid) (iteration-next current)
      (if valid
          (progn
            (unless (eq #'identity transform)
              (setf next (funcall transform next)))
            (values next state nil))
          (progn
            (rplaca state :done)
            (rplacd state nil)
            (values nil state t))))))

(defparameter *within-print-iterator* nil
    "[Cyc] Used to suppress initializing the lazy iterator while merely printing it.")

(defun new-iterator (state done next &optional (finalize #'true))
  "[Cyc] Return a new iterator for incrementally iterating over objects in STATE.

STATE is a datastructure which is the initial state of the iteration.

DONE must be a unary function which when called on STATE returns non-NIL iff the iteration is complete.

NEXT must be a unary function which when called on STATE returns three values:
[1] The next raw iteration item from the state
[2] The resulting updated state
[3] A non-NIL value if the iteration halted prematurely (and we are thus done)

FINALIZE is a unary function which is applied to STATE when the iterator is destroyed. While it is not strictly necessary, by convention the output should be non-NIL if and only if the finalization was successful.  NB: this function should be robust about finalizing an already-finalized iterator."
  (make-iterator :state state
                 :done done
                 :next next
                 :finalize finalize))

(defun iterator-done (iterator)
  "[Cyc] Return NIL iff ITERATOR has not yet been exhausted."
  (funcall (it-done iterator) (it-state iterator)))

(defun-inline iteration-next-funcall (next-func next-state)
  ;; This dispatched on next-func symbols to direct distant forward-referencing calls,
  ;; likely in an optimization attempt.
  ;; Simplified this to calling next-func directly, as it should be more performant
  ;; and eliminates the forward-references
  (funcall next-func next-state))

(defun iteration-next (iterator)
  "[Cyc] Return the next item in the iteration of ITERATOR.
The second value returned is non-NIL iff the value returned is valid."
  (if (not (funcall (it-done iterator) (it-state iterator)))
      (multiple-value-bind (raw-item raw-state halted-prematurely)
          (iteration-next-funcall (it-next iterator) (it-state iterator))
        (setf (it-state iterator) raw-state)
        (if (not halted-prematurely)
            (values raw-item t)
            (values nil nil)))
      (values nil nil)))

(defun iteration-next-without-values (iterator &optional invalid-token)
  "[Cyc] Return the next item in the iteration of ITERATOR or INVALID-TOKEN if the return value is invalid.  Unlike ITERATION-nEXT, only 1 value is returned."
  (if (not (funcall (it-done iterator) (it-state iterator)))
      (multiple-value-bind (raw-item raw-state halted-prematurely)
          (iteration-next-funcall (it-next iterator) (it-state iterator))
        (setf (it-state iterator) raw-state)
        (if halted-prematurely invalid-token raw-item))
      invalid-token))

;; TODO - make macro
(defun iteration-next-without-values-macro-helper (iterator &optional invalid-token)
  (iteration-next-without-values iterator invalid-token))

(defun iteration-finalize (iterator)
  (funcall (it-finalize iterator) (it-state iterator)))

;; TODO - this likely is what DO-ITERATOR does as well in macro form
(defun map-iterator (function iterator)
  "[Cyc] Apply FUNCTION to each object in the iteration of ITERATOR."
  (loop do (multiple-value-bind (item valid) (iteration-next iterator)
             (unless valid
               (return nil))
             (funcall function item))))



;; singleton iterator

(defun new-singleton-iterator (item)
  "[Cyc] Return an iterator that will just return ITEM and halt."
  (if item
      (new-iterator item #'null #'iterate-non-null-singleton-next)
      (new-list-iterator '(nil))))

(defun iterate-non-null-singleton-next (state)
  (values state nil))



;; list iterators

(defun list-iterator-p (object)
  "[Cyc] Return T iff OBJECT is a list iterator."
  (and (iterator-p object)
       (eq #'iterate-list-next (it-next object))))

(defun new-list-iterator (list)
  (new-iterator (make-iterator-list-state list) #'iterate-list-done #'iterate-list-next))

(defun get-list-iterator-list (iterator)
  "[Cyc] Returns the list of elements that are sequenced through by ITERATOR"
  (the list (it-state iterator)))

(defun list-iterator-size (list-iterator)
  "[Cyc] Return the remaining number of objects to iterate in LIST-ITERATOR."
  (length (get-list-iterator-list list-iterator)))

(defun make-iterator-list-state (list)
  list)

(defun iterate-list-done (state)
  (null state))

(defun iterate-list-next (state)
  (values (car state) (cdr state)))



;; hash table iterators


;; TODO DESIGN - would me more efficient as (key . value)
(defun new-hash-table-iterator (hash-table)
  "[Cyc] Returns an iterator for HASH-TABLE.
Values returned are tuples of the form (<key> <value>)."
  (new-iterator (make-iterator-hash-table-state hash-table)
                #'iterator-hash-table-done
                #'iterator-hash-table-next))

(defun make-iterator-hash-table-state (hash-table)
  ;; The original grabbed all the keys, then called GETHASH to find values during iteration.
  ;; I don't think it's much more expensive to preemptively grab the keys and values,
  ;; and it's cumulatively expensive to constantly to GETHASH during iteration.
  ;; Thus, this degenerates to a list iterator.
  ;; But we'll keep the function identities unique so it won't pass LIST-ITERATOR-P.
  (let ((pairs nil))
    (maphash (lambda (k v) (list k v)) hash-table)
    pairs))

(defun iterator-hash-table-done (state)
  (null state))

(defun iterator-hash-table-next (state)
  (values (car state) (cdr state)))



;; iterator iterators

(defun new-iterator-iterator (iterators)
  "[Cyc] Returns an iterator that sequences through the iterators in ITERATORS."
  (cond
    ;; TODO - easy to implement
    ((null iterators) (missing-larkc 22987))
    ((singleton? iterators) (car iterators))
    (t (new-iterator (make-iterator-iterator-state iterators)
                     #'iterator-iterator-done
                     #'iterator-iterator-next
                     ;; TODO - missing-larkc function reference
                     'iterator-iterator-finalize))))

(defun make-iterator-iterator-state (iterators)
  ;; TODO - this called check-type on each of them, but that's empty in LarKC
  iterators)

(defun iterator-iterator-done (state)
  "[Cyc] Returns T IFF the iteartors are exhausted.  State can be NIL when all of the iterators have been processed, or can be a singleton or can be a list of iterators."
  (cond
    ((consp state) (every #'iterator-done state))
    ((null state) t)
    (t (iterator-done state))))

(defun iterator-iterator-next (state)
  (let ((working-state state)
        (next nil)
        (valid-next? nil)
        (premature-end? nil))
    ;; Loop through states until we find one that's not done
    (until (or valid-next? premature-end?)
      (let ((current (car working-state)))
        (if (iterator-done current)
            (progn
              (pop working-state)
              (unless working-state
                (return))
              (setf premature-end? (not working-state)))
            (multiple-value-bind (value valid?) (iteration-next current)
              (when valid?
                (setf next value)
                (setf valid-next? t))))))
    (values next working-state premature-end?)))



;; filter iterator, without values

(defun new-filter-iterator-without-values (input-iterator filter-method
                                           &optional filter-args invalid-token)
  "[Cyc] Return an iterator that filters each raw-value from another ITERATOR.
RAW-VALUE is returned iff (apply FILTER-METHOD RAW-VALUE FILTER-ARGS) is non-NIL.
Unlike NEW-FILTER-ITERATOR, the INPUT-ITERATOR is iterated under the assumption that INVALID-TOKEN is used to indicate an invalid value rather than a second value."
  (new-iterator (make-filter-iterator-without-values-state input-iterator filter-method
                                                           filter-args invalid-token)
                #'default-iterator-done-p
                #'filter-iterator-without-values-next))

(defun make-filter-iterator-without-values-state (input-iterator filter-method filter-args invalid-token)
  (list input-iterator filter-method filter-args invalid-token))

(defun filter-iterator-without-values-next (state)
  (destructuring-bind (current filter-method filter-args invalid-token) state
    (let ((answer nil)
          (done nil)
          (invalid nil))
      (while (not done)
        (let ((next (iteration-next-without-values current invalid-token)))
          (if (not (eq next invalid-token))
              (when (apply filter-method next filter-args)
                (setf answer next)
                (setf done t))
              (progn
                (rplaca state :done)
                (rplacd state nil)
                (setf done t)
                (setf invalid t)))))
      (values answer state invalid))))

(defun default-iterator-done-p (state)
  (eq :done (elt state 0)))
