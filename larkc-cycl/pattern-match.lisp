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


;; TODO OPTIMIZATION - change (destructuring-bind (single) list ...) to a specific check that the list is of length 1, erroring otherwise.  Would be cheaper than calling into the destructuring-bind mechanism, assuming pattern matching is commonly called.

;; TODO - maybe make this a closure variable for performance
(defparameter *pattern-matches-tree-bindings* nil)
;; TODO - could be a bit faster if it was (operator . method)
(defparameter *pattern-matches-tree-atomic-methods* nil
    "[Cyc] Additional atomic methods for PATTERN-MATCHES-TREE.
Entries are of the form (operator method).
OPERATOR is a token indicating the match method.
METHOD must be suitable for (funcall method <tree>).")
(defparameter *pattern-matches-tree-methods* nil
    "[Cyc] Additional methods for PATTERN-MATCHES-TREE.
Entries are of the form (operator method).
OPERATOR is a token indicating the match method.
METHOD must be suitable for (funcall method <pattern> <tree>).")

(defun* add-pattern-matches-tree-binding (variable value)
    (:inline t)
  (setf *pattern-matches-tree-bindings*
        (alist-enter-without-values *pattern-matches-tree-bindings*
                                    variable value #'eql)))

(defun pattern-matches-tree (pattern tree)
  "[Cyc] Return T iff PATTERN matches TREE."
  ;; Converted ignore-errors to handler-case in order to keep the number of return values consistent.
  (handler-case (let ((*pattern-matches-tree-bindings* nil))
                  (when (pattern-matches-tree-internal pattern tree)
                    (values t (nreverse *pattern-matches-tree-bindings*))))
    (error () (values nil nil))))

(defun* pattern-matches-tree-without-bindings (pattern tree)
    (:inline t)
  "[Cyc] Return T iff PATTERN matches TREE.
:BIND expressions are not allowed in PATTERN."
  (pattern-matches-tree-internal pattern tree))

(defun* pattern-matches-tree-internal (pattern tree)
    (:inline t)
  "[Cyc] For use by pattern match methods in other files."
  (pattern-matches-tree-recursive pattern tree))

(defun* pattern-matches-tree-atomic-method-funcall (method tree)
    (:inline t)
  (funcall method tree))

(defun* pattern-matches-tree-method-funcall (method pattern tree)
    (:inline t)
  (funcall method pattern tree))

(defun pattern-matches-tree-recursive (pattern tree)
  (if (atom pattern)
      (case pattern
        (:anything t)
        (:nothing nil)
        (otherwise (dolist (method-info *pattern-matches-tree-atomic-methods*
                            (equal pattern tree))
                     (when (eq (first method-info) pattern)
                       (return (pattern-matches-tree-atomic-method-funcall (second method-info) tree))))))
      (destructuring-bind (pattern-operator . pattern-args) pattern
        (declare (list pattern-args))
        ;; TODO OPTIMIZATION - this smacks of needing precompilation, just like regex patterns
        (case pattern-operator
          (:bind (pattern-matches-tree-bind pattern tree))
          ;; TODO DESIGN - is this for testing an already bound variable?
          (:value (missing-larkc 32006))
          (:and (pattern-matches-tree-and pattern tree))
          (:or (pattern-matches-tree-or pattern tree))
          ;; TODO - is this just ensuring there is 1 element in the list?
          (:not (destructuring-bind (sub-pattern) pattern-args
                  (not (pattern-matches-tree-recursive sub-pattern tree))))
          ;; TODO - this presumably calls fixed-arity funcall functions in the original code, from 1-4 pattern-args. If there's more, there's no case to handle that and it silently contnues, which seems odd.  However, maybe the intent is that covers all cases?  Using the general case here.
          (:test (apply (first pattern-args) (rest pattern-args)))
          (:tree-find (destructuring-bind (sub-pattern) pattern-args
                        (pattern-matches-tree-tree-find sub-pattern tree)))
          (:quote (destructuring-bind (quoted-object) pattern-args
                    (equal quoted-object tree)))
          (otherwise (dolist (method-info *pattern-matches-tree-methods*
                              (pattern-matches-tree-cons pattern tree))
                       (when (eq (car method-info) pattern-operator)
                         (return (pattern-matches-tree-method-funcall (second method-info) pattern tree)))))))))

(defun pattern-matches-tree-bind (pattern tree)
  (let ((variable (second pattern)))
    (add-pattern-matches-tree-binding variable tree)))

(defun pattern-matches-tree-and (pattern tree)
  (dolist (sub-pattern (rest pattern) t)
    (unless (pattern-matches-tree-recursive sub-pattern tree)
      (return nil))))

(defun pattern-matches-tree-or (pattern tree)
  (dolist (sub-pattern (rest pattern) nil)
    (when (pattern-matches-tree-recursive sub-pattern tree)
      (return t))))

(defun pattern-matches-tree-test-funcall (test tree)
  (funcall test tree))

(defun pattern-matches-tree-tree-find (sub-pattern tree)
  (tree-find sub-pattern tree #'pattern-matches-tree-recursive))

(defun pattern-matches-tree-cons (pattern tree)
  (unless (atom tree)
    (destructuring-bind (pattern-operator . pattern-args) pattern
      (destructuring-bind (tree-operator . tree-args) tree
        (when (pattern-matches-tree-recursive pattern-operator tree-operator)
          (do ((rest-pattern-args pattern-args (cdr rest-pattern-args))
               (rest-tree-args tree-args (cdr rest-tree-args)))
              ((or (atom rest-pattern-args)
                   (atom rest-tree-args))
               (pattern-matches-tree-recursive rest-pattern-args rest-tree-args))
            (unless (pattern-matches-tree-recursive (car rest-pattern-args)
                                                    (car rest-tree-args))
              (return nil))))))))

(defparameter *pattern-transform-tree-bindings* nil)
(defparameter *pattern-transform-match-method* nil)

(defun pattern-transform-tree (pattern tree &optional bindings)
  "[Cyc] Use PATTERN to transform TREE, assuming BINDINGS.
Return transformation result and updated BINDINGS."
  (let ((*pattern-transform-tree-bindings* bindings))
    (values (pattern-transform-tree-internal pattern tree)
            *pattern-transform-tree-bindings*)))

(defun pattern-transform-tree-internal (pattern tree)
  "[Cyc] For use by pattern transform methods in other files."
  (pattern-transform-tree-recursive pattern tree))

;; TODO OPTIMIZATION - again, this would really want precompilation
(defun pattern-transform-tree-recursive (pattern tree)
  (if (atom pattern)
      (case pattern
        (:input tree)
        (:bindings *pattern-transform-tree-bindings*)
        (otherwise pattern))
      (destructuring-bind (pattern-operator . pattern-args) pattern
        (case pattern-operator
          (:value (destructuring-bind (variable) pattern-args
                    (alist-lookup-without-values *pattern-transform-tree-bindings*
                                                 variable #'eql nil)))
          (:tuple (pattern-transform-tuple pattern tree))
          (:template (pattern-transform-template pattern tree))
          (:call (pattern-transform-call pattern tree))
          (:quote (destructuring-bind (quoted-object) pattern-args
                    quoted-object))
          (otherwise (pattern-transform-cons pattern tree))))))

(defun pattern-transform-tuple (pattern tree)
  (destructuring-bind (operator variables subpattern) pattern
    ;; The operator was already matched in pattern-perform-tree-recursive
    (declare (ignore operator))
    (when (and (listp tree)
               (listp variables)
               (same-length-p tree variables))
      (mapc (lambda (variable subtree)
              (add-pattern-matches-tree-binding variable subtree))
            variables tree)
      (pattern-transform-tree-recursive subpattern nil))))

(defun pattern-transform-template (pattern tree)
  (destructuring-bind (operator match-pattern subpattern) pattern
    (declare (ignore operator))
    (multiple-value-bind (success bindings) (if *pattern-transform-match-method*
                                                (funcall *pattern-transform-match-method* match-pattern tree)
                                                (pattern-matches-tree match-pattern tree))
      (when success
        (dolist (binding bindings)
          (destructuring-bind (variable . value) binding
            (add-pattern-matches-tree-binding variable value)))
        (pattern-transform-tree-recursive subpattern nil)))))

(defun pattern-transform-call (pattern tree)
  (destructuring-bind (operator method &rest method-args) pattern
    (declare (list method-args)
             (ignore operator))
    ;; The original fast-pathed 0-4 args with a bunch of tests.
    ;; Not sure that helps in modern processors.  TODO - profile
    (apply method (mapcar (lambda (arg) (pattern-transform-tree arg tree)) method-args))))

(defun pattern-transform-cons (pattern tree)
  (let ((answer (copy-list pattern)))
    ;; Transform the CARs and final CDR of the copied list.
    (do ((rest-answer answer (cdr answer)))
        ((atom (cdr rest-answer))
         (rplaca rest-answer (pattern-transform-tree-recursive (car rest-answer) tree))
         (when (cdr rest-answer)
           (rplacd rest-answer (pattern-transform-tree-recursive (cdr rest-answer) tree))))
      (rplaca rest-answer (pattern-transform-tree-recursive (car rest-answer) tree)))
    answer))
