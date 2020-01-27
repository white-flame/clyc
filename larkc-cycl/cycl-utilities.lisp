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


;; TODO - defun as symbol, forward referenced in this file
(defparameter *opaque-arg-function* 'default-opaque-arg?
  "[Cyc] The function to use to determine argument opacity.")

;; TODO - defun as symbol, forward referenced in this file
(defparameter *opaque-seqvar-function* 'default-opaque-seqvar?
  "[Cyc] The function to use to determine sequence variable opacity.")

(defun opaque-arg? (formula argnum)
  "[Cyc] Returns T iff FORMULA is opaque in the argument position ARGNUM, meaning that it should not be recursed into in that arg position. By convention, if ARGNUM is greater than the arity of FORMULA, this denotes the sequence variable in FORMULA."
  (opaque-arg?-int formula argnum *opaque-arg-function*))

(defun-inline expression-nsubst-free-vars (new old expression &optional (test #'eql))
  (expression-nsubst-free-vars-int new old expression test))

(defun expression-nsubst-free-vars-int (new old expression test)
  "[Cyc] Replaces free var in the EXPRESSION. Takes quoting into account. *CANONICALIZE-VARIABLES?* determines whether #$EscapeQuotes will be removed / reduced from the quoted terms. If variables are to be canonicalized then the #$EscapeQuotes will already contain HL variables due to the various czer steps before. This step just removes the #$EscapeQuotes to complete the canonicalization of the variables."
  (cond
    ((funcall test expression old) (if *inside-quote*
                                       expression
                                       new))
    
    ((not (el-formula-p expression)) expression)
    
    ((or (fast-escape-quote-term-p expression)
         (fast-quasi-quote-term-p expression))
     (let ((*inside-quote* nil))
       (if *canonicalize-variables?*
           (expression-nsubst-free-vars-int new old (formula-arg1 expression) test)
           (make-unary-formula (formula-arg0 expression)
                               (expression-nsubst-free-vars-int new old
                                                                (formula-arg1 expression)
                                                                test)))))
    
    ((fast-quote-term-p expression)
     (let ((*inside-quote* t))
       (make-unary-formula #$Quote (expression-nsubst-free-vars-int new old
                                                                    (formula-arg1 expression)
                                                                    test))))
    
    ((and (expand-subl-fn-p expression)
          (member? old (formula-arg1 expression) :test test))
     (make-binary-formula #$ExpandSubLFn
                          (subst new old (formula-arg1 expression) :test test)
                          (expression-nsubst-free-vars-int new old
                                                           (formula-arg2 expression)
                                                           test)))
    
    (t (let* ((seqvar (sequence-var expression))
              (substituted-seqvar (if (when seqvar
                                        (not (opaque-seqvar? expression)))
                                      (expression-nsubst-free-vars-int new old seqvar test)
                                      seqvar)))
         (do* ((rest-of-expression expression (rest rest-of-expression))
               (term (car rest-of-expression) (car rest-of-expression))
               (argnum 0 (1+ argnum)))
              ;; End test
              ((not (consp (cdr rest-of-expression)))
               ;; Return forms
               (unless (opaque-arg-wrt-free-vars? expression argnum)
                 (rplaca rest-of-expression (expression-nsubst-free-vars-int new old term test)))
               (rplacd rest-of-expression substituted-seqvar)
               expression)
           ;; Iteration body
           (unless (opaque-arg-wrt-free-vars? expression argnum)
             (rplaca rest-of-expression (expression-nsubst-free-vars-int new old term test))))))))

(defun-inline opaque-seqvar? (formula)
  "[Cyc] Returns T iff FORMULA contains an opaque sequence variable, which should not be considered a proper part of the formula."
  (funcall *opaque-seqvar-function* formula))

(defun-inline nat-arg2 (nat &optional (seqvar-handling :ignore))
  "[Cyc] Returns the 2nd argument of NAT. Returns NIL if NAT is not a nat."
  (nat-arg nat 2 seqvar-handling))

(defun-inline opaque-arg?-int (formula argnum opaque-arg-function)
  ;; This did a case to find known function names and bypass funcall.
  (funcall opaque-arg-function formula argnum))

(defun-inline formula-arg4 (formula &optional (seqvar-handling :ignore))
  "[Cyc] Returns the 4th argument of FORMULA. Returns NIL if FORMULA is not a formula."
  (formula-arg formula 4 seqvar-handling))

(defun-inline formula-arg5 (formula &optional (seqvar-handling :ignore))
  "[Cyc] Returns the 5th argument of FORMULA. Returns NIL if FORMULA is not a formula."
  (formula-arg formula 5 seqvar-handling))

(defun default-opaque-arg? (formula argnum)
  (when (formula-arity< formula argnum)
    (missing-larkc 29826))
  (subl-escape-p formula))

(defun opaque-arg-wrt-free-vars? (formula argnum)
  (cond
    ((and (= 2 argnum)
          (el-formula-with-operator-p formula #$SkolemFunctionFn))
     t)
    ((formula-arity< formula argnum) (missing-larkc 29827))
    (t (subl-quote-p formula))))

(defun opaque-arg-wrt-quoting? (formula argnum)
  "[Cyc] Return T iff arg number ARGNUM of FORMULA is quoted or otherwise opaque."
  (or (default-opaque-arg? formula argnum)
      (and (not (tou-lit? formula))
           (or (fast-quote-term-p (formula-arg formula argnum))
               (quoted-argument? (formula-arg0 formula) argnum)))))

(defun opaque-arg-wrt-quoting-not-counting-logical-ops? (formula argnum)
  (unless (or (el-formula-with-operator-p formula #$trueSentence)
              (cyc-const-logical-operator-p (formula-operator formula))
              (cyc-const-quantifier-p (formula-operator formula)))
    (opaque-arg-wrt-quoting? formula argnum)))

(defun hl-term-with-el-counterpart-p (object)
  "[Cyc] Return T iff OBJECT is an HL term with an EL counterpart."
  (or (valid-nart-handle? object)
      (valid-assertion-handle? object)
      (variable-p object)))

(defun careful-hl-term-to-el-term (hl-term)
  "[Cyc] Converts HL-TERM to an EL term if HL-TERM has an EL counterpart, otherwise leaves HL-TERM unchanged. Not robust against invalid narts or assertions.
Note: Careful: the result is not destructible!"
  (cond
    ((nart-p hl-term) (funcall *nart-key* hl-term))
    ((assertion-p hl-term) (funcall *assertion-key* hl-term))
    ((variable-p hl-term) (default-el-var-for-hl-var hl-term))
    (t hl-term)))

(defun reified-term-p (object)
  (or (atomic-reified-term-p object)
      (reified-formula-p object)))

(defun-inline atomic-reified-term-p (object)
  (constant-p object))

(defun-inline reified-formula-p (object)
  (hl-formula-p object))

;;TODO DESIGN - again a long list of parameters that are passed through multiple levels.  Reevaluate.
(defun expression-gather-int-2 (expression pred penetrate-hl-structures? key subs-too?)
  (let ((result (when (funcall pred (funcall key expression))
                  (list expression))))
    (when (or subs-too?
              result)
      (cond
        ((and penetrate-hl-structures?
              (hl-term-with-el-counterpart-p expression))
         (npush-list (expression-gather-int-2 (careful-hl-term-to-el-term expression)
                                              pred t key subs-too?)
                     result))
        
        ((el-formula-p expression) nil)

        (t (dolistn (argnum term (formula-terms expression :regularize))
             (unless (opaque-arg? expression argnum)
               (npush-list (expression-gather-int-2 term pred penetrate-hl-structures?
                                                    key subs-too?)
                           result))))))
    result))

(defun expression-gather-int (expression pred penetrate-hl-structures? test key subs-too?)
  (fast-delete-duplicates (expression-gather-int-2 expression pred penetrate-hl-structures? key subs-too?)
                          test key))

(defun-inline expression-gather (expression pred &optional penetrate-hl-structures?
                                            (test #'eql)
                                            (key #'identity)
                                            (subs-too? t))
  "[Cyc] Return a list of all objects within EXPRESSION which pass the test PRED, without duplicates but in no particular order.
See file-level documentation for explanation of PENETRATE-HL-STRUCTURES? and #$ExpandSubLFn.
Returns the singleton list containing EXPRESSION if EXPRESSION passes PRED."
  (expression-gather-int expression pred penetrate-hl-structures? test key subs-too?))

(defun assertion-gather (pred assertion &optional penetrate-hl-structures?
                                          (test #'eql)
                                          (key #'identity)
                                          (subs-too? t))
  (when (hl-term-with-el-counterpart-p assertion)
    (let ((mt-objects (expression-gather-int (assertion-mt assertion)
                                             pred penetrate-hl-structures? test key subs-too?))
          (formula-objects (expression-gather-int (assertion-cons assertion)
                                                  pred penetrate-hl-structures? test key subs-too?)))
      (cond
        ((not mt-objects) formula-objects)
        ((not formula-objects) mt-objects)
        (t (fast-delete-duplicates (nconc mt-objects formula-objects) test key))))))

(defparameter *expression-count-item* nil)
(defparameter *expression-count-test* nil)

(defun-inline formula-gather (formula pred &optional penetrate-hl-structures?
                                      (test #'eql)
                                      (key #'identity)
                                      (subs-too? t))
  "[Cyc] Return a list of all objects within the EL formula FORMULA which pass the test PRED, without duplicates but in no particular order.
See file-level documentation for explanation of PENETRATE-HL-STRUCTURES? and #$ExpandSubLFn."
  (expression-gather formula pred penetrate-hl-structures? test key subs-too?))

(defun-inline expression-narts (expression &optional penetrate-hl-structures? (subs-too? t))
  "[Cyc] Return a list of the narts mentioned in EXPRESSION, without duplicates but in no particular order.
See file-level documentation for explanation of PENETRATE-HL-STRUCTURES? and #$ExpandSubLFn.
Returns the singleton list containing EXPRESSION if EXPRESSION is a nart."
  (expression-gather-int expression #'nart-p penetrate-hl-structures? #'eq #'identity subs-too?))

(defparameter *containing-subexpressions-labmda-term* nil)

(defun expression-find-if-int (test expression penetrate-hl-structures? key)
  (let* ((transformed-expression (if (eq key #'identity)
                                     expression
                                     (funcall key expression)))
         (test-succeeded? (funcall test transformed-expression)))
    (cond
      (test-succeeded? expression)

      ((and penetrate-hl-structures?
            (hl-term-with-el-counterpart-p expression))
       (expression-find-if-int test (careful-hl-term-to-el-term expression) t key))

      ((el-formula-p expression) nil)

      (t (dolistn (argnum term (formula-terms expression :regularize))
           (unless (opaque-arg? expression argnum)
             (return (expression-find-if-int test term penetrate-hl-structures? key))))))))

(defun expression-find-if (test expression &optional penetrate-hl-structures?
                                             (key #'identity))
  "[Cyc] Return an object which passes the test TEST if such an object exists within the CycL expression EXPRESSION. Otherwise NIL.
See file-level documentation for explanation of PENETRATE-HL-STRUCTURES? and #$ExpandSubLFn."
  (unless (and (not penetrate-hl-structures?)
               (not (tree-find-if test expression key)))
    (expression-find-if-int test expression penetrate-hl-structures? key)))

(defun-inline formula-find-if (test formula &optional penetrate-hl-structures?
                                    (key #'identity))
  "[Cyc] Return an object which passes the test TEST if such an object exists within the EL formula FORMULA. Otherwise NIL.
See file-level documentation for explanation of PENETRATE-HL-STRUCTURES? and #$ExpandSubLFn."
  (expression-find-if test formula penetrate-hl-structures? key))

(defun expression-find-int (object expression penetrate-hl-structures? test key)
  (cond
    ((funcall test object (funcall key expression))
     expression)
    
    ((and penetrate-hl-structures?
          (hl-term-with-el-counterpart-p expression))
     (expression-find-int object (careful-hl-term-to-el-term expression) t test key))
    
    ((not (el-formula-p expression))
     nil)
    
    (t (dolistn (argnum term (formula-terms expression :regularize))
         (unless (opaque-arg? expression argnum)
           (return (expression-find-int object term penetrate-hl-structures? test key)))))))

(defun-inline expression-find (object expression &optional penetrate-hl-structures?
                                      (test #'eql)
                                      (key #'identity))
  "[Cyc] Return OBJECT if it is found within the CycL expression EXPRESSION, otherwise NIL.
See file-level documentation for explanation of PENETRATE-HL-STRUCTURES? and #$ExpandSubLFn."
  (expression-find-int object expression penetrate-hl-structures? test key))

(deflexical *default-transformation-limit* 212)

;; These arglists are getting nuts. Not bothering with formatting these.
(defun expression-ntransform-int (expression pred transform transform-sequence-variables? transformation-limit transformation-level test-pred-on-transformation-result? negate-pred?)
  "[Cyc] Opacity can change during transformation - it's unclear what the desired behaviour is, though. PRED has a different meaning based on test-pred-on-transformation-result? and negate-pred?"
  (when (> transformation-level transformation-limit)
    (throw :transformation-limit-exceeded transformation-limit))
  (let ((transformed-expression nil))
    (if test-pred-on-transformation-result?
        (let ((transform-result (funcall transform expression)))
          ;; makeBoolean(..) != makeBoolean(..) should be equivalent to XOR
          ;; a single NOT should booleanify the variables, and we only care about equivalence
          (if (not (eq (not negate-pred?)
                       (not (funcall pred transform-result))))
              (setf transformed-expression (expression-ntransform-int transform-result pred transform transform-sequence-variables? transformation-limit (1+ transformation-level) test-pred-on-transformation-result? negate-pred?))
              (setf transformed-expression expression)))
        (if (not (eq (not negate-pred?)
                     (not (funcall pred expression))))
            (let ((transform-result (funcall transform expression)))
              (if (not (eq expression transform-result))
                  (setf transformed-expression (expression-ntransform-int transform-result pred transform transform-sequence-variables? transformation-limit (1+ transformation-level) test-pred-on-transformation-result? negate-pred?))
                  (setf transformed-expression expression)))
            (setf transformed-expression expression)))
    (unless (el-formula-p transformed-expression)
      transformed-expression)
    (let* ((seqvar (sequence-var transformed-expression))
           (transformed-seqvar (if (and seqvar
                                        transform-sequence-variables?
                                        (missing-larkc 29828))
                                   ;; Missing-larkc kills this anyway
                                   nil ;;(expression-ntransform-int seqvar pred transform t transformation-limit (1+ transformation-level) test-pred-on-transformation-result? negate-pred?)
                                   seqvar))
          (ist-sentence? (ist-sentence-p transformed-expression))
          (new-mt nil))
      ;; This is mutating the actual list, so we do need to expose the cons cells
      (loop for rest-of-expression on transformed-expression
         for term = (car rest-of-expression)
         for argnum from 0
         do (let* ((mt-var new-mt)
                   (*relevant-mt-function* (possibly-in-mt-determine-function mt-var))
                   (*mt* (possibly-in-mt-determine-mt mt-var)))
              (unless (opaque-arg? transformed-expression argnum)
                (rplaca rest-of-expression (expression-ntransform-int term pred transform transform-sequence-variables? transformation-limit (1+ transformation-level) test-pred-on-transformation-result? negate-pred?))
                (when (and ist-sentence?
                           (= argnum 1))
                  (setf new-mt (car rest-of-expression)))))
         ;; Last cons elemnt
         ;; TODO - this test is redundant with for-on, maybe wrap our own macro for a raw DO form.
         do (when (not (consp (cdr rest-of-expression)))
              (unless (opaque-arg? transformed-expression argnum)
                (rplaca rest-of-expression (expression-ntransform-int term pred transform transform-sequence-variables? transformation-limit (1+ transformation-level) test-pred-on-transformation-result? negate-pred?)))
              (rplacd rest-of-expression transformed-seqvar)))
      transformed-expression)))

(defun-inline expression-transform (expression pred transform &optional transform-sequence-variables? (transformation-limit *default-transformation-limit*))
  "[Cyc] Recursively tests PRED within the CycL expression EXPRESSION. If PRED applies to EXPRESSION or a subexpression/subterm of EXPRESSION, TRANSFORM is called on that term or expression. If an expression is transformed into another expression, the result is itself subjected to the transformation if PRED applies to the result. Thus one must take care when calling this function, to avoid infinite recursion. It does not penetrate into HL structures."
  (expression-ntransform-int (copy-expression expression) pred transform transform-sequence-variables? transformation-limit 0 nil nil))

(defun expression-nsublis-free-vars-int (alist expression test)
  "[Cyc] Replaces free vars in the EXPRESSION. Takes quoting into account. *CANONICALIZE-VARIABLES?* determines whether #$EscapeQuotes will be removed / reduced from the quoted terms. If variables are to be canonicalized then the #$EscapeQuotes will already contain HL variables due to the various czer steps before. This step just removes the #$EscapeQuotes to complete the canonicalization of the variables."
  ;; TODO - this was oldXnew in the java, not sure what the punc was supposed to be
  (when-let ((old-new (assoc expression alist :test test)))
    (if *inside-quote*
        expression
        (cdr old-new)))
  (cond
    ((not (el-formula-p expression)) expression)
    
    ((or (fast-escape-quote-term-p expression)
         (fast-quasi-quote-term-p expression))
     (let ((*inside-quote* nil))
       (if *canonicalize-variables?*
           (expression-nsublis-free-vars-int alist (formula-arg1 expression) test)
           (make-unary-formula (formula-arg0 expression)
                               (expression-nsublis-free-vars-int alist
                                                                 (formula-arg1 expression)
                                                                 test)))))
    
    ((fast-quote-term-p expression)
     (let ((*inside-quote* t))
       (make-unary-formula #$Quote (expression-nsublis-free-vars-int alist
                                                                     (formula-arg1 expression)
                                                                     test))))
    
    ((expand-subl-fn-p expression)
     (let* ((arg1 (formula-arg1 expression))
            (vars (expression-gather arg1 #'cyc-var?))
            (non-opaque-var-list nil))
       (when vars
         (dolist (var vars)
           (when-let ((old-new (assoc var alist :test test)))
             (push old-new non-opaque-var-list)))
         (if non-opaque-var-list
           (make-binary-formula #$ExpandSubLFn
                                (expression-nsublis-free-vars-int alist arg1 test)
                                (expression-nsublis-free-vars-int non-opaque-var-list
                                                                  (formula-arg2 expression)
                                                                  test))
           expression))))
    
    (t (let* ((seqvar (sequence-var expression))
              (substituted-seqvar (if (and seqvar
                                           (missing-larkc 29829))
                                      ;; missing-larkc kills that anyway
                                      nil ;;(expression-nsublis-free-vars-int alist seqvar test)
                                      seqvar)))
         (loop for rest-of-expression on expression
            for term = (car rest-of-expression)
            for argnum from 0
            do (unless (opaque-arg-wrt-free-vars? expression argnum)
                 (rplaca rest-of-expression (expression-nsublis-free-vars-int alist term test)))
            ;; Last cons element
            do (when (not (consp (cdr rest-of-expression)))
                 (unless (opaque-arg-wrt-free-vars? expression argnum)
                   (rplaca rest-of-expression (expression-nsublis-free-vars-int alist term test)))
                 (rplacd rest-of-expression substituted-seqvar)))
         expression))))

(defun-inline expression-nsublis-free-vars (alist expression &optional (test #'eql))
  (expression-nsublis-free-vars-int alist expression test))

;; TODO - probably from a missing-larkc defun-memoized?
(deflexical *permute-list-cached-caching-state* nil)

(defun canonical-commutative-permutations (formula &optional (var? #'cyc-var?) penetrate-args?)
  "[Cyc] Return the permutations of the formula that can be possibly canonical. For fully bound formula, it returns the formula. For non fully-bound formula, it return the permutations of the variable arg with the other args in canonical order. Doesn't permute sequence vars."
  (if (ground? formula var?)
      (list (canonicalize-literal-commutative-terms formula))
      (let ((variable-argnums (variable-argnums formula var?)))
        (cond
          ((and (not penetrate-args?)
                (not variable-argnums))
           (list (canonicalize-literal-commutative-args formula)))
          
          ((and penetrate-args?
                (not variable-argnums))
           (nreverse (args-canonical-commutative-permutations (canonicalize-literal-commutative-terms formula) var?)))

          (t (let ((target-formulas (if (and penetrate-args?
                                             variable-argnums)
                                        (args-canonical-commutative-permutations (canonicalize-literal-commutative-terms formula) var?)
                                        (list (canonicalize-literal-commutative-args formula)))))
               (nreverse (formulas-canonical-permutations target-formulas))))))))

(defun variable-argnums (formula &optional (var? #'cyc-var?))
  (unless (ground? formula var?)
    (let ((argnums nil))
      (dolistn (argnum arg (formula-args formula :ignore))
        (when (funcall var? arg)
          (push argnum argnums)))
      argnums)))
                                                     
(defun args-canonical-commutative-permutations (formula var?)
  "[Cyc] Result is destructible. If any of the arg of the formula has a commutative relation formula, the commutative permutations for those args are generated."
  (let ((target-formulas (list (copy-formula formula))))
    (dolistn (argnum arg (formula-args formula :ignore))
      (cond 
        ((subl-escape-p arg)
         nil)
        ((naut? arg)
         (missing-larkc 29803))
        ((el-relation-expression? arg)
         (dolist (formula-permutation (canonical-commutative-permutations arg var? t))
           (unless (equal formula-permutation arg)
             (push (nreplace-formula-arg argnum formula-permutation (copy-formula formula))
                   target-formulas))))))
    target-formulas))

(defun formulas-canonical-permutations (source-formulas)
  (let ((target-formulas nil)
        (permuted? nil))
    (dolist (source-formula source-formulas)
      (dolist (commutative-argnums (commutative-argnums source-formula))
        (let* ((variable-argnums (variable-argnums source-formula))
               (argnums-to-permute (fast-intersection commutative-argnums variable-argnums))
               (other-argnums nil)
               (argnum-permutations nil))
          (if argnums-to-permute 
              (progn
                (setf other-argnums (nreverse (set-difference commutative-argnums
                                                              argnums-to-permute)))
                (setf argnum-permutations (permutations-merge other-argnums argnums-to-permute))
                (setf permuted? t)
                (dolist (argnum-permutation argnum-permutations)
                  (push (canonical-permute-formula source-formula commutative-argnums
                                                   argnum-permutation)
                        target-formulas)))
              (push source-formula target-formulas)))))
    (if permuted?
        (delete-duplicates target-formulas :test #'equal)
        source-formulas)))

(defun canonical-permute-formula (source-formula argnums-to-permute argnum-permutation)
  "[Cyc] Result is destructible."
  (let ((target-formula (copy-formula source-formula)))
    (dolistn (index source-argnum argnum-permutation)
      (let ((target-argnum (nth index argnums-to-permute)))
        (unless (eq target-argnum source-argnum)
          (let ((target-term (formula-arg source-formula source-argnum)))
            (setf target-formula (nreplace-formula-arg target-argnum target-term target-formula))))))
    target-formula))

(defun split-list-set (l)
  (let ((splits (list l nil))
        (length (length l)))
    (loop for i from 1 below length
       do (multiple-value-bind (list1 list2) (split-list l i)
            (push (list list1 list2) splits)))
    splits))

(defun permutations-merge (list1 list2)
  (let ((merged nil)
        (permutations (permute-list list2))
        (list1-splits (split-list-set list1)))
    ;; TODO - test and ensure all this nesting is correct
    (dolist (list1-split list1-splits)
      (destructuring-bind (front1 rest1) list1-split
        (dolist (permutation permutations)
          (let ((list2-splits (split-list-set permutation)))
            (dolist (list2-split list2-splits)
              (destructuring-bind (front2 rest2) list2-split
                (push (append front1 front2 rest1 rest2) merged)
                (push (append front2 front1 rest2 rest1) merged)))))))
    (delete-duplicates merged :test #'equal)))

(defparameter *renamed-default-el-var-prefix* "?RENAMED-VAR")

(deflexical *non-indexed-constants* (append *cyc-const-unary-logical-ops*
                                            *cyc-const-binary-logical-ops*
                                            *cyc-const-ternary-logical-ops*
                                            *cyc-const-quaternary-logical-ops*
                                            *cyc-const-quintary-logical-ops*
                                            *cyc-const-variable-arity-logical-ops*
                                            *cyc-const-regular-quantifiers*
                                            *cyc-const-bounded-existentials*
                                            *cyc-const-exception-operators*
                                            *cyc-const-pragmatic-requirement-operators*)
  "[Cyc] Cyc constants that have no indexing maintained for them.
All other constants except instances of #$ELRelation are indexed (4/3/00)")

(defun functional-in-some-arg? (pred)
  "[Cyc] Returns non-NIL iff PRED is functional in some argument."
  (or (some-pred-assertion-somewhere? #$functionalInArgs pred 1)
      (some-pred-assertion-somewhere? #$strictlyFunctionalInArgs pred 1)))

(defun reify-arg-when-closed-naut (reln psn)
  (let ((object (formula-arg reln psn)))
    (if (arg-types-prescribe-unreified? reln psn) 
        object
        (reify-when-closed-naut object))))

(defun reify-when-closed-naut (object)
  (cond
    ((not (possibly-naut-p object)) object)
    ((closed-naut? object) (or (nart-substitute object)
                               object))
    ((el-formula? object) (loop for psn from 0 below (formula-length object :ignore)
                               collect (reify-arg-when-closed-naut object psn)))
    (t object)))

(defun find-closed-naut (object)
  "[Cyc] If OBJECT is a closed, unreified, specification of a reified non-atomic term, then return the NART implementing the reification; otherwise return NIL."
  (when (closed-naut? object)
    (missing-larkc 10332)))

(defun find-ground-naut (object)
  "[Cyc] If OBJECT is a ground, unreified, specification of a reified non-atomic term, then return the NART implementing the reification; otherwise return NIL."
  (when (ground-naut? object)
    (missing-larkc 10333)))

(defun atomic-sentence-with-pred-p (asent pred)
  "[Cyc] Returns T iff ASENT is (possibly) an atomic sentence with predicate PRED.
Assumes equality can be tested with #'eq."
  (and (possibly-atomic-sentence-p asent)
       (eq pred (atomic-sentence-predicate asent))))

(defun atomic-sentence-with-any-of-preds-p (asent preds)
  "[Cyc] Returns T iff ASENT is (possibly) an atomic sentence with a predicate in PREDS.
Assumes equality can be tested with #'eq."
  (and (possibly-atomic-sentence-p asent)
       (member-eq? (atomic-sentence-predicate asent) preds)))

(defun possibly-cycl-formula-p (object)
  "[Cyc] Returns T iff OBJECT is an EL formula, a nart, or an assertion."
  (or (el-formula-p object)
      (nart-p object)
      (assertion-p object)))

(defun negated? (form)
  "[Cyc] Assuming FORM is a valid CycL formula, return T IFF it is negated."
  (and (consp form)
       (eq (first form) #$not)
       (length= form 2)))

(defun negate (form)
  "[Cyc] Assuming FORM is a valid CycL formula, return a negated version of it."
  (if (negated? form)
      (formula-arg1 form)
      (list #$not form)))

(defun possibly-negate (sentence truth)
  "[Cyc] Assuming SENTENCE is a CycL sentence, return a negated version of it if TRUTH is :FALSE"
  ;; Technically, the comment should say "is not :TRUE".
  (if (eq truth :true)
      sentence
      (negate sentence)))

(defun formula-arg (formula argnum &optional (seqvar-handling :ignore))
  "[Cyc] Returns the ARGNUMth argument of FORMULA.  An ARGNUM of 0 will return the operator. Works with forts and assertions.
If seqvar-handling is :IGNORE, it will return NIL if asked for the arg position where the sequence variable is.
If seqvar-handling is :REGULARIZE, it will return the sequence variable if asked for its position.
e.g. (formula-arg (<pred> <arg1> . ?SEQ) 2 :IGNORE)     -> NIL
but  (formula-arg (<pred> <arg1> . ?SEQ) 2 :REGULARIZE) -> ?SEQ"
  (cond
   ((not (non-negative-integer-p argnum)) nil)
   ((el-formula-p formula) (el-formula-arg formula argnum seqvar-handling))
   ((nart-p formula) (missing-larkc 10395))
   ((assertion-p formula) (el-formula-arg (assertion-hl-formula formula) argnum))))

(defun el-formula-arg (el-formula argnum &optional (seqvar-handling :ignore))
  "[Cyc] Returns the ARGNUMth argument of EL-FORMULA.  An ARGNUM of 0 will return the operator.
If seqvar-handling is :ignore, it will return NIL if asked for the arg position where the sequence variable is.
If seqvar-handling is :regularize, it will return the sequence variable if asked for its position.
e.g. (el-formula-arg (<pred> <arg1> . ?SEQ) 2 :IGNORE)     -> NIL
but  (el-formula-arg (<pred> <arg1> . ?SEQ) 2 :REGULARIZE) -> ?SEQ"
  (when (el-formula-arity>= el-formula argnum seqvar-handling)
    (nth argnum (el-formula-terms el-formula seqvar-handling))))

(defun formula-arg0 (formula)
  "[Cyc] Returns the 0th argument of FORMULA, which is by convention the operator. Returns NIL if FORMULA is not a formula."
  (cond
    ((el-formula-p formula) (el-formula-operator formula))
    ((nart-p formula) (missing-larkc 10396))
    ((assertion-p formula) (el-formula-operator (assertion-hl-formula formula)))))

(defun-inline formula-operator (formula)
  "[Cyc] Returns the operator of FORMULA. Returns NIL if FORMULA is not a formula."
  (formula-arg0 formula))

(defun-inline el-formula-operator (el-formula)
  "[Cyc] Returns the operator of EL-FORMULA."
  (car el-formula))

(defun-inline formula-arg1 (formula &optional (seqvar-handling :ignore))
  (formula-arg formula 1 seqvar-handling))

(defun-inline formula-arg2 (formula &optional (seqvar-handling :ignore))
  "[Cyc] Returns the 2nd argument of FORMULA. Returns NIL if FORMULA is not a formula."
  (formula-arg formula 2 seqvar-handling))

(defun-inline formula-arg3 (formula &optional (seqvar-handling :ignore))
  "[Cyc] Returns the 3rd argument of FORMULA. Returns NIL if FORMULA is not a formula."
  (formula-arg formula 3 seqvar-handling))

(defun formula-args (formula &optional (seqvar-handling :ignore))
  "[Cyc] Returns the arguments of FORMULA.
If seqvar-handling is :IGNORE, it chops off the sequence var if there is one.
If seqvar-handling is :REGULARIZE, it treats the sequence var as a regular variable.
If seqvar-handling is :INCLUDE, it returns it as a sequence var.
Note that using the :INCLUDE option may cause formula-args to return a variable instead of a list!
e.g. (formula-args (#$different . ?X) :INCLUDE) -> ?X
Does the right thing for narts and assertions, but ignores the MT of the assertion.
Returns NIL if FORMULA is not a possibly-cycl-formula-p."
  (cond
    ((el-formula-p formula) (el-formula-args formula seqvar-handling))
   ((nart-p formula) (missing-larkc 10397))
   ((assertion-p formula) (el-formula-args (assertion-hl-formula formula)))))

(defun el-formula-args (el-formula &optional (seqvar-handling :ignore))
  "[Cyc] Return the arguments of FORMULA.
If seqvar-handling is :IGNORE, it chops off the sequence var if there is one.
If seqvar-handling is :REGULARIZE, it treats the sequence var as a regular variable.
If seqvar-handling is :INCLUDE, it returns it as a sequence var.
Note that using the :INCLUDE option may cause el-formula-args to return a variable instead of a list!
e.g. (el-formula-args (#$different . ?X) :INCLUDE) -> ?X"
  (if (non-dotted-list-p el-formula)
      (cdr el-formula)
      (formula-terms-int (cdr el-formula) seqvar-handling)))

(defun formula-terms (formula &optional (seqvar-handling :ignore)) 
  "[Cyc] R a list of the terms in FORMULA.
If seqvar-handling is :IGNORE, it chops off the sequence var if there is one.
If seqvar-handling is :REGULARIZE, it treats the sequence var as a regular variable.
If seqvar-handling is :INCLUDE, it returns it as a sequence var.
Does the right thing for narts and assertions, but ignores the MT of the assertion.
returns NIL if FORMULA is not a possibly-cycl-formula-p."
  (cond
    ((el-formula-p formula) (el-formula-terms formula seqvar-handling))
    ((nart-p formula) (missing-larkc 10398))
    ((assertion-p formula) (el-formula-terms (assertion-hl-formula formula)))))

(defun-inline el-formula-terms (el-formula &optional (seqvar-handling :ignore)) 
  "[Cyc] Returns a list of the terms in EL-FORMULA.
If seqvar-handling is :IGNORE, it chops off the sequence var if there is one.
If seqvar-handling is :REGULARIZE, it treats the sequence var as a regular variable.
If seqvar-handling is :INCLUDE, it returns it as a sequence var."
  (formula-terms-int el-formula seqvar-handling))

(defun formula-terms-int (formula seqvar-handling &optional force-one-pass?)
  (if force-one-pass?
      (formula-terms-int-one-pass formula seqvar-handling)
      (formula-terms-int-two-pass formula seqvar-handling)))

(defun formula-terms-int-two-pass (formula seqvar-handling)
  "[Cyc] Returns the terms of FORMULA. This version makes two passes if FORMULA has a sequence variable, but it avoids the consing done by VALUES in Allegro for formulas without sequence variables. Also it avoids cdr recursion."
  (cond
   ((formula-with-sequence-term? formula) (formula-terms-int-one-pass formula seqvar-handling))
   ((consp formula) formula)
   (t (formula-terms-int-one-pass formula seqvar-handling))))

(defun formula-terms-int-one-pass (formula seqvar-handling)
  "[Cyc] Return 0: The terms of FORMULA.
Return 1: Whether to cons.
In the case of formula-args having the optional sequence var argument be :INCLUDE, we can simply use rest; in the case it is :IGNORE or :REGULARIZE, we could use a recursive internal function that recurses down the arg-list until a sequence var is encountered and, only if one is encountered conses the car (the arg) while unwinding; if no result is consed-up (e.g., no sequence var is found), it can simply return rest. No separate call to proper-list-p would be made.  This would seem to minimize both consing and cdr'ing through the formula args. -ksm"
  (cond ((not formula) (values nil nil))
        ((consp formula) (multiple-value-bind (subformula cons?)
                             (formula-terms-int (rest formula) seqvar-handling t)
                           (if cons?
                               (values (cons (car formula) subformula) t)
                               (values formula nil))))
        ((cyc-var? formula) (case seqvar-handling
                              (:ignore (values nil t))
                              (:regularize (values (list formula) t))
                              (:include (values formula nil))
                              (otherwise (values nil t))))
         (t (el-error 3 "formula-terms-int got a non-el-variable or cons: ~a~%" formula)
            (values nil t))))

(defun-inline nat-args (nat &optional (seqvar-handling :ignore)) 
  "[Cyc] Returns (as a list or a variable) the arguments of NAT. Returns NIL if NAT is not a nat.
If seqvar-handling is :IGNORE, it chops off the sequence var if there is one.
If seqvar-handling is :REGULARIZE, it treats the sequence var as a regular variable.
If seqvar-handling is :INCLUDE, it returns it as a sequence var."
  (formula-args nat seqvar-handling))

(defun-inline nat-arg (nat n &optional (seqvar-handling :ignore)) 
  "[Cyc] Return the argument in position N of non-atomic term NAT.
If seqvar-handling is :IGNORE, it will return NIL if asked for the arg position where the sequence variable is.
If seqvar-handling is :REGULARIZE, it will return the sequence variable if asked for its position.
e.g. (nat-arg (<func> <arg1> . ?SEQ) 2 :IGNORE)     -> NIL
but  (nat-arg (<func> <arg1> . ?SEQ) 2 :REGULARIZE) -> ?SEQ"
  (formula-arg nat n seqvar-handling))

(defun-inline nat-functor (nat) 
  "[Cyc] Returns the functor of NAT. Returns NIL if NAT is not a nat."
  (nat-arg0 nat))

(defun-inline naut-functor (naut)
  "[Cyc] Returns the functor of NAUT."
  (el-formula-operator naut))

(defun nat-arg0 (nat) 
  "[Cyc] Returns the 0th argument of NAT, which is by convention the functor. Returns NIL if NAT is not a nat."
  (cond
    ((el-formula-p nat) (naut-functor nat))
    ((nart-p nat) (missing-larkc 10400))))

(defun-inline nat-arg1 (nat &optional (seqvar-handling :ignore)) 
  "[Cyc] Returns the 1st argument of NAT. Returns NIL if NAT is not a nat."
  (nat-arg nat 1 seqvar-handling))

(defun-inline sentence-arg (sentence argnum &optional (seqvar-handling :ignore))
  (formula-arg sentence argnum seqvar-handling))

(defun-inline sentence-args (sentence &optional (seqvar-handling :ignore))
  (formula-args sentence seqvar-handling))

(defun-inline sentence-truth-function (sentence)
  (formula-arg0 sentence))

(defun-inline sentence-arg0 (sentence)
  (formula-arg0 sentence))

(defun-inline sentence-arg1 (asent &optional (seqvar-handling :ignore))
  (formula-arg1 asent seqvar-handling))

(defun-inline sentence-arg2 (asent &optional (seqvar-handling :ignore))
  (formula-arg2 asent seqvar-handling))

(defun-inline atomic-sentence-arg (asent argnum &optional (seqvar-handling :ignore))
  (formula-arg asent argnum seqvar-handling))

(defun-inline atomic-sentence-args (asent &optional (seqvar-handling :ignore))
  (formula-args asent seqvar-handling))

(defun-inline atomic-sentence-predicate (asent)
  (formula-arg0 asent))

(defun-inline atomic-sentence-arg1 (asent &optional (seqvar-handling :ignore))
  (formula-arg1 asent seqvar-handling))

(defun-inline atomic-sentence-arg2 (asent &optional (seqvar-handling :ignore))
  (formula-arg2 asent seqvar-handling))

(defun-inline atomic-sentence-arg3 (asent &optional (seqvar-handling :ignore))
  (formula-arg3 asent seqvar-handling))
