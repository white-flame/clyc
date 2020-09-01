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

;; TODO - a lot of integerp tests around argnums, probably could be fixnum instead, as well as type declarations.

(defun* relation? (relation) (:inline t)
  "[Cyc] Return T iff RELATION is a relationship."
  (relation-p relation))

(defun* irreflexive-predicate? (predicate) (:inline t)
  "[Cyc] Return T iff PREDICATE is an irreflexive predicate."
  (irreflexive-binary-predicate-p predicate))

(defun* asymmetric-predicate? (predicate) (:inline t)
  "[Cyc] Return T iff PREDICATE is an asymmetric predicate."
  (asymmetric-binary-predicate-p predicate))

(defun result-isa (functor &optional mt)
  "[Cyc] Return a list of the collections that include as instances the results of non-predicate function constant FUNCTOR."
  (cond
    ((fort-p functor) (pred-values-in-relevant-mts functor #$resultIsa mt))
    ((naut-with-corresponding-nart? functor) (result-isa (find-nart functor) mt))
    ((reifiable-nat? functor #'cyc-var? mt) (meta-result-isa (nat-functor functor) mt))
    ((function-to-arg-term? functor) (argn-isa (nat-arg2 functor) (nat-arg1 functor) mt))))

(defun function-to-arg-term? (term)
  (and (consp term)
       (eql #$FunctionToArg (nat-functor term))))

(defun meta-result-isa (meta-functor &optional mt)
  "[Cyc] META-FUNCTOR is the functor of a function-denoting function; return the resultIsa collections inherited to instances of the resultIsa of META-FUNCTOR."
  (cond
    ((fort-p meta-functor)
     (let ((meta-result-isa nil))
       (let ((*mapping-fn* #'pred-arg-values-in-relevant-mts)
             (*mapping-fn-arg* 1)
             (*mapping-fn-arg2* #$relationAllInstance)
             (*mapping-fn-arg3* #$resultIsa)
             (*mapping-fn-arg4* mt)
             (*mapping-fn-arg5* 2)
             (*mapping-fn-arg6* 1)
             ;; TODO - are these memory allocation spaces?  remove them?
             (*sbhl-space* (get-sbhl-marking-space))
             (*sbhl-gather-space* (get-sbhl-marking-space))
             (*sbhl-suspend-new-spaces?* t))
         (dolist (result-isa (result-isa meta-functor mt))
           (setf meta-result-isa (nconc meta-result-isa (gather-all-genls #'mapping-funcall-arg result-isa mt)))))
       meta-result-isa))

    ((naut-with-corresponding-nart? meta-functor)
     (meta-result-isa (find-nart meta-functor) mt))))

(defun* anti-symmetric-predicate? (predicate) (:inline t)
  "[Cyc] Return T iff PREDICATE is an anti-symmetric predicate."
  (anti-symmetric-binary-predicate-p predicate))

(defun* transitive-predicate? (predicate) (:inline t)
  "[Cyc] Return T iff PREDICATE is a transitive predicate."
  (transitive-binary-predicate-p predicate))

(defun* anti-transitive-predicate? (predicate) (:inline t)
  "[Cyc} Return T iff PREDICATE is an anti-transitive predicate."
  (anti-transitive-binary-predicate-p predicate))

(defun binary-predicate? (predicate)
  "[Cyc] Return T iff PREDICATE is a predicate of arity 2."
  (and (predicate-p predicate)
       (eql (arity predicate) 2)))

(defun admitting-defns? (col &optional mt)
  (or (sufficient-defns? col mt)
      (defining-defns? col mt)))

(defun* sufficient-defns? (col &optional mt) (:inline t)
  (some-pred-value-in-relevant-mts col #$defnSufficient mt))

(defun* necessary-defns? (col &optional mt) (:inline t)
  (some-pred-value-in-relevant-mts col #$defnNecessary mt))

(defun* defining-defns? (col &optional mt) (:inline t)
  (some-pred-value-in-relevant-mts col #$defnIff mt))

(defun* cyclist? (term) (:inline t)
  "[Cyc] Return T iff TERM is an instance of #$Cyclist somewhere."
  (isa-in-any-mt? term #$Cyclist))

(defun decontextualized-predicate? (predicate)
  "[Cyc] Return T iff PREDICATE is decontextualized, i.e. it can be thought of as having its complete extent in all mts."
  (some-pred-value-in-relevant-mts predicate #$decontextualizedPredicate *decontextualized-predicate-mt*))

(defun predicate-convention-mt (predicate)
  (or (fpred-value-in-any-mt predicate #$predicateConventionMt)
      *default-convention-mt*))

(defun decontextualized-collection? (collection)
  "[Cyc] Return T iff COLLECTION is decontextualized, i.e. it can be thought of as having its complete collection extent in all mts."
  (some-pred-value-in-relevant-mts collection #$decontextualizedCollection *decontextualized-collection-mt*))

(defun collection-convention-mt (collection)
  (or (fpred-value-in-any-mt collection #$collectionConventionMt)
      *default-convention-mt*))

(defun decontextualized-literal? (literal)
  (when (el-formula-p literal)
    (let ((predicate (literal-predicate literal)))
      (and (fort-p predicate)
           (or (decontextualized-predicate? predicate)
               (decontextualized-collection-literal? literal))))))

(defun decontextualized-literal-convention-mt (literal)
  (if (decontextualized-collection-literal? literal)
      (collection-convention-mt (sentence-arg2 (literal-atomic-sentence literal)))
      (predicate-convention-mt (literal-predicate literal))))

(defun decontextualized-collection-literal? (literal)
  (when (el-binary-formula-p literal)
    (let ((predicate (literal-predicate literal))
          (arg2 (literal-arg2 literal)))
      (and (or (eql #$isa predicate)
               (eql #$genls predicate))
           (fort-p arg2)
           (decontextualized-collection? arg2)))))

(defun decontextualized-atomic-cnf? (cnf)
  (when (atomic-clause-p cnf)
    (let ((asent (atomic-cnf-asent cnf)))
      (decontextualized-literal? asent))))

(defparameter *decontextualized-weakening-prohibited?* t
  "[Cyc] Temporary control variable:  When non-nil, we don't decontextualize to a lower microtheory from a strictly higher one.")

(defun* decontextualized-weakening-prohibited? () (:inline t)
  *decontextualized-weakening-prohibited?*)

(defun mt-matches-convention-mt? (given-mt convention-mt)
  (or (eq given-mt convention-mt)
      (hlmt-equal? given-mt convention-mt)
      (and (decontextualized-weakening-prohibited?)
           (proper-genl-mt? convention-mt given-mt))))

(defun possibly-convention-mt-for-decontextualized-cnf (mt cnf)
  (block nil
    (when (decontextualized-atomic-cnf? cnf)
      (let* ((asent (atomic-cnf-asent cnf))
             (convention-mt (decontextualized-literal-convention-mt asent)))
        (when (and convention-mt
                   (not (mt-matches-convention-mt? mt convention-mt)))
          (return convention-mt))))
    mt))

(defun quoted-argument? (relation argnum)
  "[Cyc] Return T iff arg number ARGNUM of RELATION is quoted via #$quotedArgument."
  (and (fort-p relation)
       (numberp argnum)
       (> argnum 0)
       (pred-u-v-holds-in-any-mt #$quotedArgnument relation argnum)))

(defun complete-extent-asserted-gaf (predicate &optional mt)
  "[Cyc] If PREDICATE's extent has been completely asserted, returns an assertion justifying this claim."
  (and (some-pred-assertion-somewhere? #$completeExtentAsserted predicate 1)
       (fpred-value-gaf-in-relevant-mts predicate #$completeExtentAsserted mt)))

(defun complete-extent-asserted-for-value-in-arg-gaf (predicate value argnum &optional mt)
  "[Cyc] If PREDICATE's curried extent is completely asserted once VALUE is its ARGNUMth argument, returns an assertion justifying this claim."
  (declare (ignore mt)) ;; likely in missing-larkc
  (when (some-pred-assertion-somewhere? #$completeExtentAssertedForValueInArg predicate 1)
    (let ((asent (make-ternary-formula #$completeExtentAssertedForValueInArg predicate value argnum)))
      (declare (ignore asent))
      (missing-larkc 12717))))

(defun complete-extent-enumerable-gaf (predicate &optional mt)
  "[Cyc] If PREDICATE's extent can be completely enumerated, returns an assertion justifying this claim."
  (when (some-pred-assertion-somewhere? #$completeExtentEnumerable predicate 1)
    (fpred-value-gaf-in-relevant-mts predicate #$completeExtentEnumerable mt)))

(defun complete-extent-decidable-gaf (predicate &optional mt)
  "[Cyc] If PREDICATE's extent can be completely decided, returns an assertion justifying this claim."
  (when (some-pred-assertion-somewhere? #$completeExtentDecidable predicate 1)
    (fpred-value-gaf-in-relevant-mts predicate #$completeExtentDecidable mt)))

(defun complete-extent-enumerable-for-arg-gaf (predicate argnum &optional mt)
  "[Cyc] If PREDICATE's curried extent is enumerable once its ARGNUMth argument is fixed, returns an assertion justifying this claim."
  (declare (ignore argnum mt))
  (when (some-pred-assertion-somewhere? #$completeExtentEnumerableForArg predicate 1)
    (missing-larkc 30008)))

(defun complete-extent-enumerable-for-value-in-arg-gaf (predicate value argnum &optional mt)
  "[Cyc] If PREDICATE's curried extent is enumerable once VALUE is its ARGNUMth argument, returns an assertion justifying this claim."
  (declare (ignore mt))
  (when (some-pred-assertion-somewhere? #$completeExtentEnumerableForValueInArg predicate 1)
    (let ((asent (make-ternary-formula #$completeExtentEnumerableForValueInArg predicate value argnum)))
      (declare (ignore asent))
      (missing-larkc 12718))))

(defun complete-extent-decidable-for-value-in-arg-gaf (predicate value argnum &optional mt)
  "[Cyc] If PREDICATE's curried extent is decidable once VALUE is its ARGNUMth argument, returns an ssertion justifying this claim."
  (declare (ignore mt))
  (when (some-pred-assertion-somewhere? #$completeExtentDecidableForValueInArg predicate 1)
    (let ((asent (make-ternary-formula #$completeExtentDecidableForValueInArg predicate value argnum)))
      (declare (ignore asent))
      (missing-larkc 12719))))

(defun* completely-enumerable-collection? (collection &optional mt) (:inline t)
  "[Cyc] Return whether COLLECTION is completely enumerable in MT."
  (completely-enumerable-collection-gaf collection mt))

(defun completely-enumerable-collection-gaf (collection mt)
  (and (fort-p collection)
       (fpred-value-gaf-in-relevant-mts collection #$completelyEnumerableCollection mt)))

(defun* backchain-required? (predicate mt) (:inline t)
  (some-pred-value-in-relevant-mts predicate #$backchainRequired mt))

(defun* backchain-forbidden (predicate &optional mt) (:inline t)
  (some-pred-value-in-relevant-mts predicate #$backchainForbidden mt))

(defun* collection-isa-backchain-required? (collection &optional mt) (:inline t)
  (some-pred-value-in-relevant-mts collection #$collectionIsaBackchainRequired mt))

(defun* collection-genls-backchain-required? (collection &optional mt) (:inline t)
  (some-pred-value-in-relevant-mts collection #$collectionGenlsBackchainRequired mt))

(defun* collection-backchain-required? (collection &optional mt) (:inline t)
  (some-pred-value-in-relevant-mts collection #$collectionBackchainRequired mt))

(defun skolemize-forward-somewhere? (function)
  (and (fort-p function)
       (some-pred-assertion-somewhere? #$skolemizeForward function 1)))

(defun skolemize-forward? (function &optional mt)
  (and (fort-p function)
       (skolemize-forward-somewhere? function)
       (some-pred-value-in-relevant-mts function #$skolemizeForward mt)))

(defun forward-reification-rule? (function rule &optional mt)
  (when (and (fort-p function)
             (rule-assertion? rule))
    ;; TODO - mt macro
    (let ((*mt* (update-inference-mt-relevance-mt mt))
          (*relevant-mt-function* (update-inference-mt-relevance-function mt))
          (*relevant-mts* (update-inference-mt-relevance-mt-list mt)))
      (pred-u-v-holds #$forwardReificationRule function rule 1 2))))

(defun arg-and-rest-isa-min-argnum (relation &optional mt)
  "[Cyc] Returns the smallest integer in an #$argAndRestIsa gaf constraining RELN if one exists, otherwise returns NIL."
  (let ((result nil))
    (when (some-arg-and-rest-isa-assertion-somewhere? relation)
      (dolist (argnum (pred-values-in-relevant-mts relation #$argAndRestIsa mt))
        (when (integerp argnum)
          (when (or (not result)
                    (< argnum result))
            (setf result argnum)))))
    result))

(defun arg-and-rest-isa-applicable? (reln argnum &optional mt)
  "[Cyc] Returns T iff ARGNUM of RELN is constrained via #$argAndRestIsa"
  (let ((min-argnum (arg-and-rest-isa-min-argnum reln mt)))
    (and (integerp min-argnum)
         (>= argnum min-argnum))))

(defun arg-and-rest-quoted-isa-min-argnum (relation &optional mt)
  "[Cyc] Returns the smallest integer in an #$argAndRestQuotedIsa gaf constraining RELN if one exists, otherwise returns NIL."
  (let ((result nil))
    (dolist (argnum (pred-values-in-relevant-mts relation #$argAndRestQuotedIsa mt))
      (cond
        ((not (integerp argnum)) nil)
        ((not result) (setf result argnum))
        ((< argnum result) (setf result argnum))))
    result))

(defun arg-and-rest-quoted-isa-applicable? (reln argnum &optional mt)
  "[Cyc] Returns T iff ARGNUM of RELN is constrained via #$argAndRestIsa."
  (let ((min-argnum (arg-and-rest-quoted-isa-min-argnum reln mt)))
    (and (integerp min-argnum)
         (>= argnum min-argnum))))

(defun* argn-isa (relation argnum &optional mt) (:inline t)
  "[Cyc] Returns a list of the local isa constraints applied to the ARGNUMth argument of RELATION (#$argsIsa conjoins with #$arg1Isa et al)."
  (argn-isa-int relation argnum mt))

(defun argn-quoted-isa (relation argnum &optional mt)
  "[Cyc] Returns a list of the local isa constraints applied to the ARGNUMth argument of RELATION (#$argsIsa conjoins with #$arg1Isa et al)."
  (if (within-czer-memoization-state?)
      (copy-list (czer-argn-quoted-isa-int relation argnum (mt-info mt)))
      (argn-quoted-isa-int relation argnum mt)))

(defun argn-isa-int (relation argnum mt)
  (cond
    ((fort-p relation) (argn-isa-int-2 relation argnum mt))
    ((reifiable-nat? relation #'cyc-var? mt) (missing-larkc 10348))))

(defun argn-isa-int-2 (relation argnum mt)
  (let ((result nil))
    (when (some-args-isa-assertion-somewhere? relation)
      (setf result (nconc result (pred-values-in-relevant-mts relation #$argsIsa mt))))
    (when (some-arg-and-rest-isa-assertion-somewhere? relation)
      (missing-larkc 6808))
    (when (positive-integer-p argnum)
      (setf result (nconc result
                          (if-let ((arg-isa-pred (arg-isa-pred-int argnum)))
                            (cached-arg-isas-in-mt relation argnum mt)
                            (pred-values-in-relevant-mts relation arg-isa-pred mt)))))
    (delete-duplicate-forts result)))

(defun argn-quoted-isa-int (relation argnum mt)
  (cond
    ((fort-p relation) (cond
                         ;; TODO - do these need to be freshly consed lists?  can we use literal lists with #$ ?
                         ((cyc-const-logical-operator-p relation)
                          (list #$CyclSentence-Assertible))
                         ((and (cyc-const-quantifier-p relation)
                               (= argnum (quantified-sub-sentence-argnum-for-operator relation)))
                          (list #$CyclSentence-Assertible))
                         ((cyc-const-quantifier-p relation)
                          (missing-larkc 30632))
                         (t (let ((result nil))
                              (dolist (arg-quoted-isa-pred (arg-quoted-isa-preds argnum relation mt))
                                (cond
                                  ((member? arg-quoted-isa-pred *arg-quoted-isa-binary-preds*)
                                   (setf result (nconc result (pred-values-in-relevant-mts relation arg-quoted-isa-pred mt))))
                                  ((member? arg-quoted-isa-pred *arg-quoted-isa-ternary-preds*)
                                   (missing-larkc 6814))
                                  ((isa? arg-quoted-isa-pred #$ArgQuotedIsaBinaryPredicate)
                                   (setf result (nconc result (pred-values-in-relevant-mts relation arg-quoted-isa-pred mt))))
                                  ((isa? arg-quoted-isa-pred #$ArgQuotedIsaTernaryPredicate)
                                   (missing-larkc 6815))
                                  (t (el-error 3 "Illegal arg-quoted-isa-pred encountered in argn-isa: ~s" arg-quoted-isa-pred))))
                              (remove-duplicate-forts result)))))
    ((reifiable-nat? relation #'cyc-var? mt) (missing-larkc 10349))))

(defun arg-isa-pred-int (index)
  ;; TODO - convert to a bounds-checked array lookup, instead of this junk
  (case index
    (1 #$arg1Isa)
    (2 #$arg2Isa)
    (3 #$arg3Isa)
    (4 #$arg4Isa)
    (5 #$arg5Isa)
    (6 #$arg6Isa)
    (0 #$argsIsa)
    (otherwise (el-error 3 "Illegal index specification for arg-isa-pred: ~s" index))))

(defun arg-isa-pred (index &optional reln mt)
  "[Cyc] Returns the appropriate predicate for constraining the INDEXth argument of RELN."
  (cond
    (reln (let ((argnum (arg-and-rest-isa-min-argnum reln mt)))
            (if (integerp argnum)
                (if (>= index argnum)
                    #$argAndRestIsa
                    (arg-isa-pred-int index))
                (if (variable-arity? reln)
                    #$argsIsa
                    (arg-isa-pred-int index)))))
    ((valid-argnum-p index) (arg-isa-pred-int index))
    (t (el-error 3 "Illegal index specification for arg-isa-pred: ~s" index))))

(defun arg-isa-preds (argnum &optional reln mt)
  "[Cyc] Returns the appropriate predicates for constraining the INDEXth argument of RELN."
  (cond
    ((valid-argnum-p argnum) (let ((result nil))
                               (when-let ((var (arg-isa-pred-int argnum)))
                                 (push var result))
                               (pushnew #$argsIsa result)
                               (when (and (fort-p reln)
                                          (arg-and-rest-isa-applicable? reln argnum mt))
                                 (push #$argAndRestIsa result))
                               (nreverse result)))
    (t (el-error 3 "Illegal argnum specification for arg-isa-preds: ~s" argnum))))

(defun arg-quoted-isa-pred-int (index)
  ;; TODO - convert to a bounds-checked array lookup, instead of this junk
  (case index
    (1 #$arg1QuotedIsa)
    (2 #$arg2QuotedIsa)
    (3 #$arg3QuotedIsa)
    (4 #$arg4QuotedIsa)
    (5 #$arg5QuotedIsa)
    (6 #$arg6QuotedIsa)
    (t (el-error 3 "Illegal index specification for arg-quoted-isa-pred: ~s" index))))

(defun arg-quoted-isa-pred (index &optional reln mt)
  "[Cyc] Returns the appropriate predicate for constraining the INDEXth argument of RELN."
  (cond
    (reln (let ((argnum (arg-and-rest-quoted-isa-min-argnum reln mt)))
            (if (integerp argnum)
                (if (>= index argnum)
                    #$argAndRestQuotedIsa
                    (arg-quoted-isa-pred-int index))
                (if (variable-arity? reln)
                    #$argsQuotedIsa
                    (arg-quoted-isa-pred-int index)))))
    ((valid-argnum-p index) (arg-quoted-isa-pred-int index))
    (t (el-error 3 "Illegal index specification for arg-quoted-isa-pred: ~s" index))))

(defun arg-quoted-isa-preds (argnum &optional reln mt)
  "[Cyc] Returns the appropriate predicates for constraining the INDEXth argument of RELN."
  (cond
    ((valid-argnum-p argnum) (let ((result nil))
                               (when-let ((var (arg-quoted-isa-pred-int argnum)))
                                 (push var result))
                               (pushnew #$argsQuotedIsa result)
                               (when (and (fort-p reln)
                                          (arg-and-rest-quoted-isa-applicable? reln argnum mt))
                                 (push #$argAndRestQuotedIsa result))
                               (nreverse result)))
    (t (el-error 3 "Illegal argnum specification for arg-quoted-isa-preds: ~s" argnum))))

(defun arg-isa-inverse (index &optional reln mt)
  "[Cyc] Returns the appropriate predicate for constraining the inverse of the INDEXth argument of RELN."
  (case index
    (1 (arg-isa-pred 2 reln mt))
    (2 (arg-isa-pred 1 reln mt))))

(defun arg-quoted-isa-inverse (index &optional reln mt)
  "[Cyc] Returns the appropriate predicate for constraining the inverse of the INDEXth argument of RELN."
  (case index
    (1 (arg-quoted-isa-pred 2 reln mt))
    (2 (arg-quoted-isa-pred 1 reln mt))))

(defun inverse-argnum (argnum)
  "[Cyc] Return the inverse argnum of ARGNUM."
  (case argnum
    (1 2)
    (2 1)
    (otherwise (error "Invalid argument to inverse-argnum: ~s" argnum))))

(defun isa-pred-arg (isa-pred)
  "[Cyc] Return the arg constrained by ISA-PRED (e.g., (isa-pred-arg #$arg1Isa) -> 1)
By convention (isa-pred-arg #$argsIsa) -> 0."
  ;; TODO - ensure that the constants are manifesting properly here, because CASE doesn't evaluate its test terms
  (case isa-pred
    (#$arg1Isa 1)
    (#$arg2Isa 2)
    (#$arg3Isa 3)
    (#$arg4Isa 4)
    (#$arg5Isa 5)
    (#$arg6Isa 6)
    (#$arg0Isa 0)))

(defun argn-genl (relation argnum &optional mt)
  "[Cyc] Returns the local genl constraints applied to the ARGNUMth argument of RELATION."
  (cond
    ((fort-p relation) (let ((result nil))
                         (dolist (arg-genl-pred (arg-genl-preds argnum relation mt))
                           (cond
                             ((member? arg-genl-pred *arg-genl-binary-preds*)
                              (setf result (nconc result (pred-values-in-relevant-mts relation arg-genl-pred mt))))
                             ((member? arg-genl-pred *arg-genl-ternary-preds*)
                              (missing-larkc 6803))
                             ((isa? arg-genl-pred #$ArgGenlBinaryPredicate)
                              (setf result (nconc result (pred-values-in-relevant-mts relation arg-genl-pred mt))))
                             ((isa? arg-genl-pred #$ArgGenlTermaryPredicate)
                              (missing-larkc 6804))
                             (t (el-error 3 "Illegal arg-genl-pred encountered in argn-genl: ~s" arg-genl-pred))))
                         (remove-duplicate-forts result)))
    ((reifiable-nat? relation #'cyc-var? mt) (missing-larkc 10350))))

(defun arg-and-rest-genl-min-argnum (relation &optional mt)
  "[Cyc] Returns the smallest integer in an #$argAndRestGenl gaf constraining RELN if one exists, otherwise returns NIL."
  (let ((result nil))
    (dolist (argnum (pred-values-in-relevant-mts relation #$argAndRestGenl mt))
      (cond
        ((not (integerp argnum)) ;; do nothing
         )
        ((not result) (setf result argnum))
        ((< argnum result) (setf result argnum))))
    result))

(defun arg-and-rest-genl-applicable? (reln argnum &optional mt)
  "[Cyc] Returns T iff ARGNUM of RELN is constrained via #$argAndRestGenl."
  (let ((min-argnum (arg-and-rest-genl-min-argnum reln mt)))
    (and (integerp min-argnum)
         (>= argnum min-argnum))))

(defun arg-genl-pred-int (index)
  ;; TODO - convert to bounds-checked array lookup
  (case index
    (1 #$arg1Genl)
    (2 #$arg2Genl)
    (3 #$arg3Genl)
    (4 #$arg4Genl)
    (5 #$arg5Genl)
    (6 #$arg6Genl)
    (0 #$argsGenl)
    (otherwise (el-error 3 "Illegal index specification for arg-genl-pred: ~s" index))))

(defun arg-genl-pred (index &optional reln mt)
  "[Cyc] Returns the appropriate predicate for constraining the INDEXth argument of RELN."
  (cond
    (reln (let ((argnum (arg-and-rest-genl-min-argnum reln mt)))
            (if (integerp argnum)
                (if (>= index argnum)
                    #$argAndRestGenl
                    (arg-genl-pred-int index))
                (if (variable-arity? reln)
                    #$argsGenl
                    (arg-genl-pred-int index)))))
    ((valid-argnum-p index) (arg-genl-pred-int index))
    (t (el-error 3 "Illegal index specification for arg-genl-pred: ~s" index))))

(defun arg-genl-preds (argnum &optional reln mt)
  "[Cyc] Returns the appropriate predicates for constraining the ARGNUMth argument of RELN."
  (cond
    ((valid-argnum-p argnum) (let ((result nil))
                               (when-let ((var (arg-genl-pred-int argnum)))
                                 (push var result))
                               (pushnew #$argsGenl result)
                               (when (and (fort-p reln)
                                          (arg-and-rest-genl-applicable? reln argnum mt))
                                 (push #$argAndRestGenl result))
                               (nreverse result)))
    (t (el-error 3 "Illegal argnum specification for arg-genl-preds: ~s" argnum))))

(defun arg-genl-inverse (index &optional reln mt)
  "[Cyc] Returns the appropriate predicate for constraining the inverse of the INDEXth argument of RELN."
  (case index
    (1 (arg-genl-pred 2 reln mt))
    (2 (arg-genl-pred 1 reln mt))))

(defun argn-format-inverse (n)
  "[Cyc] Returns the appropriate arg-format predicate for constraining the inverse of N."
  (case n
    (1 (argn-format-pred 2))
    (2 (argn-format-pred 1))))

(defun argn-format-pred (n)
  ;; TODO - convert to bounds-checked array
  (case n
    (1 #$arg1Format)
    (2 #$arg2Format)
    (3 #$arg3Format)
    (4 #$arg4Format)
    (5 #$arg5Format)
    (6 #$arg6Format)
    (otherwise (el-error 3 "Illegal arg specification for argn-format-pred: ~s" n))))

(defun inter-arg-format-pred (ind-arg dep-arg)
  (let ((ind-candidates (inter-arg-format-preds-ind ind-arg))
        (dep-candidates (inter-arg-format-preds-dep dep-arg))
        (pred nil))
    ;; TODO - return-from would be simpler & faster
    (csome (candidate ind-candidates pred)
      (when (member? candidate dep-candidates)
        (setf pred candidate)))
    pred))

(defun inter-arg-format-preds-dep (arg)
  ;; TODO - convert to bounds-checked array
  (case arg
    (1 '(#$interArgFormat2-1 #$interArgFormat3-1 #$interArgFormat4-1 #$interArgFormat5-1))
    (2 '(#$interArgFormat1-2 #$interArgFormat3-2 #$interArgFormat4-2 #$interArgFormat5-2))
    (3 '(#$interArgFormat1-3 #$interArgFormat2-3 #$interArgFormat4-3 #$interArgFormat5-3))
    (4 '(#$interArgFormat1-4 #$interArgFormat2-4 #$interArgFormat3-4 #$interArgFormat5-4))
    (5 '(#$interArgFormat1-5 #$interArgFormat2-5 #$interArgFormat3-5 #$interArgFormat4-5))))

(defun inter-arg-format-preds-ind (arg)
  ;; TODO - convert to bounds-checked array
  (case arg
    (1 '(#$interArgFormat1-2 #$interArgFormat1-3 #$interArgFormat1-4 #$interArgFormat1-5))
    (2 '(#$interArgFormat2-1 #$interArgFormat2-3 #$interArgFormat2-4 #$interArgFormat2-5))
    (3 '(#$interArgFormat3-1 #$interArgFormat3-2 #$interArgFormat3-4 #$interArgFormat3-5))
    (4 '(#$interArgFormat4-1 #$interArgFormat4-2 #$interArgFormat4-3 #$interArgFormat4-5))
    (5 '(#$interArgFormat5-1 #$interArgFormat5-2 #$interArgFormat5-3 #$interArgFormat5-4))))

(defun fan-out-arg (pred &optional mt)
  "[Cyc] Which arg is the #$fanOutArg for hierarchically transitive PRED."
  (or (asserted-fan-out-arg pred mt) 1))

(defun asserted-fan-out-arg (pred &optional mt)
  "[Cyc] Return which arg is asserted to be the #$fanOutArg for hierarchically transitive PRED, or NIL."
  (fpred-value-in-relevant-mts pred #$fanOutArg mt))

(defun assertion-still-there? (assertion truth)
  (some (lambda (argument) (eq truth (argument-truth argument)))
        (assertion-arguments assertion)))

(defun scoping-args (relation &optional mt)
  (when (and (fort-p relation)
             (some-scoping-arg-somewhere? relation))
    (pred-values-in-relevant-mts relation #$scopingArg mt)))

(defun some-scoping-arg-somewhere? (relation)
  (some-pred-assertion-somewhere? #$scopingArg relation 1))

(defun all-term-assertions (term &optional remove-duplicates?)
  "[Cyc] Return a list of all the assertions indexed via the indexed term TERM."
  (term-assertions term #$InferencePSC remove-duplicates?))

(defun term-assertions (term &optional mt remove-duplicates?)
  (with-inference-mt-relevance mt
    (gather-index term remove-duplicates?)))

(defun not-assertible-predicate? (pred &optional mt)
  (if (fort-p pred)
      (some-pred-value-in-relevant-mts pred #$notAssertible mt)
      (not (reifiable-nat? pred #'cyc-var? mt))))

(defun not-assertible-collection? (collection &optional mt)
  (when (fort-p collection)
    (some-pred-value-in-relevant-mts collection #$notAssertibleCollection mt)))

(defun not-assertible-mt? (mt)
  (let ((reduced-mt (canonicalize-hlmt mt)))
    (when (and (fort-p reduced-mt)
               (possibly-mt-p reduced-mt))
      (some-pred-value-in-any-mt reduced-mt #$notAssertibleMt))))

(deflexical *common-non-skolem-indeterminate-term-denoting-functions* '(#$RelationAllExistsFn
                                                                        #$RelationExistsAllFn
                                                                        #$RelationInstanceExistsFn
                                                                        #$RelationExistsInstanceFn)
  "[Cyc] Functions commonly known to denote non-skolem indeterminate terms.")

(defun common-non-skolem-indeterminate-term-denoting-function? (object)
  (member-eq? object *common-non-skolem-indeterminate-term-denoting-functions*))

(defun non-skolem-indeterminate-term-denoting-function? (object)
  (and (fort-p object)
       (function? object)
       (skolem-function-p object)
       (isa? object #$IndeterminateTermDenotingFunction)))

(defun fast-non-skolem-indeterminate-term? (term)
  "[Cyc] Will sometimes return false negatives."
  (cond
    ((or (variable-p term)
         (cycl-unrepresented-term-p term))
     nil)
    ((and (el-formula-p term)
          (common-non-skolem-indeterminate-term-denoting-function? (formula-operator term)))
     t)
    ((and (el-formula-p term)
          (non-skolem-indeterminate-term-denoting-function? (formula-operator term)))
     t)))

(defparameter *smallest-num-index-so-far* nil)

(defparameter *most-specialized-fort-so-far* nil)





