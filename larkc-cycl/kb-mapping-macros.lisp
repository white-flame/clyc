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

;; DESIGN - abstracted the serious repetition and massive symbol names into a large macro. Hopefully this all works. I couldn't tell which macro this came from, so made my own.

;; TODO - this file has DO- macros for each iterator type in here. Implement them.

;; TODO - put warnings or deprecation in incomplete iterator definitions to ensure nobody uses them

(defmacro define-kb-final-index-spec-iterator (name lambda-list
                                               &key
                                                 ;; list of (name init-form), with lambda-list values in scope
                                                 slots
                                                 singleton-form
                                                 ;; slots are in scope
                                                 done-form
                                                 next-form
                                                 relevant-keylist
                                                 quiesce-step)
  
  (let* ((all-lambda-list-terms (remove '&optional lambda-list))
         (iterator (symbolicate name "-FINAL-INDEX-SPEC-ITERATOR"))
         (state (symbolicate iterator "-STATE"))
         (with (symbolicate "WITH-" iterator))
         (fn-next (symbolicate iterator "-NEXT"))
         (fn-initialize (symbolicate "INITIALIZE-" state))
         (fn-quiesce (symbolicate iterator "-QUIESCE"))
         (fn-quiesce-one-step (symbolicate fn-quiesce "-ONE-STEP")))
    (alexandria:with-gensyms (result done?)
      `(progn
         
         ;; Define the accessors for the state vector
         
         (defstruct (,state (:type vector))
           ,@ (mapcar #'first slots))

         (defmacro ,with (state &body body)
           `(with-accessors ,',(mapcar (lambda (slot)
                                         (let ((name (car slot)))
                                           (list name (symbolicate state "-" name))))
                                       slots)
                ,state
              ,@body))

         ;; Functions
         
         (defun ,(symbolicate "NEW-" name "-FINAL-INDEX-SPEC-ITERATOR") ,lambda-list
           "[Cyc] Makes an iterator which spits out final-index-specs, each of which is a complete path (i.e. a list of keys) leading down to a final index (a list) of assertions."
           (if (simple-indexed-term-p ,(first lambda-list))
               (new-singleton-iterator ,singleton-form)
               (let ((state (,fn-initialize ,@all-lambda-list-terms)))
                 (new-iterator state
                               (lambda (state)
                                 (,with state
                                        ,done-form))
                               #',fn-next))))

         (defun ,fn-initialize ,all-lambda-list-terms
           (declare (ignorable ,@all-lambda-list-terms))
           (vector ,@ (mapcar #'second slots)))

         (defun-inline ,fn-quiesce-one-step (state)
           (,with state ,quiesce-step))
         
         (defun-inline ,fn-quiesce (state)
           ;; TODO - there is no <name>-current-keylist as the comments originally referenced
           "[Cyc] Iterates over the keys in STATE until it ends up with its current keylist being valid and relevant, with validity and relevance being determined by :RELEVANT-KEYLIST. It may not need to iterate over any keys in STATE, in which case STATE is left unchanged.
Return 0: The relevant final-index-spec list thus formed, if any.
Return 1: Whether quiescence terminated early due to running out of keys."
           (,with state
                  (let ((,result nil)
                        (,done? nil))
                    (until (or ,result
                               ,done?)
                      (let ((keylist ,relevant-keylist))
                        (if keylist
                            ,(when slots
                               `(,with state
                                       (setf ,result (list* ,(first lambda-list) ,(make-keyword name) keylist))))
                            (setf ,done? (,fn-quiesce-one-step state)))))
                    (values ,result ,done?))))
         
         (defun ,fn-next (state)
           (,with state
                  (multiple-value-bind (final-index-spec done?) (,fn-quiesce state)
                    ,next-form
                    (values final-index-spec done?))))))))











;; GAF-ARG

(define-kb-final-index-spec-iterator gaf-arg (term &optional argnum predicate)
  :slots (;; [Cyc] The input term.
          (term term)
          ;; [Cyc] The input predicate.
          (predicate predicate)
          ;; [Cyc] A note containing information about the state of the keys, used to control code flow.
          (note :argnum-keys-are-fresh)
          ;; [Cyc] The remaining argnums to iterate over.
          (argnum-keys (if argnum
                           (list argnum)
                           (key-gaf-arg-index-cached term)))
          ;; [Cyc] The remaining predicates left to iterate over.
          (predicate-keys nil)
          ;; [Cyc] The remaining MTs left to iterate over.
          (mt-keys nil))

  :singleton-form (new-gaf-simple-final-index-spec term
                                                   (or argnum :any)
                                                   predicate
                                                   nil)
  
  :done-form (or (not argnum-keys)
                 (and (not note)
                      (length= argnum-keys 1)
                      (length<= predicate-keys 1)
                      (not mt-keys)))

  :next-form (pop mt-keys)

  ;; [Cyc] If STATE's current keylist is valid and relevant, returns it. Otherwise returns NIL.
  ;; Valid means that none of its current keys are null.
  ;; Relevant means that all of its current keys (mt and predicate) are deemed relevant (relevance is established from outside).
  :relevant-keylist (when-let ((argnum (first argnum-keys))
                               (predicate-key (first predicate-keys))
                               (mt (first mt-keys)))
                      (if (and (not predicate)
                               (not (relevant-pred? predicate-key)))
                          (progn
                            (setf mt-keys nil)
                            (setf note nil))
                          (and (relevant-mt? mt)
                               (list argnum predicate-key mt))))

  ;; [Cyc] STATE is assumed to be invalid or irrelevant. This function fixes one cause of invalidity or irrelevance.
  ;; Invalidity is caused by having no more pending keys in a slot -- refill them.
  ;; Irrelevance is caused by having the current mt key be irrelevant -- pop it.
  ;; Returns whether we failed to quiesce because we ran out of keys.
  :quiesce-step (or (not argnum-keys)
                    (prog1 nil
                      (cond
                        ((not predicate-keys) (gaf-arg-final-index-spec-iterator-refill-predicate-keys state))
                        ((not mt-keys) (gaf-arg-final-index-spec-iterator-refill-mt-keys state))
                        (t (pop mt-keys))))))





(defun-inline do-gaf-arg-index-key-validator (term argnum predicate)
  "[Cyc] Return T iff TERM, ARGUM, and PREDICATE are valid keys for DO-GAF-ARG-INDEX."
  ;; TODO - this is in kb-indexing?  move it here
  (gaf-arg-index-key-validator term argnum predicate))



;; TODO - abstract all the refill functions
(defun gaf-arg-final-index-spec-iterator-refill-predicate-keys (state)
  "[Cyc] Refill the predicate-keys by popping an argnum but don't actually pop the argnum if it's fresh, just note that it's unfresh now."
  (with-gaf-arg-final-index-spec-iterator state
    (if (eq :argnum-keys-are-fresh note)
        (setf note nil)
        (pop argnum-keys))
    (when-let ((argnum-key (car argnum-keys)))
      (if predicate
          (setf predicate-keys (list predicate))
          (setf predicate-keys (key-gaf-arg-index-cached term argnum-key)))
      (setf note :predicate-keys-are-fresh))))

(defun gaf-arg-final-index-spec-iterator-refill-mt-keys (state)
  "[Cyc] Refill the mt-keys by popping a predicate but don't actually pop the predicate if it's fresh, just note that it's unfresh now."
  (with-gaf-arg-final-index-spec-iterator state
    (if (eq :predicate-keys-are-fresh note)
        (setf note nil)
        (pop predicate-keys))
    (when-let ((predicate-key (car predicate-keys)))
      (if (only-specified-mt-is-relevant?)
          (setf mt-keys (list *mt*))
          (let ((argnum-key (car argnum-keys)))
            (setf mt-keys (key-gaf-arg-index-cached term argnum-key predicate-key)))))))






;; PREDICATE-EXTENT

(define-kb-final-index-spec-iterator predicate-extent (predicate)
  :slots (;; [Cyc] The input predicate.
          (predicate predicate)
          ;; [Cyc] The remaining MTs left to iterate over.
          (mt-keys (key-predicate-extent-index predicate)))
    
  :singleton-form (new-gaf-simple-final-index-spec predicate
                                                   nil
                                                   predicate
                                                   nil)
  :done-form (not mt-keys)
  :next-form (pop mt-keys)
  ;; [Cyc] If STATE's current keylist is valid and relevant, returns it. Otherwise returns NIL.
  ;; Valid means that none of its current keys are null.
  ;; Relevant means that its MT is deemed relevant (relevance is established from outside).
  :relevant-keylist (when-let ((mt (car mt-keys)))
                      (and (relevant-mt? mt)
                           (list mt))))

(defun-inline do-predicate-extent-index-key-validator (predicate)
  "[Cyc] Return T iff PREDICATE is a valid key for DO-PREDICATE-EXTENT-INDEX."
  (fort-p predicate))






;; NART-ARG

(define-kb-final-index-spec-iterator nart-arg (term &optional argnum function)
  :slots ((term term)
          (function function)
          ;; [Cyc] A note containing information about the state of the keys.
          (note :argnum-keys-are-fresh)
          ;; [Cyc] The remaining argnums to iterate over.
          (argnum-keys (if argnum
                           (list argnum)
                           (key-nart-arg-index term)))
          ;; [Cyc] The remaining functions left to iterate over.
          (function-keys nil))
  :singleton-form (new-nart-simple-final-index-spec term
                                                    (or argnum :any)
                                                    function)
  :done-form (or (not argnum-keys)
                 (and (not note)
                      (length= argnum-keys 1)
                      (not function-keys)))
  ;; TODO - this one is missing a lot
  :next-form (missing-larkc 30408))

(defun do-nart-arg-index-key-validator (term index function)
  "[Cyc] Return T iff TERM, INDEX, and FUNCTION are valid keys for DO-NART-ARG-INDEX."
  (and (indexed-term-p term)
       (or (not index)
           (positive-integer-p index))
       (or (not function)
           (fort-p function))))







;; FUNCTION-EXTENT

(defun-inline do-function-extent-index-key-validator (function)
  "[Cyc] Return T iff FUNCTION is a valid key for DO-FUNCTION-EXTENT-INDEX."
  (fort-p function))

(defun function-extent-final-index-spec (function)
  "[Cyc] Makes the single final-index-spec for FUNCTION. This is the only complete path (i.e. a list of keys) leading down to a final index (a list) of assertions."
  (if (simple-indexed-term-p function)
      (new-gaf-simple-final-index-spec function '(2 0) #$termOfUnit *tou-mt*)
      (list function :function-extent)))






;; PREDICATE-RULE

(define-kb-final-index-spec-iterator predicate-rule (predicate &optional sense direction)
  :slots (;; [Cyc] The input predicate.
          (predicate predicate)
          ;; [Cyc] The input direction
          (direction direction)
          ;; [Cyc] A note containing information about the state of the keys, used to control code flow.
          (note :sense-keys-are-fresh)
          ;; [Cyc] The remaining senses to iterate over.
          (sense-keys (if sense
                          (list sense)
                          (key-predicate-rule-index predicate)))
          ;; [Cyc] The remaining MTs left to iterate over.
          (mt-keys nil)
          ;; [Cyc] The remaining directions left to iterate over.
          (direction-keys nil))

  :singleton-form (new-rule-simple-final-index-spec predicate
                                                    sense
                                                    #'predicate-rule-index-asent-match-p)
  :done-form (or (not sense-keys)
                 (and (not note)
                      (length= sense-keys 1)
                      (length<= mt-keys 1)
                      (not direction-keys)))
  :next-form (pop direction-keys)
  ;; [Cyc] If STATE's current keylist is valid and relevant, returns it. Otherwise returns NIL.
  ;; Valid means that none of its current keys are null.
  ;; Relevant means that its MT is deemed relevant (relevance is established from outside).
  :relevant-keylist (when-let ((sense (car sense-keys))
                               (mt (car mt-keys))
                               (direction (car direction-keys)))
                      (if (relevant-mt? mt)
                          (list sense mt direction)
                          (setf direction-keys nil)))
  :quiesce-step (or (not sense-keys)
                    (prog1 nil
                      (cond
                        ((not mt-keys) (predicate-rule-final-index-spec-iterator-refill-mt-keys state))
                        ((not direction-keys) (predicate-rule-final-index-spec-iterator-refill-direction-keys state))
                        (t (error "PREDICATE-RULE iterator quiescence failed with ~s" state))))))


(defun do-predicate-rule-index-key-validator (predicate sense direction)
  (and (fort-p predicate)
       (or (not sense)
           (sense-p sense))
       (or (not direction)
           (direction-p direction))))


(defun predicate-rule-final-index-spec-iterator-refill-mt-keys (state)
  "[Cyc] Refill the mt-keys by popping a sense but don't actually pop the sense if it's fresh, just note that it's unfresh now."
  (with-predicate-rule-final-index-spec-iterator-state state
    (if (eq :sense-keys-are-fresh note)
        (setf note nil)
        (pop sense-keys))
    (let ((sense-key (car sense-keys)))
      (when sense-key
        (if (only-specified-mt-is-relevant?)
            (setf mt-keys (list *mt*))
            (setf mt-keys (key-predicate-rule-index predicate sense-key)))
        (setf note :mt-keys-are-fresh)))))

(defun predicate-rule-final-index-spec-iterator-refill-direction-keys (state)
  "[Cyc] Refill the direction-keys by popping an MT but don't actually pop the MT if it's fresh, just note that it's unfresh now."
  (with-predicate-rule-final-index-spec-iterator-state state
    (if (eq :mt-keys-are-fresh note)
        (setf note nil)
        (pop mt-keys))
    (let ((mt-key (car mt-keys)))
      (when mt-key
        (if direction
            (setf direction-keys (list direction))
            (let ((sense-key (car sense-keys)))
              (setf direction-keys (key-predicate-rule-index-predicate sense-key mt-key))))))))






;; DECONTEXTUALIZED-IST

(define-kb-final-index-spec-iterator decontextualized-ist (predicate sense direction)
  :slots (;; [Cyc] The input predicate.
          (predicate predicate)
          ;; [Cyc] The input direction.
          (direction direction)
          ;; [Cyc] A note containing information about the state of the keys, used to control code flow.
          (note :sense-keys-are-fresh)
          ;; [Cyc] The remaining senses to iterate over.
          (sense-keys (if sense
                          (list sense)
                          (key-decontextualized-ist-predicate-rule-index predicate)))
          ;; [Cyc] The remaining directions left to iterate over.
          (direction-keys nil))
  :singleton-form (new-rule-simple-final-index-spec predicate
                                                    sense
                                                    #'decontextualized-ist-predicate-rule-index-asent-match-p)
  :done-form (or (not sense-keys)
                 (and (not note)
                      (length= sense-keys 1)
                      (not direction-keys)))
  :next-form (pop direction-keys)
  :relevant-keylist (when-let ((sense (car sense-keys))
                               (direction (car direction-keys)))
                      (list sense direction))
  :quiesce-step (or (not sense-keys)
                    (prog1 nil
                      (cond
                        ((not direction-keys) (decontextualized-ist-predicate-rule-final-index-spec-iterator-refill-direction-keys state))
                        (t (error "IST-PREDICATE-RULE iterator quiescense failed with ~s" state))))))
  

(defun do-decontextualized-ist-predicate-rule-index-key-validator (predicate sense direction)
  (and (fort-p predicate)
       (or (not sense)
           (sense-p sense))
       (or (not direction)
           (direction-p direction))))

(defun decontextualized-ist-predicate-rule-final-index-spec-iterator-refill-direction-keys (state)
  "[Cyc] Refill the direction-keys by popping a sense but don't actually pop the sense if it's fresh, just note that it's unfresh now."
  (with-decontextualized-ist-final-index-spec-iterator state
    (if (eq :sense-keys-are-fresh note)
        (setf note nil)
        ;; TODO - Was missing-larkc, assuming this is it since our macros filled in all the accessors, and the sense-keys writer was missing in the java
        (pop sense-keys))
    (let ((sense-key (car sense-keys)))
      (when sense-key
        (if direction
            (setf direction-keys (list direction))
            (setf direction-keys (key-decontextualized-ist-predicate-rule-index predicate sense-key)))))))





;; ISA-RULE - incomplete

(define-kb-final-index-spec-iterator isa-rule (collection &optional sense direction)
  :singleton-form (new-rule-simple-final-index-spec collection
                                                    sense
                                                    #'isa-rule-index-asent-match-p))

(defun do-isa-rule-index-key-validator (collection sense direction)
  (do-pred-arg2-rule-index-key-validator collection sense direction))




;; QUOTED-ISA-RULE - incomplete

(define-kb-final-index-spec-iterator quoted-isa-rule (collection &optional sense direction)
  :singleton-form (new-rule-simple-final-index-spec collection
                                                    sense
                                                    #'quoted-isa-rule-index-asent-match-p))

(defun do-quoted-isa-rule-index-key-validator (collection sense direction)
  (do-pred-arg2-rule-index-key-validator collection sense direction))


;; GENLS-RULE - incomplete

(define-kb-final-index-spec-iterator genls-rule (collection &optional sense direction)
  :singleton-form (new-rule-simple-final-index-spec collection
                                                    sense
                                                    #'genls-rule-index-asent-match-p))

(defun do-genls-rule-index-key-validator (collection sense direction)
  (do-pred-arg2-rule-index-key-validator collection sense direction))


;; GENL-MT-RULE - incomplete

(define-kb-final-index-spec-iterator genl-mt-rule (genl-mt &optional sense direction)
  :singleton-form (new-rule-simple-final-index-spec genl-mt
                                                    sense
                                                    #'genl-mt-rule-index-asent-match-p))

(defun do-genl-mt-rule-index-key-validator (genl-mt sense direction)
  (do-pred-arg2-rule-index-key-validator genl-mt sense direction))




;; PRED-ARG2-RULE - semi-incomplete?  there was no new-* function, guessing at lambda list

(define-kb-final-index-spec-iterator pred-arg2-rule (pred top-level-key arg2 sense direction)
  :slots ( ;; [Cyc] The input arg2.
          (arg2 arg2)
          ;; [Cyc] The input direction.
          (direction direction)
          ;; [Cyc] A note containing information about the state of the keys, used to control code flow.
          (note :sense-keys-are-fresh)
          ;; [Cyc] The remaining senses to iterate over.
          (sense-keys (if sense
                          (list sense)
                          (key-pred-arg2-rule-index pred arg2)))
          ;; [Cyc] The remaining MTs left to iterate over.
          (mt-keys nil)
          ;; [Cyc] The remaining directions left to iterate over.
          (direction-keys nil)
          ;; [Cyc] The input pred.
          (pred pred)
          ;; [Cyc] The top-level key to the final index, used for subclassing.
          (top-level-key top-level-key))

  :done-form (or (not sense-keys)
                 (and (not note)
                      (length= sense-keys 1)
                      (length<= mt-keys 1)
                      (not direction-keys)))
  :next-form (pop direction-keys)

  ;; [Cyc] If STATE's current keylist is valid and relevant, returns it. Otherwise returns NIL.
  ;; Valid means that none of its current keys are null.
  ;; Relevant means that its mt is deemed relevant (relevance is established from outside)
  :relevant-keylist (when-let ((sense (car sense-keys))
                          (mt (car mt-keys))
                          (direction (car direction-keys)))
                      (if (relevant-mt? mt)
                          (list sense mt direction)
                          (setf direction-keys nil)))
  ;; [Cyc] STATE is assumed to be invalid or irrelevant.
  ;; This function fixes one cause of invalidity or irrelevance.
  ;; Invalidity is caused by having no more pending keys in a slot -- refill them.
  ;; Irrelevance is caused by having the current mt key be irrelevant -- pop it.
   :quiesce-step (or (not sense-keys)
                    (prog1 nil
                      (cond
                        ((not mt-keys) (pred-arg2-rule-final-index-spec-iterator-refill-mt-keys state))
                        ((not direction-keys) (pred-arg2-rule-final-index-spec-iterator-refill-direction-keys state))
                        (t (error "PRED-ARG2-RULE iterator quiescense failed with ~s" state)))))
  )

(defun do-pred-arg2-rule-index-key-validator (arg2 sense direction)
  (and (fort-p arg2)
       (or (not sense)
           (sense-p sense))
       (or (not direction)
           (direction-p direction))))

(defun pred-arg2-rule-final-index-spec-iterator-refill-mt-keys (state)
  "[Cyc] Refill the mt-keys by popping a sense but don't actually pop the sense if it's fresh, just note that it's unfresh now."
  (with-pred-arg2-rule-final-index-spec-iterator state
    (if (eq :sense-keys-are-fresh note)
        (setf note nil)
        (pop sense-keys))
    (when-let ((sense-key (car sense-keys)))
      (if (only-specified-mt-is-relevant?)
          (setf mt-keys (list *mt*))
          (setf mt-keys (key-pred-arg2-rule-index pred arg2 sense-key)))
      (setf note :mt-keys-are-fresh))))

(defun pred-arg2-rule-final-index-spec-iterator-refill-direction-keys (state)
  "[Cyc] Refill the direction-keys by popping an MT but don't actually pop the MT if it's fresh, just note that it's unfresh now."
  (with-pred-arg2-rule-final-index-spec-iterator state
    (if (eq :mt-keys-are-fresh note)
        (setf note nil)
        (pop mt-keys))
    (when-let ((mt-key (car mt-keys)))
      (if direction
          (setf direction-keys (list direction))
          (let ((sense-key (car sense-keys)))
            (setf direction-keys (key-pred-arg2-rule-index pred arg2 sense-key mt-key)))))))

(defun key-pred-arg2-rule-index (pred arg2 &optional sense mt)
  (case pred
    (#$isa (key-isa-rule-index arg2 sense mt))
    (#$quotedIsa (missing-larkc 12753))
    (#$genls (key-genls-rule-index arg2 sense mt))
    (#$genlMt (key-genl-mt-rule-index arg2 sense mt))
    (t (error "Unexpected pred in PREG-ARG2 indexing: ~s" pred))))




;; FUNCTION-RULE - incomplete?

(define-kb-final-index-spec-iterator function-rule (function &optional direction)
  :slots ((function function)
          (direction direction)
          ;; [Cyc] A note containing information about the state of the keys, used to control code flow.
          (note :mt-keys-are-fresh)
          ;; [Cyc] The remaining MTs left to iterate over.
          (mt-keys (if (only-specified-mt-is-relevant?)
                       (list *mt*)
                       (key-function-rule-index function)))
          ;; [Cyc] The remaining directions left to iterate over.
          (direction-keys nil))
  :singleton-form (new-rule-simple-final-index-spec function
                                                    :neg
                                                    #'function-rule-index-asent-match-p)
  :done-form (or (not mt-keys)
                 (and (not note)
                      (length= mt-keys 1)
                      (not direction-keys)))
  
  )

(defun do-function-rule-index-key-validator (function direction)
  (and (fort-p function)
       (or (not direction)
           (direction-p direction))))


;; EXCEPTION-RULE - incomplete, no INITIALIZE- function to get the slot values from

(define-kb-final-index-spec-iterator exception-rule (rule &optional direction)
  :slots ((rule (missing-larkc 30402))
          (direction (missing-larkc 30402)))
  :singleton-form (new-rule-simple-final-index-spec rule
                                                    :pos
                                                    #'exception-rule-index-asent-match-p))

(defun do-exception-rule-index-key-validator (rule direction)
  (and (rule-assertion-p rule)
       (or (not direction)
           (direction-p direction))))


;; PRAGMA-RULE - incomplete, no INITIALIZE- function to get the slot values from

(define-kb-final-index-spec-iterator pragma-rule (rule &optional direction)
  :slots ((rule (missing-larkc 30403))
          (direction (missing-larkc 30403)))
  :singleton-form (new-rule-simple-final-index-spec rule
                                                    :pos
                                                    #'pragma-rule-index-asent-match-p))

(defun do-pragma-rule-index-key-validator (rule direction)
  (and (rule-assertion? rule)
       (or (not direction)
           (direction-p direction))))








;; MT

(defun do-mt-index-key-validator (mt type)
  (and (fort-p mt)
       (not (broad-mt? mt))
       (or (not type)
           (missing-larkc 32091))))

(defun mt-final-index-spec (mt)
  "[Cyc] Makes the single final-index-spec for MT. This is the only complete path (i.e. a list of keys) leading down to a final index (a list) of assertions."
  (if (simple-indexed-term-p mt)
      (new-assertion-simple-final-index-spec mt #'mt-index-assertion-match-p)
      (list mt :ist)))




;; OTHER

(defun do-other-index-key-validator (term type)
  (and (indexed-term-p term)
       (or (not type)
           (missing-larkc 32092))))

(defun other-final-index-spec (term)
  "[Cyc] Makes the single final-index-spec for TERM. This is the only complete path (i.e. a list of keys) leading down to a final index (a list) of assertions."
  (if (simple-indexed-term-p term)
      (new-assertion-simple-final-index-spec term #'other-index-assertion-match-p)
      (list term :other)))

(defun-inline other-simple-final-index-spec-p (object)
  (and (eq (car object) :simple)
       ;; TODO - symbol vs function of final-index-spec
       (eq (fourth object) #'other-index-assertion-match-p)))

(defun-inline other-complex-final-index-spec-p (object)
  (eq :other (second object)))

(defun other-final-index-spec-p (final-index-spec)
  "[Cyc] The other index is the only one that needs to do post-hoc semantic filtering. It's no tonly redundant for ohter indexes, it's INCORRECT in the case of the mt-index. The mt-index needs to NOT do post-hoc semantic filtering, but the ohter index requires it. Therefore, we need to gate it based on whether these assertions came from the other index."
  (or (other-simple-final-index-spec-p final-index-spec)
      (other-complex-final-index-spec-p final-index-spec)))

(defun other-index-assertion-match-p (assertion term)
  (matches-other-index assertion term))



;; TERM - unique, doesn't use the define- macro, dispatches to the above iterators

(defun do-term-index-key-validator (term type)
  (and (indexed-term-p term)
       (or (not type)
           (missing-larkc 32093))))

(defun new-term-final-index-spec-iterator (term type)
  "[Cyc] Makes an iterator which spits out final-index-specs, each of which is a complete path (i.e. a list of keys) leading down to a final index (a list) of assertions."
  (let ((iterators nil))
    ;; GAF type
    (when (or (not type)
              (eq :gaf type))
      (when (do-gaf-arg-index-key-validator term nil nil)
        (push (new-gaf-arg-final-index-spec-iterator term) iterators))
      (when (do-predicate-extent-index-key-validator term)
        (push (new-predicate-extent-final-index-spec-iterator term) iterators))
      (when (do-nart-arg-index-key-validator term nil nil)
        (push (new-nart-arg-final-index-spec-iterator term) iterators))
      (when (do-function-extent-index-key-validator term)
        (push (new-singleton-iterator (function-extent-final-index-spec term)) iterators)))

    ;; RULE type
    (when (or (not type)
              (eq :rule type))
      (when (do-predicate-rule-index-key-validator term nil nil)
        (push (new-predicate-rule-final-index-spec-iterator term) iterators))
      (when (do-isa-rule-index-key-validator term nil nil)
        (push (new-isa-rule-final-index-spec-iterator term) iterators))
      (when (do-genls-rule-index-key-validator term nil nil)
        (push (new-genls-rule-final-index-spec-iterator term) iterators))
      (when (do-genl-mt-rule-index-key-validator term nil nil)
        (push (new-genl-mt-rule-final-index-spec-iterator term) iterators))
      (when (do-function-rule-index-key-validator term nil)
        (push (new-function-rule-final-index-spec-iterator term) iterators))
      (when (do-exception-rule-index-key-validator term nil)
        (push (new-exception-rule-final-index-spec-iterator term) iterators))
      (when (do-pragma-rule-index-key-validator term nil)
        (push (new-pragma-rule-final-index-spec-iterator term) iterators)))

    (when (do-mt-index-key-validator term nil)
      (push (new-singleton-iterator (mt-final-index-spec term)) iterators))
    (when (do-other-index-key-validator term nil)
      (push (new-singleton-iterator (other-final-index-spec term)) iterators))

    (new-iterator-iterator (nreverse iterators))))

(defun do-term-index-assertion-match-p (assertion final-index-spec)
  "[Cyc] The :OTHER index is the only one that needs this post-hoc semantic filtering."
  (declare (ignore assertion))
  (if (other-final-index-spec-p final-index-spec)
      (missing-larkc 31124)
      t))







(defun do-gli-extract-method (lookup-index)
  (lookup-index-get-property lookup-index :index-type))

(defun do-gli-vga-extract-keys (lookup-index)
  (values (lookup-index-get-property lookup-index :term)
          (lookup-index-get-property lookup-index :argnum)
          (lookup-index-get-property lookup-index :predicate)))

(defun do-gli-vpe-extract-key (lookup-index)
  (lookup-index-get-property lookup-index :predicate))






;; Simple final index spec, tool to compose the above indexes.

(defun simple-final-index-spec-p (final-index-spec)
  (eq :simple (car final-index-spec)))

(defun simple-final-index-spec-term (final-index-spec)
  (second final-index-spec))

(defun new-final-index-iterator (final-index-spec &optional type truth direction)
  "[Cyc] If FINAL-INDEX-SPEC is simple, then get the syntactically filtered list from the other side, then wrap it witha  filter iterator to do the semantic filtering on this side.
If FINAL-INDEX-SPEC is complex, then get the list from the other side. This list is already filtered by type, truth, and direction, and the keys in FINAL-INDEX-SPEC have already been filtered by MT and predicate relevance, so we don't need a filter."
  (if (simple-final-index-spec-p final-index-spec)
      (let* ((assertions (simple-term-assertion-list-filtered final-index-spec
                                                              type
                                                              truth
                                                              direction))
             (syntactic-iterator (new-list-iterator assertions)))
        ;; was called semantic-iterator
        (new-filter-iterator-without-values syntactic-iterator
                                            #'assertion-semantically-matches-simple-final-index-spec?
                                            (list final-index-spec)))
      ;; TODO - check symbol vs function validity
      (new-hl-store-iterator (list 'final-index-iterator-filtered
                                   '(quote final-index-spec)
                                   type
                                   truth
                                   direction)
                             1)))

(defun assertion-semantically-matches-simple-final-index-spec? (assertion simple-final-index-spec)
  "[Cyc] Assumes that ASSERTION syntactically matches SIMPLE-FINAL-INDEX-SPEC."
  (destructuring-bind (simple term type . rest) simple-final-index-spec
    (declare (ignore term))
    (must (eq :simple simple)
          "Unexpected non-simple index ~s" simple-final-index-spec)
    ;; This block returns T unless something else usurps it with a NIL
    ;; Technically this could be mashed into a big AND statement, but whatever.
    (block nil
      (cond
        ((eq :gaf type) (destructuring-bind (argnum-spec pred-spec mt-spec) rest
                          (declare (ignore argnum-spec))
                          (when (or (and (not mt-spec)
                                         (not (assertion-matches-mt? assertion)))
                                    (and (not pred-spec)
                                         (not (all-preds-are-relevant?))
                                         (not (relevant-pred? (gaf-predicate assertion)))))
                            (return nil))))
        ((eq :nart type) (return t))
        ((eq :rule type) (unless (assertion-matches-mt? assertion)
                           (return nil)))
        ((not type) (progn
                      (destructuring-bind (assertion-func) rest
                        (when (eq #'mt-index-assertion-match-p assertion-func)
                          (return t)))
                      (unless (assertion-matches-mt? assertion)
                        (return nil))))
        (t (error "Unexpected type ~s in simple final index spec ~s" type simple-final-index-spec)))
      t)))

(defun-inline destroy-final-index-iterator (final-index-iterator)
  (iteration-finalize final-index-iterator))

(defun final-index-iterator-filtered (final-index-spec type-spec truth-spec direction-spec)
  "[Cyc] Gets the index of TERM, then follows each key in KEYS in succession. It must end up at NIL or a final index or it will signal an error. Then it turns the final index into an iterator and filters it by TYPE-SPEC, TRUTH-SPEC and DIRECTION-SPEC."
  (destructuring-bind (term . keys) final-index-spec
    (when-let ((final-index (get-subindex term keys)))
      (check-type final-index #'final-index-p)
      (let* ((raw-iterator (new-set-iterator final-index))
             (filtered-iterator (new-filter-iterator-without-values
                                 raw-iterator
                                 #'assertion-matches-type-truth-and-direction
                                 (list type-spec truth-spec direction-spec))))
        filtered-iterator))))



;; Simple final index specs

(defun new-gaf-simple-final-index-spec (term argnum-spec predicate-spec mt-spec)
  "[Cyc] Returns a 'gaf simple final index spec' -- a constraint object used to filter gafs.
TERM: The simply indexed term from which to get the unfiltered list of gafs.
ARGNUM-SPEC: see GAF-MATCHES-SIMPLE-ARGNUM-SPEC?
PREDICATE-SPEC: NIL or predicate-p, the predicate of the gaf.
MT-SPEC: NIL or HLMT-P, the MT of the gaf."
  (list :simple term :gaf argnum-spec predicate-spec mt-spec))

(defun new-nart-simple-final-index-spec (term argnum-spec functor-spec)
  "[Cyc] Returns a 'nart simple final index spec' -- a constraint object used to filter narts.
TERM: The simply indexed term from which to get the unfiltered list of narts.
ARGNUM-SPEC: see TOU-SYNTACTICALLY-MATCHES-SIMPLE-NART-FINAL-INDEX-SPEC?.
FUNCTOR-SPEC: NIL or FUNCTOR-P, the functor of the nart."
  (list :simple term :nart argnum-spec functor-spec))

(defun new-rule-simple-final-index-spec (term sense-spec asent-func)
  "[Cyc] Returns a 'rule simple final index spec' -- a constraint object used to filter rules.
TERM: The simply indexed term from which to get the unfiltered list of rules.
SENSE-SPEC: NIL or SENSE-P, the sense of the literal we're looking for.
ASENT-FUNC: We will (funcall ASET-FUNC asent term) for each ASENT with sense SENSE-SPEC, and the rule is admitted iff there is such a literal."
  (list :simple term :rule sense-spec asent-func))

(defun new-assertion-simple-final-index-spec (term assertion-func)
  "[Cyc] Returns a 'simple final index spec' -- a constraint object used to filter assertions.
ASSERTION-FUNC: We will (funcall ASSERTION-FUNC assertion term), and the assertion is admitted iff it returns true."
  (list :simple term nil assertion-func))





;; These 2 funs support the memoized function below

(defun clear-simple-term-assertion-list-filtered ()
  (when-let ((cs *simple-term-assertion-list-filtered-caching-state*))
    (caching-state-clear cs)))

(defun-inline simple-term-assertion-list-filtered-internal (simple-final-index-spec type truth direction)
  "[Cyc] Returns the list of all assertions referencing the TERM in FINAL-INDEX-SPEC which match TYPE, TRUTH, DIRECTION, and the syntactic constraints expressed in FINAL-INDEX-SPEC."
  (let ((result nil)
        (term (simple-final-index-spec-term simple-final-index-spec)))
    (dolist (assertion (simple-term-assertion-list term))
      (when (and (assertion-syntactically-matches-simple-final-index-spec? assertion simple-final-index-spec)
                 (assertion-matches-type-truth-and-direction? assertion type truth direction))
        (push assertion result)))
    ;; TODO - is it important that the ordering can be retained?  Should we use a tail-push keeping the last cons around for speed?
    (nreverse result)))

(defun-memoized simple-term-assertion-list-filtered (simple-final-index-spec type truth direction)
    (:test equal :clear-when :hl-store-modified)
  (simple-term-assertion-list-filtered-internal simple-final-index-spec type truth direction))


(defun assertion-syntactically-matches-simple-final-index-spec?
    (assertion simple-final-index-spec)
  "[Cyc] Assumes all simple final-index-specs are one of the three forms:
   (:simple term :gaf  argnum-spec predicate mt)
   (:simple term :nart argnum-spec functor)
   (:simple term :rule sense       asent-func)
   (:simple term nil   assertion-func)"
  (destructuring-bind (simple term type . rest) simple-final-index-spec
    (must (eq :simple simple)
          "Unexpected non-simple index ~s" simple-final-index-spec)
    (when (assertion-matches-syntactic-indexing-type? assertion type)
      (cond
        ((eq :gaf type) (gaf-syntactically-matches-simple-gaf-final-index-spec? assertion
                                                                                term
                                                                                rest))
        ((eq :nart type) (missing-larkc 30427))
        ((eq :rule type) (rule-syntactically-matches-simple-rule-final-index-spec? assertion
                                                                                   term
                                                                                   rest))
        ((not type) (assertion-syntactically-matches-simple-assertion-final-index-spec? assertion
                                                                                        term
                                                                                        rest))
        (t (error "Unexpected type ~s in simple final index spec ~s" type simple-final-index-spec))))))

(defun assertion-matches-syntactic-indexing-type? (assertion type)
  (if (eq :nart type)
      (term-of-unit-assertion-p assertion)
      (assertion-matches-type? assertion type)))

(defun gaf-syntactically-matches-simple-gaf-final-index-spec? (gaf term gaf-final-index-spec)
  (destructuring-bind (argnum-spec predicate-spec mt-spec) gaf-final-index-spec
    (and (or (not predicate-spec)
             (gaf-assertion-has-pred-p gaf predicate-spec))
         (gaf-matches-simple-argnum-spec? gaf term argnum-spec)
         (or (not mt-spec)
             (missing-larkc 31008)))))

(defun gaf-matches-simple-argnum-spec? (gaf term argnum-spec)
  "[Cyc] ARGNUM-SPEC is a specification for how TERM must appear in some argunment position of GAF.
   NIL          means that it doesn't matter.
   an integer N means that TERM must appear as the Nth argument in GAF.
   :any         means that TERM must appear as a top-level argument in GAF.
   (N M)        means that TERM must appear as the Mth argunment in the formula that is the Nth argunment of GAF.
   (N :any)     means that TERM must appear as a top-level argument in the formula that is the Nth argunment of GAF."
  (cond
    ((not argnum-spec) t)
    ((eq :any argnum-spec) (gaf-has-term-in-some-argnum? gaf term))
    ;; Since this is indexing an in-memory list, fixnum should be safe
    ;; TODO - search for all integerp, stringp, and other expensive tests and reevaluate
    ((fixnump argnum-spec) (gaf-has-term-in-argnum? gaf term argnum-spec))
    ((and (consp argnum-spec)
          (length= argnum-spec 2)
          (fixnump (first argnum-spec)))
     (let* ((n (first argnum-spec))
            (m (second argnum-spec))
            (subformula (gaf-arg gaf n)))
       (check-type subformula #'el-formula-p)
       (if (eq m :any)
           (term-is-one-of-args? term subformula)
           ;; TODO - doable missing-larkc
           (missing-larkc 30563))))))

(defun rule-syntactically-matches-simple-rule-final-index-spec? (rule term rule-final-index-spec)
  "[Cyc] Returns whether RULE has a SENSE-lit ASENT such that (funcall ASENT-FUNC asent TERM) holds.
RULE-FINAL-INDEX-SPEC: a (SENSE ASENT-FUNC) pair."
  (destructuring-bind (sense asent-func) rule-final-index-spec
    (rule-syntactically-matches-simple-rule-final-index-spec-int? rule sense term asent-func)))

;; TODO - why is this broken out?
(defun rule-syntactically-matches-simple-rule-final-index-spec-int? (rule sense term asent-func)
  (if (not sense)
      (rule-syntactically-matches-simple-rule-final-index-spec-int? rule :neg term asent-func)
      (when (valid-assertion-handle? rule)
        (let ((asents (clause-sense-lits (assertion-cnf rule) sense))
              (match nil))
          (csome (asent asents match)
            (setf match (asent-syntactically-matches-simple-rule-final-index-spec? asent
                                                                                   term
                                                                                   asent-func)))
          match))))

(defun asent-syntactically-matches-simple-rule-final-index-spec? (asent term asent-func)
  ;; This was another case form that called known function names
  ;; These were missing-larkc:
  ;;   isa-rule-index-asent-match-p
  ;;   genl-mt-rule-index-asent-match-p
  ;;   function-rule-index-asent-match-p
  ;;   exception-rule-index-asent-match-p
  (funcall asent-func asent term))

(defun assertion-syntactically-matches-simple-assertion-final-index-spec? (assertion term assertion-final-index-spec)
  (destructuring-bind (assertion-func) assertion-final-index-spec
    (funcall assertion-func assertion term)))

(defun predicate-rule-index-asent-match-p (asent predicate)
  (and (eq predicate (atomic-sentence-predicate asent))
       (predicate-rule-index-asent-p asent)))

(defun predicate-rule-index-asent-p (asent)
  (let ((pred (atomic-sentence-predicate asent)))
    (when (fort-p pred)
      (case pred
        (#$isa (missing-larkc 30406))
        (#$genls (not (genls-rule-index-asent-p asent)))
        (#$genlMt (missing-larkc 30401))
        (#$termOfUnit (missing-larkc 30399))
        (#$abnormal (missing-larkc 30396))
        (#$meetsPragmaticRequirement (not (pragma-rule-index-asent-p asent)))
        (otherwise t)))))

(defun decontextualized-ist-predicate-rule-index-asent-match-p (asent predicate)
  (and (eq #$ist (atomic-sentence-predicate asent))
       (eq predicate (literal-predicate (atmoic-sentence-arg2 asent)))
       (missing-larkc 30350)))

(defun genls-rule-index-asent-match-p (asent collection)
  (and (genls-rule-index-asent-p asent)
       (eq collection (atomic-sentence-arg2 asent))))

(defun genls-rule-index-asent-p (asent)
  (and (eq #$genls (atomic-sentence-predicate asent))
       (formula-arity= asent 2)
       (fort-p (atomic-sentence-arg2 asent))))

(defun pragma-rule-index-asent-match-p (asent rule)
  (and (pragma-rule-index-asent-p asent)
       (eq rule (atomic-sentence-arg2 asent))))

(defun pragma-rule-index-asent-p (asent)
  (and (eq #$meetsPragmaticRequirement (atomic-sentence-predicate asent))
       (formula-arity= asent 2)
       (assertion-p (atomic-sentence-arg2 asent))))

(defun mt-index-assertion-match-p (assertion mt)
  (hlmt-equal? mt (assertion-mt assertion)))
