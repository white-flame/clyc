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

(defvar *hlmts-supported?* t
  "[Cyc] Whether we allow non-atomic unreified MTs.")

(defun* hlmts-supported? () (:inline t)
  *hlmts-supported?*)

(defun disable-hlmts ()
  (setf *hlmts-supported?* nil))

(deflexical *default-monad-mt* #$UniversalVocabularyMt)
(deflexical *default-temporal-mt-time-parameter* #$Null-TimeParameter)
(deflexical *default-atemporal-time-parameter* #$Null-TimeParameter)
(deflexical *default-atemporal-genlmt-time-parameter* #$TimePoint)
(deflexical *default-atemporal-specmt-time-parameter* #$Null-TimeParameter)
(deflexical *default-mt-time-interval* #$Always-TimeInterval)
(deflexical *default-mt-time-parameter* #$Null-TimeParameter)
(deflexical *mt-space-function* #$MtSpace)
(deflexical *temporal-dimension-functions* (list #$MtTimeWithGranularityDimFn
                                                 #$MtTimeDimFn))
(deflexical *mt-dimension-types* (list :monad
                                       :time)
  "[Cyc] The identifiers used to specify different types of slices of MT space. This is an ordered list, and describes the canonical ordering of dimensions.")

(deflexical *mt-dimension-functions* (list #$MtDim
                                           #$MtTimeDimFn
                                           #$MtTimeWithGranularityDimFn)
  "[Cyc] The functions which carve out slices of MT space.")
  
(deflexical *context-space-functions* (cons *mt-space-function*
                                            *mt-dimension-functions*)
  "[Cyc] The functions which are valid arg0s for a HLMT.")

(deflexical *anytime-psc* #$AnytimePSC)
(deflexical *anytime-during-psc-fn* #$AnytimeDuringPSCFn)

(deflexical *unindexed-hlmt-syntax-constants* (list #$MtSpace
                                                    #$MtDim
                                                    #$MtTimeDimFn
                                                    #$MtTimeWithGranularityDimFn
                                                    #$mtTimeIndex
                                                    #$mtTimeParameter
                                                    #$mtMonad
                                                    #$Always-TimeInterval
                                                    #$Null-TimeParameter
                                                    #$TimePoint)
  "[Cyc] Constants which are part of HLMT syntax and which therefore are not fully indexed, for pragmatic reasons.")

(defun* possibly-mt-p (object) (:inline t)
  (possibly-hlmt-p object))

(defun* possibly-hlmt-p (object) (:inline t)
  (or (possibly-fo-naut-p object)
      (hlmt-p object)))

(defun* fort-or-chlmt-p (object) (:inline t)
  "[Cyc] Whether OBJECT is a fort-p or closed-hlmt-p."
  (or (fort-p object)
      (closed-hlmt-p object)))

(defun* chlmt-p (object) (:inline t)
  "[Cyc] Shorthand for closed-hlmt-p."
  (closed-hlmt-p object))

(defun* closed-hlmt-p (object) (:inline t)
  "[Cyc] Return T iff OBJECT is an HLMT, and is closed."
  (ret (cand (hlmt-p object)
             (cycl-ground-expression-p object))))

(defun hlmt-p (object)
  "[Cyc] Returns T if OBJECT might be an HLMT, that is, an HL representation of a microtheory. Hence it must be closed.
NOTE: Returns true iff OBJECT actually is an HLMT."
  (if *hlmts-supported?*
      (hlmt-p-time object)
      (hlmt-p-no-time object)))

(defun hlmt-p-no-time (object)
  (or (valid-fort? object)
      (mt-union-naut-p object)))

(defun possibly-hlmt-naut-p (object)
  (when (possibly-naut-p object)
    (let ((functor (naut-functor object)))
      (or (missing-larkc 12260)))))

(defun hlmt-naut-p (object)
  (and (possibly-hlmt-naut-p object)
       (or (missing-larkc 12292))))

(defun mt-space-naut-p (object)
  (and (possibly-naut-p object)
       (missing-larkc 12303)))

(defun mt-union-naut-p (object)
  (and (possibly-naut-p object)
       (mt-union-function-p (naut-functor object))))

(defun* mt-union-function-p (object) (:inline t)
  (eq object #$MtUnionFn))

(defun hlmt? (object)
  "[Cyc] Like HLMT-P but also does fort-types checks that the relevant bits have fort-types of microtheories, and arity checks on all the dimensions."
  (and (cycl-represented-term-p object)
       (hlmt?-int object)))

(defun hlmt?-int (object)
  (if *hlmts-supported?*
      (missing-larkc 12280)
      (hlmt?-no-time object)))

(defun hlmt?-no-time (object)
  (and (hlmt-p object)
       (or (monad-mt? object)
           (mt-union-naut? object))))

(defun mt-union-naut? (object)
  (and (possibly-naut-p object)
       (mt-union-function-p (naut-functor object))
       (missing-larkc 12311)))

(defun hlmt-equal (object0 object1)
  "[Cyc] Return whether OBJECT0 is equal to OBJECT1."
  (if *hlmts-supported?*
      (equal object0 object1)
      (eq object0 object1)))

(defun hlmt-equal? (object0 object1)
  "[Cyc] Return whether OBJECT0 is equal to OBJECT1 insofar as all of their dimensions are equal.  What constitutes equality for a given dimension is dependent on that dimension."
  (cond
    ((hlmt-equal object0 object1) t)
    ((and (monad-mt-p object0)
          (monad-mt-p object1))
     nil)
    (t (missing-larkc 12276))))

(defun get-hlmt-dimension (dim hlmt)
  "[Cyc] Returns the dimension of HLMT specified by DIM."
  (let ((mt nil)
        (found? nil))
    ;; TODO - this probably was a large macro expansion but the branches ended up missing-larkc
    (if (monad-mt-p hlmt)
        (unless found?
          (let ((dim-var :monad)
                (mt-var hlmt))
            (when (eq dim dim-var)
              (setf mt mt-var)
              (setf found? t))))
        (missing-larkc 12293))
    mt))

(defun reduce-hlmt (hlmt &optional (minimize-mt-union-mts? t))
  "[Cyc] Removes default values from HLMT, for more compact storage.
NOTE: Result is destructible."
  (block nil
    (setf hlmt (transform-mt-union-nauts hlmt minimize-mt-union-mts?))
    (let ((monad (hlmt-monad-mt hlmt)))
      (when (any-or-all-mts-relevant-to-mt? monad)
        (return monad))
      (when (eq monad hlmt)
        (return hlmt)))
    (let ((substantial-dimensions nil))
      (cond
        ((monad-mt-p hlmt) (missing-larkc 12270))
        ((missing-larkc 12294))))))

(defun transform-mt-union-nauts (hlmt minimize-mt-union-mts?)
  (cond
    ((mt-union-naut-p hlmt) (missing-larkc 12338))
    ((consp hlmt) (cons (transform-mt-union-nauts (car hlmt) minimize-mt-union-mts?)
                        (transform-mt-union-nauts (cdr hlmt) minimize-mt-union-mts?)))
    (t hlmt)))

(defun valid-hlmt-p (hlmt &optional robust)
  (when (hlmt-p hlmt)
    (if robust
        (cond
          ((monad-mt-p hlmt) (missing-larkc 12345))
          ((missing-larkc 12295)))
        (valid-monad-mt-p (hlmt-monad-mt hlmt)))))

(defun monad-mt-p (object)
  (or (and (fort-p object)
           (not (anytime-psc-p object)))
      (and (mt-union-naut-p object)
           (every-in-list #'monad-mt-p (missing-larkc 12315)))))

(defun valid-monad-mt-p (mt)
  (or (not mt)
      (and (valid-fort? mt)
           (mt? mt))))

(defun monad-mt? (object)
  (or (and (fort-p object)
           (mt? object))
      (and (mt-union-naut-p object)
           (every-in-list #'monad-mt? (missing-larkc 12316)))))

(defun hlmt-monad-mt (hlmt)
  (or (hlmt-monad-mt-without-default hlmt)
      *default-monad-mt*))

(defun hlmt-monad-mt-without-default (hlmt)
  "[Cyc] Returns nil or monad-mt-p"
  (if (hlmt-naut-p hlmt)
      (get-hlmt-dimension :monad hlmt)
      hlmt))

(deflexical *temporal-dimension-predicates* (list #$mtTimeIndex
                                                  #$mtTimeParameter))

(defun* anytime-psc-p (object) (:inline t)
  (eq object *anytime-psc*))

(defun* hlmt-temporal-mt (hlmt) (:inline t)
  (get-hlmt-dimension :time hlmt))
