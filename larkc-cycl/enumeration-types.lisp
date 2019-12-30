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



(defconstant *valid-directions* '(:backward :forward :code))

(declaim (inline valid-directions))
(defun valid-directions ()
  *valid-directions*)

(defun direction-p (object)
  "[Cyc] Return T iff OBJECT is a valid assertion inference direction
:backward :forward or :code."
  (member-eq? object *valid-directions*))

(defun encode-direction (direction)
  (position direction *valid-directions*))

(defun decode-direction (fixnum)
  (nth fixnum *valid-directions*))

(defconstant *valid-assertion-types* '(:gaf :rule))
(defconstant *valid-el-strengths* '(:default :monotonic))

(defun el-strength-p (object)
  "[Cyc] Return T iff OBJECT is a valid CycL assertion strength
:default or :monotonic."
  (member-eq? object *valid-el-strengths*))

(defun el-strength-implies (strength1 strength2)
  "[Cyc] Return T iff STRENGTH2 is subsumed by STRENGTH1"
  (not (position-< strength1 strength2 *valid-el-strengths*)))

(defconstant *valid-truths* '(:true :unknown :false))
(defun valid-truths ()
  *valid-truths*)

(defun truth-sense (truth)
  (case truth
    (:true :pos)
    (:false :neg)
    (:unknown :neg)
    (otherwise (error "~s is not a TRUTH-P" truth))))

(defconstant *valid-senses* '(:neg :pos))

(defun sense-p (object)
  "[Cyc] Return T iff OBJECT is a valid CycL literal sense
:neg or :pos."
  (member-eq object *valid-senses*))

(defun inverse-sense (sense)
  (case sense
    (:pos :neg)
    (:neg :pos)
    (otherwise (error "~s is not a SENSE-P" sense))))

(defun sense-truth (sense)
  (case sense
    (:pos :true)
    (:neg :false)
    (otherwise (error "~s is not a SENSE-P" sense))))

(defconstant *valid-hl-truth-values* '(:true-mon :true-def :unknown :false-def :false-mon))
(defun valid-hl-truth-values ()
  *valid-hl-truth-values*)

(defun encode-tv (tv)
  (position tv *valid-hl-truth-values*))

(defun decode-tv (fixnum)
  (nth fixnum *valid-hl-truth-values*))

(defun tv-strength (tv)
  (case tv
    (:true-def :default)
    (:true-mon :monotonic)
    (:false-def :default)
    (:false-mon :monotonic)
    (:unknown :default)
    (otherwise (error "~s does not satisfy TV-P" tv))))

(defun tv-truth (tv)
  (case tv
    (:true-def :true)
    (:true-mon :true)
    (:false-def :false)
    (:false-mon :false)
    (:unknown :unknown)
    (otherwise (error "~s does not satisfy TV-P" tv))))

(defun tv-from-truth-strength (truth strength)
  (case truth
    (:true (case strength
             (:default :true-def)
             (:monotonic :true-mon)
             (otherwise (error "~s is not a STRENGTH-P" strength))))
    (:false (case strength
              (:default :false-def)
              (:monotonic :false-mon)
              (otherwise (error "~s is not a STRENGTH-P" strength))))
    (:unknown :unknown)
    (otherwise (error "~s is not a TRUTH-P" truth))))

(defconstant *term-args* '(1 2 0 :neg :pos 3 4 5 :ist :other))
