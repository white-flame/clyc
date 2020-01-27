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

;; A ton of these functions are very simplistic.  I wonder if they're used to pass around as lambdas.  Else, it's a lot easier to just type out their effects than remember which fiddly bits are in here under what name.

(defun-inline 2* (number)
  "[Cyc] Return (* NUMBER 2)."
  (* 2 number))

(defun-inline onep (object)
  "[Cyc] Return T iff OBJECT is 1."
  (eql 1 object))

(defun-inline encode-boolean (object)
  "[Cyc] Convert any object to either 1 or 0."
  (if object 1 0))

(defun-inline decode-boolean (integer)
  "[Cyc] Convert 1 or 0 to T or NIL."
  (not (eq 0 integer)))

(defun-inline get-bit (bits bit-number)
  "[Cyc] Return the boolean value encoded in BIT-NUMBER of BITS."
  (declare (fixnum bits bit-number))
  (decode-boolean (ldb (byte 1 bit-number) bits)))
  
(defun-inline set-bit (bits bit-number &optional (boolean t))
  (declare (fixnum bits bit-number))
  (dpb (encode-boolean boolean) (byte 1 bit-number) bits))

;; TODO - are things doing speedup checks that would take advantage of this being larger?
(deflexical *large-immediate-positive-integer* (ash 1 26)
  "[Cyc] A large positive integer guaranteed to be stored immediately.")

(defconstant *e* 2.718281828459045d0
  "[Cyc] exp1, what a silly name for e.")

(defun-inline bytep (object)
  (typep object '(integer 0 255)))

(defun-inline zero-number-p (object)
  "[Cyc] Like ZEROP, but doesn't error on non-numbers."
  (or (eq object 0)
      (eql object 0.0d0)))

;; TODO - not defconstant?  are we going to have dynamically updated fp formats?
(deflexical *maximum-float-significant-digits* 16
  "[Cyc] The maximum possible number of significant digits for a floating-point number.")

(defun significant-digits (number digits)
  (declare (fixnum digits))
  (cond
    ((infinite-number-p number) number)
    ((scientific-number-p number) (missing-larkc 690))
    ((zerop number) 0)
    ((minusp number) (significant-digits (- number) digits))
    ((and (integerp number)
          (>= digits (missing-larkc 31687)))
     number)
    ((and (doublep number)
          (>= digits *maximum-float-significant-digits*))
     number)
    (t (let* ((normalization-power (floor (log number 10)))
              (normalization-ratio (expt 10 normalization-power))
              (normalized-number (/ number normalization-ratio))
              (significant-ratio (expt 10 (1- digits)))
              (scaled-normalized (* normalized-number significant-ratio))
              (scaled-significant (round scaled-normalized))
              (normalized-significant (/ scaled-significant significant-ratio))
              (significant (* normalized-significant normalization-ratio)))
         (when (integerp number)
           (let* ((unnormalization-power (- normalization-power (1- digits)))
                  (unnormalization-ratio (expt 10 unnormalization-power)))
             (setf significant (* scaled-significant unnormalization-ratio))))
         (when (and (doublep significant)
                    (>= number most-negative-fixnum)
                    (<= number most-positive-fixnum))
           (let ((nearest-integer (round significant)))
             ;; TODO - what is this doing, casting a double to an integer?
             (when (= nearest-integer significant)
               (setf significant nearest-integer))))
         (if (doublep significant)
             (significant-digits-optimize-float significant)
             significant)))))

(defun significant-digits-optimize-float (float)
  (multiple-value-bind (significand exponent sign) (integer-decode-float float)
    (let ((tersest-length most-positive-fixnum)
          (tersest-float nil))
      (loop for delta from 2 below 3
         do (let* ((candidate-significand (+ significand delta))
                   (candidate-float (* sign (scale-float (float candidate-significand)
                                                         exponent)))
                   (candidate-length (length (prin1-to-string candidate-float))))
              (when (< candidate-length tersest-length)
                (setf tersest-length candidate-length)
                (setf tersest-float candidate-float))))
      tersest-float)))

(defun-inline percent (numerator &optional (denominator 1) significant-digits)
  "[Cyc] Express NUMERATOR/DENOMINATOR as a percent.
The answer is limited to SIGNIFICANT-DIGITS, when non-NIL."
  (let ((result (* 100 (/ numerator denominator))))
    (if significant-digits
        (significant-digits result significant-digits)
        result)))

;; TODO DESIGN - SBCL does support infinite numbers in sb-ext:, including math operators, numeric comparisons, and predicates.  The serialization process, however, would need to be extended to support it, if these are ever asserted.  It's a tossup which way to go, but I'm hoping that the potentially-infinite-* functions aren't the norm and thus these manual implementations won't have too much of a burden.
;; TODO - instrument or deprecate these to measure their presence.

(defun-inline potentially-infinite-number-p (object)
  (or (numberp object)
      (infinite-number-p object)))

(defun-inline positive-infinity ()
  :positive-infinity)

(defun-inline negative-infinity-p (object)
  (eq object :negative-infinity))

(defun-inline positive-infinity-p (object)
  (eq object :positive-infinity))

(defun-inline infinite-number-p (object)
  (or (negative-infinity-p object)
      (positive-infinity-p object)))

(defun-inline potentially-infinite-number-= (num1 num2)
  (eql num1 num2))

(defun potentially-infinite-number-< (num1 num2)
  (cond
    ((negative-infinity-p num1) (not (negative-infinity-p num2)))
    ((negative-infinity-p num2) nil)
    ((positive-infinity-p num2) (not (positive-infinity-p num1)))
    ((positive-infinity-p num1) nil)
    (t (< num1 num2))))

(defun-inline potentially-infinite-number-> (num1 num2)
  (not (potentially-infinite-number-< num1 num2)))

(defun potentially-infinite-number-plus (num1 num2)
  (cond
    ;; TODO - this missing-larkc is missing.  Maybe fallout from there not being a negative-infinity function?
    ((negative-infinity-p num1) (missing-larkc 31726))
    ((negative-infinity-p num2) (missing-larkc 31727))
    ((positive-infinity-p num1) (missing-larkc 31742))
    ((positive-infinity-p num2) (missing-larkc 31743))
    (t (+ num1 num2))))

(defun potentially-infinite-number-times (num1 num2)
  (cond
    ((negative-infinity-p num1) (missing-larkc 31728))
    ((negative-infinity-p num2) (missing-larkc 31729))
    ((positive-infinity-p num1) (missing-larkc 31744))
    ((positive-infinity-p num2) (missing-larkc 31745))
    (t (* num1 num2))))

(defun potentially-infinite-number-divided-by (num1 num2)
  ;; Not sure if this is intended to return 2 values
  (cond
    ;; This hits a missing-larkc on infinite stuff before it has the opportunity to check div-by-zero
    ((zero-number-p num2) (missing-larkc 31731))
    ((negative-infinity-p num1) (missing-larkc 31723))
    ((negative-infinity-p num2) (missing-larkc 31688))
    ((positive-infinity-p num1) (missing-larkc 31739))
    ((positive-infinity-p num2) (missing-larkc 31689))
    (t (/ num1 num2))))

(defun-inline potentially-infinite-number-max (num1 num2)
  "[Cyc] Returns the potentially-infinite-number-p max value between NUM1 and NUM2."
  (if (potentially-infinite-number-> num1 num2)
      num1
      num2))

(defun-inline potentially-infinite-number-min (num1 num2)
  "[Cyc] Returns the potentially-infinite-number-p min value between NUM1 and NUM2."
  (if (potentially-infinite-number-< num1 num2)
      num1
      num2))

(defun-inline potentially-infinite-integer-= (int1 int2)
  (eql int1 int2))

(defun-inline potentially-infinite-integer-< (int1 int2)
  (potentially-infinite-number-< int1 int2))

(defun-inline potentially-infinite-integer-> (int1 int2)
  (potentially-infinite-integer-< int2 int1))

(defun-inline potentially-infinite-integer-<= (int1 int2)
  (not (potentially-infinite-integer-> int1 int2)))

(defun-inline potentially-infinite-integer-plus (int1 int2)
  (potentially-infinite-number-plus int1 int2))

(defun-inline potentially-infinite-integer-times (int1 int2)
  (potentially-infinite-number-times int1 int2))

;; Cool, SBCL detects the infinite number paths as unreachable from the -times and -divided routines.
(defun potentially-infinite-integer-times-number-rounded (int1 num2)
  "[Cyc] A truncated version of (INT1 * NUM2)."
  (let ((raw-product (potentially-infinite-number-times int1 num2)))
    (if (infinite-number-p raw-product)
        raw-product
        (truncate raw-product))))

(defun potentially-infinite-integer-divided-by-number-rounded (int1 num2)
  "[Cyc] A truncated version of (INT1 / NUM2)."
  (let ((raw-product (potentially-infinite-number-divided-by int1 num2)))
    (if (infinite-number-p raw-product)
        raw-product
        (truncate raw-product))))

(defun maximum (items &optional (key #'identity))
  "[Cyc] Returns the maximal element of ITEMS.
KEY: A unary function that will return a NUMBERP when applied to any item in ITEMS."
  (must (consp items) "Cannot compute the maximum of an atom or empty list.")
  (if (or (eq key #'identity)
          (eq key 'identity))
      (let ((maximum (car items)))
        (dolist (item (cdr items) maximum)
          (when (> item maximum)
            (setf maximum item))))
      (extremal items #'> key)))

(defun median (items &optional key)
  "[Cyc] If KEY is provided, and ITEMS are non-numeric, then ITEMS must be of odd length."
  (must (consp items) "Cannot compute the median of an atom or empty list.")
  (let ((sorted-items (if key
                          (sort (copy-list items) #'< :key key)
                          (sort (copy-list items) #'<))))
    (median-sorted sorted-items)))

(defun median-sorted (items &optional length)
  "[Cyc] This will simply access the middle of the list of items or average the two middle items in the case of an evenp length."
  (must (consp items) "Cannot compute the median of an atom or empty list")
  (unless length
    (setf length (length items)))
  (let ((middle-position (floor length 2)))
    (if (oddp length)
        (nth middle-position items)
        (/ (+ (nth middle-position items)
              (nth (- middle-position 1) items))
           2))))

(defun decode-integer-multiples (integer multiples)
  (declare (integer integer)
           (list multiples))
  (let ((answer nil))
    (dolist (multiple multiples)
      (when (zerop integer)
        (unless answer
          (push integer answer))
        (return))
      (multiple-value-bind (whole mod) (truncate integer multiple)
        (push mod answer)
        (setf integer whole)))
    (nreverse answer)))

;; TODO - not constants?
(deflexical *valid-number-string-characters* "0123456789.-+deDE")
(deflexical *valid-exponent-markers* "deDE")
(deflexical *valid-sign* "+-")
(defconstant *largest-prime-by-binary-width* '((2 . 3)
                                               (3 . 7)
                                               (4 . 13)
                                               (5 . 31)
                                               (6 . 61)
                                               (7 . 127)
                                               (8 . 251)
                                               (9 . 509)
                                               (10 . 1021)
                                               (11 . 2039)
                                               (12 . 4093)
                                               (13 . 8191)
                                               (14 . 16381)
                                               (15 . 32749)
                                               (16 . 65521)
                                               (17 . 131071)
                                               (18 . 262139)
                                               (19 . 524287)
                                               (20 . 1048573)
                                               (21 . 2097143)
                                               (22 . 4194301)
                                               (23 . 8388593)
                                               (24 . 16777213)
                                               (25 . 33554393)
                                               (26 . 67108859)
                                               (27 . 134217689)
                                               (28 . 268435399)
                                               (29 . 536870909)
                                               (30 . 1073741789)
                                               (31 . 2147483647)
                                               (32 . 4294967291)
                                               (33 . 8589934583)
                                               (34 . 17179869143)
                                               (35 . 34359738337)
                                               (36 . 68719476731)
                                               (37 . 137438953447)
                                               (38 . 274877906899)
                                               (39 . 549755813881)
                                               (40 . 1099511627689)
                                               (41 . 2199023255531)
                                               (42 . 4398046511093)
                                               (43 . 8796093022151)
                                               (44 . 17592186044399)
                                               (45 . 35184372088777)
                                               (46 . 70368744177643)
                                               (47 . 140737488355213)
                                               (48 . 281474976710597)
                                               (49 . 562949953421231)
                                               (50 . 1125899906842597)
                                               (51 . 2251799813685119)
                                               (52 . 4503599627370449)
                                               (53 . 9007199254740881)
                                               (54 . 18014398509481951)
                                               (55 . 36028797018963913)
                                               (56 . 72057594037927931)
                                               (57 . 144115188075855859)
                                               (58 . 288230376151711717)
                                               (59 . 576460752303423433)
                                               (60 . 1152921504606846883)
                                               (61 . 2305843009213693951)
                                               (62 . 4611686018427387847)
                                               (63 . 9223372036854775783)
                                               (64 . 18446744073709551557)))
(defconstant *checksum-implementation-width* (truncate (log (abs most-negative-fixnum) 2))
  "[Cyc] How many bits the high and the low part represent.")
(defconstant *checksum-base* (cdr (assoc *checksum-implementation-width*
                                         *largest-prime-by-binary-width*))
  "[Cyc] Largest prime smaller than 2^implementation-width.")
(defconstant *checksum-initial-value-sum* 1)
(defconstant *checksum-initial-value-length* 0)
(defparameter *checksum-sum* 1)
(defparameter *checksum-length* 0)
(defparameter *hex-to-dec-table* '((#\a 10)
                                   (#\A 10)
                                   (#\b 11)
                                   (#\B 11)
                                   (#\c 12)
                                   (#\C 12)
                                   (#\d 13)
                                   (#\D 13)
                                   (#\e 14)
                                   (#\E 14)
                                   (#\f 15)
                                   (#\F 15)))
