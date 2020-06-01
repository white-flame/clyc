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


(defun function-symbol-p (obj)
  "[Cyc] Return T iff OBJECT is a symbol with a function definition."
  (and (symbolp obj)
       (fboundp obj)))

(defun function-symbol-arglist (function-symbol)
  "[Cyc] Return the arglist of FUNCTION-SYMBOL"
  #+sbcl (sb-impl::%fun-lambda-list (symbol-function function-symbol))
  #-sbcl (error "FUNCTION-SYMBOL-ARGLIST unimplemented in this CL implementation"))

;; PERFORMANCE - instead of suboptimal RSUBLIS custom implementations, just reverse the alist.  However, this means macros instead of functions to take advantage of compiler optimizations
(defun reverse-alist-pairs (alist)
  "Reverses each entry from (KEY . VALUE) to (VALUE . KEY)"
  (mapcar (lambda (cons)
            (cons (cdr cons) (car cons)))
          alist))

(defmacro rsublis (alist &rest rest)
  "[Cyc] Like SUBLIS except ALIST is interpreted as (VALUE . KEY) pairs"
  `(sublis (reverse-alist-pairs ,alist) ,@rest))

(defmacro nrsublis (alist &rest rest)
  "[Cyc] Like NSUBLIS except ALIST is interpreted as (VALUE . KEY) pairs"
  `(nsublis (reverse-alist-pairs ,alist) ,@rest))

(defun elapsed-universal-time (past-time &optional (current-time (get-universal-time)))
  (- current-time past-time))

(defun ensure-physical-pathname (pathname)
  "[Cyc] Convert PATHNAME to a physical pathname (performing any logical pathname translations)"
  (truename pathname))

(defmacro member? (item list &optional (test '#'eql) (key '#'identity))
  `(member ,item ,list
           ,@(and test `(:test ,test))
           ,@(and key `(:key ,key))))

;; PERFORMANCE - do we assume fixnum for subl-level code?
(defun* positive-integer-p (obj)
    (:inline t)
  (typep obj '(integer 1)))

(defun* non-negative-integer-p (obj)
    (:inline t)
  (typep obj '(integer 0)))
