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


(defconstant *hl-variable-prefix-char* #\?
  "[Cyc] The character used as the first character of an HL variable's name.")

(defconstant *default-el-variable-prefix* "?VAR"
  "[Cyc] The prefix for all default EL vars. By no coincidence, it is the upcase version of the prefix in PRINT-VARIABLE.")

(defstruct (variable (:conc-name "VAR-"))
  id)

(defmethod sxhash ((object variable))
  (or (var-id object)
      99))

(deflexical *variable-max* 200
  "[Cyc] The total number of interned HL variables.")

(defglobal *variable-array* nil)

(defun-inline get-variable (num)
  "[Cyc] Return HL variable number NUM."
  (aref *variable-array* num))

(defun setup-variable-table ()
  "[Cyc] Setup the array of interned HL variables."
  ;; TODO - Lazy initialization might be after *variable-max* is updated.  Else, allocate it up front.
  (unless *variable-array*
    (setf *variable-array* (prog1-let ((array (make-vector *variable-max*)))
                             (dotimes (i *variable-max*)
                               (setf (aref array i)
                                     (make-variable :id i)))))))

(defun-inline variable-id (variable)
  "[Cyc] Return ID of HL variable VARIABLE."
  (var-id variable))

;; TODO - obsolete
(defun-inline find-variable-by-id (id)
  "[Cyc] Return the HL variable wiht ID, or NIL if not present."
  (get-variable id))

(defun-inline variable-< (var1 var2)
  (< (variable-id var1)
     (variable-id var2)))

(defun-memoized default-el-var-for-hl-var (variable)
    (:test eq
     :doc "[Cyc] Return a readable EL var from HL var VARIABLE.")
  (make-el-var (prin1-to-string variable)))

(defun-inline sort-hl-variable-list (hl-variable-list)
  (sort hl-variable-list #'variable-<))

(defun-inline fully-bound-p (object)
  "[Cyc] Return T iff OBJECT contains no HL variables, and therefore is fully bound."
  (not (not-fully-bound-p object)))

(defun not-fully-bound-p (object)
  "[Cyc] Return T iff OBJECT contains some HL variable, and therefore is not fully bound."
  (if (atom object)
      (variable-p object)
      ;; TODO - this is a form of SOME that also tests the dotted value?
      (do* ((rest object (cdr rest))
            (next (car rest) (car rest)))
           ((atom (cdr rest))
            (or (not-fully-bound-p next)
                (variable-p (cdr rest)))))))

(defun-inline cycl-ground-expression-p (expression)
  (not (expression-find-if #'cyc-var? expression)))

