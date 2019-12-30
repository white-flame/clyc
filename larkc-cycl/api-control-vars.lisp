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


(defparameter *cfasl-constant-handle-func* nil
  "[Cyc] Fucntion used to determine constant handles during CFASL output.
If nIL, the default used is CONSTANT-INTERNAL-ID")
(defparameter *cfasl-constant-handle-lookup-func* nil
  "[Cyc] Function used to look up constants from their handles during CFASL input.
If NIL, the default used is FIND-CONSTANT-BY-INTERNAL-ID")
(defparameter *cfasl-nart-handle-func* nil
  "[Cyc] Function used to determine NART handles during CFASL output.
If NIL, the default used is NART-ID.")
(defparameter *cfasl-nart-handle-lookup-func* nil
  "[Cyc] Function used to look up NARTs from their handles during CFASL input.
If NIL, the default used is FIND-NART-BY-ID")
(defparameter *cfasl-assertion-handle-func* nil
  "[Cyc] Function used to determine assertion handles during CFASL output.
If NIL, the default used is ASSERTION-ID")
(defparameter *cfasl-assertion-handle-lookup-func* nil
  "[Cyc] Function used to look up assertions from their handles during CFASL input.
If NIL, the default used is FIND-ASSERTION-BY-ID")
(defparameter *cfasl-deduction-handle-func* nil
  "[Cyc] Function used to determine deduction handles during CFASL output.
If NIL, the default used is DEDUCTION-ID")
(defparameter *cfasl-deduction-handle-lookup-func* nil
  "[Cyc] Function used to look up deductions from their handles during CFASL input.
If NIL, the default used is FIND-DEDUCTION-BY-ID")
(defparameter *cfasl-kb-hl-support-handle-func* nil
  "[Cyc] Function used to determine KB HL supports during CFASL output.
If NIL, the default used is KB-HL-SUPPORT-ID")
(defparameter *cfasl-kb-hl-support-handle-lookup-func* nil
  "[Cyc] Function used to look up KB HL supports from their handles during CFASL input.
If NIL, the default used is FIND-KB-HL-SUPPORT-BY-ID")
(defparameter *cfasl-clause-struc-handle-func* nil
  "[Cyc] Function used to determine clause-struc handles during CFASL output.
If NIL, the default used is CLAUSE-STRUC-ID")
(defparameter *cfasl-clause-struc-handle-lookup-func* nil
  "[Cyc] Function used to look up clause-strucs from their handles during CFASL input.
If NIL, the default used is FIND-CLAUSE-STRUC-BY-ID")
(defvar *the-cyclist* nil)
(defparameter *use-local-queue?* t)
(defparameter *default-ke-purpose* nil
  "[Cyc] The purpose to use for KE by default.  NIL = General Cyc KE.")
(defparameter *ke-purpose* *default-ke-purpose*
  "[Cyc] This variable constains current KE purpose for asserting formulas to the system.
NIL means that the KB purpose is generic extension of Cyc's knowledge.")
(defparameter *generate-readable-fi-results* t)
