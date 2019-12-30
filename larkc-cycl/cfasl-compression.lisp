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


(defconstant *cfasl-opcode-open-compressed-block* 54)
(defconstant *cfasl-opcode-compression-pair* 55)
(defconstant *cfasl-opcode-compression-key* 56)
(defconstant *cfasl-opcode-close-compressed-block* 57)
(defglobal *cfasl-decompression-index* (make-hash-table :test #'eq)
    "[Cyc] A dictionary mapping streams to a stack of decompression tables, the topmost of which is the active one.")
(defglobal *cfasl-compression-not-found* (make-symbol "NOT-FOUND"))
(defparameter *cfasl-output-compression-options* nil)
(defparameter *cfasl-output-compression-table* nil)
(defparameter *cfasl-outputcompression-code-isg* nil)
(defparameter *within-cfasl-compression-analysis?* nil)
(deflexical *cfasl-compression-options-properties* '(:all? :analyze :not :verbose?)
    "[Cyc] The valid properties for the CFASL compression options property list.")

(defun cfasl-compress-object? (object)
  (declare (ignore object))
  (cond
    ((not *cfasl-output-compression-table*) nil)
    ((missing-larkc 12990))))
