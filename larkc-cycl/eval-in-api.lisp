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


;; TODO DESIGN - setting this to T is not supported
(defvar *eval-in-api?* nil
  "[Cyc] Process all API commands using a SubL interpreter which validates API function calls.")

(defun cyc-api-eval (api-request)
  "[Cyc] Evaluate API-REQUEST under the evaluation assumptions of the CYC-API server"
  (if *eval-in-api?*
      (missing-larkc 10828)
      (eval-in-api-subl-eval api-request)))

(defun possibly-cyc-eval (api-request)
  "[Cyc] Call EVAL on API-REQUEST.
Functions defined via the Cyc API are also supported."
  (if *eval-in-api?*
      (missing-larkc 10829)
      (eval api-request)))

(defun possibly-cyc-api-function-spec-p (object)
  "[Cyc] Return T iff OBJECT is suitable for FUNCALL.
Functions defined via the Cyc API are also supported."
  (or (function-spec-p object)
      (and (symbolp object)
           (api-function-p object))))

(defun possibly-cyc-api-funcall (func &rest args)
  "[Cyc] Funcall FUNC on ARGS.
Functions defined via the Cyc API are also supported."
  ;; Note that the original had fixed arity funcall-1, funcall-2, etc
  (if (function-spec-p func)
      (apply func args)
      (cyc-api-eval (cons func (mapcar #'quotify args)))))

(defglobal *eval-in-api-mutable-global* nil)

(defun register-api-mutable-global (var)
  (push var *eval-in-api-mutable-global*)
  var)

(defglobal *eval-in-api-immutable-global* nil)

(defun register-api-immutable-global (var)
  (push var *eval-in-api-immutable-global*)
  var)

(defparameter *eval-in-api-env* nil
  "[Cyc] The association list of API variables and bound values.")
(defglobal *api-special-verify-table* (make-hash-table :test #'eq))

(defun register-api-special-verify (operator handler)
  (setf (gethash operator *api-special-verify-table*) handler)
  operator)

(defglobal *api-function-table* (make-hash-table :test #'eq))

(defun api-function-p (operator)
  (gethash operator *api-function-table*))

(defglobal *api-macro-table* (make-hash-table :test #'eq))
(defglobal *subl-eval-method* 'eval)

(defun eval-in-api-subl-eval (form)
  "[Cyc] Trampoline to EVAL from within EVAL-IN-API"
  ;; TODO - this is defined in Eval.Java, but nothing seems to use it.  Probably configures how full SubL runs EVAL, especially if there are EVAL forms inside FORM.
  ;;(let ((*evaluator-method* *subl-eval-method*)))
  (funcall *subl-eval-method* form))

(defparameter *eval-in-api-traced-fns* nil
  "[Cyc] The lsit of functions to be traced.")
(defparameter *eval-in-api-trace-log* nil
  "[Cyc] The log of trace events.")

(defun initialize-eval-in-api-env ()
  nil)

(defparameter *eval-in-api-level* -1
  "[Cyc] Indicates top level evaluation when value equals 0.")
(defparameter *eval-in-api-function-level* -1
  "[Cyc] Indicates function level for diagnostic trace output.")
(defparameter *eval-in-api-macro-stack* nil
  "[Cyc] The stack of macros that we're currently evalling in the context of.")
(defparameter *verify-in-api-verbose-mode?* nil)
(defparameter *verify-in-api-bound-symbols* nil
  "[Cyc] A list of the symbols introduced in the form being verified.")
(defparameter *verify-in-api-fbound-symbols* nil
  "[Cyc] A list of the function symbols introduced in the form being verified.")
(defparameter *verify-in-api-macro-stack* nil
  "[Cyc] The stack of macros that we're currently verifying in the context of.")
(deflexical *api-user-variables* nil
  "[Cyc] The dictionary of persistent api user variables and values.")
