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

(defparameter *perform-cfasl-externalization* nil)
(defun* cfasl-externalization-mode? () (:inline t)
  *perform-cfasl-externalization*)

(defun cfasl-set-mode-externalized ()
  "[Cyc] Switch this connection into external setting."
  (setf *perform-cfasl-externalization* t))

(defun cfasl-server-handler (in-stream out-stream)
  (cfasl-server-top-level in-stream out-stream))

(defparameter *cfasl-kernel-standard-output* nil
    "[Cyc] The standard output stream for debugging within a task-processor-request.")

(defun cfasl-server-toplevel (in-stream out-stream)
  (let ((*cfasl-common-symbols* nil))
    (cfasl-set-common-symbolscommon-symbols nil)
    (let ((*perform-cfasl-externalization* nil)
          (*generate-readable-fi-results* nil)
          (*default-api-input-protocol* 'read-cfasl-request)
          (*default-api-validate-method* 'validate-cfasl-request)
          (*default-api-output-protocol* 'send-cfasl-result)
          (*cfasl-kernel-standard-output* *standard-output*))
      (api-server-top-level in-stream out-stream))))

(defun cfasl-quit ()
  "[Cyc] Explicity quit this cfasl connection."
  (api-quit))

(defun cfasl-port ()
  "[Cyc] Returns the local cfasl-port according to defined system parameters."
  (+ *base-tcp-port* *cfasl-port-offset*))

(defun read-cfasl-request (in-stream &optional (eof-error-p t) (eof-value :eof))
  ;; TODO - this Java body catches an error message, tests for its presence, but does literally nothing if one is caught.  This might be an ignore-errors expansion, but I think I saw one of those elsewhere where it was clear without extra claptrap like this.  Just using ignore-errors for now anyway as it seems functionally equivalent.
  (ignore-errors
    (cfasl-input in-stream eof-error-p eof-value)))

(defun validate-cfasl-request (api-request)
  (unless (proper-list-p api-request)
    (error "Invalid API Request: ~s is not a proper list" api-request))
  (unless (or *eval-in-api?*
              (valid-api-function-symbol (car (api-request))))
    (error "Invalid API Request: ~s is not a valid API function symbol" (car api-request))))

(defun send-cfasl-result (out-stream cfasl-result &optional error)
  (when error
    (setf cfasl-result (make-cfasl-api-exception cfasl-result)))
  (cfasl-output (null error) out-stream)
  (if (cfasl-externalization-mode?)
      (cfasl-output-externalized cfasl-result out-stream)
      (cfasl-output cfasl-result out-stream))
  (force-output out-stream)
  cfasl-result)

(defun make-cfasl-api-exception (string)
  (list 'cyc-exception :message string))

(defparameter *cfasl-eof-exception* (make-cfasl-api-exception "EOF occurred on CFASL API stream"))

(defun task-processor-request (request id priority requestor client-bindings &optional uuid-string)
  "REQUEST: consp for evaluation
ID: integer
PRIORITY integer
REQUESTOR string
CLIENT-BINDINGS: consp of (var value) pairs
UUID-STRING: identifies the clietn to which the response will be sent
Submits the REQUEST form to the task request queue with ID, PRIORITY, REQUESTOR, BINDINGS, and OUT-STREAM."
  (let ((server-bindings (list* (list '*use-local-queue?* nil)
                                (list '*the-cyclist* (the-cyclist))
                                (list '*ke-purpose* *default-ke-purpose*)
                                '((*eval-in-api-env* nil)
                                  (*eval-in-api-level* 0)
                                  (*eval-in-api-traced-fns* nil)
                                  (*eval-in-api-trace-log* "")
                                  (*ignore-warns?* t)
                                  (*ignore-breaks?* t)
                                  (*silent-progress?* t)
                                  (*continue-cerror?* t)
                                  (*standard-output* *null-output*)
                                  (*error-output* *null-output*)
                                  (*package* (find-package :cyc))))))
    (api-task-processor-request request id priority requestor
                                (append server-bindings client-bindings)
                                uuid-string)))
