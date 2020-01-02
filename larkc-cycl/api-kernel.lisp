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

;; TODO - this will not work as send-api-result is missing-larkc, but can be reconstituted.

(defun api-server-handler (in-stream out-stream)
  (api-server-top-level in-stream out-stream))

(defparameter *within-api-server* nil)

(defun api-server-top-level (in-stream out-stream)
  (let ((*package* (find-package "CLYC"))
        (*read-default-float-format* 'double-float)
        (*print-readably* nil)
        (*read-eval* nil)
        (*within-api-server* t))
    (catch :api-quit
      (api-server-loop in-stream out-stream))))

(defun api-quit ()
  "[Cyc] Explicitly quit this API connection."
  (when *within-api-server*
    (throw :api-quit nil)))

(defparameter *default-api-input-protocol* 'default-api-input-protocol
    "[Cyc] The default API input protocol to use.")
(defparameter *api-input-protocol* *default-api-input-protocol*)
(defparameter *default-api-validate-method* 'default-api-validate-api-request
    "[Cyc] The default API input validator to use.")
(defparameter *api-validate-method* *default-api-validate-method*
    "[Cyc] When non-NIL, a function which is called to validate any API request before evaluation.")
(deflexical *default-api-result-method* nil)
(defparameter *api-result-method* *default-api-validate-method*
    "[Cyc] When non-NIL, a function which is called to transform any API result before returning the output.")
(defparameter *default-api-output-protocol* 'default-api-output-protocol
    "[Cyc] The default API output protocol to use.")
(defparameter *api-output-protocol* *default-api-output-protocol*)
(defparameter *api-in-stream* nil
    "[Cyc] The API input stream (for use by the java-api-kernel).")
(defparameter *api-out-stream* nil
    "[Cyc] The API output stream (for use by the java-api-kernel).")

(defun api-server-loop (in-stream out-stream)
  (let ((*use-local-queue?* nil)
        (*the-cyclist* (the-cyclist))
        (*ke-purpose* *default-ke-purpose*)
        (*api-in-stream* in-stream)
        (*api-out-stream* out-stream)
        (*api-input-protocol* *default-api-input-protocol*)
        (*api-validate-method* *default-api-validate-method*)
        (*api-result-method* *default-api-result-method*)
        (*api-output-protocol* *default-api-output-protocol*)
        (*eval-in-api-env* (initialize-eval-in-api-env))
        (*eval-in-api-traced-fns* nil)
        (*eval-in-api-trace-log* "")
        (*ignore-warns?* t)
        (*ignore-breaks?* t)
        (*silent-progress?* t)
        (*continue-cerror?* t)
        (*standard-output* *null-output*)
        (*error-output* *null-output*))
    (while t
      (api-server-one-complete-request in-stream out-stream))))

(defun api-server-one-complete-request (in-stream out-stream)
  (let ((error-message nil)
        (api-request nil)
        (api-result nil))
    (setf-error error-message
      (setf api-request (read-api-request in-stream))
      (validate-api-request api-request)
      (record-api-request api-request))
    (if (eq 'task-processor-request (car api-request))
        (progn
          (bt:with-lock-held (*api-task-process-pool-lock*)
            (unless (api-task-processors-initialized-p)
              (initialise-api-task-processors)))
          (destructuring-bind (function request id priority requestor client-bindings uuid-string)
              api-request
            (declare (ignore function))
            (task-processor-request request id priority requestor client-bindings uuid-string)))
        (progn
          (unless error-message
            (setf-error error-message
              (setf api-result (perform-api-request api-request))))
          (if error-message
              (progn
                (send-api-result out-stream error-message t)
                (record-api-result error-message t))
              (progn
                (send-api-result out-stream api-result nil)
                (record-api-result api-result nil)))
          (update-api-protocol)))))

(defglobal *api-input-eof-marker* (make-symbol "API Input EOF Marker"))

(defun read-api-request (in-stream)
  (let ((request (funcall (api-input-protocol)
                          in-stream nil *api-input-eof-marker*)))
    (when (eq request *api-input-eof-marker*)
      (api-quit))
    request))

(defun validate-api-request (api-request)
  (if *api-validate-method*
      (funcall *api-validate-method* api-request)
      t))

(defun valid-api-function-symbol (symbol)
  (and (symbolp symbol)
       (fboundp symbol)))

(defparameter *record-api-messages?* nil)
(defparameter *api-message-sink* nil
    "[Cyc] Either a list or a stream. If a list, the messages are stuck on the list. If a stream, the messages are output to it.")

(defun record-api-request (api-request)
  (when *record-api-messages?*
    (let ((sink *api-message-sink*))
      (cond
        ((streamp sink) (progn
                          (prin1 api-request sink)
                          (terpri sink)
                          (force-output sink)))
        ((listp sink) (push api-request *api-message-sink*))))))

(defun record-api-result (result error?)
  (when *record-api-messages?*
    (let ((sink *api-message-sink*))
      (cond
        ((streamp sink) (default-api-output-protocol sink result error?))
        ((listp sink) (push (list error? result) *api-message-sink*))))))

(defun perform-api-request (api-request)
  (reset-fi-error-state)
  (let ((result (cyc-api-eval api-request)))
    (when (fi-error-signaled?)
      (missing-larkc 11154))
    (if *api-result-method*
        (funcall *api-result-method* result)
        result)))

(defun send-api-result (out-stream api-result error?)
  "[CYc] Send API-RESULT to OUT-STREAM respecting ERROR?"
  ;; TODO - this contained a nonsensical (+ 1 2)?
  ;; TODO - should be easy to reimplement, likely api-output-protocol?  don't want to edit it in in translation yet though
  (on-error (funcall (missing-larkc 31555) out-stream api-result error?)
    (api-quit)))

(defparameter *api-success-code* 200)
(defparameter *api-error-code* 500)

(defun default-api-output-protocol (out-stream api-result &optional error?)
  (let ((result-code (if error? *api-error-code* *api-success-code*)))
    (format out-stream "~d ~s" result-code api-result))
  (missing-larkc 7458)
  ;;(force-output out-stream)
  api-result)

(defparameter *new-api-input-protocol* nil)
(defparameter *new-api-output-protocol* nil)

(defun update-api-protocol ()
  (when (and *new-api-input-protocol*
             *new-api-output-protocol*)
    (set-api-input-protocol *new-api-input-protocol*)
    (set-api-output-protocol *new-api-output-protocol*)
    (setf *new-api-input-protocol* nil)
    (setf *new-api-output-protocol* nil)))

(defun set-api-input-protocol (input-protocol)
  (setf *api-input-protocol* input-protocol))

(defun set-api-output-protocol (output-protocol)
  (setf *api-output-protocol* output-protocol))

(defun api-port ()
  "[Cyc] Returns the local api-port according to defined system parameters."
  (+ *base-tcp-port* *fi-port-offset*))

(deflexical *cyc-api-eof-exception* :eof)
