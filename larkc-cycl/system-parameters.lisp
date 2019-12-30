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


(deflexical *valid-system-parameter-types* '(t-or-nil nil-or-string string full-path integer symbol none)
    "[Cyc] The list of all known valid system parameter types.")

(defglobal *system-parameters* nil
    "[Cyc] The list of system parameters defined by DEFINE-SYSTEM-PARAMETER.")

(defun register-system-parameter (name default type description)
  (unless (member type *valid-system-parameter-types*)
    (warn "~s ~s has an unknown type ~s." 'define-system-parameter name type))
  (remove-system-parameter name)
  (push (list name default type description) *system-parameters*)
  name)

(defmacro define-system-parameter (name default type &body description)
  "Assuming behavior from context, was referenced in the .java, but without implementation. &body is used purely for indentation."
  `(progn
     (defvar ,name :unset ,@description)
     (register-system-parameter ',name ,default ,type ,@description)))

(defun remove-system-parameter (name)
  "[Cyc] Remove NAME from the system parameters."
  (declare (symbol name))
  (setf *system-parameters* (delete name *system-parameters* :test #'eq :key #'car)))

(defun system-parameter-value-unset-p (val)
  (eq val :unset))

(defun check-system-parameters ()
  (dolist (entry *system-parameters*)
    (destructuring-bind (symbol default type description) entry
      (declare (ignore default description))
      (cond
        ((not (boundp symbol)) (warn "The system parameter ~s is not bound." symbol))
        ((system-parameter-value-unset-p symbol)
         (warn "The system parameter ~s was not initialized." symbol))
        (t (let* ((val (symbol-value symbol)))
             (unless (case type
                       (t-or-nil (typep val 'boolean))
                       (nil-or-string (typep val '(or null string)))
                       (string (stringp val))
                       (full-path (typep val '(or string pathname)))
                       (integer (integerp val))
                       (symbol (symbolp val))
                       (none t))
               (warn "The system parameter ~s has the value ~s, but it is supposed to be of type ~s."
                     symbol val type))))))))

(defun load-system-parameters ()
  "[Cyc] Load the system-parameters file."
  (let ((filename (merge-pathnames #P"src/main/resources/parameters.lisp"
                                   *cyc-home-directory*)))
    (if (probe-file filename)
        (with-open-file (stream filename)
          (loop for form = (read-parameter-form stream)
             until (eq :eof form)
             do (evaluate-parameter-form form)))
        (warn "System parameters file ~a not loaded." filename))))

;; TODO - does this need to be a separate function?  inline it in the above?
(defun read-parameter-form (stream)
  (let ((*read-eval* nil))
    (read stream nil :eof)))

(defun evaluate-parameter-form (form)
  (destructuring-bind (operator &rest args) form
    (case operator
      ;; TODO - which package in clyc?
      (in-package (eval '(in-package "CYC")))
      ((setq csetq setf) (destructuring-bind (symbol value) args
                           (when (member symbol *system-parameters* :key #'first)
                             (set symbol (evaluate-parameter-value value)))))
      (check-system-parameters (check-system-parameters)))))

(defun evaluate-parameter-value (value)
  (cond
    ((atom value) value)
    ((eq 'quote (first value)) (second value))))

;; TODO - this doesn't quite properly envelope toplevel forms for the macroexpansion
(define-system-parameter *auto-continue-transcript-problems* t 't-or-nil
  "[Cyc] Possible values: NIL, T. If NIL, transcript problems will cause error breaks that make the system stop. If T, such problems will not cause breakage.")
(define-system-parameter *continue-agenda-on-error* t 't-or-nil
  "[Cyc] Possible values: NIL, T. If NIL, agenda errors will cause the system to halt. If T, they will be automatically continued.")
(define-system-parameter *suspend-sbhl-type-checking?* nil 't-or-nil
  "[Cyc] Possible values: T, NIL. Type checking occurs in SBHL modules IFF this is NIL.")
(define-system-parameter *really-count-transcript-ops* nil 't-or-nil
  "[Cyc] Possible values: T, Nil. If NIL, the System Info page (accessible to administrators only) will estimate, rather than actually count, the number of operations in the transcript. If T, it will actually count them, which takes longer but is accurate.")
(define-system-parameter *dont-record-operations-locally* nil 't-or-nil
  "[Cyc] Possible values: NIL, T. If NIL, a local transcript will always be written when operations are done, even if those operations are also being written to the master transcript. If T, then the image does not quire to a local transcript file, and will write to the master transcript when it is set to transmit operations. This allows an image that is standalone, and always in :TRANSMIT-AND-RECEIVE, to keep only a single copy of its operations.")
(define-system-parameter *startup-communication-mode* :deaf 'symbol
  "[Cyc] Possible values -- :TRANSMIT-AND-RECEIVE, :RECEIVE-ONLY, :ISOLATED, :DEAF, :DEAD. This is the communication mode the cyc image should get initialized to at startup.")
(define-system-parameter *start-agenda-at-startup?* t 't-or-nil
  "[Cyc] Possible values: T, NIL. If NIL, the Cyc agenda is not started at startup, but can be enabled later by the user. If T, the agenda is enabled at startup.")
(define-system-parameter *base-tcp-port* 3600 'integer
  "[Cyc] The base port offset for all the TCP services for the Cyc image.")
(define-system-parameter *fi-port-offset* 1 'integer
  "[Cyc] Possible values: A number. This parameter specifies the offset of the Cyc API (application programming interface) service from *BASE-TCP-PORT*.")
(define-system-parameter *cfasl-port-offset* 14 'integer
  "[Cyc] Possible values: A number. This parameter specifies the offset of the Cyc CFASL-server service from *BASE-TCP-PORT*.")
(define-system-parameter *permit-api-host-access* t 't-or-nil
  "[Cyc] Possible values: T, NIL. If T, then API functions can access host services including the file system and outbound tcp connections. The most secure configuration leaves this parameter NIL.")
(define-system-parameter *use-transcript-server* nil 't-or-nil
  "[Cyc] Possible values: T, NIL. If T, then writing to the master transcsript will be controlled by the Cyc Transcript Server, which will need to be installed at your site. You only need to set this to T if you are running multiple instances of Cyc. If NIL, then Cyc will read and write to the master transcript without regard to other processes doing the same.")
(define-system-parameter *master-transcript-lock-host* nil 'nil-or-string
  "[Cyc] Possible values: NIL or a string. This parameter is only used if *USE-TRANSCRIPT-SERVER* is T. If so, then this parameter should be set to the name of the host offering the cyc-serializer serivce.")
(define-system-parameter *master-transcript-service-port* 3608 'integer
  "[Cyc] Possible values: A number. This parameter is only used if *USE-TRANSCRIPT-SERVER* is T. If so, then this parameter should be set to the port number fo the cyc-serializer read service.")
(define-system-parameter *allow-guest-to-edit?* nil 't-or-nil
  "[Cyc] Possible values: T, NIL. If NIL, require authentication before allowing modifications to the knowledge base. If T, any user is allowed to modify the knowledge base.")
(define-system-parameter *default-cyclist-name* "Guest" 'string
  "[Cyc] Possible values: The name of a constant representing a Cyclist. This is the default Cyclist initially logged into the system.")
(define-system-parameter *html-image-directory* "/cycdoc/img/" 'string
  "[Cyc] The directory under which Cyc images (.gif or otherwise) are stored")
(define-system-parameter *html-javacript-directory* "/cycdoc/js/" 'string
  "[Cyc] The directory under which Javascript files used by the browser are stored")
(define-system-parameter *html-css-directory* "/cycdoc/css/" 'string
    "[Cyc] The directory under which CSS files used by the browser are stored")
(define-system-parameter *cyc-execution-context* :unknown 'symbol
  "[Cyc] Possible values: One of the symbols :CYCORP or :UNKNOWN. If the execution context is set to :CYCORP, then the CYC image can assume that it is running in Cycorp's development environment and make strong assumptions about setup and infrastructure.")
