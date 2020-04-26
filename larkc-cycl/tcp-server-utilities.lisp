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


;; TODO - all the missing-larkc in this file should be easily implementable.
;; TODO - this originally SubL file has functionality overlap with port mappings that the originally Java tcp.lisp file has.  Combine the two.

(deflexical *tcp-server-lock* (bt:make-lock "TCP Server Lock"))

(defparameter *remote-address* nil
    "[Cyc] Within a TCP server handler, this is bound to an integer representing the socket's remote machine IP address.")

(defparameter *remote-hostname* nil
    "[Cyc] WIthin a TCP server handler, this is bound to a string representing the socket's remote machine hostname")

(defun tcp-port-p (object)
  "[Cyc] Return T iff OBJECT is a valid integer for a TCP port."
  (and (fixnump object)
       (eq object (logand 65535 object))))

(defun enable-tcp-server (type port)
  "[Cyc] Enable a new TCP server of TYPE bound to PORT.
TYPE must have already been declared via DEFINE-TCP-SERVER.
Any TCP server currently bound to PORT is first disabled."
  (when (> (disable-tcp-server port) 0)
    (sleep 1))
  (let ((tcp-server (new-tcp-server type port)))
    (register-tcp-server tcp-server)
    tcp-server))

(defun disable-tcp-server (designator)
  "[Cyc] Disable all TCP servers specified by DESIGNATOR.
Returns the total number of servers disabled.
If DESIGNATOR is a TCP-SERVER-P, disable that server.
If DESIGNATOR is a TCP-PORT-P, disable the server at that port.
Otherwise, disable all servers with DESIGNATOR as their type."
  (cond
    ((tcp-server-p designator) (missing-larkc 31593))
    ((tcp-port-p designator) (alexandria:if-let ((tcp-server (find-tcp-server-by-port designator)))
                               (disable-tcp-server tcp-server)
                               0))
    (t (missing-larkc 31597))))

(defun validate-all-tcp-servers ()
  (missing-larkc 31596))

(defstruct (tcp-server (:conc-name "TCPS-"))
  type
  ;; NIL if disabled
  (port nil :type (or null fixnum))
  process)

(defun* tcp-server-port (tcp-server) (:inline t)
  "[Cyc] Return the port of TCP-SERVER, or NIL if disabled."
  (tcps-port tcp-server))

(defun new-tcp-server (type port)
  (let ((handler (tcp-server-type-handler type)))
    (make-tcp-server :type type
                     :port port
                     :process (start-tcp-server-process type port handler))))

(defglobal *all-tcp-servers* nil)

(defun find-tcp-server-by-port (port)
  (find port *all-tcp-servers* :key #'tcp-server-port))

(defun all-tcp-servers ()
  "[Cyc] Return a list of all TCP servers that are currently enabled."
  (copy-list *all-tcp-servers*))

(defun register-tcp-server (tcp-server)
  (bt:with-lock-held (*tcp-server-lock*)
    (push tcp-server *all-tcp-servers*)))

(defglobal *tcp-server-type-table* nil)

(defun register-tcp-server-type (type handler &optional (mode :text))
  "[Cyc] Register that TCP servers of TYPE use HANDLER with MODE."
  (deregister-tcp-server-type type)
  (bt:with-lock-held (*tcp-server-lock*)
    (push (list type handler mode) *tcp-server-type-table*)))

(defun deregister-tcp-server-type (type)
  (bt:with-lock-held (*tcp-server-lock*)
    (deletef type *tcp-server-type-table* :key #'first)))

(defun tcp-server-type-handler (type)
  (second (find type *tcp-server-type-table* :key #'first)))

(defun start-tcp-server-process (type port handler)
  "[Cyc] Method for starting a new TCP server of TYPE at PORT which has HANDLER."
  (declare (ignore type))
  (start-tcp-server port handler))
