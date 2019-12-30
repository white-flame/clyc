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
;; Originally larkc-3.0/src/main/java/com/cyc/tool/subl/jrtl/nativeCode/subLisp/Tcp.java

(defvar *retain-client-socket?* nil)
(defvar *tcp-localhost-only? nil)
(defvar *remote-hostname* nil)
(defvar *remote-address* nil)

(defvar *port-to-server-socket-process-map* (make-hash-table :synchronized t)
  "Maps numeric port to usocket listener socket")

(defun open-tcp-stream (host port)
  ;; java.net.ServerSocket deals with bytes
  (usocket:socket-connect host port :element-type '(unsigned-byte 8)))

(defun start-tcp-server (port handler)
  ;; TODO - test what the handler protocol is.  usocket passes a stream object to it.  The java interface seems to pass the stream into it twice, maybe a reader & writer stream separately?
  ;; TODO - Error capture & reporting? The Java SafeRunnable seems to just enable Lisp conditions to catch exceptions, and print errors to stdout if not caught.
  ;; socket-server returns the thread & the socket, when multithreading is enabled
  (let ((socket (nth-value 1 (usocket:socket-server
                              nil port handler nil
                              :multi-threading t
                              :name (format nil "Socket Server (port: ~a handler: ~a"
                                            port handler)))))
    (setf (gethash port *port-to-server-socket-process-map*) socket)))

(defun stop-tcp-server (port)
  (or (sb-ext:with-locked-hash-table (*port-to-server-socket-process-map*)
        (gethash-and-remove port *port-to-server-socket-process-map*))
      (error "~s is not a TCP server port designator." port)))

