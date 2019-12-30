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


;; Note that this uses ye olde LISP definition of "process" to mean what we now call "thread"

;; TODO - lots of useful stuff is missing, not sure how meaningful this will be.

(defstruct task
  process
  completed)

(defstruct (thinking-task (:conc-name "T-TASK-"))
  lock
  thread
  name
  status
  progress-message
  progress-so-far
  progress-total
  start-time
  finish-time
  result
  error-message
  properties)

(defparameter *thinking-task* nil)

(defstruct ipc-queue
  lock
  semaphore
  data-queue)

(defstruct (ordered-ipc-queue (:conc-name "ORDRD-IPCQ-"))
  lock
  producer-isg
  consumer-isg
  payload)

(defconstant *ordered-ipcq-empty* (make-symbol "empty ordered IPC queue entry"))

(defun process-exhaust-immediately-fn ()
  nil)

(defun make-exhausted-process (name)
  "[Cyc] A wrapper for creating an already exhausted process."
  (bt:make-thread #'process-exhaust-immediately-fn :name name))

;; TODO - can't dispatch on a CL type, only a class, so hitting internals
(defmethod visit-defstruct-object ((object sb-thread:thread) visitor-fn)
  (visit-defstruct-object-process object visitor-fn))

(defun visit-defstruct-object-process (process visitor-fn)
  (funcall visitor-fn process :begin 'make-exhausted-process 1)
  (funcall visitor-fn process :slot :name (bt:thread-name process))
  (funcall visitor-fn process :end 'make-exhausted-process 1)
  process)

;; TODO - can't dispatch on a CL type, only a class, so hitting internals
(defmethod visit-defstruct-object ((object sb-thread:mutex) visitor-fn)
  (visit-defstruct-object-lock object visitor-fn))

(defun visit-defstruct-object-lock (lock visitor-fn)
  (funcall visitor-fn lock :begin 'bt:make-lock 1)
  ;; TODO - bordeaux-threads doesn't expose accessing the name?
  (funcall visitor-fn lock :slot :name #+sbcl (sb-thread:mutex-name lock))
  (funcall visitor-fn lock :end 'bt:make-lock 1))

(defstruct process-wrapper
  id
  process
  state
  lock
  plist)

;; TODO DESIGN - never used
(defglobal *process-wrapper-isg* (new-integer-sequence-generator))

