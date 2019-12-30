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


(defvar *hl-store-modification-and-access* :local-local
    "[Cyc] Where HL store modification and access should be done. There are four valid values:
  :local-local   (modify and access locally)
  :remote-remote (modify and access remotely)
  :both-local    (modify and access locally, also modify remotely)
  :both-remote   (modify and access remotely, also modify locally)
  :none-local    (access locally, do not modify)")

(defvar *override-hl-store-remote-access?* nil
    "[Cyc] A non-NIL value means that access will be done locally regardless of the valeu of *hl-store-modification-and-access*.")

(defun hl-modify-local? ()
  (member-eq? *hl-store-modification-and-access* '(:local-local :both-local :both-remote)))

(defun hl-modify-remote? ()
  (member-eq? *hl-store-modification-and-access* '(:remote-remote :both-local :both-remote)))

(defun hl-modify-anywhere? ()
  (member-eq? *hl-store-modification-and-access* '(:local-local :remote-remote :both-local :both-remote)))

(defun hl-access-remote? ()
  (and
   (not *override-hl-store-remote-access?*)
   (member-eq? *hl-store-modification-and-access* '(:remote-remote :both-remote))))

(defglobal *remote-hl-store-image* nil
    "[Cyc] The remote HL store image")
(defglobal *remote-hl-store-connection-pool* (create-queue))
(deflexical *remote-hl-store-connection-pool-lock* (bt:make-lock "Remote HL Store Connection Pool Lock"))
(deflexical *remote-hl-store-connection-pool-max-size* 9)

(defun define-hl-modifier-preamble ()
  "[Cyc] Code to execute before the internals of the hl-modifier (or hl-creator)."
  (clear-hl-store-dependent-caches))

(defun define-hl-modifier-postamble ()
  "[Cyc] Code to execute before the internals of the hl-modifier (or hl-creator)."
  ;; probably should be "after" in the comment?
  (clear-hl-store-dependent-caches))

(defparameter *hl-store-error-handling-mode* nil)
(defglobal *hl-store-iterators* (make-hash-table))
(declaim (fixnum *next-hl-store-iterator-id*))
(defglobal *next-hl-store-iterator-id* 0)

(defun-inline candidate-next-hl-store-iterator-id ()
  (let* ((retval *next-hl-store-iterator-id*)
         (next (1+ retval)))
    (declare (fixnum retval next))
    (setf *next-hl-store-iterator-id* (if (= most-positive-fixnum next) 0 next))
    retval))

(defun new-hl-store-iterator-id ()
  (loop for next-id = (candidate-next-hl-store-iterator-id)
     while (lookup-hl-store-iterator next-id)
     finally (return next-id)))

(deflexical *hl-store-iterator-lock* (bt:make-lock "HL Store Iterator Lock"))

;; INCOMPLETE
