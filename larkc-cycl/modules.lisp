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

(deflexical *module-lock* (bt:make-lock "Module Lock"))
(deflexical *system-lock* (bt:make-lock "System Lock"))

(defstruct module
  basis ;; field2
  name
  system
  pathname
  test-cases
  test-suites
  provisional-p ;; field8
  )

(defglobal *module-index* (make-hash-table :test #'equalp))

(defstruct system
  basis ;; field2
  name
  default-pathname
  modules
  provisional-p ;; field6
  )

(deflexical *system-index* nil
    "[Cyc] List of systems")


(defun module-store (module)
  (bt:with-lock-held (*module-lock*)
    (setf (gethash (list (module-name module) (module-system module)) *module-index*) module)))

(defun module-new (name system-name &optional provisional-p pathname)
  (declare (string name system-name)
           (ignore provisional-p pathname))
  (let ((system (system-lookup system-name)))
    (must (system-p system) "~a is not the name of a known system." system-name)
    (let* ((name (string-downcase name))
           (system-name (string-downcase system-name))
           (new (make-module :name name :system system-name)))
      (module-store new)
      (system-add-module system new)
      new)))

(defun create-module (name system-name)
  (module-new name system-name))

(defun module-get-name (module)
  (module-name module))

(defun module-get-system (module)
  (module-system module))

(defun system-store (system)
  (bt:with-lock-held (*system-lock*)
    (pushnew system *system-index*)))

(defun system-lookup (system-name)
  (declare (string system-name))
  (find system-name *system-index* :test #'equal :key #'system-name))

(defun system-new (system-name &optional provisional-p default-pathname)
  (declare (string system-name)
           (ignore provisional-p default-pathname))
  (let ((new (make-system :name system-name)))
    (system-store new)
    new))

(defun create-system (system-name)
  (system-new system-name))

(defun system-get-name (system)
  (system-name system))

(defun system-add-module (system module)
  "[Cyc] This is only called when MODULE is a provisional module, or we are running translated C code!"
  (declare (system system)
           (module module))
  (bt:with-lock-held (*system-lock*)
    (pushnew module (system-modules system))))
