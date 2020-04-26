#|
  Copyright (c) 2019-2020 White Flame

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

;; TODO - uses of deduction-id indicate going from object -> id -> manager lookup, which is roundabout and inefficient.

(defstruct (deduction-content (:conc-name "D-CONTENT-"))
  tv
  assertion
  supports)

(defun create-deduction-content (id assertion supports)
  (let ((deduction-content (make-deduction-content :tv :unknown
                                                   :assertion assertion
                                                   :supports supports)))
    (register-deduction-content id deduction-content)
    deduction-content))

(defun destroy-deduction-content (id)
  (when-let ((deduction-content (lookup-deduction-content id)))
    (deregister-deduction-content id)
    (setf (d-content-tv deduction-content)
          (setf (d-content-assertion deduction-content)
                (setf (d-content-supports deduction-content)
                      nil)))
    t))

(defun* lookup-deduction-tv (id) (:inline t)
  (d-content-tv (lookup-deduction-content id)))

(defun* lookup-deduction-assertion (id) (:inline t)
  (d-content-assertion (lookup-deduction-content id)))

(defun* lookup-deduction-supports (id) (:inline t)
  (d-content-supports (lookup-deduction-content id)))

(defun* se-deduction-tv (id new-tv) (:inline t)
  (setf (d-content-tv (lookup-deduction-content id)) new-tv)
  (mark-deduction-content-as-muted id))

(defun load-deduction-content (deduction stream)
  (let* ((id (deduction-id deduction))
         (tv (cfasl-input stream))
         (assertion (cfasl-input stream))
         (supports (cfasl-input stream)))
    (load-deduction-content-int id assertion supports tv))
  deduction)

(defun load-deduction-content-int (id assertion supports tv)
  (let ((deduction-content (create-deduction-content id assertion supports)))
    (setf (d-content-tv deduction-content) tv)
    id))

(defun kb-create-deduction-kb-store (assertion supports truth)
  (let* ((internal-id (make-deduction-id))
         (deduction (make-deduction-shell internal-id)))
    (kb-create-deduction-int deduction internal-id assertion supports truth)
    internal-id))

(defun kb-create-deduction-int (deduction internal-id assertion supports truth)
  (let ((tv (tv-from-truth-strength truth :default)))
    (create-deduction-content internal-id assertion supports)
    (reset-deduction-tv deduction tv)
    (when (assertion-p assertion)
      (add-new-assertion-argument assertion deduction))
    (add-deduction-dependents deduction)))

(defun add-deduction-dependents (deduction)
  (dolist (support (deduction-supports-internal deduction))
    (cond
      ((assertion-p support) (add-assertion-dependent support deduction))
      ((kb-hl-support-p support) (kb-hl-support-add-dependent support deduction)))))

(defun kb-remove-deduction-internal (deduction)
  (let ((id (deduction-id deduction)))
    (destroy-deduction-content id)
    (deregister-deduction-id id))
  (free-deduction deduction))

(defun remove-deduction-dependents (deduction)
  (dolist (support (deduction-supports-internal deduction))
    (cond
      ((valid-assertion? support) (remove-assertion-dependent support deduction))
      ((valid-kb-hl-support? support) (kb-hl-support-remove-dependent support deduction)))))

(defun* reset-deduction-tv (deduction new-tv) (:inline t)
  "[Cyc] Primitively change the tv of DEDUCTION to NEW-TV."
  (set-deduction-tv (deduction-id deduction) new-tv))

(defun kb-set-deduction-strength-internal (deduction new-strength)
  (let* ((truth (argument-truth deduction))
         (new-tv (tv-from-truth-strength truth new-strength)))
    (reset-deduction-tv deduction new-tv)))

(defun find-deduction-internal (assertion supports truth)
  (cond
    ((assertion-p assertion) (dolist (argument (assertion-arguments assertion))
                               (when (and (deduction-p argument)
                                          (deduction-matches-specification argument assertion supports truth))
                                 (return argument))))
    ((kb-hl-support-p assertion) (missing-larkc 11006))))

(defun deduction-matches-specification (deduction assertion supports &optional (truth :true))
  (and (equal assertion (deduction-assertion deduction))
       (eq truth (argument-truth deduction))
       (deduction-supports-equal supports (deduction-supports deduction))))

(defun* deduction-assertion-internal (deduction) (:inline t)
  (lookup-deduction-assertion (deduction-id deduction)))

(defun* deduction-tv (deduction) (:inline t)
  "[Cyc] Return the tv of DEDUCTION."
  (lookup-deduction-tv (deduction-id deduction)))

(defun* deduction-supports-internal (deduction) (:inline t)
  (lookup-deduction-supports (deduction-id deduction)))

(defun* deduction-truth-internal (deduction) (:inline t)
  (tv-truth (deduction-tv deduction)))

(defun* deduction-strength-internal (deduction) (:inline t)
  (tv-strength (deduction-tv deduction)))
