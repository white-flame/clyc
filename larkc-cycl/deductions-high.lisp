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

(defun-inline create-deduction-spec (supports)
  (cons :deduction (canonicalize-supports supports t)))

(defun-inline deduction-spec-supports (deduction-spec)
  "[Cyc] Returns the list of supports specified by DEDUCTION-SPEC."
  (cdr deduction-spec))

(defun create-deduction-with-tv (assertion supports tv)
  (prog1-let ((deduction (create-deduction assertion supports (tv-truth tv))))
    (set-deduction-strength deduction (tv-strength tv))))

(defun create-deduction-for-hl-support (hl-support justification)
  (create-deduction-with-tv hl-support
                            justification
                            (hl-support-tv hl-support)))

(defun-inline create-deduction (assertion supports truth)
  (kb-create-deduction assertion supports truth))

(defun-inline remove-deduction (deduction)
  (kb-remove-deduction deduction))

(defun-inline set-deduction-strength (deduction new-strength)
  (kb-set-deduction-strength deduction new-strength))

(defun-inline find-deduction (assertion supports &optional (truth :true))
  "[Cyc] Find the deduction that justifies ASSERTION via SUPPORTS having TRUTH.
Return NIL if not present."
  (kb-lookup-deduction assertion supports truth))

(defun deduction-supports-equal (supports1 supports2)
  ;; TODO - could create a LENGTH-SAME helper function that doesn't bother with numeric length at all.
  (and (length= supports1 (length supports2))
       (sets-equal? supports1 supports2 #'support-equal)))

(defun-inline deduction-assertion (deduction)
  "[Cyc] Return the support for which DEDUCTION is a deduction."
  (and (deduction-handle-valid? deduction)
       (kb-deduction-assertion deduction)))

(defun deduction-truth (deduction)
  "[Cyc] Return the truth of DEDUCTION -- either :TRUE or :FALSE or :UNKNOWN."
  (if (deduction-handle-valid? deduction)
      (kb-deduction-truth deduction)
      :unknown))

(defun deduction-strength (deduction)
  (and (deduction-handle-valid? deduction)
       (possibly-unreify-kb-hl-supports (kb-deduction-supports deduction))))

(defparameter *deduction-dump-id-table* nil)

(defun-inline find-deduction-by-dump-id (dump-id)
  (find-deduction-by-id dump-id))
