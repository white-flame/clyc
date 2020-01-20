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


(define-hl-creator kb-create-deduction (assertion supports truth)
    "[Cyc] Create a new deduction consisting of SUPPORTS for ASSERTION.
TRUTH is the truth value of the deduction.
Hook up the indexing for the new deduction."
    nil
  (if (hl-modify-remote?)
      (missing-larkc 32156)
      (kb-create-deduction-local assertion supports truth)))

(defun kb-create-deduction-local (assertion supports truth)
  (let ((internal-id (create-deduction-kb-store assertion supports truth)))
    (find-deduction-by-id internal-id)))

(define-hl-modifier kb-remove-deduction (deduction)
    "[Cyc] Remove DEDUCTION from the KB, and unhook its indexing."
    nil
  (remove-deduction-dependents deduction)
  (let ((deduction-assertion (deduction-assertion deduction)))
    (cond
      ((assertion-p deduction-assertion)
       (when (valid-assertion? deduction-assertion)
         (remove-assertion-argument deduction-assertion deduction)))
      ((hl-support-p deduction-assertion)
       (let ((kb-hl-support (find-kb-hl-support deduction-assertion)))
         (when kb-hl-support
           (missing-larkc 11044)))))
    (kb-remove-deduction-internal deduction)))

;; Reusing helper macro from assertions-interface

(define-kb-non-remote lookup-deduction (assertion supports truth)
  "[Cyc] Return the deduction with ASSERTION, SUPPORTS, and TRUTH, if it exists. Return NIL otherwise."
  find-deduction-internal)

(define-kb-non-remote deduction-assertion (deduction)
  "[Cyc] Return the assertion for DEDUCTION.")

(define-kb-non-remote deduction-supports (deduction)
  "[Cyc] Return the supports for DEDUCTION.")

(define-kb-non-remote deduction-truth (deduction)
  "[Cyc] Return the truth for DEDUCTION.")

(define-kb-non-remote deduction-strength (deduction)
  "[Cyc] Return the strength for DEDUCTION.")

(define-hl-modifier kb-set-deduction-strength (deduction new-strength)
    "[Cyc] Change the strength of DEDUCTION to NEW-STRENGTH."
    nil
  (kb-set-deduction-strength-internal deduction new-strength))


