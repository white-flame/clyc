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

;; A bunch of checking, debug reporting, & error handling stuff for the sbhl layer

(in-package :clyc)

(defparameter *sbhl-object-type-checking-p* t
    "[Cyc] Parameter that governs whether we perform work within SBHL-CHECK-TYPE.")

(defun-inline sbhl-object-type-checking-p ()
  ;; TODO - the cyc comments use "accessor" when they mean "reader" which goes against the CLHS definition.
  "[Cyc] The boolean value of *SBHL-OBJECT-TYPE-CHECKING-P*."
  (or *sbhl-object-type-checking-p*
      (not (suspend-sbhl-type-checking?))))

(defparameter *sbhl-type-error-action* :error
    "[Cyc] Parameter that guides error behavior.")

(defparameter *sbhl-trace-level* 1
    "[Cyc] Controls extent of tracing, warnings, etc., for the sbhl modules [0 .. 5].")

(defun sbhl-error (level format-str &rest args)
  "[Cyc] If *SBHL-TRACE_LEVEL* si greater than or equal to LEVEL, signal a cerror with FORMAT-STR. If *SBHL-TRACE_LEVEL* is within 2 of LEVEL, warn with FORMAT-STR."
  (cond
    ((>= *sbhl-trace-level* level)
     (apply #'cerror "Continue anyway" args))
    ((>= (+ 2 *sbhl-trace-level*) level)
     (apply #'warn format-str args))))

(defun sbhl-cerror (level continue-str format-str &rest args)
  "[Cyc] If *SBHL-TRACE-LEVEL* is greater than or equal to LEVEL, signal a cerror with CONTINUE-STR and FORMAT-STR. If *SBHL-TRACE-LEVEL* is within 2 of LEVEL, warn with FORMAT-STR."
  (cond
    ((>= *sbhl-trace-level* level)
     (apply #'cerror continue-str format-str args))
    ((>= (+ 2 *sbhl-trace-level*) level)
     (apply #'warn format-str args))))

(defun sbhl-warn (level format-str &rest args)
  "[Cyc] If *SBHL-TRACE-LEVEL* is greater than or equal to LEVEL, warn with FORMAT-STR."
  (when (>= *sbhl-trace-level* level)
    (apply #'warn format-str args)))

(defun suspend-sbhl-type-checking? ()
  (or *suspend-type-checking?*
      *suspend-sbhl-type-checking?*
      (some-fort-being-removed?)
      *within-unassert*))

(defparameter *sbhl-test-level* 3
  "[Cyc] Controls extent of integrity testing for teh sbhl modules [0 .. 5].")
(defparameter *sbhl-test-max?* nil
    "[Cyc] Controls very expensive sbhl testing.")
(defparameter *sbhl-trace-max?* nil
    "[Cyc] Controls very expensive sbhl tracing.")
(defparameter *suppress-sbhl-recaching?* nil
    "[Cyc] Suppress retyping of forts iff this is non-NIL.")
(defparameter *suspend-sbhl-cache-use?* nil
    "[Cyc] Suppress use of fort types, and call the SBHL instead.
This should be set to NIL during for types initialization.")

(defun check-sbhl-caches? ()
  (and *suspend-sbhl-cache-use?*
       (sbhl-caches-initialized-p)))


;; TODO DESIGN - maybe an option to disable typechecking at compilation time, too

;; Expansion taken from multiple instances in sbhl-module-vars
;; This macro used to contain the following function body inside it.
;; To me, that's a waste of instruction cache, so we'll just macro the check, and call when it's enabled.
;; Disablable error handling doesn't need to be fast anyway, as long as the inline portion of the check is small.
(defmacro sbhl-check-type (object type-test &optional (level 1))
  `(when (and (sbhl-object-type-checking-p)
              (,type-test ,object))
     (sbhl-handle-type-check-failure ,object ',type-test ,level)))

(defun sbhl-handle-type-check-failure (object type-test level)
  ;; TODO - this is kind of dumb. Can't we register a function instead of a keyword?
  (case *sbhl-type-error-action*
    (:error (sbhl-error level "~a is not a ~a." object type-test))
    ;; TODO - is this supposed to be sbhl-cerror?  it does exist
    (:cerror (missing-larkc 2198))
    (:warn (warn "~a is not a ~a." object type-test))
    (otherwise
     (warn "~a is not a valid *SBHL-TYPE-ERROR-ACTION* value." *sbhl-type-error-action*)
     (cerror "Continue anyway" "~a is not a ~a." object type-test))))
