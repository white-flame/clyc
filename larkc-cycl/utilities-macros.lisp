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


(defun register-cyc-api-function (name arglist doc-string argument-types return-types)
  "[Cyc] Register NAME as a (public) Cyc API function. Note its ARGLIST, DOC-STRING, ARGUMENT-TYPES and RETURN-TYPES."
  (register-api-predefined-function name)
  (register-cyc-api-symbol name)
  (register-cyc-api-args name arglist)
  (register-cyc-api-function-documentation name doc-string)
  (register-cyc-api-arg-types name argument-types)
  (register-cyc-api-return-types name return-types))

(defun register-cyc-api-macro (name pattern doc-string)
  "[Cyc] Register NAME as a (public) Cyc API macro. Note its PATTERN and DOC-STRING."
  (register-api-predefined-macro name)
  (register-cyc-api-symbol name)
  (register-cyc-api-args name pattern)
  (register-cyc-api-function-documentation name doc-string))

(defglobal *api-special-table* (make-hash-table :test #'eq))

(defun api-special-p (operator)
  (gethash operator *api-special-table*))

(defun register-api-special (operator handler)
  (if (or (api-predefined-function-p operator)
          (api-predefined-macro-p operator))
      (warn "Attempted to register ~a as special even though it's already predefined!" operator)
      (setf (gethash operator *api-special-table*) handler))
  operator)

(defglobal *api-predefined-function-table* (make-hash-table :test #'eq))

(defun api-predefined-function-p (operator)
  (if (api-predefined-host-function-p operator)
      *permit-api-host-access*
      (gethash operator *api-predefined-function-table*)))

(defun register-api-predefined-function (operator)
  "[Cyc] Permit the use of the OPERATOR function in the CYC api"
  (unless (api-special-p operator)
    (setf (gethash operator *api-predefined-function-table*) t))
  operator)

(defglobal *api-predefined-host-function-table* (make-hash-table :test #'eq))

(defun api-predefined-host-function-p (operator)
  (unless (api-special-p operator)
    (setf (gethash operator *api-predefined-host-function-table*) t))
  operator)

(defglobal *api-predefined-macro-table* (make-hash-table :test #'eq))

(defun api-predefined-macro-p (operator)
  (if (api-predefined-host-macro-p operator)
      *permit-api-host-access*
      (gethash operator *api-predefined-macro-table*)))

(defun register-api-predefined-macro (operator)
  "[Cyc] Permit the use of the OPERATOR macro in the CYC api"
  (unless (api-special-p operator)
    (setf (gethash operator *api-predefined-macro-table*) t))
  operator)

(defglobal *api-predefined-host-macro-table* (make-hash-table :test #'eq))

(defun api-predefined-host-macro-p (operator)
  (gethash operator *api-predefined-host-macro-table*))

(defun register-api-predefined-host-macro (operator)
  "[Cyc] Permit the use of the OPERATOR host-access macro in the CYC api"
  (unless (api-special-p operator)
    (setf (gethash operator *api-predefined-host-macro-table*) t))
  operator)

(defglobal *api-symbols* nil)

(defun register-cyc-api-symbol (name)
  "[Cyc] Register the symbol NAME as a defined Cyc API function or macro. Return the NAME."
  (declare (symbol name))
  (put name :cyc-api-symbol t)
  (pushnew name *api-symbols*)
  name)

(defun register-cyc-api-args (name arglist)
  "[Cyc] For the symbol NAME, register the Cyc API ARGLIST since SMUCL does not record the macro argument list. Return the NAME."
  (declare (symbol name)
           (list arglist))
  (put name :cyc-api-args arglist)
  name)

(defun register-cyc-api-function-documentation (name documentation-string)
  "[Cyc] Register DOCUMENTATION-STRING as the function documentation for NAME."
  (declare (symbol name)
           (string documentation-string)
           (ignore name documentation-string)))

(defun register-cyc-api-arg-types (name argument-type-list)
  "[Cyc] For the symbol NAME, register the Cyc API function argument types, whcih take the form of a list of argument type expressions. Return the NAME."
  (declare (symbol name)
           (list argument-type-list))
  (put name :cyc-api-arg-types argument-type-list))

(defun register-cyc-api-return-types (name return-types)
  (declare (symbol name)
           (list return-types))
  (dolist (return-type return-types)
    (validate-return-type return-type))
  (put name :cyc-api-return-types return-types)
  name)

(defglobal *api-types* nil)

(defun validate-return-type (return-type)
  "[Cyc] Ensure that each symbol denoting a predicate in the RETURN-TYPE expression is recorded as an api type function. Return T if OK, otherwise signal an error."
  (if (atom return-type)
      (progn
        (pushnew return-type *api-types*)
        t)
      (progn
        (must (= 2 (length return-type))
              "~s return type expression not list length 2." return-type)
        (if (member (first return-type) '(list nil-or))
            (validate-return-type (second return-type))
            (error "~s complex return type expression" return-type)))))

(defglobal *kb-function-table* (make-hash-table :test #'eq))

(defun register-kb-symbol (symbol)
  "[Cyc] Note that SYMBOL is expected to be a symbol referenced by the KB."
  (declare (symbol symbol))
  (setf (gethash symbol *kb-function-table*) t)
  symbol)

(macro-helpers define-kb
  (defun register-kb-function (function-symbol)
    "[Cyc] Note that SYMBOL is expected to be a function symbol referenced by the KB."
    (register-kb-symbol function-symbol)))

(defglobal *funcall-helper-property* :funcall-helper)

(macro-helpers define-private-funcall
  (defun note-funcall-helper-function (symbol)
    "[Cyc] Note that SYMBOL has been defined via DEFINE-PRIVATE-FUNCALL"
    (put symbol *funcall-helper-property* t)))

(defglobal *unprovided* '#:unprovided
    "[Cyc] A unique marker which can be used as a default optional argument to indicate that no argument was explicitly provided. The ONLY references to this variable should be as the default value for an argument in the formal arguments for a function.")

(defun unprovided-argument-p (arg)
  "[Cyc] Return T iff ARG indicates that it was an unprovided argument to a function call.."
  (eq arg *unprovided*))

(defun declare-control-parameter-internal (variable fancy-name description settings-spec)
  (declare (symbol variable)
           (string fancy-name)
           (string description))
  (put variable :fancy-name fancy-name)
  (put variable :description description)
  (put variable :settings-spec settings-spec)
  variable)

(defun mapping-finished ()
  (throw :mapping-done nil))

(defparameter *cfasl-stream* nil)
(defglobal *global-locks* nil)

(defun initialize-global-locks ()
  "[Cyc] Initialize all global locks"
  (dolist (pair *global-locks*)
    (destructuring-bind (global name) pair
      (initialize-global-lock-internal global name))))

;; TODO - SubL can have macros and functions with the same name?
(macro-helpers register-global-lock
  (defun register-global-lock (global name)
    (declare (symbol global)
             (string name))
    (setf *global-locks* (cons (cons global name)
                               (delete global *global-locks* :key #'car)))))

(defun global-lock-initialization-form (name)
  (declare (string name))
  (list 'make-lock name))

(macro-helpers initialize-global-locks
  (defun initialize-global-lock-internal (global name)
    (eval (list 'setf global (global-lock-initialization-form name)))
    global))


(defglobal *fi-state-variables* nil)

(macro-helpers def-state-variable
  (defun note-state-variable-documentation (variable documentation)
    (put variable :variable-doc documentation)
    variable))

(defglobal *gt-state-variables* nil)
(defglobal *at-state-variables* nil)
(defglobal *defn-state-variables* nil)
(defglobal *kbp-state-variables* nil)
(defparameter *current-forward-problem-store* nil
    "[Cyc] The current problem store in use for forward inference.")

;; Converted this from a function to a macro, since it contains distant forward code references
(defmacro within-normal-forward-inference? ()
  "[Cyc] A more strict version of (within-forward-inference?) that returns NIL when we're in a special forward inference mode."
  `(and (within-forward-inference?)
        (not *within-assertion-forward-propagation?*)
        (not *prefer-forward-skolemization*)))

(defparameter *tracing-level* nil
    "[Cyc] An alist of things to trace and the level at which they should be traced. Current items are :CYCL, :PLANNER, and :EXECUTOR.  A level of NIL or 0 means don't print out any tracing information. Higher numbers mean do more tracing.")
(deflexical *structure-resourcing-enabled* nil
    "[Cyc] Controls whether or not a free list is maintained and used for a structure resource declared via DEFINE-STRUCTURE-RESOURCE.")
(defparameter *structure-resourcing-make-static* nil
    "[Cyc] Controls whether or not any new structure is statically allocated for a structure resource declared via DEFINE-STRUCTURE-RESOURCE.")
(defvar *silent-progress?* nil)
(defparameter *noting-progress-start-time* nil)

(defmacro noting-progress ((title) &body body)
  "Assumed from usage"
  `(let ((*noting-progress-start-time* (get-universal-time)))
     (noting-progress-preamble ,title)
     ,@body
     (noting-progress-postamble)))

(macro-helpers noting-progress
  (defun noting-progress-preamble (string)
    (unless *silent-progress?*
      (format t "~%~a" string)
      (force-output)))

  (defun noting-progress-postamble ()
    (unless *silent-progress?*
      (let ((elapsed (elapsed-universal-time *noting-progress-start-time*)))
        (format t "DONE (~a)~%" (elapsed-time-abbreviation-string elapsed))
        (force-output)))))

(defvar *last-percent-progress-index* nil)
(defparameter *last-percent-progress-prediction* nil
    "[Cyc] Bound to the latest prediction we made about how long the process will take, or NIL if we haven't made such a prediction.")
(defvar *within-noting-percent-progress* nil)
(defvar *percent-progress-start-time* nil)

(defmacro noting-percent-progress ((note) &body body)
  ;; from utilities_macros.java, list217
  `(let ((*last-percent-progress-index* 0)
         (*last-percent-progress-prediction* nil)
         (*within-noting-percent-progress* t)
         (*percent-progress-start-time* (get-universal-time)))
    (noting-percent-progress-preamble ,note)
    ,@body
    (noting-percent-progress-postamble)))

(macro-helpers noting-percent-progress
  (defun noting-percent-progress-preamble (string)
    (unless *silent-progress?*
      (format t "~&~a~% [" string)
      (force-output)))

  (defun noting-percent-progress-postamble ()
    (unless *silent-progress?*
      (let ((elapsed (elapsed-universal-time *percent-progress-start-time*)))
        (format t " DONE (~a" (elapsed-time-abbreviation-string elapsed))
        (when (> elapsed 600)
          (format t ", ended (missing-larkc 23127)"))
        (format t ") ]~%")
        (force-output))))

  (defun note-percent-progress (index max)
    (when (and (not *silent-progress?*)
               *within-noting-percent-progress*
               index)
      (let ((percent (compute-percent-progress index max)))
        (when (> percent *last-percent-progress-index*)
          (let* ((elapsed (elapsed-universal-time *percent-progress-start-time*))
                 (predicted-total-seconds (truncate (* elapsed 100) percent)))
            (cond
              ((and (or (= 1 percent) (= 1 index))
                    (>= predicted-total-seconds (* 5 *seconds-in-a-minute*)))
               (write-char #\.)
               (possibly-note-percent-progress-prediction elapsed predicted-total-seconds 300 600))

              ((and (or (= 2 percent) (= 2 index))
                    (>= predicted-total-seconds (* 10 *seconds-in-a-minute*)))
               (write-char #\.)
               (possibly-note-percent-progress-prediction elapsed predicted-total-seconds 300 600))

              ((<= predicted-total-seconds 5) nil)

              ((= 0 (mod percent 10))
               (print-progress-percent percent)
               (possibly-note-percent-progress-prediction elapsed predicted-total-seconds 600 1200))

              ((< max 60)
               (write-char #\.))

              ((<= predicted-total-seconds 20) nil)

              (t (progn
                   (when (= 0 (mod percent 2))
                     (write-char #\.))
                   (when *last-percent-progress-prediction*
                     (let ((threshold (+ *last-percent-progress-prediction*
                                         (truncate *last-percent-progress-prediction* 10))))
                       (when (> predicted-total-seconds threshold)
                         (print-progress-percent percent)
                         (possibly-note-percent-progress-prediction elapsed predicted-total-seconds threshold 1200))))))))
          (force-output)
          (setf *last-percent-progress-index* percent))))))

(defun print-progress-percent (percent)
  (write-char #\Space)
  (print-2-digit-nonnegative-integer percent *standard-output*)
  (write-char #\%))

(defun print-2-digit-nonnegative-integer (integer stream)
  (format stream "~2,'0d" integer))

(defun possibly-note-percent-progress-prediction (elapsed predicted-total-seconds threshold &optional show-ending-threshold)
  (when (and (> predicted-total-seconds threshold)
             (> predicted-total-seconds elapsed))
    (format t " (~a of ~~~a" (elapsed-time-abbreviation-string elapsed)
            (elapsed-time-abbreviation-string predicted-total-seconds))
    (when (and show-ending-threshold
               (> predicted-total-seconds show-ending-threshold))
      (format t ", ending ~~(missing-larkc 13128)")
      )
    (format t ")~%  ")
    t))

(defun compute-percent-progress (index max)
  (cond
    ((or (<= max 0) (<= index 0)) 0)
    ((>= index max) 100)
    (t (let* ((target-length 10)
              (current-length (integer-length max))
              (scale-factor (- target-length current-length)))
         (when (minusp scale-factor)
           (setf index (ash index scale-factor))
           (setf max (ash max scale-factor))))
       (min 99 (truncate (* 100 index) max)))))

(defparameter *progress-note* "")
(defparameter *progress-start-time* (get-universal-time))
(defparameter *progress-total* 1)
(defparameter *progress-so-far* 0)

(defmacro progress-dolist ((var list &optional (message "")) &body body)
  ;; expansion modeled from memoization_state.clear_all_globally_cached_functions
  ;; TODO - though it works like dolist, the var's called csome, but there's no early exit functionality?
  (alexandria:once-only (list)
    `(progn
       ;; TODO - why do these set instead of making a new binding?
       (setf *progress-note* ,message
             *progress-total* (length ,list)
             *progress-so-far* 0)
       (noting-percent-progress (*progress-note*)
         (dolist (,var ,list)
           (note-percent-progress *progress-so-far* *progress-total*)
           (incf *progress-so-far*)
           ,@body)))))

(defparameter *util-var-error-format-string* "~s - var ~s is not a symbol.")
(defparameter *util-func-error-format-string* "~s - function ~s is not a symbol.")
(defparameter *util-search-type-error-format-string* "~s - search type ~s is not one of (:depth-first :breadth-first).")
(deflexical *process-resource-tracking-100s-of-nanoseconds-properties* '(:user-time :system-time)
    "[Cyc] Properties whose values are expressed in 100s of nanoseconds")

(macro-helpers with-process-resource-tracking-in-seconds
  (defun convert-process-resource-tracking-timing-info-to-seconds (timing-info)
    "[Cyc] Destructively divides the times in TIMING-INFO to convert them into seconds. Assumes they were originally in 100s of nanoseconds."
    (let ((converted-timing-info nil))
      (loop for (property value) on timing-info by #'cddr
         do (let ((new-value (if (member property *process-resource-tracking-100s-of-nanoseconds-properties* :test #'eq)
                                 (/ value 10000000)
                                 value)))
              (setf (getf converted-timing-info property) new-value)))
      converted-timing-info))

  (defun nadd-clock-time-to-process-resource-timing-info (clock-time timing-info)
    "[Cyc] @hack until with-process-resource-tracking supports wall clock time"
    (setf (getf timing-info :wall-clock-time) clock-time)))

(defglobal *kb-var-initializations* nil
    "[Cyc] Store of (var init-func) pairs that specify kb-variables and their kb-dependent initialization")

(macro-helpers def-kb-variable
  (defun register-kb-variable-initialization (var-symbol func)
    "[Cyc] Associates FUNC as the initialization function for VAR-SYMBOL in @xref *kb-var-initializations*"
    (setf *kb-var-initializations* (alist-enter *kb-var-initializations* var-symbol func))))

(defun initialize-kb-variables ()
  "[Cyc] Initializes all of the KB_vars with their initialization functions. @xref *kb-var-initializations*"
  (noting-progress ("Initializing KB variables...")
    (dolist (cons *kb-var-initializations*)
      (destructuring-bind (var-symbol . func) cons
        (set var-symbol (funcall func))))))

(defun register-obsolete-cyc-api-function (name replacements arglist doc-string argument-types return-types)
  "[Cyc] Register NAME as a deprecated (public) Cyc API function. Note its REPLACEMENTS, ARGLIST, DOC-STRING, ARGUMENT-TYPES and RETURN-TYPES."
  (register-cyc-api-function name arglist doc-string argument-types return-types)
  (register-obsolete-cyc-api-replacements name replacements))

(defun register-obsolete-cyc-api-replacements (name replacements)
  "[Cyc] For the symbol NAME, denoting a deprecated Cyc API register the Cyc API replacements. Return the NAME."
  (declare (symbol name)
           (list replacements))
  (put name :obsolete-cyc-api-replacements replacements))

(defparameter *partial-results-accumulator* nil
    "[Cyc] Partial results can be accumulated here.")
(defparameter *partial-results-size* nil
    "[Cyc] How many partial results have been stacked up.")
(defparameter *partial-results-threshold* 40
    "[Cyc] When the partial results, if ever, are supposed to be flushed.")
(defparameter *partial-results-total-size* nil
    "[Cyc] How many results have been computed altogether up.")
(defparameter *partial-results-total-accumulator* nil
    "[Cyc] Once the partial results have been notified, they can be added to here.")
(defparameter *partial-results-initialization-fn* 'initialization-for-partial-list-results
    "[Cyc] How the partial results have to be setup.")
(defparameter *partial-results-accumulation-fn* 'accumulation-for-partial-list-results
    "[Cyc] Who adds a new result to the partial results we already have.")
(defparameter *partial-results-consolidation-fn* 'consolidation-for-partial-list-results
    "[Cyc] Who adds the partial results in the accumulator to the total result set.")
(defparameter *partial-results-notification-fn* 'notification-for-partial-list-results
    "[Cyc] Who gets the partial results as they become available.")
(defparameter *partial-results-final-result-fn* 'final-results-for-partial-list-results
    "[Cyc] How the partial results will be processed for final usage.")
(defparameter *accumulating-partial-results?* nil)
(defconstant *sxhash-bit-limit* 27
    "[Cyc] The number of bits used in the internal integer computations done by sxhash-external.")
(deflexical *sxhash-update-state-vector* #(7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 0 1 2 3 4 5 6))

(defun sxhash-update-state (state)
  "[Cyc] Update the composite hash STATE"
  (aref *sxhash-update-state-vector* state))

(defparameter *sxhash-composite-state* nil)
(defparameter *sxhash-composite-hash* nil)
