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
|#

;; Bits and bobs for providing SubL and Java stdlib stuff

(defpackage :clyc
  (:use :common-lisp)
  (:shadow cl:check-type
           cl:defvar
           cl:defparameter
           cl:defconstant
           cl:sxhash))

(in-package :clyc)

(import '(alexandria:symbolicate
          alexandria:when-let))

;;------
;; Variable definitions

;; There's 2 main options in defining variables, which creates 4 ways of binding them.  The two options are:
;;  dynamic = can have dynamic bindings per thread
;;  reinitializing = if the code is (re)loaded, the initialization value overwrites the prior value.

;;  defglobal    = plain baseline variable, initialized once on creation
;;  deflexical   = reinitializing (eg, this file lexically "owns" the value dec)
;;  defvar       = dynamic (same as CL)
;;  defparameter = dynamic + reinitializing (same as CL)
;;
;;  defconstant  = plain variable, but the binding is immutable

(defmacro defglobal (name val &optional doc)
  "Plain variable."
  `(#+sbcl sb-ext:defglobal #-sbcl cl:defvar ,name ,val ,@ (and doc (list doc))))

(defmacro deflexical (name val &optional doc)
  "Reinitialized variable.  The 'lexical' in the name implies this file 'owns' the var, since it is in charge of setting its value."
  ;; SBCL's defglobal works like defvar, so we need to manually update it if it exists
  #+sbcl `(progn
            (sb-ext:defglobal ,name nil ,@ (and doc (list doc)))
            (setf ,name ,val))
  #-sbcl `(cl:defparameter ,name ,val ,@ (and doc (list doc))))


(defmacro defvar (name val &optional doc)
  "Dynamic variable."
  `(cl:defvar ,name ,val ,@ (and doc (list doc))))

(defmacro defparameter (name val &optional doc)
  "Dynamic, reinitialized variable."
  `(cl:defparameter ,name ,val ,@ (and doc (list doc))))

(defmacro defconstant (name val &optional doc)
  "Constant variable binding. If the value is composite, its internals can still be changed."
  ;; The Cyc codebase uses *-earmuffed constant names instead of '+',
  ;; which is a SBCL runtime style warning
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind (#+sbcl(sb-kernel:asterisks-around-constant-variable-name #'muffle-warning))
       (unless (boundp ',name)
         (cl:defconstant ,name ,val ,doc)))))



;;------
;; Cross referencing tools

;; Left as a macro to hook in file-load behavior, if necessary
(defmacro toplevel (&body rest) `(progn ,@rest))

(defparameter *file-defs* (make-hash-table :test #'eq)
  "symbol -> declaration in the form of (<filename> <defintion-type> <symbol>).")
(defparameter *file-refs* (make-hash-table :test #'equal)
  "filename -> list of declarations as above.  Intermediately, this holds just filename->list-of-symbol mappings until the final computation.")

(defun reset-file-cross-references ()
  (clrhash *file-defs*)
  (clrhash *file-refs*))

(defun cross-reference-files ()
  (maphash (lambda (filename syms)
             ;; unceremoniously trampling the value
             (setf (gethash filename *file-refs*)
                   ;; Try to find each sym in the defs table
                   (loop for sym in syms
                      for def = (gethash sym *file-defs*)
                      ;; Filter out refs that are defined in this file
                      when (and def (not (equal filename (first def))))
                      collect def)))
           *file-refs*))

(defmacro macro-helpers (macro-name &body body)
  ;; TODO - verify the macros exist, and that no other references to the function does
  (declare (ignore macro-name))
  `(progn
     ,@body))

;; TODO - add (%meta ...) into defun & defmacro
;;  :inline t
;;  :deprecate <favored function>
;;  :private t
;; also scan the body for cross-reference purposes

(defmacro file (filename &body body)
  ;; Analyze the body for what it defines, for dependency tracking
  (let (;; List of (defun funcname) etc
        (defs nil)
        ;; List of non-imported symbols used in the body, includes locals, literals, etc
        ;; This will be pruned by searching the union of all defs later.
        ;; Of course a true code walker would be handy, but that filter will work.
        (raw-refs nil))
    (labels ((scan-for-defs (form)
               (when (consp form)
                 (let ((head (car form)))
                   ;; Anything starting with DEF is assumed to have a naming symbol
                   (if (string= "DEF" (subseq (symbol-name head) 0 3))
                       (push (list filename (first form) (second form)) defs)
                       ;; Scall all the subelements, as the toplevel might be wrapping defs
                       (mapc #'scan-for-defs form)))))
             (scan-for-refs (form)
               (cond
                 ((consp form) (progn (scan-for-refs (car form))
                                      (scan-for-refs (cdr form))))
                 (t (when (and (symbolp form)
                               (eq *package* (symbol-package form)))
                      (pushnew form raw-refs))))))
      (mapc #'scan-for-defs body)
      (scan-for-refs body))
    ;; Now the actual expansion.
    `(progn
       ;; Cross-reference stuff only runs at toplevel, not compile-time
       (dolist (def ',defs) (setf (gethash (third def) *file-defs*) def))
       (setf (gethash ',filename *file-refs*) ',raw-refs)
       ;; The actual body must be available at compile-time for access from subsequent files
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@body))))







;;------
;; Random stuff to fill in stdlib, accreted as needed.  TODO - organize when it's settled in.


(defmacro missing-larkc (num)
  `(error ,(format nil "This call was replaced for LarKC purposes. Originally a method was called. Refer to number ~a" num)))

(let ((package (find-package "KEYWORD")))
  (defun make-keyword (str)
    (intern (string str) package)))

(defmacro defun-inline (name params &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params ,@body)))

(defmacro defun-ignore (name params &body body)
  "For filling around missing behavior in function bodies, this ignores params and just returns the body, usually T or NIL."
  `(defun-inline ,name ,params
     (declare (ignorable ,@params))
     ,@body))

(declaim (inline check-type))
(defun check-type (obj type-symbol)
  "This is configured to be ignored in the LarKC version."
  (declare (ignorable obj type-symbol)))

(defun enforce-type (obj predicate)
  "The predicate is a symbol which names a unary function, as well as can be printed."
  (unless (funcall predicate obj)
    (error "Got invalid type for object: ~s. Wanted type: ~a Actual type: ~s" obj predicate (type-of obj))))

(defmacro csome ((var list exit-test) &body body)
  "This is often used in general list iteration processing, not necessarily as an exists check. DONE-FORM is a form called before each iteration. If it returns non-NIL the loop is aborted. VAR and LIST work like dolist. Returns the last computed value from the body, or NIL if no iteration ran, which is the main difference from CL:SOME."
  `(let ((result nil))
     (dolist (,var ,list)
       (when ,exit-test
         (return result))
       (setf result (progn ,@body)))))

(defmacro push-last (val place)
  (alexandria:with-gensyms (new-cons list)
    `(let ((,new-cons (cons ,val nil))
           (,list ,place))
       (if ,list
           (rplacd-last ,list ,new-cons)
           (setf ,place ,new-cons)))))

(defmacro pushnew-last (val place &optional (test #'eql))
  (alexandria:once-only (val)
    `(unless (member ,val ,place :test ,test)
       (push-last ,val ,place))))

(defun put (symbol key val)
  (setf (get symbol key) val))

(defmacro must (expr &rest error-stuff)
  `(unless ,expr
     (funcall #'error ,@error-stuff)))

(defmacro must-not (expr &rest rest)
  `(must (not ,expr) ,@rest))

(defmacro while (expr &body body)
  `(loop while ,expr do (progn ,@body)))

(defmacro until (expr &body body)
  `(loop until ,expr do (progn ,@body)))

(declaim (inline fixnump))
(defun fixnump (expr)
  (typep expr 'fixnum))

(declaim (inline doublep))
(defun doublep (expr)
  (typep expr 'double-float))

(defmacro do-plist ((key val list) &body body)
  `(loop for (,key ,val) on ,list by #'cddr
        do (progn ,@body)))

(defmacro dohash ((key val hashtable) &body body)
  `(maphash (lambda (,key ,val)
              (declare (ignorable ,key ,val))
              ,@body)
            ,hashtable))

(defmacro dovector ((index val vector) &body body)
  ;; TODO - convert to DO, probably faster as this version maintains a hidden index anyway
  ;; All subl vectors are simple, saves some good speed
  `(loop for ,val across (the simple-vector ,vector)
         for ,index fixnum from 0
      do (progn ,@body)))

(defmacro dolistn ((index val list) &body body)
  "Like dolist, but also binds a 0-based index variable as the iteration progresses."
  `(let ((,index 0))
     (declare (fixnum ,index))
     (dolist (,val ,list)
       ,@body
       (incf ,index))))

(declaim (inline simple-reader-error))
(defun simple-reader-error (&rest rest)
  (apply #'error rest))

(declaim (inline make-vector))
(defun make-vector (size &optional initial-element)
  (make-array (list size) :initial-element initial-element))

;; These two are used as default functions, like IDENTITY
(defun true (&rest rest)
  (declare (ignore rest))
  t)

(defun false (&rest rest)
  (declare (ignore rest))
  nil)

(defmacro deletef (item place &rest params &key test key)
  (declare (ignore test key))
  `(setf ,place (delete ,item ,place ,@params)))

(defmacro symbol-mapping (&rest plist)
  `(progn
     ,@ (loop for (from to) on plist by #'cddr
           collect `(progn
                      (define-symbol-macro ,from ',to)
                      (defmacro ,from (&rest rest) (cons ',to rest))))))

(defun read-32bit-be (stream)
  "Reads a 4-byte, unsigned, big-endian binary number from the stream."
  ;; TODO - might be faster if we read a single 4-byte sequence, then assemble from that.
  (logior (ash 24 (read-byte stream))
          (ash 16 (read-byte stream))
          (ash 8 (read-byte stream))
          (read-byte stream)))

(declaim (inline set-file-position))
(defun set-file-position (stream index)
  (file-position stream index))

(defmacro on-error (form &body handler)
  `(handler-case ,form
     (error () ,@handler)))

(defmacro setf-error (place &body body)
  "If an error is raised in running BODY, store it in PLACE and return."
  `(handler-case (progn ,@body)
     (error (e) (setf ,place e))))

(declaim (inline get-process-id))
(defun get-process-id ()
  (sb-posix:getpid))

(defun gethash-and-remove (key hashtable &optional default)
  (multiple-value-bind (val found?) (gethash key hashtable)
    (if found?
        (progn
          (remhash key hashtable)
          val)
        default)))

(defmacro defpolymorphic (name args &body body)
  "Default args & body for the non-specialized default implementation."
  `(progn
     (cl:defgeneric ,name ,args)
     (cl:defmethod ,name ,args ,@body)))

(defun function-spec-p (obj)
  (or (functionp obj)
      (and (symbolp obj)
           (fboundp obj))))

;; A java ReentrantReadWriteLock allows multiple readers as long as there's no writer, or a single exclusive writer with no readers. So all will lock a central lock, check if it can work, or wait on an appropriate condition variable, and release the central lock. Exiting a "lock" will again gain the central lock, deregister itself, and notify any appropriate cv.
;; For optimization, fairness, and good ordering semantics, writers could be queued in order of attempt, and readers can block if a writer is already queued instead of starting new read work when write work is waiting.
;; TODO HACK - just to get this up and running, I'll serialize all access for now. Plus, I don't know how often this is used anyway.

(defstruct (rw-lock (:constructor %make-rw-lock))
  lock)

(defun new-rw-lock (name)
  (%make-rw-lock :lock (bt:make-lock name)))

(defmacro with-rw-read-lock ((rw-lock) &body body)
  `(bt:with-lock-held ((rw-lock-lock ,rw-lock))
     ,@body))

(defmacro with-rw-write-lock ((rw-lock) &body body)
  `(bt:with-lock-held ((rw-lock-lock ,rw-lock))
     ,@body))

