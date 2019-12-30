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


(defmacro declare-cfasl-opcode (name val func)
  ;; These two seem to happen for each opcode
  `(progn
     (defconstant ,name ,val)
     (register-cfasl-input-function ,name ,func)))

(defstruct cfasl-encoding-stream
  internal-stream)

(defun cfasl-output-maybe-externalized (object stream externalized?)
  (if externalized?
      (cfasl-output-externalized object stream)
      (cfasl-output object stream)))

(defparameter *terse-guid-serialization-enabled?* nil
  "[Cyc] Temporary control variable, should eventually stay T.")
(defparameter *terse-guid-serialization-enabled-for-cfasl-encode-externalized?* :uninitialized
  "[Cyc] Temporary control variable, the only controls whether cfasl-encode-externalized-terse uses terse GUID serialization")

(defstruct cfasl-decoding-stream
  internal-stream)

(defstruct cfasl-count-stream
  position)

(defparameter *cfasl-stream-extensions-enabled* nil)
(defparameter *cfasl-unread-byte* nil)

(defun cfasl-output (object stream)
  "[Cyc] Output OBJECT to STREAM in the CFASL protocol. The encoding is relevant to this image only."
  (when (cfasl-compress-object? object)
    (missing-larkc 12988))
  (cfasl-output-object object stream))

(defun cfasl-output-externalized (object stream)
  "[Cyc] Output OBJECT to STREAM in the CFASL protocol. The encoding is relevant to any image, not just this one."
  (cfasl-output-externalization object stream))

  ;; TODO - part of the method dispatch stuff?

(deflexical *cfasl-output-object-method-table* (make-array '(256)))

(defpolymorphic cfasl-output-object (object stream)
  ;; TODO - is this correct?  does Errors.handleMissingMethodError fill in for any expression but blows up?
  (missing-larkc 31000))

(defun cfasl-raw-write-byte (byte stream)
  (cond
    ((cfasl-count-stream-p stream) (missing-larkc 30957))
    ((cfasl-encoding-stream-p stream) (missing-larkc 30966))
    (t (write-byte byte stream))))

(defparameter *cfasl-input-to-static-area* nil
  "[Cyc] If non-NIL, then structure created during CFASL input is allocated in the static area.")

(defun cfasl-input (stream &optional (eof-error-p t) (eof-value :eof))
  "[Cyc] Input an object from STREAM in the CFASL protocol. EOF-ERROR-P indicates whether an end-of-file is considered an error. EOF-VALUE indicates a value to return when end-of-file is encountered and EOF-ERROR-P is NIL."
  (cfasl-input-internal stream eof-error-p eof-value))

(defun cfasl-opcode-peek (stream &optional (eof-error-p t) (eof-value :eof))
  "[Cyc] Peek at STREAM to return the opcode for the next object to be read in CFASL protocol. EOF-ERROR-P indicates whether an end-of-file is considered an error. EOF-VALUE indicates a value to return when end-of-file is encountered and EOF-ERROR-P is NIL."
  (cfasl-opcode-peek-internal stream eof-error-p eof-value))

(defun cfasl-input-object (stream)
  (cfasl-input-internal stream nil nil))

(defun cfasl-input-internal (stream eof-error-p eof-value)
  (let ((opcode (cfasl-raw-read-byte stream)))
    (cond
      ((null opcode) (if eof-error-p
                         (error "end-of-file on stream ~s" stream)
                         eof-value))
      ((cfasl-immediate-fixnum-opcode opcode) (cfasl-extract-immediate-fixnum opcode))
      (t (let ((cfasl-input-method (cfasl-input-method opcode)))
           (if (eq 'cfasl-input-error cfasl-input-method)
               (error "Undefined cfasl opcode ~s in ~s" opcode stream)
               (funcall cfasl-input-method stream)))))))

(defun cfasl-opcode-peek-internal (stream eof-error-p eof-value)
  (let ((opcode (cfasl-raw-peek-byte stream)))
    (cond
      ((null opcode) (if eof-error-p
                         (error "end-of-file on stream ~s" stream)
                         eof-value))
      (t opcode))))

(defconstant *cfasl-max-opcode* 128)
(defglobal *cfasl-input-method-table* (make-array (list *cfasl-max-opcode*) :initial-element 'cfasl-input-error)
  "[Cyc] Dispach table used by CFASL-INPUT.")

(defun cfasl-input-method (cfasl-opcode)
  (aref *cfasl-input-method-table* cfasl-opcode))

(defun register-cfasl-input-function (cfasl-opcode function)
  (setf (aref *cfasl-input-method-table* cfasl-opcode) function))

(defun cfasl-raw-peek-byte (stream)
  ;; Rewritten since CL doesn't have unread-byte
  ;; Pulled error detection from cfasl-raw-read-byte
  (when (cfasl-decoding-stream-p stream)
    (missing-larkc 30961))
  (flexi-streams:peek-byte stream))

(defun cfasl-raw-read-byte (stream)
  (when (cfasl-decoding-stream-p stream)
    (missing-larkc 30961))
  (read-byte stream))

(defparameter *within-cfasl-externalization* nil)

(defun within-cfasl-externalization-p ()
  "[Cyc] Return T iff we are assuming CFASL externalization."
  *within-cfasl-externalization*)

(defparameter *cfasl-channel-externalized?* t)
(defglobal *cfasl-extensions* nil)
(declare-cfasl-opcode *cfasl-opcode-externalization* 51 'cfasl-input-externalization)

(defun cfasl-output-externalization (object stream)
  (cfasl-raw-write-byte *cfasl-opcode-externalization* stream)
  (let ((*within-cfasl-externalization* t))
    (cfasl-output object stream)))

(defun cfasl-input-externalization (stream)
  (let ((*within-cfasl-externalization* t))
    (cfasl-input-object stream)))

(defparameter *current-cfasl-defstruct-output-stream* nil
  "[Cyc] The current stream to which the CFASL-DEFSTRUCT-RECIPE-OUTPUT-METHOD is writing.")
(declare-cfasl-opcode *cfasl-opcode-defstruct-recipe* 44 'cfasl-input-defstruct-recipe)

(defun cfasl-output-defstruct-recipe (object stream)
  "[Cyc] This method expects to be called by CFASL-OUTPUT in the case where no implementation is available for a STRUCTURE-P."
  (declare (ignorable object stream))
  ;; TODO - missing means executing is aborted?  this seems pretty important to have working
  (missing-larkc 30992))

(defun cfasl-output-defstruct-recipe-visitorfn (obj phase param-a param-b)
  (let ((stream *current-cfasl-defstruct-output-stream*))
    (case phase
      (:begin (cfasl-output param-a stream) ;; constructor-fn
              (cfasl-output param-b stream) ;; num-of-slots
              )
      (:slot (cfasl-output param-a stream) ;; slot-name
             (cfasl-output param-b stream) ;; slot-value
             )
      (:end))
    obj))

(defun cfasl-input-defstruct-recipe (stream)
  "[Cyc] this method is dispatched to by the CFASL-INPUT infrastructure after the *CFASL-OPCODE-DEFSTRUCT-RECIPE* has been read."
  (let ((constructor-fn (cfasl-input stream))
        (num-of-slots (cfasl-input stream))
        (plist nil)
        (cursor nil))
    (must (function-spec-p constructor-fn) "Error: Expected constructor for defstruct, got ~a" constructor-fn)
    (setf plist (make-list (+ num-of-slots num-of-slots)))
    (setf cursor plist)
    (dotimes (i num-of-slots)
      (let ((slot-name (cfasl-input stream))
            (slot-value (cfasl-input stream)))
        (must (keywordp slot-name) "Expected keyword at slot #~a of structure with constructor ~a, got ~a"
              i constructor-fn slot-name)
        (setf (nth 0 cursor) slot-name)
        (setf (nth 1 cursor) slot-value)
        (setq cursor (cddr cursor))))
    (funcall constructor-fn plist)))

(defconstant *cfasl-immediate-fixnum-cutoff* *cfasl-max-opcode*)
(defconstant *cfasl-immediate-fixnum-offset* (- 256 *cfasl-immediate-fixnum-cutoff*))

(defun cfasl-immediate-fixnump (object)
  (and (typep object 'fixnum)
       (>= object 0)
       (< object *cfasl-immediate-fixnum-cutoff*)))

(defun cfasl-output-immediate-fixnum (object stream)
  (cfasl-raw-write-byte (+ *cfasl-immediate-fixnum-offset* object) stream)
  object)

(defun cfasl-immediate-fixnum-opcode (opcode)
  (>= opcode *cfasl-immediate-fixnum-offset*))

(defun cfasl-extract-immediate-fixnum (opcode)
  (- opcode *cfasl-immediate-fixnum-offset*))

(declare-cfasl-opcode *cfasl-opcode-p-8bit-int* 0 'cfasl-input-p-8bit-int)
(declare-cfasl-opcode *cfasl-opcode-n-8bit-int* 1 'cfasl-input-n-8bit-int)
(declare-cfasl-opcode *cfasl-opcode-p-16bit-int* 2 'cfasl-input-p-16bit-int)
(declare-cfasl-opcode *cfasl-opcode-n-16bit-int* 3 'cfasl-input-n-16bit-int)
(declare-cfasl-opcode *cfasl-opcode-p-24bit-int* 4 'cfasl-input-p-24bit-int)
(declare-cfasl-opcode *cfasl-opcode-n-24bit-int* 5 'cfasl-input-n-24bit-int)
(declare-cfasl-opcode *cfasl-opcode-p-32bit-int* 6 'cfasl-input-p-32bit-int)
(declare-cfasl-opcode *cfasl-opcode-n-32bit-int* 7 'cfasl-input-n-32bit-int)
(declare-cfasl-opcode *cfasl-opcode-p-bignum-int* 23 'cfasl-input-p-bignum)
(declare-cfasl-opcode *cfasl-opcode-n-bignum-int* 24 'cfasl-input-n-bignum)

(defun cfasl-output-object-integer-method (object stream)
  (cfasl-output-integer object stream))

(defun cfasl-output-integer (integer stream)
  (if (cfasl-immediate-fixnump integer)
      (cfasl-output-immediate-fixnum integer stream)
      (let ((positive (plusp integer))
            (value (abs integer)))
        (if (< value 2147483648)
            (multiple-value-bind (num-bytes pos-opcode neg-opcode)
                (cond
                  ((< value 128) (values 1 *cfasl-opcode-p-8bit-int* *cfasl-opcode-n-8bit-int*))
                  ((< value 32768) (values 2 *cfasl-opcode-p-16bit-int* *cfasl-opcode-n-16bit-int*))
                  ((< value 8388608) (values 3 *cfasl-opcode-p-24bit-int* *cfasl-opcode-n-24bit-int*))
                  (t (values 4 *cfasl-opcode-p-32bit-int* *cfasl-opcode-n-32bit-int*)))
              (cfasl-raw-write-byte (if positive pos-opcode neg-opcode) stream)
              (cfasl-output-integer-internal value num-bytes stream))
            ;; Bignum handling
            (let ((bignum-spec (disassemble-integer-to-fixnums integer)))
              (destructuring-bind (sign . fixnums) bignum-spec
                (cfasl-raw-write-byte (if (minusp sign)
                                          *cfasl-opcode-n-bignum-int*
                                          *cfasl-opcode-p-bignum-int*)
                                      stream)
                (cfasl-output (length fixnums) stream)
                (dolist (fixnum fixnums)
                  (cfasl-output fixnum stream)))))))
  integer)

(defun cfasl-output-integer-internal (integer bytes stream)
  ;; bytes = count of bytes
  (if (= 1 bytes)
      (cfasl-raw-write-byte integer stream)
      (let ((low-part (logand integer 255))
            (high-part (ash integer -8)))
        (cfasl-raw-write-byte low-part stream)
        (cfasl-output-integer-internal high-part (1- bytes) stream)))
  integer)

(defun cfasl-input-n-8bit-int (stream)
  (- (cfasl-input-integer 1 stream)))
(defun cfasl-input-p-8bit-int (stream)
  (cfasl-input-integer 1 stream))
(defun cfasl-input-n-16bit-int (stream)
  (- (cfasl-input-integer 2 stream)))
(defun cfasl-input-p-16bit-int (stream)
  (cfasl-input-integer 2 stream))
(defun cfasl-input-n-24bit-int (stream)
  (- (cfasl-input-integer 3 stream)))
(defun cfasl-input-p-24bit-int (stream)
  (cfasl-input-integer 3 stream))
(defun cfasl-input-n-32bit-int (stream)
  (- (cfasl-input-integer 4 stream)))
(defun cfasl-input-p-32bit-int (stream)
  (cfasl-input-integer 4 stream))

(defun assemble-2-fixnums-to-non-negative-integer (fixnum0 fixnum1)
  (logior fixnum0 (ash fixnum1 8)))
(defun assemble-3-fixnums-to-non-negative-integer (fixnum0 fixnum1 fixnum2)
  (logior fixnum0 (ash fixnum1 8) (ash fixnum2 16)))
(defun assemble-4-fixnums-to-non-negative-integer (fixnum0 fixnum1 fixnum2 fixnum3)
  (logior fixnum0 (ash fixnum1 8) (ash fixnum2 16) (ash fixnum3 24)))

(defun cfasl-input-integer (bytes stream)
  ;; bytes seems to be a byte count again
  (cond
    ((not (cfasl-decoding-stream-p stream)) (read-byte-sequence-to-positive-integer bytes stream))
    ;; TODO BUG - this breaks past 8 bytes!
    ((> bytes 4) (let ((high-recursive (cfasl-input-integer (- bytes 4) stream))
                       (low-4 (cfasl-input-integer 4 stream)))
                   (logior (ash high-recursive 32) low-4)))
    (t (case bytes
         (4 (assemble-4-fixnums-to-non-negative-integer (cfasl-raw-read-byte stream)
                                                        (cfasl-raw-read-byte stream)
                                                        (cfasl-raw-read-byte stream)
                                                        (cfasl-raw-read-byte stream)))
         (3 (assemble-3-fixnums-to-non-negative-integer (cfasl-raw-read-byte stream)
                                                        (cfasl-raw-read-byte stream)
                                                        (cfasl-raw-read-byte stream)))
         (2 (assemble-2-fixnums-to-non-negative-integer (cfasl-raw-read-byte stream)
                                                        (cfasl-raw-read-byte stream)))
         (1 (cfasl-raw-read-byte stream))
         (0 0)))))

  ;;(declare-cfasl-opcode *cfasl-opcode-p-float* 8 'cfasl-input-p-float)
  ;;(declare-cfasl-opcode *cfasl-opcode-n-float* 9 'cfasl-input-n-float)


(defmethod cfasl-output-object ((object float) stream)
  ;; TODO - shouldn't be hard to implement
  (missing-larkc 30985))

(defmethod cfasl-output-object ((object symbol) stream)
  (cond
    ((null object) (cfasl-output-nil stream))
    ((cfasl-common-symbol-p object) (missing-larkc 30983))
    ((keywordp object) (missing-larkc 30988))
    (t (cfasl-output-other-symbol object stream))))

(declare-cfasl-opcode *cfasl-opcode-keyword* 10 'cfasl-input-keyword)

(defun cfasl-input-keyword (stream)
  (let ((name (cfasl-input-object stream)))
    (when (char= #\: (char name 0))
      (setf name (subseq name 1)))
    (make-keyword name)))

(declare-cfasl-opcode *cfasl-opcode-other-symbol* 11 'cfasl-input-other-symbol)

(defun cfasl-output-other-symbol (object stream)
  (cfasl-raw-write-byte *cfasl-opcode-other-symbol* stream)
  (unless (cyc-package-symbol-p object)
    (cfasl-output (symbol-package object) stream))
  (cfasl-output-string (symbol-name object) stream)
  object)

(defun cyc-package-symbol-p (object)
  "[Cyc] Return T iff OBJECT is a symbol in the Cyc package"
  (and (symbolp object)
       (not (keywordp object))
       (or (eq *cyc-package* (symbol-package object))
           (eq object (find-symbol (symbol-name object) *cyc-package*)))))

(defun cfasl-input-other-symbol (stream)
  (let ((input (cfasl-input-object stream)))
    (if (stringp input)
        (intern input *cyc-package*)
        (let ((package input)
              (name (cfasl-input-object stream)))
          (if package
              (intern name package)
              (make-symbol name))))))

(declare-cfasl-opcode *cfasl-opcode-nil* 12 'cfasl-input-nil)

(defun cfasl-output-nil (stream)
  (cfasl-raw-write-byte *cfasl-opcode-nil* stream)
  nil)

(defun cfasl-input-nil (stream)
  (declare (ignore stream))
  nil)

(defparameter *cfasl-common-symbols* nil
  "[Cyc] A list of commonly used symbols for which it is cost-effective to output in a terser representation.")

(defun cfasl-set-common-symbols (symbols)
  "[Cyc] Set the currently used list of common symbols for CFASL to be SYMBOLS."
  (setf *cfasl-common-symbols* (apply #'vector symbols)))

(defun cfasl-set-common-symbols-simple (symbols)
  "[Cyc] Set the currently used list of common symbols for CFASL to be SYMBOLS, assuming that we're only doing a few cfasl functions and so will sacrifice time for space and not make a vector"
  (setf *cfasl-common-symbols* symbols))

(defun cfasl-current-common-symbols ()
  "[Cyc] Get the currently active common symbols in a form that can be used in conjunction with CFASL-SET-COMMON-SYMBOLS."
  (let ((syms *cfasl-common-symbols*))
    (if (vectorp syms)
        (coerce syms 'list)
        syms)))

(defun cfasl-common-symbol-p (object)
  (and *cfasl-common-symbols*
       (symbolp object)
       (position object *cfasl-common-symbols* :test #'eq)))

(defun cfasl-decode-common-symbol (integer)
  (let ((syms *cfasl-common-symbols*))
    (if (vectorp syms)
        (aref syms integer)
        (nth integer syms))))

(defun cfasl-input-common-symbol (stream)
  (let* ((encoding (cfasl-input-object stream))
         (symbol (cfasl-decode-common-symbol encoding)))
    (unless symbol
      (cerror "Use NIL." "Common symbol at index ~d was not found in ~s." '*cfasl-common-symbols*))
    symbol))

(defmethod cfasl-output-object ((object cons) stream)
  (if (proper-list-p object)
      (cfasl-output-list object stream)
      (cfasl-output-dotted-list object stream)))

(defglobal *cfasl-list-methods* nil)
(declare-cfasl-opcode *cfasl-opcode-list* 13 'cfasl-input-list)

(defun cfasl-output-list (list stream)
  (let ((length (list-length list)))
    (cond
      ((null length) (cerror "Output NIL instead" "Trying to output a circular list.")
       (cfasl-output-nil stream))
      ((zerop length) (cfasl-output-nil stream))
      ;; TODO - huh?
      ;;((and *cfasl-list-methods* (missing-larkc 31001))
      (t (cfasl-raw-write-byte *cfasl-opcode-list* stream)
         (cfasl-output-integer length stream)
         (dolist (item list)
           (cfasl-output item stream))
         ;; TODO - do these output functions really need to always return the object?
         list))))

(defun cfasl-input-list (stream)
  (let* ((length (cfasl-input-object stream))
         (list (make-list length)))
    (loop for cell on list
       do (rplaca cell (cfasl-input-object stream)))
    list))

(declare-cfasl-opcode *cfasl-opcode-dotted-list* 17 'cfasl-input-dotted-list)

(defun cfasl-output-dotted-list (object stream)
  (let ((length (dotted-length object)))
    (cfasl-raw-write-byte *cfasl-opcode-dotted-list* stream)
    (cfasl-output-integer length stream)
    (let ((cons (cons nil object)))
      (dotimes (i length)
        (pop cons)
        (cfasl-output (first cons) stream))
      ;; TODO - original has a bug that wanted to put out the cons cell itself?  wouldn't that be infinitely looping?
      (cfasl-output (cdr cons) stream))))

  ;; TODO - input and output don't match in the original source for dotted lists.  Assuming all elements + final cdr are written linearly

(defun cfasl-input-dotted-list (stream)
  (let ((length (cfasl-input-object stream)))
    (if (= 1 length)
        (cons (cfasl-input-object stream) (cfasl-input-object stream))
        (let ((list (make-list length)))
          (loop for cell on list
             do (rplaca cell (cfasl-input-object stream))
             do (when (null (cdr cell))
                  (rplacd cell (cfasl-input-object stream))
                  (return)))
          list))))

(defmethod cfasl-output-object ((object vector) stream)
  ;; TODO - should be easy
  (missing-larkc 4931))


  ;; TODO - vector types
  ;;(declare-cfasl-opcode *cfasl-opcode-general-vector* 14)
  ;;(declare-cfasl-opcode *cfasl-opcode-byte-vector* 26)

(declare-cfasl-opcode *cfasl-opcode-string* 15 'cfasl-input-string)

(defmethod cfasl-output-object ((object string) stream)
  (cfasl-output-string object stream))

(defun cfasl-output-string (string stream)
  (cfasl-raw-write-byte *cfasl-opcode-string* stream)
  (map nil (lambda (char)
             (cfasl-raw-write-byte (char-code char) stream))
       string))

(defparameter *cfasl-input-string-resource* nil
  "[Cyc] If non-NIL, a string that is destructively re-used when loading a string of the same length.")

(defun cfasl-input-string (stream)
  (let* ((length (cfasl-input-object stream))
         (string (let ((old *cfasl-input-string-resource*))
                   (if (and (stringp old)
                            (= length (length old)))
                       old
                       (make-string length)))))
    (if (not (cfasl-decoding-stream-p stream))
        (read-sequence string stream)
        (dotimes (i length)
          (setf (char string i) (code-char (cfasl-raw-read-byte stream)))))
    string))

  ;; TODO
  ;;(declare-cfasl-opcode *cfasl-opcode-character* 16 'cfasl-input-character)


(defmethod cfasl-output-object ((object character) stream)
  ;; TODO
  (missing-larkc 30982))

(declare-cfasl-opcode *cfasl-opcode-hashtable* 18 'cfasl-input-hashtable)

(defun cfasl-input-hashtable (stream)
  (let* ((test (cfasl-input-object stream))
         (size (cfasl-input-object stream))
         (hashtable (make-hash-table :size size :test test)))
    (dotimes (i size)
      (let ((key (cfasl-input-object stream))
            (value (cfasl-input-object stream)))
        (setf (gethash key hashtable) value)))
    hashtable))

(defconstant *cfasl-opcode-guid* 43)

;; TODO - the class GUID is not defined yet, it's a native type in Java SubL
;; (defmethod cfasl-output-object ((object guid) stream)
;;   (cfasl-output-guid object stream))


(defun cfasl-output-guid (guid stream)
  (if (not *terse-guid-serialization-enabled?*)
      (cfasl-output-legacy-guid guid stream)
      (let ((byte-vector (disassemble-guid-to-fixnums guid)))
        (cfasl-raw-write-byte *cfasl-opcode-guid* stream)
        (dotimes (i 16)
          (cfasl-raw-write-byte (aref byte-vector i) stream))
        guid)))

  ;; TODO - cfasl-input-guid is missing?


(defconstant *cfasl-opcode-legacy-guid* 25)

(defun cfasl-output-legacy-guid (guid stream)
  (cfasl-raw-write-byte *cfasl-opcode-legacy-guid* stream)
  (cfasl-output-string (guid-to-string guid) stream)
  guid)

(defun cfasl-input-legacy-guid (stream)
  (string-to-guid (cfasl-input-guid-string stream)))

(defparameter *cfasl-input-guid-string-resource* nil)

(defun cfasl-input-guid-string (stream)
  (let ((*cfasl-input-string-resource* *cfasl-input-string-resource*))
    (cfasl-input stream)))

(defun get-new-cfasl-input-guid-string-resource ()
  "[Cyc] Allocates and provides access to a new GUID string resource object."
  (make-string 36))

(defmacro with-new-cfasl-input-guid-string-resource (&body body)
  "[Cyc] Allocates a new GUID string resource and makes it available to the GUID loading infrastructure."
  ;; Guesswork, hopefully correct?
  `(let ((*cfasl-input-guid-string-resource* (get-new-cfasl-input-guid-string-resource)))
     ,@body))

  ;; TODO
  ;;(declare-cfasl-opcode *cfasl-opcode-result-set* 27 'cfasl-input-resul-set)
  ;;(declare-cfasl-opcode *cfasl-opcode-package* 28 'cfasl-input-package)


(defmethod cfasl-output-object ((object package) stream)
  (declare (ignorable object stream))
  ;; TODO
  (missing-larkc 30993))

  ;; TODO
  ;;(declare-cfasl-opcode *cfasl-opcode-wide-cfasl-opcode* 29)

(defconstant *cfasl-min-wide-opcode* *cfasl-max-opcode*
  "[Cyc] All wide opcodes have to be more than one byte, so that all narrow opcodes can be re-encoded as wide opcodes without loss of functionality.")
(defglobal *cfasl-wide-opcode-input-method-table* (make-hash-table)
  "[Cyc] DIspatch table used by the wide CFASL-INPUT methods.")

(defun register-wide-cfasl-opcode-input-function (wide-opcode function)
  (setf (gethash wide-opcode *cfasl-wide-opcode-input-method-table*) function))

  ;; TODO
  ;; (declare-cfasl-opcode *cfasl-opcode-instance* 124)
  ;; (declare-cfasl-opcode *cfasl-opcode-guid-denoted-type* 126)

(deflexical *cfasl-guid-denoted-type-input-method-table* (make-hash-table :test #'equalp)
  "[Cyc] Stores the GUIDs -> input type Mappings.")

(defun register-cfasl-guid-denoted-type-input-function (cfasl-guid-opcode function)
  (setf (gethash cfasl-guid-opcode *cfasl-guid-denoted-type-input-method-table*) function))
