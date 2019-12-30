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


;; Clyc hoists this file up earlier in the load order, so that the #$ is available everywhere.
;; This also splits up the behavior between read-time capturing of the name, and macro-expansion time resolving of the name, which defers this code from needing its dependencies compiled in.

;; Initial external dependencies:
;;  simple-reader-error
;;  valid-constant-name-char-p
;;  *read-suppress*
;;  reader-make-constant-shell
;;  *read-require-constant-exists*
;;  constant-complete-exact-name
;;  find-invalid-constant

(defconstant *constant-reader-macro-char* #\$
  "[Cyc] The character that signals the reader that what follows is to be treated as a CycL constant name.")

(defun constant-reader-macro-char ()
  "[Cyc] Returns the character that signals the reader that what follows is to be treated as a CycL constant name."
  *constant-reader-macro-char*)

(defconstant *constant-reader-prefix* (format nil "#~c" *constant-reader-macro-char*)
  "[Cyc] The string that prefixes all CycL constant names")

(declaim (inline stream-forbids-constant-creation))
(defun stream-forbids-constant-creation (stream)
  "[Cyc] Return T iff STREAM forbids the creation of constant shells for unknown constants."
  (declare (ignore stream))
  ;; TODO DESIGN - surely some test should happen on the stream. Differentiate fundamental .lisp code from user streams.
  *read-require-constant-exists*)

;; TODO DESIGN - Compile-time side effects, like filling in the trie of constant names, do not get saved through to load time.  The biggest issue is that we can't easily use #$ in the fundamental source code to define constants at compile-time with this setup, although it would work for interactive use after the system is up and running.  For now, constants will be resolved at runtime.  Later, we could try performing existence checks at load-time, but we have to ensure that the world has been loaded first.
(defun sharpsign-dollar-rmf (stream ch arg)
  (when arg
    (simple-reader-error "The ~s reader macro does not take an argument." ch))
  (let ((buffer (make-array '(64) :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for next = (peek-char nil stream nil nil)
       while (and next (valid-constant-name-char-p next))
       do (vector-push-extend (read-char stream) buffer))
    (if *read-suppress*
        (values nil t)
        (let* (;; Copy from adjustable vector to a plain one
               (name (subseq buffer 0)))
          `(values (or (load-time-value
                        (reader-make-constant-shell ,name
                                                    ,(not (stream-forbids-constant-creation stream))))
                       (error "~s is not an existing constant" ,name))
                   t)))))

(defun find-constant-by-name (name)
  (let ((constant (let ((*require-valid-constants* nil))
                    (constant-complete-exact name))))
    (or constant
        (find-invalid-constant name))))

;; Make a contained readtable, instead of mutating the global one
'(named-readtables:defreadtable clyc
  (:merge :standard)
  (:dispatch-macro-char #\# *constant-reader-macro-char* #'sharpsign-dollar-rmf))

  ;; TODO HACK - remove this later, use the named-readtable instead to scope subl-ish code

(set-dispatch-macro-character #\# #\$ #'sharpsign-dollar-rmf)
  

