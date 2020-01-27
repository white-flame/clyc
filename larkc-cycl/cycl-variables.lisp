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


(defconstant *el-variable-prefix-char* #\?
  "[Cyc] The character used as the first character of an EL variable's name.")

(defconstant *el-variable-prefix-string* (string *el-variable-prefix-char*)
  "[Cyc] The string used as the first character of an EL variable's name.")

(defconstant *el-variable-hyphen-char* #\-
  "[Cyc] The character used as the hyphen character in an EL variable's name.")

(defconstant *el-variable-invalid-hyphen-sequence* "--"
  "[Cyc] The string which is deemed as an invalid hyphen sequence in an EL variable's name.")

(defconstant *valid-el-var-regular-expression* "([?]|[?][?]) [A-Z] ([A-Z]|[0-9])* ([-] ([A-Z]|[0-9])+)*"
  "[Cyc] The current filter for valid EL variable name in regular expression format.")

(defun-inline el-variable-prefix-char ()
  "[Cyc] Returns the character used as teh first character of an EL variable's name."
  *el-variable-prefix-char*)

(defun-inline el-variable-prefix-char? (object)
  "[Cyc] Returns T if OBJECT is the character used as the first character of an EL variable's name."
  (char= object (el-variable-prefix-char)))

(defun-inline has-el-variable-prefix? (string)
  "[Cyc] Returns T if STRING begins with ?. STRING assumed to be stringp."
  ;; TODO - will crash if string is ""
  (el-variable-prefix-char? (char string 0)))

(defun-inline has-dont-care-var-prefix? (string)
  "[Cyc] Returns T if STRING begins with ??. STRING assumed to be stringp."
  (and (el-variable-prefix-char? (char string 0))
       (el-variable-prefix-char? (char string 1))))

(defun el-var? (object)
  "[Cyc] Return T iff OBJECT is a symbol which can be interpreted as an EL variable."
  (and (symbolp object)
       object
       (not (keywordp object))
       (el-var-name? (el-var-name object))))

(defun-inline el-var-name (el-var)
  "[Cyc] The name of EL-VAR. Includes the prefix character."
  (symbol-name el-var))

(defun el-var-name? (object)
  (and (stringp object)
       (length> object 1)
       (has-el-variable-prefix? object)))

(defun-inline make-el-var (object)
  (intern-el-var object))

(defun make-el-var-name (object)
  (if (el-var-name? object)
      (string-upcase object)
      (missing-larkc 31891)))

;; TODO - add a ? to the end of this name
(defun invalid-variable-name-char (object)
  "[Cyc] Returns T if OBJECT is a character which could not possibly be a valid character in an EL variable name."
  (and (not (upper-case-alphanumeric-p object))
       (missing-larkc 31890)))

(defun valid-el-var? (object)
  "[Cyc] Returns T if OBJECT is a symbol whose name satisfies the regular expression *VALID-EL-VAR-REGULAR-EXPRESSION*."
  (and (symbolp object)
       (not (keywordp object))
       (valid-el-var-name? (el-var-name object))))

(defun valid-el-var-name? (object)
  "[Cyc] Returns T if OBJECT satisfies the regular expression *VALID-EL-VAR-REGULAR-EXPRESSION*."
  ;; TODO - I bet there was a macroexpansion from the regex to this?
  (unless (or (not (stringp object))
              (empty-string-p object)
              (not (has-el-variable-prefix? object)))
    (let* ((length (length object))
           (last (1- length))
           (subseq-check-start 1))
      (when (has-dont-care-var-prefix? object)
        (setf subseq-check-start 2))
      (when (<= last subseq-check-start)
        (setf last (1+ subseq-check-start)))
      (when (valid-el-variable-name-subsequence? (subseq object subseq-check-start last))
        (upper-case-alphanumeric-p (char object (1- length)))))))

(defun valid-el-variable-name-subsequence? (object)
  (and (length> object 0)
       (upper-case-p (char object 0))
       (not (find-if #'invalid-variable-name-char (subseq object 1)))
       (not (search (invalid-hyphen-sequence) object))))

(defun-inline invalid-hyphen-sequence ()
  *el-variable-invalid-hyphen-sequence*)

(defun-inline intern-el-var (object)
  (intern (make-el-var-name object) *cyc-package*))

(defconstant *keyword-variable-prefix-char* #\:
  "[Cyc] The character used as the first character of a keyword variable's name.")

(defun-inline permissible-keyword-var? (thing)
  "[Cyc] Return T iff THING is deemed a keyword variable, and we are allowing such things."
  (and *permit-keyword-variables?*
       (keywordp thing)))

(defun-inline keyword-var? (thing)
  "[Cyc] Return T iff THING is deemed a keyword variable."
  (keywordp thing))

(defun variable-name (var)
  "[Cyc] Returns the name of the CycL variable VAR. Does not strip prefix characters."
  (cond
    ((variable-p var) (variable-name (default-el-var-for-hl-var var)))
    ((el-var? var) (el-var-name var))
    ;; TODO - this should be easy?  I guess concat a colon, or just format ~s
    ((keyword-var? var) (missing-larkc 31895))
    ;; TODO - probably just format ~s
    (t (missing-larkc 7491))))

(defun cyc-var? (thing)
  "[Cyc] Returns T iff THING is any possible kind of variable in the CycL language or any of its extensions or relata, being as permissive as possible."
  (or (el-var? thing)
      (kb-var? thing)
      (tl-var? thing)
      (permissible-keyword-var? thing)
      (generic-arg-var? thing)))

(defun generic-arg-var? (thing)
  "[Cyc] Return T iff THING is deemed a generic-arg variable."
  (declare (ignore thing))
  (and *permit-generic-arg-variables*
       (missing-larkc 3473)))

(defun variable-predicate-fn (var)
  "[Cyc] Return the SubL boolean function that admits VAR as a variable."
  (cond
    ((hl-var? var) #'hl-var?)
    ((el-var? var) #'el-var?)
    ((tl-var? var) #'tl-var?)
    ((permissible-keyword-var? var) #'keyword-var?)
    ((generic-arg-var? var) #'generic-arg-var?)
    (t #'false)))

(defun-inline kb-var? (symbol)
  (kb-variable? symbol))

(defun-inline kb-variable? (symbol)
  (variable-p symbol))

(defun-inline hl-var? (thing)
  (variable-p thing))
