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


;; TODO DESIGN - remove all TEST parameters from the string interfaces, since it will always be character comparisons. An inline EQL might be faster than CHAR= since it might do a fast EQ test without a function call.  Have to profile which is faster, but for now sticking with the default EQL.



(defconstant *point-char* #\Period)
(defconstant *space-char* #\Space)
(defconstant *tab-char* #\Tab)
(defconstant *empty-string* "")
(defconstant *new-line-string* "
")
(defvar *test-char* nil)

(defun empty-string-p (object)
  "[Cyc] Return T iff OBJECT is an empty string."
  ;; TODO - is stringp & length 0 a faster check?
  (string= object *empty-string*))

(deflexical *object-to-string-caching-state* nil)

(defun to-string (value)
  "[Cyc] This is equivalent to princ-to-string; use to-lisp-string if embedded quotes should be retained."
  (princ-to-string value))

(defun str (object)
  (format nil "~a" object))

(defun first-char (string)
  "[Cyc] Return first charcter in string."
  (char string 0))

(defun object-to-string-internal (object)
  (princ-to-string object))

;; TODO - this memoization macroexpansion doesn't seem to calculate a hash?
(defun object-to-string (object)
  "Memoizes object instances' string representation"
  (let* ((caching-state (or *object-to-string-caching-state*
                           (create-global-caching-state-for-name
                            'object-to-string
                            '*object-to-string-caching-state*
                            nil #'eql 1 1000)))
         (results (caching-state-lookup caching-state object :memoized-item-not-found)))
    (when (eq :memoized-item-not-found results)
      (setf results (multiple-value-list (princ-to-string object)))
      (caching-state-put caching-state object results))
    ;; Multiple value return
    (caching-results results)))

(defun replace-substring (string substring new)
  "[Cyc] Performs case-sensitive substitution of NEW for SUBSTRING throughout STRING."
  ;; TODO - mark deprecated, use string-substitute instead.
  (if (substring? substring string)
      (do-string-substitutions-robust string (list (cons substring new)))
      string))

(defun do-string-substitutions-robust (string subst-list)
  "[Cyc] Don't assume subst-list is in order of appearance."
  (declare (string string))
  (if (zerop (length string))
      string
      ;; TODO - this is quite the memory-constrained dangerous assumption.
      (let* ((result (make-string (max 256 (* 4 (length string)))))
             (result-end 0)
             (start 0)
             (index start)
             (something-copied? nil)
             (done nil))
        (until done
          (let ((min (length string))
                (found nil))
            (dolist (sub subst-list)
              (let ((position (search (car sub) string :start2 index)))
                (when (and position (< position min))
                  (setf min position)
                  (setf found sub))))
            (if found
                (progn
                  (setf something-copied? t)
                  ;; Pre-match contents
                  (setf result (replace result string :start1 result-end
                                        :start2 start :end2 min))
                  (incf result-end (- min start))
                  ;; Match
                  (setf result (replace result (cdr found) :start1 result-end))
                  (incf result-end (length (cdr found)))
                  ;; Jump forward
                  (setf start (+ min (length (car found))))
                  (setf index start))
                ;; TODO DESIGN - this is slow and dumb.  If there's no match found, we should be done with the search.  We don't have to iterate forward.
                ;; Not found
                (incf index))
            (setf done (>= index (length string)))))
        (if something-copied?
            (progn
              ;; Add final unmatched tail
              (replace result string :start1 result-end :start2 start :end2 index)
              (incf result-end (- index start))
              (subseq result 0 result-end))
            ;; No substitutions found
            string))))

(symbol-mapping substring subseq)

(defun ends-with (w ending)
  (and (greater-or-same-length-p w ending)
       (substring-match? w ending (- (length w) (length ending)))))

(defun starts-with (w starting)
  (and (greater-or-same-length-p w starting)
       (substring-match? w starting 0)))

(defun* char-position (char string &optional (n 0))
    (:inline t)
  "[Cyc] Return the number of the position of the first occurrence of CHAR in STRING, starting from position N in STRING."
  (declare (string string)
           (character char)
           (fixnum n))
  (position char string :start n))

(defun string-upto (string &optional (char #\Space))
  (declare (string string)
           (character char))
  (alexandria:if-let ((pos (char-position char string)))
    (subseq string 0 pos)
    string))

(defun strcat (string-list)
  "[Cyc] Like cconcatenate, but takes a list of strings as its argument."
  ;; TODO DESIGN - shouldn't this return "" when nil?
  (when string-list
    (apply #'concatenate 'string string-list)))

(defun stringify-terms (terms &optional (separator " ") (last-separator separator))
  (stringify-items terms #'fort-print-name separator last-separator))

(defun str-by-type (object)
  (cond
    ((stringp object) object)
    ((constant-p object) (constant-name object))
    (t (str object))))

(defparameter *char-set* nil
  "[Cyc] Dynamic variable used only by CHAR-SET-POSITION.")

(defun* char-type-position (char-type string &optional (start 0) (end (length string)))
    (:inline t)
  "[Cyc] Return the position of the first charcter satisfying the CHAR-TYPE in STRING. THe positions to start looking in STRING are delimited by START and END."
  (position-if char-type string :start start :end end))

(defun not-digit-char-p (thing)
  (not (digit-char-p thing)))

(defun set-nth-char (n string new-char &optional (safe? t))
  "[Cyc] Set the Nth character of STRING to NEW-CHAR.
SAFE?: should we make sure args are legit?"
  (declare (string string)
           ((and fixnum (integer 0)) n)
           (character new-char))
  (when (or (not safe?)
            (length> string n))
    (set-char string n new-char))
  string)

(defun substring? (little big &optional test (start-index 0) end-index) 
  "[Cyc] Is LITTLE a substring of BIG starting at or after START-INDEX, and ending before END-INDEX (if non-NIL)?"
  (declare (ignore test))
  (when (and (stringp little)
             (stringp big))
    (if (length= little 1)
        (find (first-char little) big :start start-index :end end-index)
        (search little big :start2 start-index :end2 end-index))))

(defun substring-match? (big little start)
  "[Cyc] Returns true if, after moving forward START characters from the beginning of the string BIG, the next few characters match (STRING=) the string SMALL."
  (let ((end (+ start (length little))))
    (and (length>= big end)
         (substring? little big nil start end))))

;; DESIGN - base64 tools elided in preference to external libraries

(defun upper-case-alphanumeric-p (object)
  (and (characterp object)
       (or (upper-case-p object)
           (digit-char-p object))))

;; TODO - what's with the duplicate here, and it getting removed in the next var?
(deflexical *raw-whitespace-chars* '(#\Space #\Tab #\Return #\Newline #\Newline)
  "All symbolic names that LISP provides for different types of whitespace.")

(deflexical *whitespace-chars* (delete-duplicates *raw-whitespace-chars* :test #'char-equal)
  "[Cyc] The actual set of white space characters in this LISP implementation.")

(defun whitespace-chars ()
  (copy-list *whitespace-chars*))

(deflexical *grammatical-punctuation-chars* '(#\, #\? #\! #\& #\\ #\/ #\" #\; #\: #\( #\))
  "[Cyc] The list of chars that are syntactic punctuation only (not used inside words). Note -- does not include periods because they can occur inside number expressions.")

(defun whitespacep (char)
  (and (characterp char)
       (find char *whitespace-chars*)))

(defun non-whitespace-p (char)
  (not (whitespacep char)))

(defun break-words (string &optional (non-break-char-test #'valid-constant-name-char-p) leave-embedded-strings?)
  "[Cyc] Takes a string (such as an English phrase or sentence). Returns a list of words, based on the break points implied by the function NON-BREAK-CHAR-TEST (characters that fail the test are break points), and whether or not embedded strings should be left alone."
  (let ((ans nil))
    (loop with len = (length string)
       with p1 = 0
       with p2 = 1
       with inside-dq? = nil
       until (> p2 len)
       do (let* ((px (1- p2))
                 (c1 (char string p1))
                 (c2 (if (< p2 len) (char string p2) nil))
                 (cx (char string px))
                 (c1-is-break? (and (characterp c1)
                                    (or (not leave-embedded-strings?)
                                        (char/= c1 #\"))
                                    (not (funcall non-break-char-test c1))))
                 (c2-is-break? (and (characterp c2)
                                    (or (not leave-embedded-strings?)
                                        (char/= c2 #\"))
                                    (not (funcall non-break-char-test c2)))))
            (when (and (char= c1 #\") leave-embedded-strings?)
              (if inside-dq?
                  (when (and (char= cx #\")
                             (/= p1 px))
                    (setf inside-dq? nil))
                  (setf inside-dq? t)))
            (cond
              ((and (= p2 len) (not c1-is-break?))
               (push (subseq string p1 p2) ans)
               (incf p2))
              (c1-is-break? (incf p1)
                            (setf p2 (1+ p1)))
              ((not (or c1-is-break? c2-is-break?)) (incf p2))
              ((and (not c1-is-break?)
                    (not inside-dq?)
                    c2-is-break?)
               (push (subseq string p1 p2) ans)
               (setf p1 (1+ p2))
               (setf p2 (1+ p1)))
              ((and (not c1-is-break?)
                    inside-dq?
                    c2-is-break?)
               (incf p2)))))
    (nreverse ans)))

(defun char-list-to-string (chars)
  "Coerce a list of characters to a string."
  (coerce chars 'string))

(defparameter *target-characters* nil)

(defun make-valid-constant-name (in-string &optional (upcase-initial-letter? t))
  "[Cyc] Make a fake constant name (a string) by capitalizing words after whitespace (and symbols) then eliminating invalid constant characters, including spaces.
Example: (make-valid-constant-name \"this is sa fake constant! 200 #$\"
         returns \"ThisIsAFakeConstant200\".
Note: We *could* use STRING-PROPER and (STRING-SUBST \"\" \" \" ...), but we're mad at those functions and don't want to call them."
  (let ((cur-string-list nil)
        (should-we-upcase? upcase-initial-letter?))
    (map nil (lambda (this-character)
               (declare (character this-character))
               (if (valid-constant-name-char-p this-character)
                   (if (alphanumericp this-character)
                       (if should-we-upcase?
                           (progn
                             (push (string (char-upcase this-character)) cur-string-list)
                             (setf should-we-upcase? nil))
                           (push (string this-character) cur-string-list))
                       (progn
                         (push (string this-character) cur-string-list)
                         (setf should-we-upcase? t)))
                   (setf should-we-upcase? t)))
         in-string)
    (strcat (nreverse cur-string-list))))

(defun string-tokenize (in-string &optional (break-list *whitespace-chars*)
                                    embed-list include-stops?
                                    ignore-empty-strings? quote-chars
                                    break-list-to-return)
  (string-tokenize-int in-string break-list break-list-to-return
                       embed-list include-stops? ignore-empty-strings? quote-chars))

(defun string-tokenize-int (in-string &optional (break-list *whitespace-chars*)
                                        break-list-to-return embed-list
                                        include-stops? ignore-empty-strings? quote-chars)
  "[Cyc] Breaks IN-STRING on any string sequence in BREAK-LIST. Returns a list of strings. EMBED-LIST should be a list of pairs of strings specifying start and end characters between which we ignore breaks. If INCLUDE-STOPS? is T then the break elements will be included in the resulting list. If IGNORE-EMPTY-STRING is T then there will be no empty strings in the result list. QUOTE-CHARS should be a list of characters (characters, not strings) which mean that the next character should be treated literally, not as a break or embed-char (a common example is backslash in UN*X. The priority of the breaks is specified in BREAK-LIST (i.e. initial entries will be broken first)."
  (when (eq :default break-list)
    (setf break-list *whitespace-chars*))
  (let ((in-length (length in-string))
        (out-string-list nil)
        (cur-string *empty-string*)
        (cur-char-list nil)
        (break-satisfied? nil)
        (this-quoted? nil)
        (complete-break-list (append break-list-to-return break-list)))
    ;; Can't use dovector to iterate the string.
    ;; setf pos-now changes the iteration location, so that needs to be the basis.
    (loop for pos-now from 0 below in-length
       for this-character = (char in-string pos-now)
       do (cond
            (this-quoted? (progn
                            (setf this-quoted? nil)
                            (push this-character cur-char-list)))
            ((member this-character quote-chars) (progn
                                                   (setf this-quoted? t)
                                                   (push this-character cur-char-list)))
            (t (dolist (this-embed embed-list)
                 (let ((this-embed-start (first-char (first this-embed)))
                       (this-embed-end (last-char (second this-embed))))
                   (when (char= this-character this-embed-start)
                     (do ((pos-find (1+ pos-now) (1+ pos-find))
                          (quoted? nil)
                          (done? nil))
                         ;; Termination test only, no return body
                         ((or done? (eq in-length pos-now)))
                       ;; Iteration body
                       (push this-character cur-char-list)
                       (setf pos-now pos-find)
                       (setf this-character (char in-string pos-now))
                       (cond
                         (quoted? (setf quoted? nil))
                         ((char= this-character this-embed-end) (setf done? t))
                         ((member this-character quote-chars) (setf quoted? t)))))))
               (let ((found-a-break? nil))
                 (csome (this-break complete-break-list found-a-break?)
                   (let* ((this-break-length (string-tokenize-break-length this-break))
                          (post-break-pos (+ pos-now this-break-length)))
                     (when (and (plusp this-break-length)
                                (length>= in-string post-break-pos)
                                (string-tokenize-break-match? in-string this-break pos-now))
                       (setf found-a-break? t)
                       (setf cur-string (char-list-to-string (nreverse cur-char-list)))
                       (unless (and ignore-empty-strings?
                                    (empty-string-p cur-string))
                         (push cur-string out-string-list))
                       (setf cur-char-list nil)
                       (when (or include-stops?
                                 (member this-break break-list-to-return))
                         (push (format nil "~a" this-break) out-string-list))
                       (setf pos-now (1- post-break-pos))
                       (setf this-character (char in-string pos-now))
                       (setf break-satisfied? t)))))
               (if break-satisfied?
                   (setf break-satisfied? nil)
                   (push this-character cur-char-list)))))
    (setf cur-string (char-list-to-string (nreverse cur-char-list)))
    (unless (and ignore-empty-strings?
                 (empty-string-p cur-string))
      (push cur-string out-string-list))
    (unless (or out-string-list
                ignore-empty-strings?)
      (setf out-string-list (list *empty-string*)))
    (nreverse out-string-list)))

(defun string-tokenize-break-length (break)
  (if (characterp break)
      1
      (length break)))

(defun string-tokenize-break-match? (in-string break pos)
  "[Cyc] Does IN-STRING match BREAK at position POS?"
  (if (characterp break)
      (char= (char in-string pos) break)
      (substring-match? in-string break pos)))

(defparameter *trigraph-metric* 0.8
    "[Cyc] Metric used for STRING-TRIGRAPH-MATCH-P, which is defined below.")

(defparameter *trigraph-tables* nil
    "[Cyc] A list of one or more utility hash tables, used by STRING-TRIGRAPH-MATCH-P, which is defined below.")

;; These are strings, not cycl constants, in the original java
(defvar *cyclify-string-expand-subl-fn-strings* (list "#$ExpandSubLFn" "ExpandSubLFn"))
(defvar *cyclify-string-subl-quote-fn-strings* (list "#$SubLQuoteFn" "SubLQuoteFn"))
(defvar *cyclify-string-quote-chars*  '(#\\))

(defstruct cyclify-status
  out-string-list
  references-added
  inside-quote?
  inside-el-var-name?
  already-cyclified?
  escape?
  inside-subl-quote-fn?
  inside-expand-subl-fn?
  inside-expand-subl-fn-arg1?
  inside-expand-subl-fn-arg2?
  immediately-following-paren?
  paren-count)

(defparameter *string-read-buffer-size* 1024)
(defparameter *string-read-buffer* (make-vector *string-read-buffer-size* #\Space))

