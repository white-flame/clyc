#|
  Copyright (c) 2019 White Flame

  This file is part of Clyc
$
  Clyc is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
$
  Clyc is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.
$
  You should have received a copy of the GNU Affero General Public License
  along with Clyc.  If not, see <https://www.gnu.org/licenses/>.
$
This file derives from work covered by the following copyright
and permission notice:

  Copyright (c) 1995-2009 Cycorp Inc.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at
 $
  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
|#





(in-package :clyc)


(defconstant *seconds-in-a-leap-year* 31622400 "[Cyc] True")
(defconstant *seconds-in-a-non-leap-year* 31536000 "[Cyc] Also True")
(defconstant *seconds-in-a-week* 604800 "[Cyc] Right")
(defconstant *seconds-in-a-day* 86400 "[Cyc] Yep.")
(defconstant *seconds-in-an-hour* 3600 "[Cyc] uh-huh.")
(defconstant *seconds-in-a-minute* 60 "[Cyc] the number of seconds in a minute")
(defconstant *minutes-in-an-hour* 60 "[Cyc] the number of minues in an hour")
(defconstant *hours-in-a-day* 24 "[Cyc] the number of hours in a day")
(defconstant *months-in-a-year* 12 "[Cyc] the number of months in a year")

(defun universal-time-seconds-from-now (seconds &optional (reference-time (get-universal-time)))
  "[Cyc] the universal time SECONDS from REFERENCE-TIME"
  (+ reference-time (if (integerp seconds)
                        seconds
                        (truncate seconds))))

(defun time-from-now (seconds)
  "[Cyc] Legacy function name"
  (universal-time-seconds-from-now seconds))

(defun timestring (&optional (universal-time (get-universal-time)))
  "[Cyc] TIMESTRING returns a string in the format mm/dd/yyyy hh:mm:ss from the universal time given. If none is given, the current time is used."
  (timestring-int universal-time))

(defun timestring-int (universal-time)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time universal-time)
    (encode-timestring second minute hour date month year)))

(defun encode-timestring (second minute hour date month year)
  (encode-datetime-string-from-template nil second minute hour date month year "mm/dd/yyyy hh:mm:ss"))

(defun universal-timestring (&optional (universal-time (get-universal-time)))
  "[Cyc] UNIVERSAL-TIMESTRING returns a string in the format yyyymmddhhmmss from the universal time given. If none is given, the current time is used."
  (multiple-value-bind (second minute hour date month year) (decode-universal-time universal-time)
    (encode-universal-timestring second minute hour date month year)))

(defun encode-universal-timestring (second minute hour date month year)
  (format nil "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year month date hour minute second))

(defun internal-real-time-p (object)
  "[Cyc] Return T iff OBJECT is an internal real time."
  (typep object '(integer 0)))

(defun elapsed-internal-real-time (reference-time &optional (comparison-time (get-internal-real-time)))
  "[Cyc] Return the number of elapsed internal real time units from COMPARISON-TIME to REFERENCE-TIME."
  (- comparison-time reference-time))

(defun elapsed-internal-real-time-to-elapsed-seconds (elapsed)
  (/ elapsed internal-time-units-per-second))

(defun encode-datetime-string-from-template (millisecond second minute hour day month year template)
  "[Cyc] Returns a string in the format specified by TEMPLATE representing the datetime having the stated values for MILLISECOND, SECOND, MINUTE, HOUR, DAY, MONTH, YEAR."
  ;; TODO - this function is missing
  ;;(check-type template #'datetime-string-template-p)
  (let* ((subtemplates (break-words template #'non-whitespace-p))
         (template1 (first subtemplates))
         (template2 (second subtemplates))
         (length (length subtemplates)))
    (cond
      ((and (= length 2)
            (date-template-p template1)
            (time-template-p template2))
       (format nil "~a ~a" (encode-date-from-template day month year template1)
               (encode-time-from-template millisecond second minute hour template2)))
      ((and (= length 2)
            (time-template-p template1)
            (date-template-p template2))
       (format nil "~a ~a" (encode-time-from-template millisecond second minute hour template1)
               (encode-date-from-template day month year template2)))
      ((and (= length 1)
            (date-template-p template1))
       (encode-date-from-template day month year template1))
      ((and (= length 1)
            (time-template-p template1))
       (encode-time-from-template millisecond second minute hour template2))
      (t (error "Template ~s is not a valid datetime-string template." template)))))

(defun valid-date-template-char (char)
  (member char '(#\y #\Y #\m #\M #\d #\D #\/ #\- #\_)))

(defun valid-date-separator (char)
  (member char '(#\/ #\- #\_)))

(defun valid-year-token (char)
  (member char '(#\y #\Y)))

(defun valid-month-token (char)
  (member char '(#\m #\M)))

(defun valid-day-token (char)
  (member char '(#\d #\D)))

(defun date-template-p (template)
  (every #'valid-date-template-char template))

(defun time-template-p (template)
  (member template '("hh:mm:ss" "hh:mm:ss.mmm" "hh:mm" "hh:mm:ss.m" "hh:mm:ss.mm") :test #'string=))

(defun n-digit-template-element-p (template n token-checker separator-checker)
  (when (>= (length template) n)
    (dotimes (index n)
      (unless (funcall token-checker (char template index))
        (return nil)))
    (unless (and (> (length template) n)
                 (not (funcall separator-checker (char template n))))
      t)))

(defun encode-date-from-template (day month year template)
  (cond
    ((n-digit-template-element-p template 4 #'valid-year-token #'valid-date-separator)
     (encode-next-date-element day month year template 4 year))
    ((n-digit-template-element-p template 2 #'valid-year-token #'valid-date-separator)
     (encode-next-date-element day month year template 2 (mod year 100)))
    ((n-digit-template-element-p template 2 #'valid-month-token #'valid-date-separator)
     (encode-next-date-element day month year template 2 month))
    ((n-digit-template-element-p template 2 #'valid-day-token #'valid-date-separator)
     (encode-next-date-element day month year template 2 day))
    (t (error "Date template or template portion ~s didn't match any expected pattern" template))))

(defun encode-next-date-element (day month year template elem-length item)
  (let ((format-string (format nil "~~~a,'0d~~a" elem-length)))
    (format nil format-string item
            (if (> (1+ elem-length) (length template))
                "" (format nil "~a~a"
                           (char template elem-length)
                           (encode-date-from-template day month year (subseq template (1+ elem-length))))))))

(defun encode-time-from-template (millisecond second minute hour template)
  (cond
    ((not hour) "")
    ((equalp template "hh:mm:ss") (format nil "~2,'0d:~2,'0d:~s,'0d" hour minute second))
    ((equalp template "hh:mm") (format nil "~2,'0d:~2,'0d" hour minute))
    ((equalp template "hh:mm:ss.mmm") (format nil "~2,'0d:~2,'0d:~2,'0d.~3,'0d" hour minute second millisecond))
    ((equalp template "hh:mm:ss.mm") (format nil "~2,'0d:~2,'0d:~2,'0d.~2,'0d" hour minute second millisecond))
    ((equalp template "hh:mm:ss.m") (format nil "~2,'0d:~2,'0d:~2,'0d.~1,'0d" hour minute second millisecond))
    (t (error "Time template or template portion ~s didn't match any expected pattern" template))))

(defun decode-elapsed-seconds (elapsed-seconds)
  "[Cyc] Decode ELAPSED-SECONDS into 4 return values: seconds minutes hours elapsed-days"
  (declare ((integer 0) elapsed-seconds))
  (multiple-value-bind (truncated-exact-seconds partial-seconds) (truncate elapsed-seconds)
    (destructuring-bind (whole-seconds &optional (minutes 0) (hours 0) (elapsed-days 0))
        (decode-integer-multiples truncated-exact-seconds (list *seconds-in-a-minute*
                                                                *minutes-in-an-hour*
                                                                *hours-in-a-day*))
      (values (+ whole-seconds partial-seconds) minutes hours elapsed-days))))

(defun universal-date-p (object)
  "[Cyc] Return T iff OBJECT is a valid universal date."
  (when (integerp object)
    (when (minusp object)
      (universal-date-p (- object)))
    (let* ((temp object)
           (day (rem temp 100)))
      (when (<= day 31)
        (setf temp (floor temp 100))
        (let ((month (rem temp 100)))
          (and (<= 1 month)
               (<= month 12)))))))

(defun get-universal-date (&optional (universal-time (get-universal-time)) time-zone)
  "[Cyc] Return the current date as an integer, i.e. 19660214."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time time-zone)
    (declare (ignore second minute hour))
    (encode-universal-date day month year)))

(defun encode-universal-date (day month year)
  "[Cyc] Encode DAY MONTH YEAR in a universal date integer of the form yyyymmdd."
  (if (minusp year)
      (- (encode-universal-date day month (- year)))
      (+ (* year 10000) (* month 100) day)))

(defconstant *julian-date-reference* (cons 20010801 2452122.5d0)
  "[Cyc] A known pair to compute offset from.
The Julian date for the start of Aug 1, 2001 is 2452122.5.")

(defglobal *julian-offsets* nil
  "[Cyc] ALISTP of number of days to add to get Julian date, with different precisions.")

(defun universal-second-p (object)
  "[Cyc] Return T iff OBJECT is a valid universal second."
  (when (and (integerp object)
             (not (minusp object))
             (<= object 235959))
    (let ((temp object))
      (when (< (rem temp 100) *seconds-in-a-minute*)
        (setf temp (floor temp 100))
        (when (< (rem temp 100) *minutes-in-an-hour*)
          (setf temp (floor temp 100))
          (< temp *hours-in-a-day*))))))

(defun get-universal-second (&optional (universal-time (get-universal-time)))
  "[Cyc] Return the current second within the day as an integer in HHMMSS form, i.e. 235959."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal-time)
    (declare (ignore day month year))
    (encode-universal-second second minute hour)))

(defun encode-universal-second (second minute hour)
  "[Cyc] Encode SECOND MINUTE HOUR in a universal second integer of the form HHMMSS."
  (must (and (<= 0 second)
             (<= second 59))
    "second ~s not in the range 0-59" second)
  (must (and (<= 0 minute)
             (<= minute 59))
    "minute ~s not in the range 0-59" minute)
  (must (and (<= 0 hour)
             (<= hour 59))
    "hour ~s not in the range 0-59" hour)
  (+ (* hour 10000)
     (* minute 100)
     second))

(defun get-utc-time-with-milliseconds ()
  "[Cyc] Returns the current UTC time with millisecond accuracy, taking into account the platform-specfic implementations of get-internal-real-time."
  (let* ((internal-real-time (get-internal-real-time))
         (universal-time (get-universal-time))
         (divisor (/ internal-time-units-per-second 1000))
         (internal-real-time-in-milliseconds (truncate (/ internal-real-time divisor)))
         (milliseconds (rem internal-real-time-in-milliseconds 1000))
         (time-in-milliseconds (+ (* universal-time 1000) milliseconds)))
    time-in-milliseconds))

(deflexical *month-duration-table* '(31 28 31 30 31 30 31 31 30 31 30 31)
  "[Cyc] The usual number of days for each month.")

(deflexical *number-wkday-table* '((0 . "Mon")
                                   (1 . "Tue")
                                   (2 . "Wed")
                                   (3 . "Thu")
                                   (4 . "Fri")
                                   (5 . "Sat")
                                   (6 . "Sun")))

(deflexical *number-month-table* '(( 1 . "Jan")
                                   ( 2 . "Feb")
                                   ( 3 . "Mar")
                                   ( 4 . "Apr")
                                   ( 5 . "May")
                                   ( 6 . "Jun")
                                   ( 7 . "Jul")
                                   ( 8 . "Aug")
                                   ( 9 . "Sep")
                                   (10 . "Oct")
                                   (11 . "Nov")
                                   (12 . "Dec")))

(defun elapsed-time-abbreviation-string (elapsed-seconds)
  (multiple-value-bind (secs mins hours days) (decode-elapsed-seconds elapsed-seconds)
    (setf secs (truncate secs))
    (cond
      ((> days 0) (format nil "~d day~:p ~d:~2,'0d:~2,'0d" days hours mins secs))
      ((> hours 0) (format nil "~d:~2,'0d:~2,'0d" hours mins secs))
      (t (format nil "~d:~2,'0d" mins secs)))))

(defconstant *seconds-in-a-century* 3155760000
    "[Cyc] HACK")
(defconstant *seconds-in-an-odd-millenium* 31556908800)
(defconstant *seconds-in-an-even-millenium* 31556995200)


