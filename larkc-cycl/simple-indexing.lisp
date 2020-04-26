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


(defun matches-gaf-arg-index (assertion term &optional argnum pred mt)
  (when (gaf-assertion? assertion)
    (matches-gaf-arg-index-internal assertion term argnum pred mt)))

(defun simple-key-gaf-arg-index (assertion accumulator term &optional arg pred)
  (when (and (gaf-assertion? assertion)
             (matches-gaf-arg-index-internal assertion term arg pred nil))
    (simple-key-gaf-arg-index-internal assertion accumulator term arg pred)))

(defun matches-gaf-arg-index-internal (assertion term arg pred mt)
  (cond
    ((not arg) (find term (gaf-args assertion)))
    ((not (equal (gaf-arg assertion arg) term)) nil)
    ((not pred) t)
    ((not (eq (gaf-predicate assertion) pred)) nil)
    ((not mt) t)
    (t (hlmt-equal (assertion-mt assertion) mt))))

(defun simple-key-gaf-arg-index-internal (assertion accumulator term arg pred)
  (if arg
      (if pred
          (pushnew (assertion-mt assertion) accumulator :test #'hlmt-equal)
          (pushnew (gaf-predicate assertion) accumulator))
      (let ((formula (gaf-formula assertion)))
        (loop for last-position = nil then position
           for position = (position term formula :start 1)
           then (position term formula :start (1+ last-position))
           while position
           do (pushnew position accumulator))))
  accumulator)

(defun matches-nart-arg-index (assertion term &optional argnum func)
  (when (gaf-assertion? assertion)
    (matches-nart-arg-index-internal assertion term argnum func)))

(defun simple-key-nart-arg-index (assertion accumulator term &optional arg func)
  (declare (ignore accumulator))
  (when (and (gaf-assertion? assertion)
             (matches-nart-arg-index-internal assertion term arg func))
    (missing-larkc 30240)))

(defun matches-nart-arg-index-internal (assertion term arg func)
  (when (eq (gaf-predicate assertion) #$termOfUnit)
    (let ((nat (gaf-arg2 assertion)))
      (cond
        (func (missing-larkc 29810))
        (arg (equal (nat-arg nat arg) term))
        (t (find term (nat-args nat)))))))

(defun matches-predicate-extent-index (assertion term &optional mt)
  (when (gaf-assertion? assertion)
    (matches-predicate-extent-index-internal assertion term mt)))

(defun matches-predicate-extent-index-internal (assertion term mt)
  (when (eq (gaf-predicate assertion) term)
    (if mt
        (hlmt-equal (assertion-mt assertion) mt)
        t)))

(defun matches-function-extent-index (assertion term)
  (declare (ignore term))
  (when (gaf-assertion? assertion)
    (missing-larkc 30226)))

(defun matches-predicate-rule-index (assertion pred &optional sense mt direction)
  (block nil
    (when (rule-assertion? assertion)
      (unless sense
        (dolist (sense *valid-senses*)
          (when (matches-predicate-rule-index assertion pred sense)
            (return-from matches-predicate-rule-index t))))
      (when (and mt
                 (or (not (hlmt-equal (assertion-mt assertion) mt))
                     (not (or (not direction)
                              (eq direction (assertion-direction assertion))))))
        (return nil))
      ;; This body can be reached if some of the toplevel conditions above pass, but not the inners
      (let* ((cnf (assertion-cnf assertion))
             (literals (if (eq :pos sense)
                           (pos-lits cnf)
                           (neg-lits cnf))))
        (find pred literals :test #'matches-predicate-rule-index-test)))))

(defun matches-predicate-rule-index-test (pred literal)
  (let ((predicate (literal-predicate literal)))
    (when (fort-p predicate)
      (eq pred predicate))))

(defun matches-ist-predicate-rule-index (assertion pred &optional sense mt direction)
  (when (rule-assertion? assertion)
    (unless sense
      (dolist (sense *valid-senses*)
        (when (matches-ist-predicate-rule-index assertion pred sense)
          (return-from matches-ist-predicate-rule-index t))))
    (when (and mt
               (or (not (hlmt-equal (assertion-mt assertion) mt))
                   (not (or (not direction)
                            (eq (assertion-direction assertion) direction)))))
      (return-from matches-ist-predicate-rule-index))
    (let* ((cnf (assertion-cnf assertion))
           (literals (if (eq sense :pos)
                         (pos-lits cnf)
                         (neg-lits cnf))))
      (find pred literals :test #'matches-ist-predicate-rule-index-test))))

(defun matches-ist-predicate-rule-index-test (pred literal)
  (and (eq #$ist (literal-predicate literal))
       (eq pred (literal-predicate (literal-arg2 literal)))))

(defun matches-decontextualized-ist-predicate-rule-index (assertion pred &optional sense direction)
  (when (rule-assertion? assertion)
    (unless sense
      (dolist (sense *valid-senses*)
        (when (matches-decontextualized-ist-predicate-rule-index assertion pred sense)
          (return-from matches-decontextualized-ist-predicate-rule-index t))))
    (when (or (not direction)
              (eq direction (assertion-direction assertion)))
      (let* ((cnf (assertion-cnf assertion))
             (literals (if (eq :pos sense)
                           (pos-lits cnf)
                           (neg-lits cnf))))
        (find pred literals :test #'matches-decontextualized-ist-predicate-rule-index-test)))))

(defun matches-decontextualized-ist-predicate-rule-index-test (pred literal)
  (and (eq #$ist (literal-predicate literal))
       (eq pred (literal-predicate (literal-arg2 literal)))))

(defun matches-isa-rule-index (assertion col &optional sense mt direction)
  (when (rule-assertion? assertion)
    (unless sense
      (dolist (sense *valid-senses*)
        (when (matches-isa-rule-index assertion col sense)
          (return-from matches-isa-rule-index t))))
    (when mt
      (unless (or (hlmt-equal (assertion-mt assertion) mt)
                  (not direction)
                  (eq (assertion-direction assertion) direction))
        (return-from matches-isa-rule-index nil)))
    (let* ((cnf (assertion-cnf assertion))
           (literals (if (eq sense :pos)
                         (pos-lits cnf)
                         (neg-lits cnf))))
      (find col literals :test #'matches-isa-rule-index-test))))

(defun matches-isa-rule-index-test (col literal)
  (let ((predicate (literal-predicate literal)))
    (when (eq #$isa predicate)
      (let ((collection (literal-arg2 literal)))
	(when (fort-p collection)
          (eq col collection))))))

(defun matches-quoted-isa-rule-index (assertion col &optional sense mt direction)
  (when (rule-assertion? assertion)
    (unless sense
      (dolist (sense *valid-senses*)
        (when (matches-quoted-isa-rule-index assertion col sense)
          (return-from matches-quoted-isa-rule-index t))))
    (when mt
      (unless (or (hlmt-equal (assertion-mt assertion) mt)
                  (not direction)
                  (eq (assertion-direction assertion) direction))
        (return-from matches-quoted-isa-rule-index nil)))
    (let* ((cnf (assertion-cnf assertion))
           (literals (if (eq :pos sense)
                         (pos-lits cnf)
                         (neg-lits cnf))))
      ;; TODO - missing-larkc test, so using a symbol instead of function to make compiler happier
      (find col literals :test 'matches-quoted-isa-rule-index-test))))

(defun matches-genls-rule-index (assertion col &optional sense mt direction)
  (when (rule-assertion? assertion)
    (unless sense
      (dolist (sense *valid-senses*)
        (when (matches-genls-rule-index assertion col sense)
          (return-from matches-genls-rule-index t))))
    (when mt
      (unless (or (hlmt-equal (assertion-mt assertion) mt)
                  (not direction)
                  (eq (assertion-direction assertion) direction))
        (return-from matches-genls-rule-index nil)))
    (let* ((cnf (assertion-cnf assertion))
           (literals (if (eq :pos sense)
                         (pos-lits cnf)
                         (neg-lits cnf))))
      (find col literals :test #'matches-genls-rule-index-test))))

(defun matches-genls-rule-index-test (col literal)
  (let ((predicate (literal-predicate literal)))
    (when (eq #$genls predicate)
      (let ((collection (literal-arg2 literal)))
	(when (fort-p collection)
	  (eq col collection))))))

(defun matches-genl-mt-rule-index (assertion genl-mt &optional sense mt direction)
  (when (rule-assertion? assertion)
    (unless sense
      (dolist (sense *valid-senses*)
        (when (matches-genl-mt-rule-index assertion genl-mt sense)
          (return-from matches-genl-mt-rule-index t))))
    (when mt
      (unless (or (hlmt-equal (assertion-mt assertion) mt)
                  (not direction)
                  (eq (assertion-direction assertion) direction))
        (return-from matches-genl-mt-rule-index nil)))
    (let* ((cnf (assertion-cnf assertion))
           (literals (if (eq :pos sense)
                         (pos-lits cnf)
                         (neg-lits cnf))))
      (find genl-mt literals :test #'matches-genl-mt-rule-index-test))))

(defun matches-genl-mt-rule-index-test (mt literal)
  (let ((predicate (literal-predicate literal)))
    (when (eq #$genlMt predicate)
      (let ((genl-mt (literal-arg2 literal)))
	(when (hlmt-p genl-mt)
          (hlmt-equal mt genl-mt))))))

(defun matches-function-rule-index (assertion func &optional mt direction)
  (when (rule-assertion? assertion)
    (when mt
      (unless (or (hlmt-equal (assertion-mt assertion) mt)
                  (not direction)
                  (eq (assertion-direction assertion) direction))
        (return-from matches-function-rule-index nil)))
    (let* ((cnf (assertion-cnf assertion))
           (literals (neg-lits cnf)))
      (find func literals :test #'matches-function-rule-index-test))))

(defun matches-function-rule-index-test (func literal)
  (declare (ignore func))
  ;; TODO - this resolves to either a NIL or an error. Would it work to always return NIL? That also affects the above function
  (let ((predicate (literal-predicate literal)))
    (when (and (eq #$termOfUnit predicate)
               (variable-p (literal-arg1 literal)))
      (let ((nat (literal-arg2 literal)))
        (when (consp nat)
          (missing-larkc 29813))))))

(defun matches-exception-rule-index (assertion rule &optional mt direction)
  (when (rule-assertion? assertion)
    (when mt
      (unless (or (hlmt-equal (assertion-mt assertion) mt)
                  (not direction)
                  (eq (assertion-direction assertion) direction))
        (return-from matches-exception-rule-index nil)))
    (let* ((cnf (assertion-cnf assertion))
           (literals (pos-lits cnf)))
      (find rule literals :test #'matches-exception-rule-index-test))))

(defun matches-exception-rule-index-test (rule literal)
  (let ((predicate (literal-predicate literal)))
    (and (eq #$abnormal predicate)
         (assertion-p rule)
         (eq (literal-arg2 literal) rule))))

(defun matches-pragma-rule-index (assertion rule &optional mt direction)
  (when (rule-assertion? assertion)
    (when mt
      (unless (or (hlmt-equal (assertion-mt assertion) mt)
                  (not direction)
                  (eq (assertion-direction assertion) direction))
        (return-from matches-pragma-rule-index nil)))
    (let* ((cnf (assertion-cnf assertion))
           (literals (pos-lits cnf)))
      (find rule literals :test #'matches-pragma-rule-index-test))))

(defun matches-pragma-rule-index-test (rule literal)
  (let ((predicate (literal-predicate literal)))
    (when (and (eq #$meetsPragmaticRequirement predicate)
               (assertion-p rule))
      (eq (literal-arg2 literal) rule))))

(defun matches-other-index (assertion term)
  (when (or (tree-find term (assertion-cnf assertion) #'equal)
            (expression-find term (assertion-mt assertion) nil #'equal))
    (and (not (matches-gaf-arg-index assertion term))
         (not (matches-nart-arg-index assertion term))
         (not (matches-predicate-extent-index assertion term))
         (not (matches-function-extent-index assertion term))
         (not (matches-predicate-rule-index assertion term))
         (not (matches-ist-predicate-rule-index assertion term))
         (not (matches-decontextualized-ist-predicate-rule-index assertion term))
         (not (matches-isa-rule-index assertion term))
         (not (matches-genls-rule-index assertion term))
         (not (matches-genl-mt-rule-index assertion term))
         (not (matches-function-rule-index assertion term))
         (not (matches-exception-rule-index assertion term))
         (not (matches-pragma-rule-index assertion term)))))

(defun add-simple-index (term assertion)
  (let* ((old-index (simple-term-assertion-list term))
	 (new-index (adjoin assertion old-index)))
    (when (not (eq old-index new-index))
      (reset-term-simple-index term new-index)
      (possibly-toggle-term-index-mode term))))

(defun rem-simple-index (term assertion)
  (let* ((old-index (simple-term-assertion-list term))
	 (new-index (delete-first assertion old-index)))
    (when (not (eq old-index new-index))
      (reset-term-simple-index term new-index))))

(defparameter *within-noting-terms-to-toggle-indexing-mode* nil)
(defparameter *terms-to-toggle-indexing-mode* nil)

(defmacro noting-terms-to-toggle-indexing-mode (&body body)
  ;; Taken from kb-indexing add-assertion-indices
  `(let ((*within-noting-terms-to-toggle-indexing-mode* t)
         (*terms-to-toggle-indexing-mode* nil))
     ,@body
     ;; from constants section
     (when *terms-to-toggle-indexing-mode*
       (noting-terms-to-toggle-indexing-mode-internal))))

(defun noting-terms-to-toggle-indexing-mode-internal ()
  (dolist (term *terms-to-toggle-indexing-mode*)
    (toggle-term-index-mode term)))

;; TODO - should these be constants?  the last 3 don't update if the 1st 2 change
(defparameter *index-convert-threshold* 20)
(defparameter *index-convert-range* 4)
(defparameter *index-convert-complex-threshold* (+ *index-convert-threshold*
                                                   (floor *index-convert-range* 2))
  "[Cyc] When a simple index grows to contain this many assertions or more, convert it to the more complex form.")
(defparameter *index-convert-simple-threshold* (- *index-convert-threshold*
                                                  (floor *index-convert-range* 2))
  "[Cyc] When a complex index shrinks to contain this many assertions or fewer, convert it to the simpler form.")

(defun possibly-toggle-term-index-mode (term)
  (when *within-noting-terms-to-toggle-indexing-mode*
    (let ((total (num-index term)))
      (if (simple-indexed-term-p term)
          (when (>= total *index-convert-complex-threshold*)
            ;; pushnew will return non-nil
            (pushnew term *terms-to-toggle-indexing-mode*))
          (when (<= total *index-convert-simple-threshold*)
            ;; pushnew will return non-nil
            (pushnew term *terms-to-toggle-indexing-mode*))))))

(defun toggle-term-index-mode (term)
  (if (simple-indexed-term-p term)
      (convert-to-complex-index term)
      (convert-to-simple-index term)))

(defun convert-to-complex-index (term)
  (let ((assertions (reverse (simple-term-assertion-list term))))
    (initialize-term-complex-index term)
    (dolist (assertion assertions)
      (add-assertion-indices assertion term))))

(defun convert-to-simple-index (term)
  (let ((assertions (gather-index-in-any-mt term t)))
    (setf assertions (delete-if-not #'valid-assertion assertions))
    (free-term-index term)
    (reset-term-simple-index term assertions)))
