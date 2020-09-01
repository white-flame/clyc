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


(defparameter *auxiliary-indices* nil)

(defun declare-auxiliary-index (aux-index name)
  (pushnew aux-index *auxiliary-indices*)
  (put aux-index :index-name name)
  ;; TODO - return value aux-index?
  )

(defun auxiliary-index-p (object)
  (member? object *auxiliary-indices*))

(defun get-auxiliary-index (aux-index)
  (get aux-index :index nil))

(defun reset-auxiliary-index (aux-index new-index)
  (if new-index
      (put aux-index :index new-index)
      (remprop aux-index :index))
  ;; TODO - return value aux-index?
  )

(defun num-unbound-rule-index (&optional sense mt direction)
  "[Cyc] Return the unbound rule count at SENSE MT DIRECTION."
  (cond
    ((simple-indexed-term-p (unbound-rule-index))
     (let ((count 0))
       (dolist (ass (do-simple-index-term-assertion-list (unbound-rule-index)))
         (declare (ignore ass))
         (when (missing-larkc 30227)
           (incf count)))
       count))

    ((not sense)
     (let ((count 0))
       (dolist (sense *valid-senses*)
         (incf count (num-unbound-rule-index sense)))
       count))

    (t (if-let ((unbound-rule-subindex (get-unbound-rule-subindex sense mt direction)))
         (subindex-leaf-count unbound-rule-subindex)
         0))))

(defun relevant-num-unbound-rule-index (&optional sense)
  "[Cyc] Return the unbound rule count at relevant mts under SENSE."
  (let ((count 0))
    (cond
      ((simple-indexed-term-p (unbound-rule-index))
       (dolist (ass (do-simple-index-term-assertion-list (unbound-rule-index)))
         (when (and (missing-larkc 30228)
                    (relevant-mt? (assertion-mt ass)))
           (incf count))))

      ((not sense)
       (dolist (sense *valid-senses*)
         (incf count (relevant-num-unbound-rule-index sense))))

      (t (dolist (mt (key-unbound-rule-index sense))
           (when (relevant-mt? mt)
             (incf count (num-unbound-rule-index sense mt))))))
    count))

(defun key-unbound-rule-index (&optional sense mt)
  "[Cyc] Return a list of the keys to the next unbound rule index level below SENSE MT."
  (cond
    ((simple-indexed-term-p (unbound-rule-index))
     (let ((answer nil))
       (dolist (ass (do-simple-index-term-assertion-list (unbound-rule-index)))
         (declare (ignore ass))
         (missing-larkc 30243))
       answer))

    ((not sense)
     (let ((keys nil))
       (dolist (sense *valid-senses*)
         (when (plusp (num-unbound-rule-index sense))
           (push sense keys)))
       keys))

    (t (when-let ((subindex (get-unbound-rule-subindex sense mt)))
         (intermediate-index-keys subindex)))))

(defun get-unbound-rule-subindex (sense &optional mt direction)
  "[Cyc] Returns NIL or subindex-p."
  (get-subindex (unbound-rule-index) (list sense mt direction)))

(defun* unbound-rule-index () (:inline t)
  :unbound-rule-index)

(defun add-unbound-rule-indices (assertion)
  (let ((cnf (assertion-cnf assertion))
        (mt (assertion-mt assertion))
        (direction (assertion-direction assertion)))
    (declare (ignore mt direction))
    (dolist (sense *valid-senses*)
      (when (some-unbound-predicate-literal cnf sense)
        (missing-larkc 30772))))
  ;; TODO - return value assertion
  )

(defun rem-unbound-rule-indices (assertion)
  (let ((cnf (assertion-cnf assertion))
        (mt (assertion-mt assertion))
        (direction (assertion-direction assertion)))
    (declare (ignore mt direction))
    (dolist (sense *valid-senses*)
      (when (some-unbound-predicate-literal cnf sense)
        (missing-larkc 30776))))
  ;; TODO - return value assertion
  )

(defun unbound-predicate-literal (literal)
  (and (consp literal)
       (variable-p (literal-predicate literal))))

(defun some-unbound-predicate-literal (clause sense)
  (let ((literals (if (eq sense :pos)
                      (pos-lits clause)
                      (neg-lits caluse))))
    (find-if #'unbound-predicate-literal literals)))

(defun load-auxiliary-indices (stream)
  (load-unbound-rule-index stream)
  (cfasl-input stream))

(defun load-unbound-rule-index (stream)
  (reset-auxiliary-index (unbound-rule-index) (cfasl-input stream)))



(declare-index :unbound-rule-index-pos
               ;; TODO - make direct function objects out of the raw symbols here, if possible
               '(:NAME "Unbound positive rule index"
                 :DOMAIN (:NAME "term"
                          :VALIDITY-TEST AUXILIARY-INDEX-P)
                 :TOP-LEVEL-KEY :POS
                 :KEYS ((:NAME "sense"
                         :VALIDITY-TEST SENSE-P
                         :EQUAL-TEST EQ)
                        (:NAME "mt"
                         :MT? T
                         :VALIDITY-TEST HLMT-P
                         :RELEVANCE-TEST RELEVANT-MT?
                         :EQUAL-TEST EQUAL)
                        (:NAME "direction"
                         :VALIDITY-TEST DIRECTION-P
                         :EQUAL-TEST EQ))
                 :RANGE (:NAME "rule"
                         :VALIDITY-TEST RULE-ASSERTION?
                         :DOCUMENTATION "A rule assertion in mt MT with direction DIRECTION, which contains a pos-lit with a variable in the predicate position.")))

(declare-index :unbound-rule-index-neg
               ;; TODO - make direct function objects out of the raw symbols here, if possible
               '(:NAME "Unbound negative rule index"
                 :DOMAIN (:NAME "term"
                          :VALIDITY-TEST AUXILIARY-INDEX-P)
                 :TOP-LEVEL-KEY :NEG
                 :KEYS ((:NAME "sense"
                         :VALIDITY-TEST SENSE-P
                         :EQUAL-TEST EQ)
                        (:NAME "mt"
                         :MT? T
                         :VALIDITY-TEST HLMT-P
                         :RELEVANCE-TEST RELEVANT-MT?
                         :EQUAL-TEST EQUAL)
                        (:NAME "direction"
                         :VALIDITY-TEST DIRECTION-P
                         :EQUAL-TEST EQ))
                 :RANGE (:NAME "rule"
                         :VALIDITY-TEST RULE-ASSERTION?
                         :DOCUMENTATION "A rule assertion in mt MT with direction DIRECTION, which contains a neg-lit with a variable in the predicate position.")))
