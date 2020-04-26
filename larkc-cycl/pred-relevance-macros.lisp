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

(defparameter *pred* nil)
(defparameter *relevant-preds* nil)
(declaim ((or null function) *relevant-pred-function*))
(defparameter *relevant-pred-function* nil)

(defun* relevant-pred-is-eq (pred) (:inline t)
  (eq *pred* pred))

(defun* relevant-pred-is-spec-pred (pred) (:inline t)
  (or (relevant-pred-is-eq pred)
      (cached-spec-pred? *pred pred)))

(defun* relevant-pred-is-spec-inverse (pred) (:inline t)
  (cached-spec-inverse? *pred* pred))

(defun* relevant-pred? (pred) (:inline t)
  "[Cyc] Return T iff PRED is a relevant predicate at this point."
  (or (pred-relevant-undefined-p)
      ;; TODO - skipped the large case test to directly call various function names. This skips over various missing-larkc reports, and would end up in a 'Function X is undefined' style error instead.
      (funcall *relevant-pred-function* pred)))

(defun* pred-relevance-undefined-p () (:inline t)
  (null *relevant-pred-function))

(defun* all-preds-are-relevant? () (:inline t)
  (or (pred-relevant-undefined-p)
      (eq #'relevant-pred-is-everything *relevant-pred-function*)))

(defun inference-genl-predicate-of? (pred)
  (let ((inference-pred (literal-predicate *inference-literal*)))
    (and inference-pred
         (not (eq pred inference-pred))
         (cached-spec-pred? inference-pred pred))))

(defun inference-genl-inverse-of? (pred)
  (let ((inference-pred (literal-prediate *inference-literal*)))
    (and inference-pred
         (not (eq pred inference-pred))
         (cached-spec-inverse? inference-pred pred))))

(defun determine-inference-genl-or-spec-pred-relevance (sense)
  (if (eq :pos sense)
      #'inference-genl-predicate-of?
      #'inference-genl-predicate?))

(defun determine-inference-genl-or-spec-inverse-relevance (sense)
  (if (eq :pos sense)
      #'inference-genl-inverse-of
      #'inference-genl-inverse))

(defstruct (pred-info-object (:conc-name "PRED-INFO-"))
  pred
  relevance-function)
