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


(defparameter *relevant-mt-function* nil)

(defun-inline mt-function-eq (mt-function symbol)
  "[Cyc] Return T iff relevant-mt function MT-FUNCTION matches that specified by SYMBOL."
  (eq mt-function symbol))

(defun-inline relevant-mt-function-eq (symbol)
  "[Cyc] Return T iff the currently relevant mt-function matches that specified by SYMBOL"
  (mt-function-eq *relevant-mt-function* symbol))

(defparameter *mt* *assertible-theory-mt-root*
    "[Cyc] A ubiquitous parameter used to dynamically bind the current mt assumptions, if they can be expressed by a single mt.")

(defparameter *relevant-mts* nil)

(defun current-mt-relevance-mt ()
  *mt*)

(defun relevant-mt-is-everything (mt)
  (declare (ignore mt))
  t)

(defun relevant-mt-is-any-mt (mt)
  (declare (ignore mt))
  t)

(defun relevant-mt-is-eq (mt)
  (hlmt-equal *mt* mt))

(defun relevant-mt-is-genl-mt (mt)
  (or (eq mt *mt*)
      (let ((spec-core? (core-microtheory-p *mt*))
            (genl-core? (core-microtheory-p mt)))
        (cond
          ((and spec-core? genl-core?) (core-genl-mt? mt *mt*))
          (genl-core? t)
          (spec-core? nil)
          (t (basemt? *mt*))))))

(defun relevant-mt? (mt)
  "[Cyc] Return T iff MT is relevant with respect to the current dynamic mt scope."
  (let ((function *relevant-mt-function*))
    (if (not function)
        (relevant-mt-is-genl-mt mt)
        (case function
          (relevant-mt-is-genl-mt (relevant-mt-is-genl-mt mt))
          (relevant-mt-is-any-mt (relevant-mt-is-any-mt mt))
          (relevant-mt-is-eq (relevant-mt-is-eq mt))
          (relevant-mt-is-in-list (missing-larkc 31127))
          (relevant-mt-is-genl-mt-of-list-member (missing-larkc 31125))
          (relevant-mt-is-genl-mt-with-any-time (missing-larkc 31126))
          ;; TODO - this is exactly the same as the above non-missing cases. If the missing functions get filled in, then this funcall can take over all cases.
          (otherwise (funcall function mt))))))

(defun genl-mts-are-relevant? ()
  (or (not *relevant-mt-function*)
      (relevant-mt-function-eq 'relevant-mt-is-genl-mt)))

(defun any-mt-is-relevant? ()
  (relevant-mt-function-eq 'relevant-mt-is-any-mt))

(defun all-mts-are-relevant? ()
  (relevant-mt-function-eq 'relevant-mt-is-everything))

(defun any-or-all-mts-are-relevant? ()
  (or (relevant-mt-function-eq 'relevant-mt-is-everything)
      (relevant-mt-function-eq 'relevant-mt-is-any-mt)))

(defun genl-mts-of-listed-mts-are-relevant? ()
  (relevant-mt-function-eq 'relevant-mt-is-genl-mt-of-list-member))

(defun any-time-is-relevant? ()
  (relevant-mt-function-eq 'relevant-mt-is-gnl-mt-with-any-time))

(defun only-specified-mt-is-relevant? ()
  (relevant-mt-function-eq 'relevant-mt-is-eq))

(defun possibly-in-mt-determine-function (mt)
  (if (or (not mt)
          (with-inference-any-mt-relevance? mt)
          (with-inference-mt-relevance-all-mts? mt)
          (genl-mts-of-listed-mts-are-relevant?)
          (any-time-is-relevant?))
      *relevant-mt-function*
      'relevant-mt-is-genl-mt))

(defun possibly-in-mt-determine-mt (mt)
  (or mt *mt*))

(defun possibly-with-just-mt-determine-function (mt)
  (if (not mt)
      *relevant-mt-function*
      'relevant-mt-is-eq))

(defun possibly-with-just-mt-determine-mt (mt)
  (or mt *mt*))

(defun with-inference-mt-relevance-validate (mt)
  (or mt (error "No microtheory was specified.")))

(defun update-inference-mt-relevance-mt (mt)
  (when mt
    (check-type mt 'hlmt-p))
  (or mt *mt*))

(defun update-inference-mt-relevance-function (mt)
  (cond
    ((not mt) *relevant-mt-function*)
    ((with-inference-any-mt-relevance? mt) 'relevant-mt-is-any-mt)
    ((with-inference-mt-relevance-all-mts? mt) 'relevant-mt-is-everything)
    ((with-mt-union-relevance? mt) 'relevant-mt-is-genl-mt-of-list-member)
    ((with-inference-anytime-relevance? mt) 'relevant-mt-is-genl-mt-with-any-time)
    (t 'relevant-mt-is-genl-mt)))

(defun update-inference-mt-relevance-mt-list (mt)
  (cond
    ((null mt) *relevant-mts*)
    ((with-mt-union-relevance? mt) (missing-larkc 12317))
    (t nil)))

(defun with-inference-any-mt-relevance? (mt)
  (eq 'psc-inference (mt-inference-function mt)))

(defun with-inference-mt-relevance-all-mts? (mt)
  (eq 'all-mts-inference (mt-inference-function mt)))

(defun with-mt-union-relevance? (mt)
  (eq 'mt-union-inference (mt-inference-function mt)))

(defun with-inference-anytime-relevance? (mt)
  (eq 'anytime-psc-inference (mt-inference-function mt)))

(defun inference-relevant-mt ()
  "[Cyc] From the dynamic mt context, return an mt suitable for passing to WITH-INFERENCE-MT-RELEVANCE. Using thsi is usually preferable to referencing *MT* directly."
  (cond
    ((all-mts-are-relevant?) #$EverythingPSC)
    ((any-mt-is-relevant?) #$InferencePSC)
    ((genl-mts-of-listed-mts-are-relevant?) (make-formula #$MtUnionFn *relevant-mts*))
    (t *mt*)))

(defun any-or-all-mts-relevant-to-mt? (mt)
  (or (with-inference-any-mt-relevance? mt)
      (with-inference-mt-relevance-all-mts? mt)))

(defun conservative-constraint-mt (mt)
  "[Cyc] Assumign that relevance is being established from MT, and we are imposing a constraint about which we need to be conservative, return the mt in which we should look for such constraints."
  (if (any-or-all-mts-relevant-to-mt? mt)
      *core-mt-floor*
      mt))

(defun any-relevant-mt? (mts)
  (some #'relevant-mt? mts))



;; Derived macros

(defmacro with-all-mts (&body body)
  `(let ((*relevant-mt-function* 'relevant-mt-is-everything)
         (*mt* #$EverythingPSC))
     ,@body))
