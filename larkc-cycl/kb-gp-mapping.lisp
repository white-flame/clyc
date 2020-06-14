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


(defparameter *mapping-arg-swap* nil)

(defun* dgaigp-binary? (predicate) (:inline t)
  (binary? predicate))

(defun gp-map-arg-index (function term arg predicate)
  "[Cyc] Like MAP-ARG-INDEX, except all spec-predicates of PREDICATE are relevant, and :true is assumed for TRUTH."
  (catch :mapping-done
    ;; TODO - pred macro, similar to mt macro?
    (let ((*relevant-pred-function* #'relevant-pred-is-spec-pred)
          (*pred* predicate))
      (kmu-do-index-iteration (assertion gaf-arg (term arg predicate) (:gaf :true nil))
        (funcall function assertion)))

    (when (dgaigp-binary? predicate)
      ;; TODO - pred macro
      (let ((*relevant-pred-function* #'relevant-pred-is-spec-inverse)
            (*pred* predicate))
        (kmu-do-index-iteration (assertion gaf-arg (term (binary-arg-swap arg) predicate) (:gaf :true nil))
          (let ((*mapping-arg-swap* (not *mapping-arg-swap*)))
            (funcall function assertion)))))))

(defun num-spec-pred-index (pred &optional mt)
  "[Cyc] only use this where PRED is a predicate."
  (let ((count 0))
    (possibly-in-mt (mt)
      (dolist (spec-pred (all-spec-preds pred))
        (incf count (Num-predicate-extent-index spec-pred))))
    count))

