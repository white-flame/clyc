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

;; TODO - this is mostly just a structure, without behavior.  See if this is used in larkc, or should be elided for now.


(defstruct graphl-search
  direction
  type
  order
  cutoff
  marking
  marking-space
  goal-space
  goal
  goal-fn
  goal-found-p
  satisfy-fn
  map-fn
  justify?
  add-to-result?
  unwind-fn-tobble
  result
  graph)

(defun new-graphl-search (plist)
  (let ((graphl-search (make-graphl-search)))
    ;; TODO - this will work if no initialization parameters are given.  The make-graphl-search had a missing-larkc for every slot keyword initializer.
    (do-plist (property value plist)
      (missing-larkc 31969))
    (possibly-initialize-graphl-marking-spaces graphl-search)
    (set-graphl-search-result graphl-search nil)
    graphl-search))

(defun destroy-graphl-search (graphl-search)
  (setf (graphl-search-direction graphl-search) :free)
  (setf (graphl-search-marking-space graphl-search) :free)
  (setf (graphl-search-result graphl-search) :free)
  (setf (graphl-search-graph graphl-search) :free))

;; Renamed getters
(symbol-mapping graphl-direction graphl-search-direction
                graphl-order graphl-search-order
                graphl-space graphl-search-marking-space
                graphl-compute-justify? graphl-search-justify?
                graphl-result graphl-search-result)
                
(defun graphl-depth-first-search-p (search)
  (eq (graphl-order search) :depth-first))

;; Renamed setters
(defun set-graphl-search-type (search type)
  (setf (graphl-search-type search) type))
(defun set-graphl-search-direction (search direction)
  (setf (graphl-search-direction search) direction))
(defun set-graphl-search-order (search order)
  (setf (graphl-search-order search) order))
(defun set-graphl-search-marking (search marking)
  (setf (graphl-search-marking search) marking))
(defun set-graphl-search-marking-space (search marking-space)
  (setf (graphl-search-marking-space search) marking-space))
(defun set-graphl-search-goal (search goal)
  (setf (graphl-search-goal search) goal))
(defun set-graphl-search-goal-found-p (search found-p)
  (setf (graphl-search-goal-found-p search) found-p))
(defun set-graphl-search-justify? (search justify?)
  (setf (graphl-search-justify? search) justify?))
(defun set-graphl-search-result (search result)
  (setf (graphl-search-result search) result))

(defun possibly-initialize-graphl-marking-spaces (search)
  (unless (graphl-search-marking-space search)
    (setf (graphl-search-marking-space search) (graphl-instantiate-new-space)))
  search)

(defun* graphl-add-to-result (search addition &optional (test #'eq)) (:inline t)
  (pushnew addition (graphl-search-result search) :test test))

(deflexical *graphl-search-size* 200)

(defun* graphl-search-size () (:inline t)
  *graphl-search-size*)

(defun graphl-instantiate-new-space ()
  (make-hash-table :size (graphl-search-size)))

(defun* graphl-forward-direction-p (direction) (:inline t)
  (eq direction :forward))

(defun* determine-graphl-relevant-directions (graphl-direction) (:inline t)
  (case graphl-direction
    (:accessible '(:forward :backward))
    (:forward '(:forward))
    (:backward '(:backward))))

