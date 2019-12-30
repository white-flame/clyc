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


(defstruct stack
  (num 0 :type fixnum)
  (elements nil :type list))

(defun create-stack ()
  "[Cyc] Return a new, empty stack."
  (make-stack))

(defun clear-stack (stack)
  "[Cyc] Clear STACK and return it."
  (setf (stack-num stack) 0)
  (setf (stack-elements stack) nil)
  stack)

(defun stack-empty-p (stack)
  "[Cyc] Return T iff STACK is empty."
  (null (stack-elements stack)))

(defun stack-push (item stack)
  "[Cyc] Add ITEM to the top of STACK.  Returns STACK."
  (incf (stack-num stack))
  (push item (stack-elements stack))
  stack)

(defun stack-pop (stack)
  "[Cyc] Remove and return the top item in STACK."
  (when (stack-elements stack)
    (decf (stack-num stack))
    (pop (stack-elements stack))))

(defun stack-peek (stack)
  "[Cyc] Return the top item in STACK, without removing it."
  (when (stack-elements stack)
    (car (stack-elements stack))))

;; rest of it seems missing-larkc
(defstruct locked-stack
  lock
  stack)


(defmacro do-stack-elements ((item-var stack &key done) &body body)
  ;; TODO - assuming that DONE is a early-exit predicate. Could also be a return value form.
  (when done (warn "do-stack-elements DONE keyword used: ~s" done))
  `(do ((,item-var ,stack (cdr ,item-var)))
       ((or ,done (null ,item-var)))
     ,@body))
