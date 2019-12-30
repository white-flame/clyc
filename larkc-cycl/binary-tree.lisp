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


(defstruct (btree (:conc-name "BT-"))
  tag
  state
  lower
  higher)

(deflexical *btree-free-list* nil
    "[Cyc] Free list for BTREE objects")
(deflexical *btree-free-lock* (bt:make-lock "BTREE resource lock")
    "[Cyc] Lock for BTREE object free list")

(defun init-btree (btree)
  "[Cyc] Initialize a BTREE for use"
  (setf (bt-tag btree)
        (setf (bt-state btree)
              (setf (bt-lower btree)
                    (setf (bt-higher btree) nil))))
  btree)

(defun free-btree-p (object)
  "[Cyc] Check if OBJECT is a BTREE that has been explicitly freed"
  (and (btree-p object)
       (eq (bt-state object) :free)))

(defun free-btree (object)
  "[Cyc] Place a BTREE onto the free list"
  (unless (free-btree-p object)
    (setf object (init-btree object))
    (setf (bt-state object) :free)
    (when *structure-resourcing-enabled*
      (bt:with-lock-held (*btree-free-lock*)
        (setf (bt-tag object) *btree-free-list*)
        (setf *btree-free-list* object)))
    t))

(defun get-btree ()
  "[Cyc] Get a BTREE from the free list, or make a new one if needed"
  (if (not *structure-resourcing-enabled*)
    (if *structure-resourcing-make-static*
        (missing-larkc 11637)
        (init-btree (make-btree)))
    (bt:with-lock-held (*btree-free-lock*)
      (let ((found nil))
        ;; TODO - this is weird. Why would btree objects be added to the freelist without having their state be tagged as :free?  And removal from this list is behind the lock as well, so we shouldn't see them come into use simultaneously.
        (do ((object *btree-free-list* (bt-tag object)))
            ((or (not object)
                 (when (free-btree-p object)
                   (setf found object
                         *btree-free-list* (bt-tag object))))))
        (unless found
          (if *structure-resourcing-make-static*
              (missing-larkc 11638)
              (setf found (make-btree)))
          (setf *btree-free-list* nil))
        (init-btree found)))))

;; TODO - enabling this is all missing-larkc, so checks are elided
(defparameter *validate-btrees* nil)

(defun btree-insert (val tag btree comp-func add-val-func)
  (let ((node (btree-find-aux tag btree comp-func t)))
    (setf (bt-state node) (and add-val-func (funcall add-val-func val (bt-state node))))
    (if btree btree node)))

(defun btree-remove (val tag btree comp-func rem-val-func &optional (empty-func #'null))
  (multiple-value-bind (node back) (btree-find-aux tag btree comp-func)
    (let ((ans btree))
      (when (btree-p node)
        (setf (bt-state node) (funcall rem-val-func val (bt-state node)))
        (when (funcall empty-func (bt-state node))
          (setf ans (btree-remove-aux node back btree comp-func))))
      ans)))

(defun btree-find-best (btree)
  (do ((back nil next)
       (next btree (bt-lower next)))
      ((not next)
       back)))

(defparameter *btree-tags* nil)

(defun incomparable (func obj1 obj2)
  (not (or (funcall func obj1 obj2)
           (funcall func obj2 obj1))))

(defun btree-find-aux (tag btree comp-func &optional create?)
  (do ((back nil next)
       (next btree (if (funcall comp-func tag (bt-tag next))
                       (bt-lower next)
                       (bt-higher next))))
      ((or (not next)
           (incomparable comp-func tag (bt-tag next)))
       (when (and (not next) create?)
         (let ((new (get-btree)))
           (setf (bt-tag new) tag)
           (when back
             (btree-insert-aux new back comp-func))
           (setf next new)))
       (values next back))))

(defun btree-insert-aux (new old comp-func)
  "[Cyc] Insert NEW off of OLD in direction idnicated by COMP-FUNC comparison of tags."
  (if (funcall comp-func old new)
      (progn
        (must-not (bt-higher old)
                  "~s info will be lost inserting ~s into ~s" (bt-higher old) new old)
        (setf (bt-higher old) new))
      (progn
        (must-not (bt-lower old)
                  "~s info will be lost inserting ~s into ~s" (bt-lower old) new old)
        (setf (bt-lower old) new)))
  old)

;; TODO - enabling this is missing-larkc, so elided from code. (debug removal, not remove the debugging features)
(defparameter *btree-remove-debugging* nil)

(defun btree-remove-aux (node back top comp-func)
  (declare (ignore comp-func))
  (let ((low (bt-lower node))
        (high (bt-higher node))
        (replacement nil)
        (re-insert nil)
        (ans top))
    (if (and low high)
        (if (zerop (random 2))
            (setf replacement low re-insert high)
            (setf replacement high re-insert low))
        (setf replacement (or low high)))
    (when re-insert
      (missing-larkc 11595))
    (if back
        (if (eq node (bt-lower back))
            (setf (bt-lower back) replacement)
            (setf (bt-higher back) replacement))
        (setf ans replacement))
    (free-btree node)
    ans))

(defconstant *cfasl-opcode-btree* 19)
(defconstant *cfasl-opcode-legacy-btree-low* 20)
(defconstant *cfasl-opcode-legacy-btree-high* 21)
(defconstant *cfasl-opcode-legacy-btree-leaf* 22)
(defparameter *btree-balance-vector-index* 0)
(defparameter *btree-valance-vector* nil)

(defstruct avl-tree
  root
  test
  size)

(defconstant *cfasl-opcode-avl-tree* 80)


(defstruct avl-tree-node
  data
  balance
  lower
  higher)

(defconstant *cfasl-opcode-avl-tree-node* 81)
