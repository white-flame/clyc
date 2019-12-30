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


(defstruct (queue (:conc-name "Q-"))
  (num 0 :type fixnum)
  elements
  last)

(defun create-queue ()
  "[Cyc] Return a new, empty queue."
  (clear-queue (make-queue)))

(defun clear-queue (queue)
  "[Cyc] Clear QUEUE and return it."
  (setf (q-num queue) 0)
  (setf (q-elements queue) nil)
  (setf (q-last queue) nil))

(declaim (inline queue-empty-p))
(defun queue-empty-p (queue)
  "[Cyc] Return T iff QUEUE is empty."
  (null (q-elements queue)))

(declaim (inline queue-size))
(defun queue-size (queue)
  "[Cyc] Return the number of elements in QUEUE."
  (q-num queue))

(defun enqueue (item queue)
  "[Cyc] Add ITEM to end of QUEUE.  Returns QUEUE."
  (let ((new-cell (list item)))
    (if (queue-empty-p queue)
        (setf (q-elements queue) new-cell)
        (rplacd (q-last queue) new-cell))
    (incf (q-num queue))
    (setf (q-last queue) new-cell))
  queue)

(defun dequeue (queue)
  "[Cyc] Remove and return the first item in QUEUE."
  (unless (queue-empty-p queue)
    (decf (q-num queue))
    ;; Manually pop to directly update the last cell
    (let* ((elements (q-elements queue))
           (item (first elements))
           (rest (rest elements)))
      (setf (q-elements queue) rest)
      (unless rest
        (setf (q-last queue) nil))
      item)))

(defun remqueue (item queue &optional (test :eql))
  "[Cyc] Remove all occurrences of ITEM from QUEUE.  Returns QUEUE."
  ;; Manually doing this to keep the count updated properly
  (do ((last (q-last queue))
       (back nil)
       (next nil (rest next)))
      ((not next))
    (if (funcall test (car next) item)
        (progn
          (when (eq next last)
            (setf (q-last queue) back))
          (if (eq next (q-elements queue))
              (setf (q-elements queue) (rest next))
              (rplacd back (rest next))))
        (setf back next)))
  queue)

(declaim (inline queue-peek))
(defun queue-peek (queue)
  (car (q-elements queue)))

(defconstant *cfasl-wide-opcode-queue* 131)

(defstruct (priority-queue (:conc-name "P-QUEUE-"))
  (num 0 :type fixnum)
  max-size
  rank-func
  comp-func
  tree)

(defun create-p-queue (max-size rank-func &optional (comp-func #'<))
  "[Cyc] Create and return a new priority queue."
  (make-priority-queue :num 0
                       :max-size max-size
                       :rank-func rank-func
                       :comp-func comp-func
                       :tree nil))

(declaim (inline p-queue-size))
(defun p-queue-size (priority-queue)
  "[Cyc] Return the number of elementsin PRIORITY-QUEUE."
  (p-queue-num priority-queue))

(declaim (inline p-queue-empty-p))
(defun p-queue-empty-p (priority-queue)
  "[Cyc] Return T iff PRIORITY-QUEUE is empty."
  (= 0 (p-queue-size priority-queue)))

(defun p-queue-full-p (priority-queue)
  "[Cyc] Return T iff PRIORITY-QUEUE is full."
  (and (fixnump (p-queue-max-size priority-queue))
       (= (p-queue-size priority-queue)
          (p-queue-max-size priority-queue))))

(defun p-queue-best (priority-queue)
  (let ((best (btree-find-best (p-queue-tree priority-queue))))
    (when (btree-p best)
      (pq-collision-next (bt-state best)))))

(defun p-enqueue (item priority-queue)
  (let ((bumped? (p-queue-full-p priority-queue)))
    (if bumped?
        (missing-larkc 29894)
        (let ((ans (btree-insert item
                                 (funcall (p-queue-rank-func priority-queue) item)
                                 (p-queue-tree priority-queue)
                                 (p-queue-comp-func priority-queue)
                                 #'pq-collision-enter)))
          (unless (eq ans (p-queue-tree priority-queue))
            (setf (p-queue-tree priority-queue) ans))
          (incf (p-queue-num priority-queue))
          (values priority-queue bumped? nil)))))

(defun p-dequeue (priority-queue &optional worst?)
  (unless (p-queue-empty-p priority-queue)
    (let* ((remove (if worst?
                       (missing-larkc 29895)
                       (p-queue-best priority-queue)))
           (ans (btree-remove remove
                              (funcall (p-queue-rank-func priority-queue) remove)
                              (p-queue-tree priority-queue)
                              (p-queue-comp-func priority-queue)
                              #'pq-collision-remove
                              #'pq-collision-empty)))
      (unless (eq ans (p-queue-tree priority-queue))
        (setf (p-queue-tree priority-queue) ans))
      (decf (p-queue-num priority-queue))
      remove)))

(defun pq-collision-enter (item queue)
  "[Cyc] Returns the list within the queue implementation that results from entering a new item which has the same key as others on this queue list."
  (nadd-to-end item queue))

(defun pq-collision-next (queue)
  "[Cyc] Returns the next item within the queue implementation that is obtained from a list of same-named keys."
  (car queue))

(defun pq-collision-remove (item queue)
  "[Cyc] Returns the list within the queue implementation that results from removing an item which has the same key as others on this queue list."
  (delete-first item queue))

(defun pq-collision-empty (queue)
  "[Cyc] Returns T iff the queue implementation list is empty, in the case where the list would contain same-named keys for queue items."
  (null queue))

(defstruct lazy-priority-queue
  ordered-items
  new-items)

(defstruct locked-queue
  lock
  queue)

(defstruct locked-p-queue
  lock
  priority-queue)
