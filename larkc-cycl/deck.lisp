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


;; No macros declared

(defstruct deck
  type
  data)

(defun create-deck (type)
  "[Cyc] Return a new, empty deck."
  (make-deck :type type
             :data (case type
                     (:queue (create-queue))
                     (:stack (create-stack)))))

(defun clear-deck (deck)
  "[Cyc] Clear DECK and return it."
  (setf (deck-data deck) (case (deck-type deck)
                           (:queue (create-queue))
                           (:stack (create-stack))))
  deck)

(defun deck-empty-p (deck)
  "[Cyc] Return T iff DECK is empty."
  (case (deck-type deck)
    (:queue (queue-empty-p (deck-data deck)))
    (:stack (stack-empty-p (deck-data deck)))))

(defun deck-push (elt deck)
  "[Cyc] Add an element ELT to DECK. Returns DECK."
  (case (deck-type deck)
    (:queue (enqueue elt (deck-data deck)))
    (:stack (stack-push elt (deck-data deck))))
  deck)

(defun deck-pop (deck)
  "[Cyc] Remove and return the next accessible element from DECK."
  (case (deck-type deck)
    (:queue (dequeue (deck-data deck)))
    (:stack (stack-pop (deck-data deck)))))


