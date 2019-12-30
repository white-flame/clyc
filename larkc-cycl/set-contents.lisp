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



;; This seems to treat single values, lists, and hashtables as sets, transitioning between them as they grow & shrink
;; Has to do introspection and branching off returned keywords and stuff, probably faster just to hashtable it
;; Besides, this introspection is pretty dangerous if you're using lists as data elements
;; The modern rule of thumb is that at 10 or so elements, hash tables become cheaper.  This one keeps lists up to 128 elements, which is nuts.  If that's a common size range for this datastructure, then we're better off just sticking with hashtables.
;; Plus it's much faster to use the baked-in hashtable tests than manually funcalling test in lists and such

;; TODO - mark all functions as deprecated

;; TODO - if we're solely going to use hashtables, then wait on defining this and see where it's used
;; (defun set-contents-p (object)
;;   "[Cyc] Return T iff OBJECT can be interpreted as the contents of a set."
;;   ;; Degenerate, there's no wrapper
;;   (declare (ignore object))
;;   t)

(defun new-set-contents (&optional (size 0) (test #'eql))
  "[Cyc] Allocate a new set-contents. Assume that SIZE elements will likely be immediately added, with TEST as the assumed equality test."
  (make-hash-table :test test :size size))

(defun copy-set-contents (set-contents)
  "[Cyc] Return a new copy of SET-CONTENTS. TEST is the assumed equality test."
  (let ((new-ht (make-hash-table :test (hash-table-test set-contents)
                                 :size (hash-table-size set-contents))))
    (maphash (lambda (key value)
               (setf (gethash key new-ht) value))
             set-contents)
    new-ht))

(defun set-contents-size (set-contents)
  "[Cyc] Return the number of items currently entered in SET."
  (hash-table-count set-contents))

(defun set-contents-empty? (set-contents)
  "[Cyc] non-nil iff SET-CONTENTS is empty, NIL otherwise"
  (hash-table-empty-p set-contents))

(defun set-contents-singleton? (set-contents)
  "[Cyc] non-NIL iff SET-CONTENTS has exactly one element"
  (= 1 (hash-table-count set-contents)))

(defun set-contents-member? (element set-contents)
  "[Cyc] T iff ELEMENT is in SET-CONTENTS. TEST is the assumed equality test."
  (gethash element set-contents))

(defun set-contents-add (element set-contents)
  "[Cyc] Add this ELEMENT into the SET-CONTENTS. TEST is the assumed equality test."
  (setf (gethash element set-contents) t))

(defun set-contents-delete (element set-contents)
  "[Cyc] If ELEMENT is present in SET-CONTENTS, then take it out of SET-CONTENTS. TEST is the assumed equality test."
  (remhash element set-contents))

(defun clear-set-contents (set-contents)
  "[Cyc] Reset SET-CONTENTS to the status of being just allocated."
  (clrhash set-contents))

(defun new-set-contents-iterator (set-contents)
  "[Cyc] Returns an iterator for the elements of SET-CONTENTS."
  ;; TODO - is this used on lists without going through new-set-contents?
  ;; TODO - not great performance, but returning a closure with WITH-HASH-TABLE-ITERATOR is undefined behavior
  (new-list-iterator (hash-table-keys set-contents)))

(defun cfasl-input-set-contents (stream set-contents size)
  (dotimes (i size)
    (setf set-contents (set-contents-add (cfasl-input stream) set-contents)))
  set-contents)

(defun set-contents-element-list (set-contents)
  "[Cyc] return a list of the elements of SET-CONTENTS."
  (hash-table-keys set-contents))

(defun set-contents-rebuild (set-contents)
  "[Cyc] Rehashes SET-CONTENTS if it's a keyhash style"
  ;; No, trust the implementation.
  set-contents)
