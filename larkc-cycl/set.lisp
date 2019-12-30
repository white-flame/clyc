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


;; Replaced the implemenation with key->key hashtables

;; TODO - could defer the inlines to set-contents instead of hash-table

(deflexical *new-set-default-test-function* #'eql)

(defun-inline new-set (&optional (test *new-set-default-test-function*) (size 0))
  "[Cyc] Allocate a new set with TEST as the equality test.
Assume that SIZE elements will likely be immediately added."
  (make-hash-table :test test :size size))

(defun-inline set-size (set)
  "[Cyc] Return the number of items currently entered in SET"
  (hash-table-count set))

(defun-inline set-empty? (set)
  "[CYc] Return non-NIL iff SET is empty, NIL otherwise."
  (hash-table-empty-p set))

(defun-inline set-member? (element set)
  "[Cyc] Return T iff ELEMENT is in SET."
  (nth-value 1 (gethash element set)))

;; TODO DESIGN - this is a lot slower than if it didn't have to have the return value test.  Same with set-remove.
(defun-inline set-add (element set)
  "[Cyc] Add this ELEMENT into the SET.
Return T iff ELEMENT was not already there."
  (unless (set-member? element set)
      (setf (gethash element set) element)
      t))

(defun-inline set-remove (element set)
  "[Cyc] If ELEMENT is present in SET, then take it out of SET.
Returns T iff ELEMENT was in SET to begin with."
  (when (set-member? element set)
    (remhash element set)
    t))

(defun-inline clear-set (set)
  "[Cyc] Reset SET to the status of being just allocated.
Returns SET."
  ;; TODO - we're not remembering its initial size.  Oh well.
  (clrhash set))

(defun-inline new-set-iterator (set)
  (new-hash-table-iterator set))

(defconstant *cfasl-opcode-set* 60)

(defun cfasl-input-set (stream)
  (let* ((test (cfasl-input stream))
         (size (cfasl-input stream))
         (set (new-set test size)))
    (cfasl-input-set-contents stream set size)))

(defconstant *cfasl-opcode-legacy-set* 67)

(defun-inline set-element-list (set)
  "[Cyc] Returns a list of the elements of SET."
  (hash-table-keys set))

;; TODO - deprecate this
(defun-inline set-rebuild (set)
  set)


