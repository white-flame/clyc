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


;; TODO DESIGN - these should go into hashtable-utilities, deprecate all these.

(defun dictionary-push (dictionary key value)
  "[Cyc] Push VALUE onto the current value at KEY in DICTIONARY. Ensures that the current value at KEY is a LISTP. If the number of entries wouldexceed the limit, revamp the dictinoary to the next better representation.
Returns KEY."
  (let ((current-val (gethash key dictionary)))
    (if (listp current-val)
        (progn
          (setf (gethash key dictionary) (cons value current-val))
          key)
        (error "Corrupted dictionary; attempting to push values on a non-LISTP ~a." current-val))))

(defun-inline dictionary-pushnew (dictionary key val &optional (test #'eql) (key-accessor #'identity))
  "[Cyc] Push VALUE onto the current value at KEY in DICTIONARY. Ensures that the current value at KEY is a LISTP and that VALUE is not yet a member of the list. If the number of entries would exceed the limit, revamp the dictionary to the next better representation. TEST is a predicate that tests elements for equality and KEY-ACCESSOR is a function that represents the key field of the element.
Returns KEY."
  (let ((current-val (gethash key dictionary)))
    (if (listp current-val)
        (progn
          (unless (member key current-val :test test :key key-accessor)
            (setf (gethash key dictionary) (cons val current-val)))
          key)
        (error "Corrupted dictionary; attempting to push values on a non-LISTP ~a." current-val))))

(defun dictionary-getf (dictionary key indicator &optional default)
  "[Cyc] Assumes that the values of DICTIONARY are property lists (plists).
Looks up KEY in DICTIONARY to get the plist, then gets the property indicated by INDICATOR from that plist."
  (getf (gethash key dictionary) indicator default))

(defun dictionary-putf (dictionary key indicator value)
  "[Cyc] Assumes that the values of DICTIONARY are property lists (plists).
Looks up KEY in DICTIONARY to get the plist, then sets the property indicated by INDICATOR on that plist to the value VALUE."
  (setf (getf (gethash key dictionary) indicator) value))

(defun dictionary-delete-first-from-value (dictionary key elt &optional (test #'eql))
  "[Cyc] Deletes the first occurrence of ELT from current value at KEY in DICTIONARY.
Ensures that the current value is a LISTP. If this deletes the last element from current value,t he perform a DICTIONARY-REMOVE."
  (let ((new-val (delete-first elt (gethash key dictionary) test)))
    (if new-val
        (setf (gethash key dictionary) new-val)
        (remhash key dictionary))))

(defun dictionary-increment (dictionary key &optional (increment 1))
  "[Cyc] Increments the value at KEY in DICTIONARY by INCREMENT.
Treats NIL a 0."
  (incf (gethash key dictionary 0) increment))

(defun new-dictionary-from-alist (alist &optional (test #'eql))
  (alexandria:alist-hash-table alist :test test :size (length alist)))

;; TODO - the function using this is missing-larkc
(defparameter *dictionary-keys-sorter-current-sorting-information* nil
    "[Cyc] A triple of dictionary, predicate, and key that is used by the DICTIONARY-KEYS-SORTED-BY-VALUES infrastructure to help apply the comparison predicate to the values looked up from the dictionary.")

(defun dictionary-has-key? (dictionary key)
  "[Cyc] Does DICTIONARY have an entry for KEY?"
  (nth-value 1 (gethash key dictionary)))


;; SBCL has built-in synchronized hashtables

(defun new-synchronized-dictionary (&optional (test #'eql) (size 0))
  (make-hash-table :test test :size size :synchronized t))

(defun-inline clear-synchronized-dictionary (dictionary)
  (clrhash dictionary))
(defun-inline synchronized-dictionary-enter (dictionary key value)
  (setf (gethash key dictionary) value))
(defun-inline synchronized-dictionary-remove (dictionary key)
  (remhash key dictionary))
(defun-inline synchronized-dictionary-lookup (dictionary key &optional default)
  (gethash key dictionary default))
(defun-inline synchronized-dictionary-keys (dictionary)
  "[Cyc] Returns a list of the given SYNCHRONIZED-DICTIONARY's keys."
  (sb-ext:with-locked-hash-table (dictionary)
    (hash-table-keys dictionary)))

;; TODO - test-dictionary-key-conflation was left in the original source. Odd, since they seem mostly removed.

