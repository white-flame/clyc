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


(defstruct cache
  (capacity 0 :type (and fixnum (integer 0)))
  map
  head-entry)

(defun-inline cache-contains-key-p (cache key)
  "[Cyc] Checks whether the key is currently associated with an entry in the cache. Other than GET, this does not change the ordering of teh entries in the cache.
CACHE: the cache within which to check for an association
KEY: the key to be checked for association
returns T if the key has an association, NIL otherwise"
  (nth-value 1 (gethash key (cache-map cache))))

(defstruct cache-entry
  newer
  key
  value
  older)

(defparameter *cache-entries-preallocate?* nil
    "[Cyc] When T, preallocate all of the cache entries and resource them")

(defun new-cache (capacity &optional (test #'eql))
  "[Cyc] Creates a new cache with the specified capacity and test function
CAPACITY: the maximal number of entries CACHE can hold"
  (declare (fixnum capacity))
  (let* ((head-entry (make-cache-entry))
         (cache (make-cache :capacity capacity
                            :map (make-hash-table :size capacity
                                                  :test test)
                            :head-entry head-entry)))
    (setf (cache-entry-newer head-entry) head-entry)
    (setf (cache-entry-older head-entry) head-entry)

    (when *cache-entries-preallocate?*
      (set-cache-free-list cache :resourced)
      (dotimes (i capacity)
        (resource-cache-entry cache (make-cache-entry))))
    
    cache))

(defun new-preallocated-cache (capacity &optional (test #'eql))
  "[Cyc] Creates a new cache under preallocation strategy
CAPACITY: the maximal number of entries CACHE can hold"
  (let ((*cache-entries-preallocate?* t))
    (new-cache capacity test)))

(defun cache-full-p (cache)
  "[Cyc] Returns true if CACHE is full, NIL otherwise"
  (>= (cache-size cache) (cache-capacity cache)))

(defun cache-empty-p (cache)
  "[Cyc] Returns true if CACHE is empty, NIL otherwise"
  (eq (cache-newest cache) (cache-head-entry cache)))

(defun cache-get (cache key)
  "[Cyc] Returns the entry associated with KEY in CACHE
returns (i) the value assocaited with KEY in CACHE (ii) a boolean value indicating whether there was an entry for KEY in CACHE"
  (cache-get-int cache key nil t))

(defun cache-get-without-values (cache key &optional default)
  "[Cyc] Returns the entry associated with KEY in CACHE
returns the value associated with KEY in CACHE, or DEFAULT if there was no such entry for KEY in CACHE"
  (cache-get-int cache key default nil))

;; TODO - split this into 2 versions based on return-entry-p, these tests are statically resolvable
(defun cache-get-int (cache key default return-entry-p)
  (let ((entry (gethash key (cache-map cache))))
    (if (not entry)
        (if return-entry-p
            (values nil nil)
            default)
        (progn
          (cache-queue-requeue cache entry)
          (if return-entry-p
              (values (cache-entry-value entry) t)
              (cache-entry-value entry))))))

(defun cache-set (cache key value)
  "[Cyc] Associates KEY with VALUE in CACHE
returns (i) the value previously associated with KEY in CACHE (ii) a boolean value indicating whether there was an entry previously associated with KEY in CACHE"
  (cache-set-int cache key value t))

(defun cache-set-without-values (cache key value)
  "[Cyc] Associates KEY with VALUE in CACHE
returns the value previously associated with KEY in CACHE"
  (cache-set-int cache key value nil))

;; TODO DESIGN - the cache test for setting when it's full goes missing-larkc, which invalidates a fundamental use of the cache in the first place.
(defun cache-set-return-dropped (cache key value)
  "[Cyc] Associates KEY with VALUE in CACHE
return 0 the key that was dropped from the cache, else NIL
return 1 the values that was dropped from the cache, else NIL
return 2 non-NIL iff a key/value association was dropped from the cache."
  (let ((oldest-key nil)
        (oldest-value nil)
        (dropped nil))
    (when (and (cache-full-p cache)
               (eq :unknown (cache-get-without-values cache key :unknown)))
      (missing-larkc 31599))
    (cache-set-without-values cache key value)
    (values oldest-key oldest-value dropped)))

(defun cache-set-int (cache key value return-old-entry)
  (declare (ignore return-old-entry))
  (let ((old-entry (gethash key (cache-map cache)))
        (previous nil))
    (if old-entry
        (progn
          (setf previous (cache-entry-value old-entry))
          (setf (cache-entry-value old-entry) value)
          (cache-queue-requeue cache old-entry))
        (let ((entry nil))
          (if (cache-full-p cache)
              ;; TODO DESIGN - bump older cache entries
              (missing-larkc 31600)
              (setf entry (get-new-cache-entry cache)))
          (setf (cache-entry-key entry) key)
          (setf (cache-entry-value entry) value)
          (setf (gethash key (cache-map cache)) entry)
          (cache-queue-enqueue cache entry)))
    ;; Based on return-old-entry, this would return 1 value or 2.  It's faster in SBCL to always return the same number of values.
    (values previous old-entry)))

(defun cache-remove (cache key)
  "[Cyc] Removes the mapping for KEY from CACHE
return (i) the value previously associated with KEY in CACHE (ii) a boolean value indicating whether a value was previously associated with KEY in CACHE"
  (let ((entry (gethash key (cache-map cache)))
        (value nil))
    (when entry
      (cache-queue-remove cache entry)
      (remhash key (cache-map cache))
      (setf value (cache-entry-value entry)))
    (values value entry)))

(defun cache-clear (cache)
  "[Cyc] Removes all entries from CACHE, either individually (if precached) or aggressively"
  (if (is-cache-preallocated-p cache)
      (until (cache-empty-p cache)
        ;; TODO - fetches oldest, then (cache-remove cache (cache-entry-key oldest))
        (missing-larkc 31601))
      (let ((head-entry (cache-head-entry cache)))
        (setf (cache-entry-newer head-entry) head-entry)
        (setf (cache-entry-older head-entry) head-entry)
        (clrhash (cache-map cache))))
  cache)

(defun cache-size (cache)
  "[Cyc] Returns the number of entries in CACHE"
  (hash-table-count (cache-map cache)))

(defun do-cache-first (cache order)
  (do-cache-next (cache-head-entry cache) order))

(defun do-cache-done? (cache entry)
  (eq entry (cache-head-entry cache)))

(defun do-cache-key (entry)
  (cache-entry-key entry))

(defun do-cache-value (entry)
  (cache-entry-value entry))

(defun do-cache-next (entry order)
  (ecase order
    (:newest (cache-entry-older entry))
    (:oldest (cache-entry-newer entry))))

(defconstant *cfasl-opcode-cache* 63)

(defun cache-newest (cache)
  "[Cyc] Returns the entry of CACHE that was added most recently"
  (cache-entry-older (cache-head-entry cache)))

(defun cache-queue-remove (cache entry)
  "[Cyc] Removes ENTRY from the CACHE's priority queue"
  (cache-queue-unlink entry)
  (possibly-resource-cache-entry cache entry)
  cache)

(defun cache-queue-requeue (cache entry)
  "[Cyc] Update the cache queue so that the entry becomes the newest ENTRY"
  (cache-queue-unlink entry)
  (cache-queue-append cache entry))

(defun cache-queue-enqueue (cache entry)
  "[Cyc] Enqueues ENTRY onto CACHE's priority queue"
  (cache-queue-append cache entry))

(defun cache-queue-append (cache entry)
  "[Cyc] Add ENTRY onto CACHE's priority queue at the end"
  (setf (cache-entry-newer entry) (cache-head-entry cache))
  (setf (cache-entry-older entry) (cache-newest cache))
  (setf (cache-entry-newer (cache-newest cache)) entry)
  (setf (cache-entry-older (cache-head-entry cache)) entry)
  cache)

(defun cache-queue-unlink (entry)
  "[Cyc] Remove entry from its neighbors"
  (setf (cache-entry-newer (cache-entry-older entry)) (cache-entry-newer entry))
  (setf (cache-entry-older (cache-entry-newer entry)) (cache-entry-older entry))
  entry)

(defun is-cache-preallocated-p (cache)
  (let ((free-list (cache-free-list cache)))
    (or (eq :resourced free-list)
        (cache-entry-p free-list))))

(defun get-new-cache-entry (cache)
  "[Cyc] Fetch an empty entry from the free list or allocate a new one."
  (or (unresource-cache-entry cache)
      (make-cache-entry)))

(defun cache-free-list (cache)
  "[Cyc] Returns the resourced list of cache entries."
  (cache-entry-key (cache-head-entry cache)))

(defun set-cache-free-list (cache head)
  "[Cyc] Set the head of the cache free list, containing the resourced cache entries"
  (setf (cache-entry-key (cache-head-entry cache)) head)
  cache)

(defun possibly-resource-cache-entry (cache entry)
  "[Cyc] Decide whether to put this entry onto the free list of teh cache"
  (when (is-cache-preallocated-p cache)
    (resource-cache-entry cache entry))
  cache)

(defun resource-cache-entry (cache entry)
  "[Cyc] Put the entry onto the free list of the cache."
  (scrub-cache-entry entry)
  (setf (cache-entry-newer entry) (cache-free-list cache))
  (set-cache-free-list cache entry))

(defun unresource-cache-entry (cache)
  "[Cyc] Get an ENTRY from the free list of CACHE if you can; return NIL otherwise."
  (let ((free-list (cache-free-list cache))
        (entry nil))
    (when (cache-entry-p free-list)
      (setf entry free-list)
      (set-cache-free-list cache (cache-entry-newer entry))
      (setf (cache-entry-newer entry) nil))
    entry))

(defun scrub-cache-entry (entry)
  "[Cyc] Clean up all slots to prevent any loitering."
  (setf (cache-entry-value entry)
        (setf (cache-entry-key entry)
              (setf (cache-entry-newer entry)
                    (setf (cache-entry-older entry) nil))))
  entry)



