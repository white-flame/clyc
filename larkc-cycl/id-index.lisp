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


;; TODO DESIGN - yet another hashtable replacement.  This one uses an array to look up non-negative integer keys.  Are these limited to fixnums?  Since they should map to array indices, which must fit in address space, it seems like they would, unless there's some offsetting delta that I'm not seeing.
;; TODO DESIGN - it seems like what's entered into the new-objects table is always <= next-id's value, looking at how optimize-id-index works. That means there's no real reason to use the hashtable, and might as well simply grow the array whenever next-id breaches the array size.
;; id-index-old-object-id-p tests for negative indices, which always falls back into the hasthable.  However, there could be another array holding negated indices.  Need to test how this stuff is used.
;; The old-objects vector is manually grown, so that the object itself can remain a simple-vector with optimized dereferencing, as opposed to using vector-push-extend
;; Need to know if arrays grow unboundedly or settle into some final size.  If the latter, then we can perform a lot of small grow operations, and the speed will amortize away over time without being wasteful in overallocation, and still be faster to dereference than the split array/hashtable implementation.




;; The original code had an empty-list marker (symbol, replaced to NIL in results), and a 'tombstone' marker for unused entries (NIL, replaced to the default in results).  The older model does more comparison than necessary, and has to do conversion on insertion.
;; I think SubL might have had to do this if it cannot control the default value of arrays and they might have always been NIL.
;; Tombstones are only used in the array, not in the hashtable.
;; Rewrote into using only tombstone marker, keeping NIL as an empty list untouched.
(defconstant +id-index-tombstone+ :%tombstone)

(defun* id-index-tombstone () (:inline t)
  +id-index-tombstone+)


(defstruct (id-index (:conc-name "IDIX-"))
  lock
  ;; Total number of objects being held (old+new)
  (count 0 :type fixnum)
  (next-id 0 :type fixnum)
  ;; vector of objects for fast lookup
  (old-objects #() :type simple-vector)
  ;; hashtable of new entries before growing the old-objects vector
  new-objects)

(defmacro with-idix-lock (id-index &body body)
  `(bt:with-lock-held ((id-index-lock ,id-index))
     ,@body))

(defun* id-index-lock (id-index) (:inline t)
  "[Cyc] Return the lock used to control modifications of ID-INDEX."
  (idix-lock id-index))

(defun* id-index-count (id-index) (:inline t)
  "[Cyc] Return the total number of objects indexed in ID-INDEX."
  (idix-count id-index))

(defun* id-index-next-id (id-index) (:inline t)
  "[Cyc] Return the next internal ID which would be used in ID-INDEX."
  (idix-next-id id-index))

(defun* set-id-index-next-id (id-index next-id) (:inline t)
  (declare (fixnum next-id))
  "[Cyc] Start reserving internal IDs in ID-INDEX at NEXT-ID."
  (setf (idix-next-id id-index) next-id))

(defun* id-index-old-objects (id-index) (:inline t)
  "[Cyc] Return the vector for old objects in ID-INDEX."
  (idix-old-objects id-index))

(defun* id-index-new-objects (id-index) (:inline t)
  "[Cyc] Return the hashtable for new objects in ID-INDEX."
  (idix-new-objects id-index))

(defun* id-index-empty-p (id-index) (:inline t)
  "[Cyc] Return T iff ID-INDEX is empty."
  (zerop (id-index-count id-index)))

(defun* id-index-new-object-count (id-index) (:inline t)
  "[Cyc] Return the number of new objects in ID-INDEX."
  (hash-table-count (id-index-new-objects id-index)))

(defun* id-index-old-object-count (id-index) (:inline t)
  "[Cyc] Return the number of old objects in ID-INDEX."
  (- (id-index-count id-index)
     (id-index-new-object-count id-index)))

(defun* id-index-new-id-threshold (id-index) (:inline t)
  "[Cyc] Return the ID at which new objects start in ID-INDEX."
  (length (id-index-old-objects id-index)))

(defun* id-index-old-object-id-p (id-index id) (:inline t)
  (declare (fixnum id))
  ;; TODO DESIGN - can probably eliminate the non-negative-integer-p check and just leave the fixnum declaration, since optimize-id-index will break if negative indexes are placed into the new-objects table anwyay.
  (and (non-negative-integer-p id)
       (< id (id-index-new-id-threshold id-index))))


;; TODO - this seems kinda huge.  Measure how big these get.
(deflexical *id-index-default-scaling-factor* 100
  "[Cyc] Number of old entries anticipated for each new entry.")
(deflexical *id-index-equality-test* #'eq
  "Used to define the hashtable test for array spillover values.  Seems like it'll stay fixnums.")

(defun new-id-index (&optional (old-objects-size 0) (new-id-start old-objects-size))
  "[Cyc] Return a new ID-INDEX with ids for new entries starting at NEW-ID-START.
Access to OLD-OBJECTS-SIZE number of ids starting at 0 will be optimized."
  (let* ((new-objects-size (max 10 (floor old-objects-size
                                          *id-index-default-scaling-factor*))))
    (make-id-index :lock (bt:make-lock "ID-INDEX")
                   :count 0
                   :next-id new-id-start
                   :old-objects (make-vector old-objects-size (id-index-tombstone))
                   :new-objects (make-hash-table :test *id-index-equality-test*
                                                 :size new-objects-size))))

(defun id-index-reserve (id-index)
  "[Cyc] Reserve an internal ID from ID-INDEX and return it."
  (with-idix-lock id-index
    (let ((next-id (id-index-next-id id-index)))
      (setf (idix-next-id id-index) (1+ next-id))
      next-id)))

(defun* id-index-tombstone-p (object) (:inline t)
  (eq object (id-index-tombstone)))

(declaim (inline id-index-lookup-int))
(defun id-index-lookup-int (id-index id)
  "[Cyc] Return the object associated with ID in ID-INDEX."
  (if (id-index-old-object-id-p id-index id)
      (aref (id-index-old-objects id-index) id)
      ;; Added a default tombstone for hash lookups
      (gethash id (id-index-new-objects id-index) (id-index-tombstone))))

(defun* id-index-lookup (id-index id &optional default) (:inline t)
  (declare (inline id-index-lookup-int)
           (fixnum id))
  (let ((result (id-index-lookup-int id-index id)))
    (if (id-index-tombstone-p result)
        default
        result)))

(defun* id-index-enter-unlocked (id-index id object) (:inline t)
  (declare (fixnum id))
  "[Cyc] Enter OBJECT in ID-INDEX as the object associated with the key ID.
ID-INDEX is assumed to be already locked from the outside."
  (let ((existing (id-index-lookup-int id-index id)))
    (if (id-index-old-object-id-p id-index id)
        (setf (aref (id-index-old-objects id-index) id) object)
        (setf (gethash id (id-index-new-objects id-index)) object))
    ;; Increment the count only if we overwrote an empty slot
    (when (id-index-tombstone-p existing)
      (incf (idix-count id-index)))))

(defun id-index-enter (id-index id object)
  "[Cyc] Enter OBJECT in ID-INDEX as the object associated with the key ID.
ID-INDEX is locked during the modification."
  (with-idix-lock id-index
    (id-index-enter-unlocked id-index id object)))

(defun id-index-enter-autoextend (id-index id object)
  "[Cyc] Enter OBJECT in ID-INDEX as the object associated with the keY ID.
ID-INDEX is locked during the modification.
If the insert fills up the old objects vector, grow the vector."
  (id-index-enter id-index id object)
  (id-index-possibly-autoextend id-index id))

(defun id-index-possibly-autoextend (id-index id)
  "[Cyc] If ID was the last id in oldspace, grow the vector."
  (let ((threshold (id-index-new-id-threshold id-index)))
    (declare (fixnum threshold id))
    (when (>= (the fixnum (1+ id)) threshold)
      (optimize-id-index id-index (+ 2 (max threshold id))))))

(defun id-index-remove (id-index id)
  (declare (fixnum id))
  "[Cyc] Remove any association for ID in ID-INDEX."
  (with-idix-lock id-index
    (let ((existing (id-index-lookup-int id-index id)))
      (if (id-index-old-object-id-p id-index id)
          (setf (aref (id-index-old-objects id-index) id) (id-index-tombstone))
          (remhash id (id-index-new-objects id-index)))
      ;; Only decrement the count if the item was found
      (unless (id-index-tombstone-p existing)
        (decf (idix-count id-index))))))

(defun clear-id-index (id-index)
  "[Cyc] Remove all ID associations in ID-INDEX."
  (with-idix-lock id-index
    (setf (idix-count id-index) 0)
    (fill (id-index-old-objects id-index) (id-index-tombstone))
    (clrhash (id-index-new-objects id-index))))

;; Macro helpers, were used in the expansion in id-index-values
(defun* id-index-skip-tombstones-p (tombstone) (:inline t)
  (eq :skip tombstone))

(defun* id-index-objects-empty-p (id-index tombstone) (:inline t)
  (when (id-index-skip-tombstones-p tombstone)
    (id-index-empty-p id-index)))

(defun* id-index-old-objects-empty-p (id-index tombstone) (:inline t)
  (when (id-index-skip-tombstones-p tombstone)
    (zerop (id-index-old-object-count id-index))))

(defun* id-index-new-objects-empty-p (id-index) (:inline t)
  (zerop (id-index-new-object-count id-index)))

;; TODO - the tombstone variable is very annoying, with its :skip option. I think the "hide tombstone" default value should be the tombstone symbol itself, since there's now 2 separate special values to check for (:%tombstone and :skip) all in the same value space, and the predicates for checking them are very poorly named.
(defmacro do-id-index ((id object id-index &key (tombstone :skip tombstone-p)
                           ordered progress-message done)
                       &body body)
  "Iterate over all values in the id-index, binding id/object to the stored key/value.  If the :tombstone is T then tombstones will be iterated as well with value NIL; else they are skipped."
  ;; TODO - don't know what DONE is supposed to do
  (when done
    (error ":DONE not supported on DO-ID-INDEX"))
  ;; Reconstructed from constant-handles set-next-constant-suid
  (alexandria:with-gensyms (total sofar new-ht)
    (alexandria:once-only (id-index tombstone)
      `(let ((,total (id-index-count ,id-index))
             (,sofar 0))
         ;; TODO - only use noting-percent-progress when :progress-message is provided
         (noting-percent-progress (,progress-message)
           ;; Skip if empty
           (unless (id-index-objects-empty-p ,id-index ,tombstone)
             ;; Vector (old contents)
             (unless (id-index-old-objects-empty-p ,id-index ,tombstone)
               (dovector (,id ,object (id-index-old-objects ,id-index))
                 (unless (id-index-tombstone-p ,object)
                   (note-percent-progress ,sofar ,total)
                   (incf ,sofar)
                   ,@body)))
             ;; Hashtable (new contents)
             (unless (id-index-new-objects-empty-p ,id-index)
               ;; Ordered must iterate the index values
               ,(if ordered
                   `(loop with ,new-ht = (id-index-new-objects ,id-index)
                       for ,id from (id-index-new-id-threshold ,id-index)
                       below (id-index-next-id ,id-index)
                       for ,object = (gethash ,id ,new-ht ,(if tombstone-p
                                                               tombstone
                                                               `(id-index-tombstone)))
                       do (unless ,(if tombstone-p nil `(id-index-tombstone-p ,object))
                            (note-percent-progress ,sofar ,total)
                            (incf ,sofar)
                            ,@body))
                   ;; Unordered can just hit the hashtable entries
                   ;; but only if we're skipping tombstones
                   (if tombstone-p
                       (error ":ORDERED + :TOMBSTONE can't both be used in DO-ID-INDEX.")
                       `(dohash (,id ,object (id-index-new-objects ,id-index))
                          (note-percent-progress ,sofar ,total)
                          (incf ,sofar)
                          ,@body))))))))))

(defconstant *cfasl-wide-opcode-id-index* 128)

(defun optimize-id-index (id-index &optional size)
  "[Cyc] Optimize ID-INDEX by merging the new objects into the old objects."
  (with-idix-lock id-index
    (let* ((next-id (id-index-next-id id-index))
           (new-size (if size (max size next-id) next-id))
           (new-objects (id-index-new-objects id-index))
           (old-objects (id-index-old-objects id-index))
           (old-object-limit (length old-objects)))
      (declare (fixnum next-id new-size old-object-limit))
      (when (> new-size old-object-limit)
        (let ((optimized-old-objects (make-vector new-size (id-index-tombstone))))
          (replace optimized-old-objects old-objects)
          ;; TODO DESIGN - no allowance for negative IDs, which id-index-old-object-id-p checks for
          (dohash (id object new-objects)
            (setf (aref optimized-old-objects id) object))
          (setf (idix-old-objects id-index) optimized-old-objects)
          (clrhash new-objects))))))

(defun id-index-values (id-index)
  "[Cyc] Returns a list of the values of ID-INDEX."
  (let ((values nil)
        (old-objects (id-index-old-objects id-index))
        (new-objects (id-index-new-objects id-index)))
    ;; The original loop did strange things with tombstone values that seem degenerate.
    ;; There are a ton of do-* macros declared, so probably an effect from those.
    (loop for id from 0 below (min (length old-objects)
                                   (id-index-count id-index))
       do (let ((object (aref old-objects id)))
            (unless (id-index-tombstone-p object)
              (push object values))))
    (dohash (id object new-objects)
      (declare (ignore id))
      (unless (id-index-tombstone-p object)
        (push object values)))
    (nreverse values)))

