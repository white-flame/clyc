#|
  Copyright (c) 2019-2020 White Flame

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

;; TODO - Ugh, this file is quite important, but came with nearly zero comments.

;; kb-hl-support = simple id
;; hl-support
;; hl-support-content

(defun-inline find-kb-hl-support (hl-support)
  (or (find-kb-hl-support-during-creation hl-support)
      (lookup-kb-hl-support hl-support)))

(defun-inline find-kb-hl-support-by-id (id)
  (lookup-kb-hl-support-by-id id))

(defun-inline 9find-kb-hl-supports-mentioning-term (term)
  (lookup-kb-hl-supports-mentioning-term term))

(defun-inline kb-hl-support-count ()
  (if *kb-hl-supports-from-ids*
      (id-index-count *kb-hl-supports-from-ids*)
      0))

(defun-inline kb-hl-support-id (kb-hl-support)
  (kb-hl-support-get-id kb-hl-support))

(defun-inline do-kb-hl-support-dependents-helper (kb-hl-support)
  (kb-hl-support-content-get-dependents (kb-hl-support-content kb-hl-support)))

(defun kb-hl-support-hl-support (kb-hl-support)
  (let* ((content (kb-hl-support-content kb-hl-support))
         (argument (kb-hl-support-content-get-argument content)))
    (cond
      ((deduction-p argument) (deduction-assertion argument))
      ((hl-support-p argument) argument))))

(defun kb-hl-support-sentence (kb-hl-support)
  (let ((hl-support (kb-hl-support-hl-support kb-hl-support)))
    (when (hl-support-p hl-support)
      (hl-support-sentence hl-support))))

(defun kb-hl-support-tv (kb-hl-support)
  (let ((hl-support (kb-hl-support-hl-support kb-hl-support)))
    (when (hl-support-p hl-support)
      (hl-support-tv hl-support))))

(defun-inline find-or-possibly-create-kb-hl-support (hl-support)
  (or (find-kb-hl-support hl-support)
      (possibly-create-kb-hl-support hl-support)))

(defstruct (kb-hl-support (:conc-name "KB-HLS-"))
  id)

(defmethod sxhash ((object kb-hl-support))
  (or (kb-hls-id object)
      787))

(defun new-kb-hl-support (id)
  (make-kb-hl-support :id id))

(defun free-kb-hl-support (kb-hl-support)
  (setf (kb-hls-id kb-hl-support) nil))

(defstruct (kb-hl-support-content (:conc-name "KB-HLSC-"))
  argument
  dependents)

(defun new-kb-hl-support-content ()
  (make-kb-hl-support-content))

(defun free-kb-hl-support-content (kb-hl-support-content)
  (setf (kb-hlsc-argument kb-hl-support-content) nil)
  (setf (kb-hlsc-dependents kb-hl-support-content) nil))

;; TODO - spurious accessors?
(defun-inline kb-hl-support-content-get-argument (kb-hl-support-content)
  (kb-hlsc-argument kb-hl-support-content))

(defun-inline kb-hl-support-content-get-dependents (kb-hl-support-content)
  (kb-hlsc-dependents kb-hl-support-content))

(defun-inline kb-hl-support-content-set-argument (kb-hl-support-content deduction)
  (setf (kb-hlsc-argument kb-hl-support-content) deduction))

(defun-inline kb-hl-support-content-set-dependents (kb-hl-support-content dependents)
  (setf (kb-hlsc-dependents kb-hl-support-content) dependents))

(defun make-kb-hl-support-shell (id)
  (let ((kb-hl-support (new-kb-hl-support id)))
    (register-kb-hl-support-id id kb-hl-support)
    kb-hl-support))

(defun-inline kb-hl-support-content (kb-hl-support)
  (lookup-kb-hl-support-content (kb-hl-support-get-id kb-hl-support)))

(defun kb-hl-support-add-dependent (kb-hl-support deduction)
  (let* ((content (kb-hl-support-content kb-hl-support))
         (old-dependents (kb-hl-support-content-get-dependents content))
         (new-dependents (set-contents-add deduction old-dependents)))
    (kb-hl-support-content-set-dependents content new-dependents)
    (mark-kb-hl-support-content-as-muted (kb-hl-support-id kb-hl-support))))

(defun kb-hl-support-remove-dependent (kb-hl-support deduction)
  (let* ((content (kb-hl-support-content kb-hl-support))
         (old-dependents (kb-hl-support-content-get-dependents content))
         (new-dependents (set-contents-delete deduction old-dependents)))
    (kb-hl-support-content-set-dependents content new-dependents)
    (mark-kb-hl-support-content-as-muted (kb-hl-support-id kb-hl-support))))

(defun remove-kb-hl-support (kb-hl-support)
  (let* ((content (kb-hl-support-content kb-hl-support))
         (argument (kb-hl-support-content-get-argument content)))
    (when (valid-deduction? argument)
      (remove-deduction argument))
    (free-kb-hl-support kb-hl-support)
    (free-kb-hl-support-content content)))

(defun-inline hl-justify-for-kb-hl-support (hl-support)
  (remove hl-support (hl-support-justify hl-support) :test #'equal))

(defun valid-kb-hl-support? (object &optional robust?)
  (and (valid-kb-hl-support-handle? object)
       (or (not robust?)
           (missing-larkc 11080))))

(defun valid-kb-hl-support-handle? (object)
  (and (kb-hl-support-p object)
       (kb-hl-support-handle-valid? object)))

(defun-inline kb-hl-support-handle-valid? (kb-hl-support)
  ;; TODO - assuming the not-integerp is just integer or nil
  (kb-hl-support-get-id kb-hl-support))

(defun tms-remove-kb-hl-supports-mentioning-term (term)
  (let ((removed-count 0))
    (dolist (kb-hl-support (find-kb-hl-supports-mentioning-term term))
      (when (valid-support? kb-hl-support)
        (tms-remove-kb-hl-support kb-hl-support))
      (incf removed-count))
    removed-count))

(defun setup-kb-hl-support-tables (size exact?)
  (setup-kb-hl-support-id-tables size exact?)
  (setup-kb-hl-support-index-table))

(defun finalize-kb-hl-supports (&optional max-kb-hl-support-id)
  (set-next-kb-hl-support-id max-kb-hl-support-id)
  (unless max-kb-hl-support-id
    (missing-larkc 11056)))

(defglobal *kb-hl-supports-from-ids* nil)

(defun-inline do-kb-hl-supports-table ()
  *kb-hl-supports-from-ids*)

(defun setup-kb-hl-support-id-tables (size exact?)
  (unless *kb-hl-supports-from-ids*
    (setf *kb-hl-supports-from-ids* (new-id-index size 0)))
  (setup-kb-hl-support-content-table size exact?))

(defun-inline lookup-kb-hl-support-by-id (id)
  (id-index-lookup *kb-hl-supports-from-ids* id))

(defun-inline next-kb-hl-support-id ()
  (id-index-next-id *kb-hl-supports-from-ids*))

(defun-inline register-kb-hl-support-id (id kb-hl-support)
  (id-index-enter *kb-hl-supports-from-ids* id kb-hl-support))

(defun-inline deregister-kb-hl-support-id (id)
  (id-index-remove *kb-hl-supports-from-ids* id))

(defun set-next-kb-hl-support-id (&optional max-kb-hl-support-id)
  (let ((max -1))
    (if max-kb-hl-support-id
        (setf max max-kb-hl-support-id)
        (do-id-index (id kb-hl-support (do-kb-hl-supports-table)
                         :progress-message "Determining maximum KB HL support")
          (setf max (max max (kb-hl-support-id kb-hl-support)))))
    (let ((next-id (1+ max)))
      (set-id-index-next-id *kb-hl-supports-from-ids* next-id)
      next-id)))

(defun increment-next-kb-hl-support-id ()
  (let ((id (next-kb-hl-support-id)))
    (set-id-index-next-id *kb-hl-supports-from-ids* (1+ id))))

(defun clear-kb-hl-support-id-tables ()
  (clear-id-index *kb-hl-supports-from-ids*)
  (clear-kb-hl-support-content-table))

(defglobal *kb-hl-support-index* nil)
(deflexical *kb-hl-support-index-lock* (bt:make-lock "KB HL support indexing lock"))
(deflexical *kb-hl-support-idnex-unindexed-terms* (list #$isa
                                                        #$DefaultSemanticsForStringFn
                                                        #$evaluate
                                                        #$genlInverse
                                                        #$genlPreds
                                                        #$genls
                                                        #$ist
                                                        #$ist-Asserted
                                                        #$SubLStringConcatenationFn
                                                        #$TheList
                                                        #$TheSet))

(defun-inline kb-hl-support-index-unindexed-term? (term)
  (member term *kb-hl-support-index-unindexed-terms* :test #'equal))

(defun kb-hl-support-index-indexed-term-p (term)
  (and (indexed-term-p term)
       (not (kb-hl-support-unindexed-term? term))))

(defun kb-hl-support-index-indexed-terms (sentence)
  (let ((terms (expression-gather sentence #'indexed-term-p nil #'equal)))
    ;; TODO - weird logic between these 2 list searches; is it right?
    (if (find-if-not #'kb-hl-support-index-unindexed-term? terms)
        (remove-if #'kb-hl-support-index-unindexed-term? terms)
        terms)))

(defun setup-kb-hl-support-index-table ()
  (unless *kb-hl-support-index*
    (setf *kb-hl-support-index* (make-hash-table :test #'eq))
    t))

(defun lookup-kb-hl-support (hl-support)
  (let ((support-sets nil))
    (destructuring-bind (module sentence mt tv) hl-support
      (bt:with-lock-held (*kb-hl-support-index-lock*)
        (when-let* ((mt-index (gethash module *kb-hl-support-index*))
                    (tv-index (gethash mt mt-index))
                    (term-index (gethash tv tv-index)))
          (let ((indexed-terms (kb-hl-support-index-indexed-terms sentence))
                (done? nil))
            (csome (term indexed-terms done?)
              (let ((support-set (gethash term term-index)))
                (cond
                  ((set-contents-empty? support-set) 
                   (setf support-sets nil)
                   (setf done? t))

                  ((set-contents-singleton? support-set)
                   (setf support-sets (list support-set))
                   (setf done? t))

                  (t (push support-set support-sets)))))))
        (when support-sets
          (let ((candidate-kb-hl-supports (if (singleton? support-sets)
                                              (car support-sets)
                                              (set-intersection support-sets #'eq)))
                (kb-hl-support nil))
            (do-set (candidate-kb-hl-support candidate-kb-hl-supports kb-hl-support)
              (let ((candidate-sentence (kb-hl-support-sentence candidate-kb-hl-support)))
                (when (equal candidate-sentence sentence)
                  (setf kb-hl-support candidate-kb-hl-support))))
            kb-hl-support))))))

(defun lookup-kb-hl-supports-mentioning-term (term)
  (let ((sentence-kb-hl-supports (lookup-kb-hl-supports-mentioning-term-in-sentence term))
        (mt-kb-hl-supports (lookup-kb-hl-supports-mentioning-term-in-mt term)))
    ;; TODO - using set-contents APIs which should transition to just set-*
    (set-contents-element-list (set-union (list sentence-kb-hl-supports mt-kb-hl-supports)
                                          #'eq))))

(defun lookup-hl-supports-mentioning-term-in-sentence (term)
  (if (kb-hl-support-index-indexed-term-p term)
      (lookup-kb-hl-supports-mentioning-indexed-term-in-sentence term)
      (missing-larkc 11055)))

(defun lookup-kb-hl-supports-mentioning-indexed-term-in-sentence (term)
  (let ((support-sets nil))
    (bt:with-lock-held (*kb-hl-support-index-lock*)
      (dohash (key mt-index *kb-hl-support-index*)
        (dohash (key tv-index mt-index)
          (dohash (key term-index tv-index)
            (when-let ((support-set (gethash term term-index)))
              (unless (hash-table-empty-p support-set)
                (push support-set support-sets)))))))
    (set-union support-sets #'eq)))

(defun lookup-kb-hl-supports-mentioning-term-in-mt (term)
  (let ((support-sets nil))
    (bt:with-lock-held (*kb-hl-support-index-lock*)
      (dohash (key mt-index *kb-hl-support-index*)
        (dohash (mt tv-index mt-index)
          (when (simple-tree-find-via-equal? term mt)
            (dohash (key term-index tv-index)
              (dohash (key support-set term-index)
                (push support-set support-sets)))))))
    (set-union support-sets #'eq)))

(defun index-kb-hl-support (kb-hl-support hl-support)
  (destructuring-bind (module sentence mt tv) hl-support
    (bt:with-lock-held (*kb-hl-support-index-lock*)
      ;; TODO - original did dictionary-p in the UNLESS forms, I'm assuming it's dictionary-p or nil which will be faster
      (let ((mt-index (gethash module *kb-hl-support-index*)))
        (unless mt-index
          (setf mt-index (make-hash-table :test #'equal))
          (setf (gethash module *kb-hl-support-index*) mt-index))
        (let ((tv-index (gethash mt mt-index)))
          (unless tv-index
            (setf tv-index (make-hash-table :test #'eq))
            (setf (gethash mt mt-index) tv-index))
          (let ((term-index (gethash tv tv-index)))
            (unless term-index
              (setf term-index (make-hash-table :test #'equal))
              (setf (gethash tv tv-index) term-index))
            (let ((indexed-terms (kb-hl-support-index-indexed-terms sentence)))
              (dolist (term indexed-terms)
                ;; TODO - this probably can be optimized because the hashtable is edited in place and doesn't need to be setf'd back like alists do.
                (let* ((old-supports (gethash term term-index))
                       (new-supports (set-add kb-hl-support old-supports)))
                  (setf (gethash term term-index) new-supports))))))))))

(defun unindex-kb-hl-support (kb-hl-support &optional robust?)
  (if robust?
      (missing-larkc 11077)
      (let ((hl-support (kb-hl-support-hl-support kb-hl-support)))
        (if (hl-support-p hl-support)
            (unindex-kb-hl-support-with-hl-support kb-hl-support hl-support)
            (missing-larkc 11078)))))

(defun unindex-kb-hl-support-with-hl-support (kb-hl-support hl-support)
  (destructuring-bind (module sentence mt tv) hl-support
    (bt:with-lock-held (*kb-hl-support-index-lock*)
      ;; TODO - assuming the java dictionary-p is testing for non-nil
      (when-let* ((mt-index (gethash module *kb-hl-support-index*))
                  (tv-index (gethash mt mt-index))
                  (term-index (gethash tv tv-index))
                  (indexed-terms (kb-hl-support-index-indexed-terms sentence)))
        (dolist (term indexed-terms)
          (let ((supports (gethash term term-index)))
            (set-remove kb-hl-support supports)
            ;; TODO - the ordering of this cleanup seems to be necessarily trickle-up, while the original did the testing unconditionally.
            (when (set-empty? supports)
              (remhash term term-index)
              (when (hash-table-empty-p term-index)
                (remhash tv tv-index)
                (when (hash-table-empty-p tv-index)
                  (remhash mt mt-index)
                  (when (hash-table-empty-p mt-index)
                    (remhash module *kb-hl-support-index*)))))))))))

(defun-inline clear-kb-hl-support-index ()
  (clrhash *kb-hl-support-index*))

(defglobal *kb-hl-supports-being-created* nil)

(defun note-kb-hl-support-creation-started (hl-support kb-hl-support)
  (unless *kb-hl-supports-being-created*
    (setf *kb-hl-supports-being-created* (make-hash-table :test #'equal)))
  (setf (gethash hl-support *kb-hl-supports-being-created*) kb-hl-support))

(defun note-kb-hl-support-creation-complete (hl-support)
  (when *kb-hl-supports-being-created*
    (remhash hl-support *kb-hl-supports-being-created*)))

(defun find-kb-hl-support-during-creation (hl-support)
  (when *kb-hl-supports-being-created*
    (gethash hl-support *kb-hl-supports-being-created*)))

(defun create-kb-hl-support (hl-support justification)
  (let* ((id (next-kb-hl-support-id))
         (kb-hl-support (new-kb-hl-support id))
         (kb-hl-support-content (new-kb-hl-support-content)))
    (note-kb-hl-support-creation-started hl-support kb-hl-support)
    (increment-next-kb-hl-support-id)
    (register-kb-hl-support-id id kb-hl-support)
    (register-kb-hl-support-content id kb-hl-support-content)
    (let* ((canon-just (canonicalize-supports justification t))
           (deduction (create-deduction-for-hl-support hl-support canon-just)))
      (kb-hl-support-content-set-argument kb-hl-support-content deduction))
    (index-kb-hl-support kb-hl-support hl-support)
    (note-kb-hl-support-creation-complete hl-support)
    kb-hl-support))

(defun destroy-kb-hl-support (kb-hl-support)
  (unindex-kb-hl-support kb-hl-support)
  (let ((id (kb-hl-support-id kb-hl-support)))
    (remove-kb-hl-support kb-hl-support)
    (deregister-kb-hl-support-id id)
    (deregister-kb-hl-support-content id)))

(defun free-all-kb-hl-support ()
  (clear-kb-hl-support-index)
  (do-id-index (id kb-hl-support (do-kb-hl-supports-table)
                   :progress-message "Freeing KB HL supports")
    (let ((content (kb-hl-support-content kb-hl-support)))
      (free-kb-hl-support kb-hl-support)
      (free-kb-hl-support-content content)))
  (clear-kb-hl-support-id-tables))

(defparameter *unreify-kb-hl-supports?* nil)

(defun possibly-unreify-kb-hl-supports (justification)
  (if *unreify-kb-hl-supports?*
      (missing-larkc 11079)
      justification))

(defparameter *tms-kb-hl-support-queue* nil)

(defun-inline enqueue-kb-hl-supports-for-tms? ()
  ;; TODO - assuming it's nil or queue-p
  *tms-kb-hl-support-queue*)

(defun process-tms-kb-hl-support-queue ()
  (until (queue-empty-p *tms-kb-hl-support-queue*)
    (let ((kb-hl-support (dequeue *tms-kb-hl-support-queue*)))
      (when (kb-hl-support-id kb-hl-support)
        (missing-larkc 11060)))))

(defun tms-remove-kb-hl-support (kb-hl-support)
  (do-set (deduction (do-kb-hl-support-dependents-helper kb-hl-support))
    (missing-larkc 12446)
    ;; can't declare ignore as this &body isn't at the top of the scope.  Could add another field to do-set for declarations, though.
    deduction)
  (destroy-kb-hl-support kb-hl-support))

(defun create-sample-invalid-kb-hl-support ()
  "[Cyc] Create a sample invalid KB HL support."
  (make-kb-hl-support))

(defparameter *kb-hl-support-dump-id-table* nil)

(defun-inline find-kb-hl-support-by-dump-id (dump-id)
  (find-kb-hl-support-by-id dump-id))

(defun load-kb-hl-support-content (kb-hl-support stream)
  (let* ((id (kb-hl-support-id kb-hl-support))
         (argument (cfasl-input stream nil))
         (dependents (cfasl-input stream nil))
         ;; TODO - no path to directly pass in slot values on construction through this
         (content (new-kb-hl-support-content)))
    (kb-hl-support-content-set-argument content argument)
    (kb-hl-support-content-set-dependents content dependents)
    (register-kb-hl-support-content id content)
    id))

(defun load-kb-hl-support-indexing-int (filename)
  (setf *kb-hl-support-index* (cfasl-load filename)))
