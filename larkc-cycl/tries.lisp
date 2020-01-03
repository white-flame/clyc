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


;; TODO - test the entire trie interface heavily.  Some things might have gotten lost in translation

;; TODO DESIGN - still don't know how a multi-trie should store its stuff, everything with it is missing-larkc

;; TODO - move to closure & LABELS
(defparameter *trie-objects* nil
  "[Cyc] Special variable used when gathering objects from a TRIE")


;; DESIGN - these features are effectively missing-larkc
(defparameter *trie-relevant-marks* :all
  "[Cyc] Special variable used while walking over relevant portions of a multi-trie.")
(defparameter *trie-ancestor-tracking* nil
  "[Cyc] Special varaible used to control whether we bother to track the ancestor path in a trie.")

(defstruct trie
  name
  top-node
  unique
  case-sensitive
  entry-test-func  ;; TODO - nothing ever seems to set this
  multi
  multi-keys
  multi-key-func)

(deflexical *trie-free-list* nil
  "[Cyc] Free list for TRIE objects")
(deflexical *trie-free-lock* (bt:make-lock "TRIE resource lock")
  "[Cyc] Lock for TRIE object free list")

(defun init-trie (trie)
  "[Cyc] Initialize a TRIE for use"
  ;; micro-optimized, because why not
  (setf (trie-name trie)
        (setf (trie-top-node trie)
              (setf (trie-unique trie)
                    (setf (trie-case-sensitive trie)
                          (setf (trie-entry-test-func trie)
                                (setf (trie-multi trie)
                                      (setf (trie-multi-keys trie)
                                            (setf (trie-multi-key-func trie) nil))))))))
  trie)

(defun get-trie ()
  "[Cyc] Get a TRIE from the free list, or make a new one if needed."
  (if (not *structure-resourcing-enabled*)
      (if *structure-resourcing-make-static*
          ;; TODO - enable structure resourcing
          (missing-larkc 12494)
          (init-trie (make-trie)))
      (bt:with-lock-held (*trie-free-lock*)
        ;; TODO - seems like DEFINE-RESOURCE stuff is excluded.  Would be reasonable to reimplement, as there's no magic to free lists.
        (missing-larkc 12492))))

  ;; DESIGN - trie nodes, they all seem to be cons cells
  ;;  (key . subnodes)
  ;;  ((key . ?) . subnodes) - where do these come from?  multi-tries?
  ;;  (:end . data) - data is a list for non-unique tries, or plain dotted cdr for unique



  ;; TODO - when is the CAR ever a list?  only chars ever get placed as the key slot

(declaim (inline trie-node-key))
(defun trie-node-key (node)
  (let ((car (car node)))
    (if (atom car) car (car car))))

(declaim (inline trie-node-data))
(defun trie-node-data (node)
  (cdr node))

(declaim (inline trie-node-subnodes))
(defun trie-node-subnodes (node)
  (cdr node))

(declaim (inline trie-leaf-node-p))
(defun trie-leaf-node-p (node)
  (eq (trie-node-key node) :end))

(declaim (inline new-trie-terminal-node))
(defun new-trie-terminal-node (object unique?)
  (if unique?
      (cons :end object)
      (list :end object)))

(defun new-trie-nonterminal-node (char case-sensitive)
  (cons (if case-sensitive
            char
            (char-downcase char))
        nil))

;; TODO - since these are used in tight search loops, they really should have the case-sensitivity decision hoisted

(defun trie-key-= (key1 key2 case-sensitive)
  (if case-sensitive
      (char= key1 key2)))

(defun trie-key-< (key1 key2 case-sensitive)
  (if case-sensitive
      (char< key1 key2)
      (char-lessp key1 key2)))

(defun add-trie-subnode (node subnode case-sensitive)
  (do* ((data (trie-node-subnodes node))
        (subkey (trie-node-key subnode))
        (back node next)
        (next data (cdr next))
        (key (trie-node-key (car next)) (trie-node-key (car next))))
       ((or (null next) (eq subkey :end) (and (not (eq key :end))
                                              (not (trie-key-< key subkey case-sensitive))))
        (rplacd back (cons subnode next)))))

(defun create-trie (unique &optional name (case-sensitive t) (test #'eql))
  "[Cyc] Return a new TRIE datastructure."
  (let ((trie (get-trie)))
    ;; Manually initialize a potentially reused trie
    (setf (trie-name trie) name)
    (setf (trie-top-node trie) (list* :top nil)) ;; TODO - weird construction of this list.  must be from a macro?
    (setf (trie-unique trie) unique)
    (setf (trie-case-sensitive trie) case-sensitive)
    (setf (trie-entry-test-func trie) test)
    (setf (trie-multi trie) nil)
    trie))

(defun trie-insert (trie string &optional (object string) (start 0) (end (length string)))
  "[Cyc] Add index to OBJECT via STRING in TRIE. START and END delimit the relvant portion of STRING."
  (declare (trie trie)
           (string string)
           (fixnum start end))
  (let ((unique (trie-unique trie))
        (case-sensitive (trie-case-sensitive trie))
        (test (trie-entry-test-func trie))
        (node (trie-top-node trie)))
    (initialize-trie-ancestor-tracking node)
    ;; In each iteration, we look at trie nodes with respect to the next character
    (do (;; This is set in the body of the iteration, so reset it to NIL at the beginning
         (next-node nil nil)
         (index start (1+ index)))
        (;; This exit test means we hit the end of the string successfully
         (= index end)
         
         ;; Here we should be able to add the object as a leaf/terminal under the current node
         (let ((existing-terminal nil))
           (csome (subnode (trie-node-subnodes node) (when (trie-leaf-node-p subnode)
                                                       (setf existing-terminal subnode))))
           (if existing-terminal
               (progn
                 (trie-ancestor-tracking-descend existing-terminal)
                 ;; Error if this is holding a different item but supposed to be unique
                 (if unique
                     (unless (funcall test object (trie-node-data existing-terminal))
                       (error "There is already an object ~s not ~s to ~s in the trie!"
                              (trie-node-data existing-terminal) test object))
                     ;; No uniqueness requirement, pushnew the item along the rest
                     ;; TODO - the original had a (missing-larkc 12506), then did the work anyway?
                     (pushnew object (cdr existing-terminal) :test test)))
               ;; no existing-terminal, create a new one
               (let ((new-terminal (new-trie-terminal-node object unique)))
                 (multi-trie-new-insert-mark trie object)
                 (add-trie-subnode node new-terminal case-sensitive)))
           (finish-trie-ancestor-tracking)
           trie))
      
      ;; This is the DO body
      
      (declare (fixnum index))

      ;; Still more characters to go, traverse a step down the branches
      (let ((ch (char string index)))
        ;; Find the subnode whose key matches our character
        (csome (subnode (trie-node-subnodes node) (let ((subkey (trie-node-key subnode)))
                                                    (when (and (characterp subkey)
                                                               (trie-key-= subkey ch case-sensitive))
                                                      ;; This non-NIL return value will break out of the loop
                                                      (setf next-node subnode)))))
        ;; Here, next-node is the node which matches our key.
        ;; Or NIL if we didn't find one and will make one
        (setf node (or next-node
                       ;; TODO - when we're creating a new node in non-case-sensitive mode, why is it downcasing?
                       ;; I would think it would just preserve case and case sensitivity is more a thing for the querier.
                       (let ((new-node (new-trie-nonterminal-node ch case-sensitive)))
                         (add-trie-subnode node new-node case-sensitive)
                         (setf node new-node))))
        (trie-ancestor-tracking-descend node)))))

(defun trie-remove (trie string &optional (object string) (start 0) (end (length string)))
  "[Cyc] Remove index to OBJECT via STRING in TRIE. START and END delimit the relevant portion of STRING."
  (declare (trie trie)
           (string string)
           (fixnum start end))
  (let ((unique (trie-unique trie))
        (case-sensitive (trie-case-sensitive trie))
        (test (trie-entry-test-func trie))
        (node (trie-top-node trie))
        (last-branching-node nil)
        (last-branch nil))
    (initialize-trie-ancestor-tracking node)
    ;; See trie-insert for more structural comments
    (do ((next-node nil nil)
         (index start (1+ index)))
        ((= index end)
         ;; DO result value body
         
         (let ((end-node nil))
           (csome (subnode (trie-node-subnodes node) (when (trie-leaf-node-p subnode)
                                                       (trie-ancestor-tracking-descend subnode)
                                                       (setf end-node subnode))))
           ;; Bail on no leaf end-node
           (unless end-node
             (finish-trie-ancestor-tracking)
             (return trie))
           (if unique
               ;; See if we're actually removing the intended object
               (when (funcall test object (trie-node-data end-node))
                 (cerror "Remove it anyway" "The object found in trie for ~s is ~s, not ~s"
                         string (trie-node-data end-node)))
               ;; Not unique, deal with a list of entries
               (let* ((old-data (trie-node-data end-node))
                      (new-data (delete object old-data :test test)))
                 (unless (eq old-data new-data)
                   (rplacd end-node new-data))
                 (when new-data
                   ;; Clean up and exit if there are more entries, leaving the node in place
                   (multi-trie-remove-mark trie object)
                   (finish-trie-ancestor-tracking)
                   (return trie))))
           ;; Remove the actual node, as it no longer contains objects
           (multi-trie-remove-mark trie object)
           (cond
             ((rest (trie-node-subnodes node))
              (rplacd node (delete end-node (trie-node-subnodes node) :test #'eq)))
             ((and last-branch last-branching-node)
              ;; TODO - this sets last-branching-node just before returning, but it's just a local variable
              ;;  note also that SBCL whines if you're discarding the return value of DELETE.
              (setf last-branching-node (delete last-branch last-branching-node :test #'eq)))
             (t (missing-larkc 12480)))
           (finish-trie-ancestor-tracking)
           (return trie)))
      
      ;; DO body
      (declare (fixnum index))
      (let ((ch (char string index)))
        (csome (subnode (trie-node-subnodes node) (let ((subkey (trie-node-key subnode)))
                                                    (when (and (characterp subkey)
                                                               (trie-key-= ch subkey case-sensitive))
                                                      (setf next-node subnode)))))
        (if next-node
            (progn
              ;; As we traverse through found nodes, record the parent details for removal
              (when (cdr (trie-node-subnodes node))
                (setf last-branching-node node)
                (setf last-branch next-node))
              (setf node next-node))
            ;; If we didn't find the entry, then silently bail
            (progn
              (finish-trie-ancestor-tracking)
              (return trie)))
        (trie-ancestor-tracking-descend node)))))

(defun trie-exact (trie string &optional case-sensitive? (start 0) end)
  "[Cyc] Return the unique object indexed by STRING in TRIE. If CASe-SENSITIVE? is non-NIL, STRING is compared case-insensitively. START and END determine the relevant portion of STRING"
  (must (trie-unique trie) "TRIE ~s does not have unique entries" trie)
  (setf case-sensitive? (and case-sensitive? (trie-case-sensitive trie)))
  (let ((node (trie-top-node trie))
        (end (or end (length string))))
    (declare (trie trie)
             (string string)
             (fixnum start end))
    (initialize-trie-ancestor-tracking node)
    (do ((next-node nil nil)
         (i start (1+ i)))
        ;; TODO - Original relied on java operator precedence here, not paren nesting. Verify behavior
        ((or (= i end)
             ;; Must have processed at least 1 character and not found the next
             (and (> i 0)
                  (not node)))
         ;; Exit return forms
         (let ((answer nil))
           (when node
             (csome (subnode (trie-node-subnodes node) (when (trie-leaf-node-p subnode)
                                                         (trie-ancestor-tracking-descend subnode)
                                                         (when (trie-relevant-ancestor-path? trie)
                                                           (let ((object (trie-node-data subnode)))
                                                             (when (trie-relevant-object trie object)
                                                               (setf answer object))))))))
           (finish-trie-ancestor-tracking)
           answer))
      ;; DO iteration body
      (let ((char (char string i)))
        (csome (subnode (trie-node-subnodes node) (let ((subkey (trie-node-key subnode)))
                                                    (when (and (characterp subkey)
                                                               (trie-key-= char subkey case-sensitive?))
                                                      (setf next-node subnode)))))
        (setf node next-node)
        (trie-ancestor-tracking-descend node)))))

(defun trie-prefix (trie string &optional case-sensitive? exact-length? (start 0) (end (length string)))
  "[Cyc] Return a list of all objects indexed in TRIE where STRING is a prefix of the index. If CASE-SENSITIVE? is non-NIL, STRING is compared case-insensitively. If EXACT-LENGTH? is non-NIL, then the index must match STRING exactly. START and END determine the relevant portion of STRING."
  (if (or case-sensitive?
          (not (trie-case-sensitive trie)))
      (missing-larkc 12549)
      (trie-prefix-recursive trie string exact-length? start end)))

(defun trie-prefix-recursive (trie string exact-length? start end)
  "[Cyc] Internal to TRIE-PREFIX"
  (declare (trie trie)
           (string string)
           (fixnum start end))
  (let ((answer nil)
        (*trie-objects* nil)
        (top-node (trie-top-node trie)))
    (initialize-trie-ancestor-tracking top-node)
    (when (trie-relevant-ancestor-path? trie)
      (dolist (subnode (trie-node-subnodes top-node))
        (trie-ancestor-tracking-descend subnode)
        (when (trie-relevant-ancestor-path? trie)
          (trie-prefix-recursive-int trie subnode string start end exact-length? (trie-unique trie)))
        (trie-ancestor-tracking-ascend)))
    (setf answer *trie-objects*)
    (finish-trie-ancestor-tracking)
    (nreverse answer)))

(defun trie-prefix-recursive-int (trie node string index stop exact-length? unique?)
  "[Cyc] Internal to TRIE-PREFIX-RECURSIVE"
  (declare (trie trie)
           (string string)
           (fixnum index stop))
  (if (= index stop)
      (if exact-length?
          (when (trie-leaf-node-p node)
            (all-trie-objects-in-leaf-node trie node unique?))
          (missing-larkc 12474))
      (let ((key (trie-node-key node)))
        (when (and (characterp key)
                   (char-equal (char string index) key))
          (dolist (subnode (trie-node-subnodes node))
            (trie-ancestor-tracking-descend subnode)
            (when (trie-relevant-ancestor-path? trie)
              (trie-prefix-recursive-int trie subnode string (1+ index) stop exact-length? unique?))
            (trie-ancestor-tracking-ascend))))))

(defun all-trie-objects-in-leaf-node (trie node unique?)
  (let ((data (trie-node-data node)))
    (if unique?
        (when (trie-relevant-object trie data)
          (push data *trie-objects*))
        (progn
          (setf data (missing-larkc 12550))
          ;;(push data *trie-objects*)
          ))))

;; Unused in larkc code
(defparameter *trie-maximum-search-size* 1000)

(defun new-trie-iterator (trie &optional (forward? t))
  (new-iterator (new-trie-iterator-state trie forward?)
                #'trie-iterator-done
                #'trie-iterator-next
                ;; TODO - this function doesn't exist.  Are iterators effectively nonfunctional?
                'trie-iterator-finalize))

(defun new-trie-iterator-state (trie forward?)
  (vector trie (trie-top-node trie) forward?
          (if (trie-unique trie) nil (create-queue))
          (create-stack)))

(defun trie-iterator-done (state)
  (and (trie-iterator-done-node (aref state 1))
       ;; TODO - because of this, are trie-iterators never used?
       (missing-larkc 12528)))

(defun trie-iterator-done-node (node)
  (not node))

(defun trie-iterator-next (state)
  (let ((trie (aref state 0))
        (node (aref state 1))
        (forward? (aref state 2))
        (queue (aref state 3))
        (stack (aref state 4)))
    (multiple-value-bind (next invalid? new-node)
        (if (queue-p queue)
            (missing-larkc 12530)
            (trie-iterator-next-unique trie node forward? stack))
      (if invalid?
          (progn
            (setf (aref state 1) nil)
            (setf (aref state 3) nil)
            (clear-stack stack))
          (setf (aref state 1) new-node))
      (values next state invalid?))))

(defun trie-iterator-next-unique (trie node forward? stack)
  (let ((next nil)
        (invalid? nil))
    (until (or next invalid?)
      (if (trie-leaf-node-p node)
          (let ((data (trie-node-data node)))
            (when (trie-relevant-object trie data)
              (setf next data)))
          (let ((subnodes (trie-node-subnodes node)))
            (if forward?
                (dolist (subnode (reverse subnodes))
                  (stack-push subnode stack))
                (dolist (subnode subnodes)
                  (stack-push subnode stack)))))
      (setf node (stack-pop stack))
      (when (and (not next)
                 (not node)
                 (stack-empty-p stack))
        (setf invalid? t)))
    (values next invalid? node)))

  ;; TODO - all of this stuff is missing-larkc'd when enabled, so I'm reducing it to nothing

(defparameter *trie-ancestor-tracking-resource* nil)
(defparameter *trie-ancestor-tracking-lock* (bt:make-lock "Ancestor tracking resource"))
(defparameter *trie-ancestor-tracking-vector-size* 100)
(defparameter *trie-ancestor-vector* nil)
(defparameter *trie-ancestor-next* nil)
(defun-ignore initialize-trie-ancestor-tracking (top-node))
(defun-ignore finish-trie-ancestor-tracking ())
(defun-ignore trie-ancestor-tracking-descend (node))
(defun-ignore trie-ancestor-tracking-ascend ())
(defun-ignore trie-relevant-ancestor-path? (trie) t)
(defun-ignore trie-relevant-object (trie object) t)
(defun-ignore multi-trie-new-insert-mark (trie object))
(defun-ignore multi-trie-remove-mark (trie object))


