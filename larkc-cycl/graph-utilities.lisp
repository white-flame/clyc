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


(defun categorize-nodes-via-links (nodes links &optional (test #'eql))
  "[Cyc] Given a list of NODES, and a list of LINKS which relate these nodes, categorize the nodes into island-groups, where each group consists of a list of connected nodes, and a list of links involving these nodes.
Returns 3 values:
CONNECTED-GROUPS -- list of island groups with multiple nodes.
ISOLATED-GROUPS  -- list of island groups with single nodes and some links.
NAKED-GROUPS     -- list of island groups with single nodes and no links."
  (let ((all-island-groups nil))
    (dolist (node nodes)
      (push (list (list node) nil) all-island-groups))
    (dolist (link links)
      (let* ((link-nodes (extract-link-nodes link nodes test))
             (island-groups (extract-island-groups link-nodes all-island-groups test))
             (island-group nil))
        (if (singleton? island-groups)
            (setf island-group (car island-groups))
            (let ((merged-island-group island-groups))
              (setf all-island-groups (set-difference all-island-groups island-groups))
              (push merged-island-group all-island-groups)
              (setf island-group merged-island-group)))
        (destructuring-bind (group-nodes group-links) island-group
          (declare (ignore group-nodes))
          (setf (nth 1 island-group) (cons link group-links)))))
    (let ((connected-groups nil)
          (isolated-groups nil)
          (naked-groups nil))
      (dolist (island-group all-island-groups)
        (destructuring-bind (group-nodes group-links) island-group
          (if (singleton? group-nodes)
              (if group-links
                  (push island-group isolated-groups)
                  (push island-group naked-groups))
              (push island-group connected-groups))))
      (when (singleton? nodes)
        (setf connected-groups isolated-groups)
        (setf isolated-groups nil))
      (setf connected-groups (sort-connected-groups connected-groups nodes links test))
      (setf isolated-groups (sort-isolated-groups isolated-groups nodes links test))
      (setf naked-groups (sort-naked-groups naked-groups nodes test))
      (values connected-groups isolated-groups naked-groups))))

;; TODO - I don't think SubL has inline lambda closures, so it uses toplevel functions and these sorts of private variable bindings, which is kinda terrible.  Refactor this pattern wherever it occurs.
(defparameter *extract-link-nodes-nodes* nil)
(defparameter *extract-link-nodes-test* nil)

(defun extract-link-nodes (link candidate-nodes &optional (test #'eql))
  "[Cyc] Extract a subset of CANDIDATE-NODES mentioned in LINK."
  (let ((*extract-link-nodes-nodes* candidate-nodes)
        (*extract-link-nodes-test* test))
    (tree-gather link #'extract-link-nodes-int test)))

(defun extract-link-nodes-int (object)
  (member? object *extract-link-nodes-nodes* :test *extract-link-nodes-test*))

(defun extract-island-groups (nodes candidate-island-groups &optional (test #'eql))
  "[Cyc] Extract a subset of CANDIDATE-ISLAND-GROUPS whose nodes intersect NODES."
  (let ((island-groups nil))
    (dolist (node nodes island-groups)
      (pushnew (island-group-for-node node candidate-island-groups test)
               island-groups))))

(defun island-group-for-node (node island-groups &optional (test #'eql))
  "[Cyc] Return the associated island-group for NODE from ISLAND-GROUPS."
  (dolist (island-group island-groups)
    (multiple-value-bind (nodes links) island-group
      (declare (ignore links))
      (when (member? node nodes :test test)
        (return island-group)))))

(defun merge-island-groups (island-groups)
  "[Cyc] Merge every island-group in ISLAND-GROUPS into a single resulting group."
  (let ((merged-group-nodes nil)
        (merged-group-links nil))
    (dolist (island-group island-groups (list merged-group-nodes merged-group-links))
      (destructuring-bind (group-nodes group-links) island-group
        (dolist (node group-nodes)
          (push node merged-group-nodes))
        (dolist (link group-links)
          (push link merged-group-links))))))

(defun sort-connected-groups (connected-groups all-nodes all-links &optional (test #'eql))
  (dolist (island-group connected-groups
           ;; Exit form
           (stable-sort (sort-via-position connected-groups all-nodes test #'caar)
                        #'< :key #'length-first))
    (destructuring-bind (nodes links) island-group
      (setf (nth 0 island-group) (sort-via-position nodes all-nodes #'eql))
      (setf (nth 1 island-group) (sort-via-position links all-links #'eql)))))

(defun* length-first (object) (:inline t)
  (length (first object)))

(defun sort-isolated-groups (isolated-groups all-nodes all-links &optional (test #'eql))
  "[Cyc] Sort ISOLATED-GROUPS based on ALL-NODES and ALL-LINKS."
  (dolist (island-group isolated-groups)
    (destructuring-bind (nodes links) island-group
      (declare (ignore nodes))
      (setf (nth 1 island-group) (sort-via-position links all-links #'eql))))
  (sort-via-position isolated-groups all-nodes test #'caar))

(defun sort-naked-groups (naked-groups all-nodes &optional (test #'eql))
  (sort-via-position naked-groups all-nodes test #'caar))


