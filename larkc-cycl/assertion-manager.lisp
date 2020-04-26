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

(defglobal *arete-assertions-touched* (make-hash-table :test #'eq))

(defun arete-note-assertion-touched (assertion)
  (possibly-note-kb-access-assertion assertion)
  (when *arete-log-kb-touches?*
    (dictionary-increment *arete-assertions-touched* assertion)))

(defglobal *assertion-content-manager* :uninitialized
    "[Cyc] The KB object manager for assertions.")

(deflexical *assertion-lru-size-percentage* 16
    "[Cyc] Based on arete experiments, only 16% of all assertions are needed for normal inference.")

(defun setup-assertion-content-table (size exact?)
  (setf *assertion-content-manager* (new-kb-object-manager "assertion" size *assertion-lru-size-percentage* #'load-assertion-def-from-cache exact?)))

(defun clear-assertion-content-table ()
  (clear-kb-object-content-table *assertion-content-manager*))

(defun* cached-assertion-count () (:inline t)
  "[Cyc] Return the number of assertions whose content is cached in memory."
  (cached-kb-object-count *assertion-content-manager*))

(defun* lookup-assertion-content (id) (:inline t)
  (arete-note-assertion-touched (find-assertion-by-id id))
  (lookup-kb-object-content *assertion-content-manager* id))

(defun* register-assertion-content (id assertion-content) (:inline t)
  "[Cyc] Note that ID will be used as the id for ASSERTION-CONTENT."
  (register-kb-object-content *assertion-content-manager* id assertion-content))

(defun* deregister-assertion-content (id) (:inline t)
  "[Cyc] Note that ID is not in use as an ASSERTION-CONTENT id."
  (deregister-kb-object-content *assertion-content-manager* id))

(defun* mark-assertion-content-as-muted (id) (:inline t)
  (mark-kb-object-content-as-muted *assertion-content-manager* id))

(defun* swap-out-all-pristine-assertions () (:inline t)
  (swap-out-all-pristine-kb-objects-int *assertion-content-manager*))

(defun initialize-assertion-hl-store-cache ()
  (initialize-kb-object-hl-store-cache *assertion-content-manager*
                                       "assertion"
                                       "assertion-index"))
