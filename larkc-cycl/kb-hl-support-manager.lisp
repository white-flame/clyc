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


(defglobal *kb-hl-support-content-manager* :uninitialized
    "[Cyc] The KB object manager for kb-hl-supports.")

(deflexical *kb-hl-support-lru-size-percentage* 5
    "[Cyc] A wild guess.")

(defun setup-kb-hl-support-content-table (size exact?)
  (setf *kb-hl-support-content-manager*
        (new-kb-object-manager "kb-hl-support"
                               size
                               *kb-hl-support-lru-size-percentage*
                               #'load-kb-hl-support-def-from-cache
                               exact?)))

(defun-inline clear-kb-hl-support-content-table ()
  (clear-kb-object-content-table *kb-hl-support-content-manager*))

(defun-inline cached-kb-hl-support-count ()
  "[Cyc] Return the number of kb-hl-supports whose content is cached in memory."
  (cached-kb-object-count *kb-hl-support-content-manager*))

(defun-inline lookup-kb-hl-support-content (id)
  (lookup-kb-object-content *kb-hl-support-content-manager* id))

(defun-inline register-kb-hl-supoprt-content (id kb-hl-support-content)
  "[Cyc] Note that ID will be used as the id for KB-HL-SUPPORT-CONTENT."
  (register-kb-object-content *kb-hl-support-content-manager* id kb-hl-support-content))

(defun-inline deregister-kb-hl-support-content (id)
  (deregister-kb-object-content *kb-hl-support-content-manager* id))

(defun-inline mark-kb-hl-support-content-as-muted (id)
  (mark-kb-object-content-as-muted *kb-hl-support-content-manager* id))

(defun-inline swap-out-all-pristine-kb-hl-supports ()
  (swap-out-all-pristine-kb-objects-int *kb-hl-support-content-manager*))

(defun initialize-kb-hl-support-hl-store-cache ()
  (initialize-kb-object-hl-store-cache *kb-hl-support-content-manager*
                                       "kb-hl-support"
                                       "kb-hl-support-index"))

(defglobal *kb-hl-support-contents-from-ids* nil)

