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

(defglobal *unrepresented-term-index-manager* :uninitialized
    "[Cyc] The KB object manager for unrepresented-term indices.")

(deflexical *unrepresented-term-index-lru-size-percentage* 10
    "[Cyc] Wild guess.")

(defun setup-unrepresented-term-index-table (size exact?)
  (setf *unrepresented-term-index-manager* (new-kb-object-manager "unrepresented-term-index" size
                                                                  *unrepresented-term-index-lru-size-percentage*
                                                                  #'load-unrepresented-term-index-from-cache exact?)))

(defun-inline clear-unrepresented-term-index-table ()
  (clear-kb-object-content-table *unrepresented-term-index-manager*))

(defun-inline cached-unrepresented-term-index-count ()
  "[Cyc] Return the number of unrepresented-term-indices whose content is cached in memory."
  (cached-kb-object-count *unrepresented-term-index-manager*))

(defun-inline lookup-unrepresented-term-index (id)
  (lookup-kb-object-content *unrepresented-term-index-manager* id))

(defun-inline register-unrepresented-term-index (id unrepresented-term-index)
  "[Cyc] Note that ID will be used as the id for UNREPRESENTED-TERM-INDEX."
  (register-kb-object-content *unrepresented-term-index-manager* id unrepresented-term-index))

(defun-inline deregister-unrepresented-term-index (id)
  "[Cyc] Note that ID is not in use as an UNREPRESENTED-TERM-INDEX id."
  (deregister-kb-object-content *unrepresented-term-index-manager* id))

(defun-inline mark-unrepresented-term-index-as-muted (id)
  (mark-kb-object-content-as-muted *unrepresented-term-index-manager* id))

(defun-inline swap-out-all-pristine-unrepresented-term-indices ()
  (swap-out-all-pristine-kb-objects-int *unrepresented-term-index-manager*))

(defun initialize-unrepresented-term-index-hl-store-cache ()
  (initialize-kb-object-hl-store-cache *unrepresented-term-index-manager*
                                       "unrepresented-term-indices"
                                       "unrepresented-term-indices-index"))
