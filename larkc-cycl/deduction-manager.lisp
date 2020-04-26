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

(defglobal *deduction-content-manager* :uninitialized
    "[Cyc] The KB object manager for deductions.")

(deflexical *deduction-lru-size-percentage* 8
    "[Cyc] This is a guess based on *ASSERTION-LRU-SIZE-PERCENTAGE*.")

(defun setup-deduction-content-tagle (size exact?)
  (setf *deduction-content-manager* (new-kb-object-manager "deduction" size *deduction-lru-size-percentage*
                                                           #'load-deduction-def-from-cache exact? )))

(defun clear-deduction-content-table ()
  (clear-kb-object-content-table *deduction-content-manager*))

(defun* cached-deduction-count () (:inline t)
  "[Cyc] Return the number of deductions whose content is cached in memory."
  (cached-kb-object-count *deduction-content-manager*))

(defun* deduction-content-completely-cached? () (:inline t)
  (= (deduction-count) (cached-deduction-count)))

(defun* lookup-deduction-content (id) (:inline t)
  (lookup-kb-object-content *deduction-content-manager* id))

(defun* register-deduction-content (id deduction-content) (:inline t)
  "[Cyc] Note that ID will be used as the id for DEDUCTION-CONTENT."
  (register-kb-object-content *deduction-content-manager* id deduction-content))

(defun* deregister-deduction-content (id) (:inline t)
  "[Cyc] Note that ID is not in use as a NART id, i.e. points to no hl-formula."
  (deregister-kb-object-content *deduction-content-manager* id))

(defun* mark-deduction-content-as-muted (id) (:inline t)
  (mark-kb-object-content-as-muted *deduction-content-manager* id))

(defun swap-out-all-pristine-deductions ()
  (swap-out-all-pristine-kb-objects-int *deduction-content-manager*))

(defun initialize-deduction-hl-store-cache ()
  (initialize-kb-object-hl-store-cache *deduction-content-manager*
                                       "deduction"
                                       "deduction-index"))
