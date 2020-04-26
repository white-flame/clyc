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


(defglobal *constant-index-manager* :uninitialized
  "[Cyc] The KB object manager for constant indices.")

(deflexical *constant-index-lru-size-percentage* 16
  "[Cyc] Based on arete experiments, only 16% of all constants are touched during normal inference, so we'll make a conservative guess that every one of those touched the constant's index.")

(defun setup-constant-index-table (size exact?)
  (setf *constant-index-manager* (new-kb-object-manager "constant-index"
                                                        size
                                                        *constant-index-lru-size-percentage*
                                                        #'load-constant-index-from-cache
                                                        exact?)))

(defun* cached-constant-index-count () (:inline t)
  "[Cyc] Return the number of constant-indices whose content is cached in memory."
  (cached-kb-object-count *constant-index-manager*))

(defun* lookup-constant-index (id) (:inline t)
  (lookup-kb-object-content *constant-index-manager* id))

(defun register-constant-index (id constant-index)
  "[Cyc] Note that ID will be used as the id for CONSTANT-INDEX."
  (register-kb-object-content *constant-index-manager* id constant-index))

(defun deregister-constant-index (id)
  (deregister-kb-object-content *constant-index-manager* id))

(deflexical *permanently-cached-constant-indices* (list #$isa
                                                        #$genls)
    "[Cyc] We never want to swap out the indices of these constants.")

(defun mark-constant-index-as-permanently-cached (id)
  "[Cyc] Firstly make sure it's swapped in. Then make sure it won't ever get swapped out."
  (lookup-constant-index id)
  (mark-constant-index-as-muted id))

(defun swap-out-all-pristine-constant-indices ()
  (swap-out-all-pristine-kb-objects-int *constant-index-manager*))

(defun initialize-constant-index-hl-store-cache ()
  (prog1 (initialize-kb-object-hl-store-cache *constant-index-manager*
                                              "indices"
                                              "indices-index")
    (dolist (constant *permanently-cached-constant-indices*)
      (mark-constant-index-as-permanently-cached (constant-suid constant)))))
