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


(defglobal *nart-hl-formula-manager* :uninitialized
    "[Cyc] The KB object manager for nart-hl-formlas.")

(deflexical *nart-hl-formula-lru-size-percentage* 5
    "[Cyc] A wild guess.")

(defun setup-nart-hl-formula-table (size exact?)
  (setf *nart-hl-formula-manager* (new-kb-object-manager "nart-hl-formula"
                                                         size
                                                         *nart-hl-formula-lru-size-percentage*
                                                         #'load-nart-hl-formula-from-cache
                                                         exact?)))

(defun clear-nart-hl-formula-table ()
  (clear-kb-object-content-table *nart-hl-formula-manager*))

(defun* cached-nart-hl-formula-count () (:inline t)
  "[Cyc] Return the number of nart-hl-formulas whose content is cached in memory."
  (cached-kb-object-count *nart-hl-formula-manager*))

(defun nart-hl-formulas-unbuilt? ()
  (unless (zerop (nart-count))
    (kb-object-manager-unbuilt? *nart-hl-formula-manager*)))

(defun swap-out-all-pristine-nart-hl-formulas ()
  (swap-out-all-pristine-kb-objects-int *nart-hl-formula-manager*))

(defun initialize-nart-hl-formula-hl-store-cache ()
  (initialize-kb-object-hl-store-cache *nart-hl-formula-manager*
                                       "nart-hl-formula"
                                       "nart-hl-formula-index"))

(defglobal *nart-hl-formula-table* nil)
