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


;; TODO DESIGN - These utilities abstract Dictionary & hashtable. But since we eliminated dictionary and only work on hashtables, this is moot.  Deprecate all of this and wrap into hash-table-utilities.
;; TODO - add file-level deprecation to the FILE form.

(symbol-mapping map-p hash-table-p
                map-size hash-table-count
                map-empty-p hash-table-empty-p
                map-get-without-values map-get)

;; Stuff that doesn't have a direction function that exactly matches the params
(defun* map-put (map key value) (:inline t)
  (setf (gethash key map) value))

(defun* map-get (map key default) (:inline t)
  (gethash key map default))

(defun* map-remove (map key) (:inline t)
  (remhash key map))
