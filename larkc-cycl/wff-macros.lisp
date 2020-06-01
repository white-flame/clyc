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

(defun* within-wff? () (:inline t)
  "[Cyc] Return T iff currently within wff checking."
  *within-wff?*)

;; TODO - where's the defun-memoized that matches this?
(defun* possibly-new-wff-memoization-state () (:inline t)
  (or *wff-memoization-state*
      (new-memoization-state)))

(defun new-wff-special-variable-state (properties)
  (check-wff-properties properties)
  (let ((svs (new-special-variable-state nil)))
    (dohash (indicator data (wff-properties-table))
      (destructuring-bind (var default) data
        (when var
          (let ((desired-value (getf properties indicator default)))
            (unless (equal desired-value default)
              (missing-larkc 31672))))))
    svs))
