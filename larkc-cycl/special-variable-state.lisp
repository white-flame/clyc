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


;; This snapshots the values of special variables

(defstruct (special-variable-state (:conc-name "SVS-"))
  variables
  values)

(defun new-special-variable-state (special-variables)
  "[Cyc] Return a new SPECIAL-VARIABLE-STATE-P based on the current values for SPECIAL-VARIABLES."
  (let ((svs (make-special-variable-state :variables (copy-list special-variables)
                                          :values (make-list (length special-variables)))))
    (update-special-variable-state svs)))

;; TODO - weird naming convention, is this a macroexpansion?
(defun-inline with-special-variable-state-variables (svs)
  (svs-variables svs))

(defun-inline with-special-variable-state-values (svs)
  (svs-values svs))

(defun update-special-variable-state (svs)
  "[Cyc] Update SPECIAL-VARIABLE-STATE SVS with the current binding values for all its special-variables."
  (update-special-variable-value-list (svs-values svs) (svs-variables-svs))
  svs)

(defun update-special-variable-value-list (values variables)
  "Performs a destructive update on the VALUES list, based on VARIABLES' values."
  (loop
     for variable in variables
     for rest-values on values
     do (rplaca rest-values (symbol-value variable)))
  values)

