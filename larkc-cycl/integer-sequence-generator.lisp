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


(defstruct (integer-sequence-generator (:conc-name "ISG-"))
  lock
  current
  start
  limit
  delta)

(defun new-integer-sequence-generator (&optional (start 0) limit (delta 1))
  (must-not (zerop delta) "DELTA must not be zero")
  (make-integer-sequence-generator :lock (bt:make-lock "ISG")
                                   :current start
                                   :start start
                                   :limit limit
                                   :delta delta))

(defun integer-sequence-generator-reset (isg)
  "[Cyc] Reset an Integer Sequence Generator to its original state."
  (bt:with-lock-held ((isg-lock isg))
    (setf (isg-current isg) (isg-start isg))))

(defconstant *cfasl-wide-opcode-isg* 130)

;; TODO DESIGN - This is a complete conversion from the .java version, but doesn't seem enough to be useful.
;; Other code creates these, but they don't ever seem to be stepped.
