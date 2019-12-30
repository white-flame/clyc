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


(defstruct (kb-object-manager (:conc-name "KBOM-"))
  name
  content-lock
  lru-size-percentage
  content-table
  usage-table
  lru-information
  file-vector
  id-threshold
  load-func
  meter-swap-time?
  swap-time
  dummy1
  dummy2
  dummy3)

(defun new-kb-object-manager (name size lru-size-percentage load-func exact-size?)
  (let ((kbom (make-kb-object-manager :name name
                                      :content-lock (bt:make-lock
                                                     (format nil "~a content manager lock" name))
                                      :lru-size-percentage lru-size-percentage
                                      :usage-table :uninitialized
                                      :file-vector nil
                                      :id-threshold 0
                                      :load-func load-func
                                      :meter-swap-time? nil
                                      :swap-time nil)))
    ;; TODO - only the first 3 slots were initialized before this was called. Ensure that this doesn't trample any of the later ones
    (setup-kb-object-content-table kbom size exact-size?)
    kbom))

(defun setup-kb-object-table (kbom size exact?)
  (declare (ignore exact?))
  (bt:with-lock-held ((kbom-content-lock kbom))
    (unless (id-index-p (kbom-content-table kbom))
      (setf (kbom-content-table kbom) (new-id-index size 0)))
    (setup-kb-object-content-support kbom nil size)))

;; INCOMPLETE
