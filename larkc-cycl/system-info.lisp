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


(deflexical *cyc-home-directory* #P"./"
  "[Cyc] The pathname for the cyc home directory (suitable for use with MERGE-PATHNAMES)")

(defglobal *available-cyc-features* nil)

;; Check for feature, that will always be nil?
(defun cyc-opencyc-feature ()
  nil)

(defun cyc-revision-string ()
  "[Cyc] Returns the current Cyc revision numbers expressed as a period-delimited string"
  *cyc-revision-string*)

(defun cyc-revision-numbers ()
  "[Cyc] Returns a list of the current Cyc revision numbers"
  *cyc-revision-numbers*)

(defglobal *cycl-start-time* nil)

(defun reset-cycl-start-time (&optional (universal-time (get-universal-time)))
  (setf *cycl-start-time* universal-time))

(defglobal *subl-initial-continuation* nil
  "[Cyc] Backpointer for the original SubL initial continuation.")


