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


(defun kb-create-asserted-argument-with-tv (assertion tv)
  (kb-create-asserted-argument assertion
                               (tv-truth tv)
                               (tv-strength tv)))

(define-hl-modifier kb-create-asserted-argument (assertion truth strength)
    "[Cyc] Create an asserted argument for ASSERTION from TRUTH and STRENGTH, and hook up all the indexing between them."
    nil
  (let* ((tv (tv-from-truth-strength truth strength))
         (asserted-argument (create-asserted-argument assertion tv)))
    (add-new-assertion-argument assertion asserted-argument)
    asserted-argument))

(define-hl-modifier kb-remove-asserted-argument (assertion asserted-argument)
    "[Cyc] Remove ASSERTED-ARGUMENT for ASSERTION."
    nil
  (set-assertion-asserted-by assertion nil)
  (set-assertion-asserted-when assertion nil)
  (set-assertion-asserted-why assertion nil)
  (set-assertion-asserted-second assertion nil)
  (remove-assertion-argument assertion asserted-argument)
  (kb-remove-asserted-argument-internal asserted-argument))

(define-hl-modifier hl-assert-bookkeeping-binary-gaf (pred arg1 arg2 mt)
    "[Cyc] Assert (PRED ARG1 ARG2) in MT to the bookkeeping store."
    nil
  (assert-bookkeeping-binary-gaf pred arg1 arg2 mt))
