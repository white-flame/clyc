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

(defun canonicalize-hlmt (mt)
  "[Cyc] Returns the mt that MT denotes, in a canonical form. Will reify the monad if it is a closed nat. Returns NIL if MT is ill-formed."
  (check-type mt #'possibly-mt-p)
  (multiple-value-bind (mt dummy-mt) (safe-precanonicalizations mt #$BaseKB)
    (declare (ignore dummy-mt))
    (unless mt
      (setf mt (reduce-hlmt mt (within-query?)))
      (setf mt (reify-when-closed-naut mt))
      (when (and (within-forward-inference?)
                 (not (within-wff?))
                 (possibly-naut-p (hlmt-monad-mt mt))
                 (tree-find-if #'skolemize-forward? (hlmt-monad-mt mt)))
        (setf mt (canonicalize-hlmt-int mt))))
    mt))

(defun canonicalize-hlmt-int (hlmt)
  (unless (mt-space-naut-p hlmt)
    (missing-larkc 12283))
  (unless (and (within-assert?)
               (hlmt-with-anytime-psc-p hlmt))
    hlmt))
