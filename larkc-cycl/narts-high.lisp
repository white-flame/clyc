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


(defun naut-p (object)
  "[Cyc] Return T iff OBJECT is a datastructure implementing a non-atomic unreified term (NAUT).
By definition, this satisfies CYCL-NAT-P but not NART-P."
  (and (possibly-naut-p object)
       (cycl-nat-p object)))

(defun find-nart (nart-hl-formula)
  "[Cyc] Return the nart implementing NART-HL-FORMULA, or NIL if none is present.
Subsitutions for existing sub-NARTs are performed."
  (let ((nart (nart-substitute nart-hl-formula)))
    (and (nart-p nart)
         nart)))

'(defun remove-dependent-narts (fort)
  "[Cyc] Remove all current NARTs which are functions of FORT."
  (dolist (dependent (dependent-narts fort))
    (missing-larkc 30883)))

(defun nart-expand (object)
  "[Cyc] Recursively expand all NARTs in OBJECT into their EL forms (NAUTs)."
  (if (tree-find-if #'nart-p object)
      (transform object #'nart-p)
      object))

(defun nart-substitute (object)
  "[Cyc] Substitute into OBJECT as many NARTs as possible.
If the entire formula can be converted to a NART, it will.
Returns OBJECT itself if no substitutions can be made."
  (if (possibly-naut-p object)
      (nart-substitute-recursive object)
      object))

(defun nart-substitute-recursive (tree)
  (if (subl-escape-p tree)
      tree
      (let ((result tree))
        (if (contains-nat-formula-as-element? tree)
            (let ((new-tree (copy-list tree)))
              (do ((list new-tree))
                  ((atom list)
                   (setf result new-tree))
                (let ((arg (car list)))
                  (when (nat-formula-p arg)
                    (let ((sub-nart (nart-substitute-recursive arg)))
                      (when sub-nart
                        (rplaca list sub-nart)))))))
            (setf result tree))
        (let ((nart (nart-lookup result)))
          (if (nart-p nart)
              nart
              result)))))

(defun contains-nat-formula-as-element? (list)
  "[Cyc] Return T iff LIST contains at least one element that could be reified as a nart. It does not consider whether LIST itself could be reified as a nart, and it does not look deeper than one level of nesting."
  (do ((rest list (cdr rest)))
      ((atom list))
    (when (nat-formula-p (car rest))
      (return t))))

(defun nart-lookup (nart-hl-formula)
  "[Cyc] Return the NART implementing NART-HL-FORMULA, or NIL if none is present.
No substitutions for sub-NARTs are performed."
  (if (and (not *bootstrapping-kb*)
           (or (not (reifiable-functor? (nat-functor nart-hl-formula)))
               (not (fully-bound-p nart-hl-formula))))
      nil
      (missing-larkc 871)))

(defparameter *nart-dump-id-table* nil)

(defun find-nart-by-dump-id (dump-id)
  "[Cyc] Return the NART with DUMP-ID during a KB load."
  (find-nart-by-id dump-id))

