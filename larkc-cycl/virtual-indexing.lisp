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

;; TODO - this file is all about the :overlap indexing style, which was quite missing-larkc in kb-indexing.lisp

(defparameter *index-overlap-enabled?* t)

(defun good-term-for-overlap-index-p (object)
  (or (indexed-term-p object)
      (and (not (consp object))
           (subl-atomic-term-p object))))

(deflexical *lookup-overlap-watermark* 50
  "[Cyc] The minimum cost, below which it's not even worth it to try the overlap method.")

(deflexical *overlap-index-expense-multiplier* 7
  "[Cyc] Overlap index is this many times more expensive than other methods, due to additional consing and multiple passes. This value was determined by experiments in August 2005 and should be periodically updated.")

(defun lookup-should-use-index-overlap? (formula &optional best-count)
  "[Cyc] Return T iff overlap will probably yield a better-focused search than any other kind of indexing.
BEST-COUNT: The smallest count of assertions indexed via the best other index."
  (cond
    ((not *index-overlap-enabled?*) nil)
    ((and best-count
          (< best-count *lookup-overlap-watermark*))
     nil)
    ((too-few-terms-for-index-overlap? formula) nil)
    ((and best-count
          (all-mts-are-relevant?))
     (missing-larkc 6920))
    (t t)))

(defun too-few-terms-for-index-overlap? (formula)
  (cond
    ((contains-subformula-p formula) nil)
    ((not (indexed-term-p (formula-operator formula))) t)
    (t (let ((num-indexed-args 0)
             (args (formula-args formula :ignore)))
         (dolist (arg args)
           (when (good-term-for-overlap-index-p arg)
             (incf num-indexed-args)))
         (<= num-indexed-args 1)))))
