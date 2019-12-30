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



(defglobal *constant-completion-table* (create-trie t "Constant Completion Table")
  "[Cyc] Table for indexing constants by the string for their name.")

(defparameter *require-valid-constants* t)

(defun constant-shell-from-name (string)
  "[Cyc] Return a constant or constant shell whose name exactly matches STRING."
  (declare (string string))
  (trie-exact *constant-completion-table* string t 0 nil))

(defun kb-constant-complete-exact-internal (string start end)
  (let ((answer (trie-exact *constant-completion-table* string t start end)))
    (unless (and (constant-p answer)
                 *require-valid-constants*
                 (not (valid-constant-handle-p answer)))
      answer)))

(defun kb-constant-complete-internal (prefix case-sensitive? exact-length? start end)
  (let ((answer (trie-prefix *constant-completion-table* prefix case-sensitive? exact-length? start end)))
    (if *require-valid-constants*
        (delete-if #'invalid-constant-handle answer)
        answer)))

(defun add-constant-to-completions (constant string)
  "[Cyc] Add CONSTANT to the completions table under the name STRING."
  (declare (constant constant)
           (string string))
  (trie-insert *constant-completion-table* string constant)
  constant)

(defun remove-constant-from-completions (constant string)
  (declare (constant constant)
           (string string))
  (trie-remove *constant-completion-table* string constant)
  constant)

(defun kb-new-constant-completion-iterator-internal (&optional (forward? t))
  (new-trie-iterator *constant-completion-table* forward?))
