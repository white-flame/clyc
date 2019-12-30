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


(defun cyc-home-filename (subdirectory-list filename &optional extension)
  "[Cyc] Construct a full filename relative to the *CYC-HOME-DIRECTORY*.
SUBDIRECTORY-LIST is the relative subdirectory.
FILENAME and EXTENSION ar eused for the file in the directory."
  (relative-filename *cyc-home-directory*
                     (construct-filename subdirectory-list filename extension t)))

(defun cyc-home-subdirectory (subdirectory-list)
  "[Cyc] Construct a pathname relative to the *CYC-HOME-DIRECTORY*.
SUBDIRECTORY-LIST is the relative subdirectory."
  (relative-filename *cyc-home-directory*
                     (construct-filename subdirectory-list "" nil t)))

(defun relative-filename (directory-string filename &optional extension)
  "[Cyc] Construct a full filename relative to the DIRECTORY-STRING from FILENAME and EXTENSION.
DIRECTORY-STRING should include the appropriate directory separator character at the end."
  (concatenate 'string directory-string (basic-filename filename extension)))

(defun basic-filename (filename &optional extension)
  "[Cyc] Construct a filename with no directory component from FILENAME and EXTENSION."
  (if extension
      (construct-filename nil filename extension t)
      filename))

;; TODO - registers in the red-infrastructure stuff, which got elided
(deflexical *temp-directory* "/tmp/"
    "[Cyc] A directory to which Cyc can write temporary files.")

(defun-inline temp-directory ()
  *temp-directory*)

(deflexical *random-path-chars* "0123456789abcdefghijklmnopqrstuvwxyz")

(defun-inline file-exists? (filename)
  "[Cyc] Like probe-file except does not error on an invalid filename."
  (ignore-errors
    (probe-file filename)
    ;;(uiop:file-exists-p filename)
    ))

(defun guess-path-type (path)
  "[Cyc] Look at the string PATH and determine whether it's for a unix or dos filesystem."
  (cond
    ((find #\/ path) :unix)
    ((find #\\ path) :dos)))

(defun absolute-path? (path)
  "[Cyc] Determine whether PATH is absolute, i.e. on unix if it starts with a /, on msdos if it starts with <drive>: or \\ (e.g. \\serverwindowsshare)."
  (let ((path-length (length path)))
    (unless (zerop path-length)
      (case (guess-path-type path)
        (:unix (char= #\/ (char path 0)))
        (:dos (and (> path-length 2)
                   (or (and (char= #\: (char path 1))
                            (alpha-char-p (char path 0)))
                       (char= #\\ (char path 0)))))))))

(defun path-separator-char (path-type)
  "[Cyc] Return the appropriate separator char for the given PATH-TYPE."
  (case path-type
    (:unix #\/)
    (:dos #\\)
    ;; Yeah, a bit too old methinks
    ;;(:mac #\:)
    ))

(defun deconstruct-path (path)
  "[Cyc] Analyze and deconstruct the path into PATH-LIST leading up to FILENAME (a file, directory, symlink,.) and the PATH-TYPE (currently :UNIX or :DOS). See RECONSTRUCT-PATH.
return 0: PATH-LIST
return 1: FILENAME
return 2: PATH-TYPE"
  (let ((path-type (guess-path-type path)))
    (when path-type
      (let ((path-list (string-tokenize path (list (string (path-separator-char path-type))))))
        (values (butlast path-list)
                (car (last path-list))
                path-type)))))

(defun reconstruct-path (path-list filename &optional (path-type :unix))
  "[Cyc] Reconstruct the deconstructed path. See DECONSTRUCT-PATH."
  (format nil (format nil "~~{~~a~a~~}~~a" (path-separator-char path-type))
          path-list filename))

(defun make-directory-recursive (directory-path &optional force? permissions)
  "[Cyc] Recursively calls MAKE-DIRECTORY to create one by one each directory leading to DIRECTORY-PATH.
FORCE?: If any of the intermediate paths exists as a file, the file is removed and a directory by the same path is created instead. o/w an error is thrown.
PERMISSIONS, stringp: If a directory is created, chmod the newly-created directory with these permissions. Otherwise, they will be created with MAKE-DIRECTORY's default, 0755."
  (let ((physical-path (ensure-physical-pathname directory-path))
        (path-list-so-far nil)
        (chmod-list nil))
    (multiple-value-bind (directories last-directory path-type)
        (deconstruct-path physical-path)
      (setf directories (nadd-to-end last-directory directories))
      (dolist (each-directory directories)
        (let ((each-directory-path (reconstruct-path path-list-so-far each-directory path-type)))
          (setf path-list-so-far (nadd-to-end each-directory path-list-so-far))
          (unless (or (directory-p each-directory-path)
                      (and (eq :dos path-type)
                           (ends-with each-directory-path ":")))
            (when (probe-file each-directory-path)
              (if force?
                  (delete-file each-directory-path)
                  (error "make-directory-recursive: Could not create ~a since it exists as a file."
                         each-directory-path)))
            (make-directory each-directory-path)
            (push each-directory-path chmod-list))))
      (when permissions
        (dolist (chmod-directory chmod-list)
          (chmod chmod-directory permissions)))
      (probe-file (reconstruct-path (butlast path-list-so-far)
                                    (car (last path-list-so-far))
                                    path-type)))))

(defun chmod (pathname permissions-string)
  "[Cyc] PATHNAME string: the full path to the file in question.
PERMISSIONS-STRING: the permissions, as specified to the operating system (currently, only unix/linux)"
  ;; TODO - while sb-posix:chmod exists, it wants a bitmask while Cyc's wants a cmdline string
  (when (external-processes-supported?)
    (system-eval-using-make-os-process-successful? "chmod" (list permissions-string pathname)
                                                   0 *null-input* *null-output*)))
