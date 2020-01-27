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


#|
A fvector is saved over 2 files:

 The index file holds 4-byte offset pointers into the data stream.
 The data stream is a concatenation of presumably arbitrary-length binary items.

This seems limited to holding a bit over 4GiB of data.

Reads are performed by having this position the data-stream to the selected index, and then using external cfasl stuff on the data-stream.
Unfortunately, writing seems to be missing-larkc.

 Presumably, data items are mutated by appending the end of the data stream if they don't fit in the prior value's footprint, then updating the index entry to point to it.  However, this would require eventual vacuuming.

Hmm, there doesn't seem to be any place in the struct or index file to hold the footprint of the prior object, so it would actually have to be incompatibly reworked to add that in somehow.  But an append-only system would function without issue.

|#


(defstruct fvector
  data-stream
  index-stream)

(defun-inline get-file-vector-data-stream (fvector)
  (fvector-data-stream fvector))

(defun new-fvector (data-stream index-stream)
  (make-fvector :data-stream data-stream
                :index-stream index-stream))

(defun file-vector-p (object)
  "[Cyc] Return T iff OBJECT is a FILE-VECTOR datastructure."
  (fvector-p object))

(defun new-file-vector (data-filename index-filename &optional (direction :input))
  (with-open-file (data-stream data-filename :element-type '(unsigned-byte 8)
                               :direction direction)
    (with-open-file (index-stream index-filename :element-type '(unsigned-byte 8)
                                  :direction direction)
      (create-file-vector data-stream index-stream))))

(defun create-file-vector (data-stream index-stream)
  (new-fvector data-stream index-stream))

(defun close-file-vector (fvector)
  "[Cyc] Close the streams associated wihth the file vector under question."
  (close (fvector-data-stream fvector))
  (close (fvector-index-stream fvector))
  fvector)

(defun file-vector-length (fvector)
  "[Cyc] Return the FIXNUMP number of entries in the file vector."
  (fvector-raw-byte-size-to-length (file-length (fvector-index-stream fvector))))

(defun file-vector-length-from-index (index-filename)
  "[Cyc] A helper function that allows getting the index without allocating the file-vector object."
  (unless (probe-file index-filename)
    (error "Invalid index filename ~a." index-filename))
  (with-open-file (stream index-filename)
    (fvector-raw-byte-size-to-length (file-length stream))))

(defun-inline fvector-raw-byte-size-to-length (bytes)
  (declare (fixnum bytes))
  (ash bytes -2))

(defun position-file-vector (fvector &optional index)
  "[Cyc] Position the data stream of the file vector. If an INDEX is supplied, the data stream is positioned to the data offset stored in the index file for that nth entry. If no index is supplied, it is positioned to th enext value in the index-stream (e.g. in the case of a loop)."
  (let ((data-stream (fvector-data-stream fvector)))
    (set-file-position data-stream (read-file-vector-index-entry fvector index))
    data-stream))

(defun read-file-vector-index-entry (fvector &optional index)
  "[Cyc] Fetch a specific entry from the file vector index. move first to the specified INDEX if provided.
Returns the NON-NEGATIVE-INTEGER-P file position in the data stream."
  (when index
    (position-file-vector fvector index))
  ;; Read big endian, the opposite order of CFASL-INPUT-INTEGER
  (read-32bit-be (fvector-index-stream fvector)))



