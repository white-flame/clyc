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


(deflexical *sbhl-directions* '(:predicate
                                :inverse
                                :link) 
  "[Cyc] Valid SBHL link directions.")

(deflexical *sbhl-directed-directions* '(:predicate
                                         :inverse)
  "[Cyc] Valid SBHL link directions for SBHL directed links.")

(deflexical *sbhl-forward-directed-direction* :predicate
  "[Cyc] Valid SBHL link direction for forward direction of sbhl-directed-links.") 

(deflexical *sbhl-forward-directed-direction-as-list* '(:predicate)
  "[Cyc] Valid SBHL link direction for forward direction of sbhl-directed-links as a list.")

(deflexical *sbhl-backward-directed-direction* :inverse
  "[Cyc] Valid SBHL link direction for backward direction of sbhl-directed-links.")

(deflexical *sbhl-backward-directed-direction-as-list* '(:inverse)
  "[Cyc] Valid SBHL link direction for backward direction of sbhl-directed-links as a list.")

(deflexical *sbhl-undirected-direction* :link
  "[Cyc] Valid SBHL link direction for undirected links.")

(deflexical *sbhl-undirected-direction-as-list* '(:link)
  "[Cyc] Valid SBHL link direction for undirected links in list form.")

(defun sbhl-directed-direction-p (direction)
  "[Cyc] Is DIRECTION a member of *SBHL-DIRECTED-DIRECTIONS*?"
  (or (eq direction *sbhl-forward-directed-direction*)
      (eq direction *sbhl-backward-directed-direction*)))

(defun* get-sbhl-directed-directions () (:inline t)
  "[Cyc] *sbhl-directed-directions*"
  *sbhl-directed-directions*)

(defun* get-sbhl-forward-directed-direction () (:inline t)
  "[Cyc] The keyword for the forward direction of *SBHL-DIRECTED-DIRECTIONS*."
  *sbhl-forward-directed-direction*)

(defun* sbhl-forward-directed-direction-p (direction) (:inline t)
  "[Cyc] Whether DIRECTION is the GET-SBHL-FORWARD-DIRECTED-DIRECTION."
  (eq direction (get-sbhl-forward-directed-direction)))

(defun* get-sbhl-backward-directed-direction () (:inline t)
  "[Cyc] The keyword for the backward direction of *SBHL-DIRECTED-DIRECTIONS*."
  *sbhl-backward-directed-direction*)

(defun* get-sbhl-undirected-direction () (:inline t)
  *sbhl-undirected-direction*)

(defun* get-sbhl-undirected-direction-as-list () (:inline t)
  *sbhl-undirected-direction-as-list*)

(defparameter *sbhl-link-direction* nil
  "[Cyc] Used to specify SBHL-LINK-DIRECTION.")

(defun* get-sbhl-link-direction () (:inline t)
  *sbhl-link-direction*)

(defun get-sbhl-opposite-link-direction ()
  "[Cyc] Return the opposite direction for directed links. Undirected links still return undirected direction."
  (let ((direction (get-sbhl-link-direction)))
    (cond
      ((eq direction (get-sbhl-forward-directed-direction)) (get-sbhl-backward-directed-direction))
      ((eq direction (get-sbhl-backward-directed-direction)) (get-sbhl-forward-directed-direction))
      ((eq direction (get-sbhl-undirected-direction)) (get-sbhl-undirected-direction))
      (t (when (sbhl-object-type-checking-p)
           (missing-larkc 2482))))))

(deflexical *sbhl-link-truth-values* (list #$MonotonicallyTrue
                                           #$DefaultTrue
                                           #$MonotonicallyFalse
                                           #$DefaultFalse)
  "[Cyc] Valid SBHL link truth values.")

(deflexical *sbhl-true-link-truth-values* (list #$MonotonicallyTrue
                                                #$DefaultTrue))

(defun truth-strength-to-sbhl-tv (truth strength)
  "[Cyc] The #$HLTruthValue corresponding to TRUTH and STRENGTH."
  (cond
    ((and (eq truth :true)
          (eq strength :monotonic))
     #$MonotonicallyTrue)
    ((and (eq truth :true)
          (eq strength :default))
     #$DefaultTrue)
    ((and (eq truth :false)
          (eq strength :monotonic))
     #$MonotonicallyFalse)
    ((and (eq truth :false)
          (eq strength :default))
     #$DefaultFalse)
    ((eq truth :unknown)
     :unknown)
    (t (sbhl-error 1 "Could not compute an SBHL tv from ~s and ~s" truth strength))))

(defun support-tv-to-sbhl-tv (support-tv)
  "[Cyc] The #$HLTruthValue corresponding to SUPPORT-TV."
  (let ((truth (tv-truth support-tv))
        (strength (tv-strength support-tv)))
    (truth-strength-to-sbhl-tv truth strength)))

(defun sbhl-link-truth-value-p (truth)
  "[Cyc] Whether TRUTH is a member of *SBHL-LINK-TRUTH-VALUES*."
  (member? truth *sbhl-link-truth-values*))

(defun* sbhl-node-object-p (object) (:inline t)
  "[Cyc] Whether OBJECT has valid structure to be an SBHL node."
  (fort-p object))

(defun* sbhl-mt-object-p (object) (:inline t)
  "[Cyc] Whether OBJECT has valid structure to be an SBHL MT."
  (chlmt-p object))

(defparameter *sbhl-randomize-lists-p* nil
  "[Cyc] Determines whether to iterate over SBHL links in random order.")

(defun* sbhl-randomize-lists-p () (:inline t)
  *sbhl-randomize-lists-p*)

(defparameter *sbhl-link-mt* nil
  "[Cyc] Parameter for link MT, rebound during link iteration.")

(defun* get-sbhl-link-mt ()  (:inline t)
  *sbhl-link-mt*)

(defparameter *sbhl-link-tv* nil
  "[Cyc] Parameter for link tv, rebound during link iteration.")

(defun* get-sbhl-link-tv () (:inline t)
  *sbhl-link-tv*)

(defparameter *sbhl-link-generator* nil
  "[Cyc] Parameter for the saving the information for which function which was used to create the current link-node.")

(deflexical *sbhl-rw-lock* (new-rw-lock "SBHL"))

(defparameter *added-assertion* nil "obsolete")
(defparameter *added-source* nil "obsolete")
