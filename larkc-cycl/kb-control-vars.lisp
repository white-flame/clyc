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


;; TODO - this file should probably go up pretty early in the load order

(defglobal *backchain-forbidden-unless-arg-chosen* #$backchainForbiddenWhenUnboundInArg)
(deflexical *kb-features* nil
  "[Cyc] The list of KB feature symbols")
(defglobal *reformulator-kb-loaded?* nil)
(defglobal *sksi-kb-loaded?* nil)
(defglobal *paraphrase-kb-loaded?* nil)
(defglobal *nl-kb-loaded?* nil)
(defglobal *lexicon-kb-loaded?* nil)
(defglobal *rtp-kb-loaded?* nil)
(defglobal *rkf-kb-loaded?* nil)
(defglobal *thesaurus-kb-loaded?* nil)
(defglobal *quant-kb-loaded?* nil)
(defglobal *time-kb-loaded?* nil)
(defglobal *date-kb-loaded?* nil)
(defglobal *cyc-task-scheduler-kb-loaded?* nil)
(defglobal *wordnet-kb-loaded?* nil)
(defglobal *cyc-secure-kb-loaded?* nil)
(defglobal *planner-kb-loaded?* nil)
(defglobal *kct-kb-loaded?* nil)

(defun-inline kct-kb-loaded-p ()
  "[Cyc] Is the portion of the KB necessary for KCTs loaded? There is currently no code analogue of this KB feature."
  *kct-kb-loaded?*)

(defun-inline unset-kct-kb-loaded ()
  (setf *kct-kb-loaded?* nil))

(defparameter *forward-inference-enabled?* t)
(defparameter *forward-propagate-from-negation* nil
  "[Cyc] Do we allow forward propagation from negated gafs.")
(defparameter *forward-propagate-to-negations* nil
  "[Cyc] Do we allow conclusion of negated fags in forward propagation.")
(defparameter *within-forward-inference?* nil)

(defun-inline within-forward-inference? ()
  *within-forward-inference?*)

(defparameter *within-assertion-forward-propagation?* nil)
(defparameter *relax-type-restrictions-for-nats* nil)
(defparameter *forward-inference-time-cutoff* nil
  "[Cyc] Amount of time we are willing to spend on each forward inference. NIL means unlimited time.")
(defparameter *forward-inference-allowed-rules* :all
  "[Cyc] When a value other than :ALL, the list of the only rules allowed for forward inference.")
(defparameter *forward-inference-environment* (create-queue)
  "[Cyc] Environment used for performing forward inference.")
(defparameter *recursive-ist-justifications?* t
  "[Cyc] Do we give full justifications for ist gafs?")
(defparameter *recording-hl-transcript-operations?* nil
  "[Cyc] Whether the HL storage modules should store the operations they perform")
(defparameter *hl-transcript-operations* nil
  "[Cyc] A list of the operations noted by the HL storage modules")

  ;; TODO - these are probably part of a def* macro for the variables.  Odd that flag names instead of values are put into a *features* value, but that's what the Java code seems to do.

(toplevel
  (dolist (item '(*reformulator-kb-loaded?*
                  *sksi-kb-loaded?*
                  *paraphrase-kb-loaded?*
                  *nl-kb-loaded?*
                  *lexicon-kb-loaded?*
                  *rtp-kb-loaded?*
                  *rkf-kb-loaded?*
                  *thesauraus-kb-loaded?*
                  *quant-kb-loaded?*
                  *time-kb-loaded?*
                  *date-kb-loaded?*
                  *cyc-task-scheduler-kb-loaded?*
                  *wordnet-kb-loaded?*
                  *cyc-secure-kb-loaded?*
                  *planner-kb-loaded?*
                  *kct-kb-loaded?*
                  *forward-inference-environment*))
    (pushnew item *kb-features*)))
