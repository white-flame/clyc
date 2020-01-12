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


;; cachemtr = cache-metrics
;; mcache = metered-cache

(defun-inline cache-strategy-gather-metrics (strategy)
  "[Cyc] Allocate a metrics object and then begin using it to gather metrics."
  (cache-strategy-object-gather-metrics strategy (new-cache-metrics)))

(defun-inline cache-strategy-p (object)
  "[Cyc] Determine whether the object is a cache strategy or not."
  (cache-strategy-object-p object))

(defun-inline cache-strategy-note-cache-miss (strategy)
  "[Cyc] If metrics are being kept, then note the cache miss.
Otherwise, the operation is a NO-OP."
  (when (cache-strategy-keeps-metrics-p strategy)
    (cache-metrics-note-miss (cache-strategy-get-metrics strategy)))
  strategy)

(defun-inline cache-metrics-note-miss (metrics)
  "[Cyc] Update the metrics to reflect that the cache lookup resulted in a miss."
  (incf (cachemtr-miss-count metrics)))

(defun-inline cache-strategy-note-reference (strategy object)
  "[Cyc] Inform the cache strategy tracking system that the object mentioned was referenced. Objects that are not currently being tracked will be ignored."
  (when (cache-strategy-tracked? strategy object)
    (cache-strategy-object-note-reference strategy object))
  strategy)

(defun-inline cache-strategy-tracked? (strategy object)
  "[Cyc] Determine whether this object is currently being tracked in the cache."
  (cache-strategy-object-tracked? strategy object))

(defgeneric cache-strategy-object-tracked? (strategy object)
  (:documentation "[Cyc] By default, we do not know whether the object is tracked."))

(defun-inline cache-strategy-mcache-object-note-reference (mcache object)
  (cache-set-without-values (mcache-cache mcache) object object))

(defun-inline cache-strategy-mcache-object-tracked? (mcache object)
  (cache-contains-key-p (mcache-cache mcache) object))

(defgeneric cache-strategy-object-note-reference (strategy object)
  (:documentation "[Cyc] By default, we do not know how to note a reference."))

(defun-inline cache-strategy-mcache-object-keeps-metrics-p (mcache)
  (cache-metrics-p (mcache-metrics mcache)))

(defun-inline cache-strategy-mcache-object-get-metrics (mcache)
  (mcache-metrics mcache))

(defun cache-strategy-note-cache-hits (strategy several)
  "[Cyc] If metrics are being kept, then note the cache hit.
Otherwise the operation is a NO-OP."
  (when (cache-strategy-keeps-metrics-p strategy)
    (let ((metrics (cache-strategy-get-metrics strategy)))
      (dotimes (i several)
        (cache-metrics-note-hit metrics))))
  strategy)

(defun-inline cache-strategy-note-cache-hit (strategy)
  "[Cyc] If metrics are being kept, then note the cache hit.
Otherwise the operation is a NO-OP."
  (when (cache-strategy-keeps-metrics-p strategy)
    (cache-metrics-note-hit (cache-strategy-get-metrics strategy)))
  strategy)

(defun-inline cache-strategy-get-metrics (strategy)
  "[Cyc] Return CACHE-METRICS-P or NIL if no metrics are being gathered."
  (and (cache-strategy-keeps-metrics-p strategy)
       (cache-strategy-object-get-metrics strategy)))

(defgeneric cache-strategy-object-get-metrics (strategy)
  (:documentation "[Cyc] By default we do not know how to get the metrics."))

(defun-inline cache-metrics-note-hit (metrics)
  "[Cyc] Update the metrics to reflect that the cache lookup resulted in a hit."
  (incf (cachemtr-hit-count metrics))
  metrics)

(defun-inline cache-strategy-keeps-metrics-p (strategy)
  "[Cyc] Determine if the cache strategy is currently gathering metrics or not."
  (cache-strategy-object-keeps-metrics-p strategy))

(defgeneric cache-strategy-object-keeps-metrics-p (strategy)
  (:documentation "[Cyc] By default, we do not know whether the strategy is keeping metrics or not."))

(defun-inline cache-strategy-track (strategy object)
  "[Cyc] Track this object in the cache. If CACHE-STRATEGY-CACHE-FULL-P is TRUE, then select an object to untrack and return that no longer tracked object; otherwise return the newly tracked object."
  (cache-strategy-object-track strategy object))

(defgeneric cache-strategy-object-track (strategy object)
  (:documentation "[Cyc] By default, we do not know how to track an object."))

(defgeneric cache-strategy-object-p (object)
  (:documentation "[Cyc] By default, nothing is a cache strategy object."))





(defstruct (cache-metrics (:conc-name "CACHEMTR-"))
  hit-count
  miss-count)

(defun new-cache-metrics ()
  "[Cyc] Create a new empty cache metrics infrastructure."
  (let ((metrics (make-cache-metrics)))
    (reset-cache-metrics-counts metrics)
    metrics))

(defun reset-cache-metrics-counts (metrics &optional (hits 0) (misses 0))
  "[Cyc] Resaet the counts in the cache metrics."
  (setf (cachemtr-hit-count metrics) hits)
  (setf (cachemtr-miss-count metrics) misses)
  metrics)

(defconstant *cfasl-wide-opcode-cache-metrics* 129)



;; TODO DESIGN - this is a mess for dispatching through behavior.  Put a slot for the metrics right on the cache objects, or even both metrics slots right in it.
(defstruct (metered-cache (:conc-name "MCACHE-"))
  cache
  metrics)

(defun cache-strategy-mcache-object-track (mcache object)
  (multiple-value-bind (key value dropped-p)
      (cache-set-return-dropped (mcache-cache mcache) object object)
    (declare (ignore value))
    (if dropped-p key object)))

(defun-inline new-metered-cache (cache)
  "[Cyc] Allocate the new metered cache, leaving the metrics slot empty for now."
  (make-metered-cache :cache cache))

(defun-inline new-metered-preallocated-cache (capacity &optional (test #'eql))
  "[Cyc] Allocate a new metered cache for a pre-allocated cache of the specified capacity and test-type."
  (new-metered-cache (new-preallocated-cache capacity test)))

(defun-inline cache-strategy-mcache-object-gather-metrics (mcache metrics)
  (setf (mcache-metrics mcache) metrics)
  mcache)

(defstruct recording-cache-strategy-facade
  cache-strategy
  records
  timestamper)




;; Moved these below the metered-cache defstruct
(defmethod cache-strategy-object-keeps-metrics-p ((strategy metered-cache))
  (cache-strategy-mcache-object-keeps-metrics-p strategy))

(defmethod cache-strategy-object-tracked? ((strategy metered-cache) object)
  (cache-strategy-mcache-object-tracked? strategy object))

(defmethod cache-strategy-object-note-reference ((strategy metered-cache) object)
  (cache-strategy-mcache-object-note-reference strategy object))

(defmethod cache-strategy-object-get-metrics ((strategy metered-cache))
  (cache-strategy-mcache-object-get-metrics strategy))

(defmethod cache-strategy-object-track ((strategy metered-cache) object)
  (cache-strategy-mcache-object-track strategy object))

(defmethod cache-strategy-object-p ((object metered-cache))
  "[Cyc] METERED-CACHE-P has a cache strategy implementation."
  t)

(defmethod cache-strategy-object-gather-metrics ((strategy metered-cache) metrics)
  (cache-strategy-mcache-object-gather-metrics strategy metrics))
