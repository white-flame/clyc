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

;; TODO - I think I screwed up here, in making a combined interface.  "Memoization" seems to be local to a dynamic state binding, while "Caching" is a singular global cache.  I probably can ignore all the actual implementation guts of these 2 (which spends most its complexity juggling multiple return values, optional params, etc) and just reimplement the exposed interfaces.  But, search for all instances of defun-memoized and check the java to see if it has calls involving "global" in its lazy cache state initialization to distinguish which one is actually being used.


(defconstant *global-caching-lock* (bt:make-lock "global-caching-lock"))
(defglobal *caching-mode-should-monitor* nil
  "[Cyc] Whether to enable cache monitoring. Need to do a retranslation after changing this.")
(defglobal *cache-monitor-hash* (make-hash-table)
  "[Cyc] Hashtable for monitoring all caching calls.")
(defglobal *cache-monitor-failure-hash* (make-hash-table)
  "[Cyc] Hashtable for monitoring cached calls that aren't already cached.")
(defglobal *allow-function-caching-to-be-disabled* nil
  "[Cyc] This indicates that when evaluating the function caching macros, whether to test if the function should be disabled. Not testing for disabled is generally faster but less flexible because then you can no longer dynamically disable function caching. You'll need to do a new translation after setting this for it to take effect.")
(defvar *caching-mode-enabled* :all
  "[Cyc] Caching mode function indicating what's enabled.")
(defvar *caching-mode-disabled* nil
  "[Cyc] Caching mode function indicating what's disabled.")
(defparameter *function-caching-enabled?* t
  "[Cyc] Global caching and memoization are disabled when NIL.")

  ;; NOTE - there were a bunch of fixed-arity sxhash_calc_N calls in here?



(defmacro multi-hash (&rest hashes)
  "Combine fixnum hashes together, using platform-specific hashing. Should not cons."
  (reduce (lambda (x y) `(sb-int::mix ,x (sxhash ,y))) hashes))

;; DESIGN - as far as I can tell, the entire point of this caching-state stuff is to eliminate the need to cons up a list containing the params as a key to pass to gethash each time the function is called.  So the sxhash of the list of args is manually calculated without consing, then collisions are manually checked with the parameter values directly, using the TEST function on each parameter (note that this allows EQ or EQL slot testing within a composite key).  On a cache miss, the key is consed up to register it.  The zero parameter caching scheme is also broken out to its own storage slot.

;; This also stores the multiple-value-list of the calculation, instead of just a single return value.  TODO DESIGN - optimizing a route for single-value returns can save consing & speed

;; If these caches are hit a ton, consing up a list key to pass to gethash would add more GC pressure, and the steps through the code from the java looks shorter than what gethash does,although there is a gethash underlying the key->collisions storage.  The test seems to be part of the generated code (though I don't have a reference macro body to work from), so there's no unnecessary dispatch.  I think it's worth keeping this claptrap instead of just going for gethash for the moment.  As always, profiling needed.

;; Also, the caches can be selectively cleared, so having knowledge of which global variables hold the cahing states of various usages needs to remain exposed.


(defstruct caching-state
  store
  zero-arg-results
  lock
  capacity
  func-symbol
  test
  args-length)

(defun create-caching-state (lock func-symbol func-args-length &optional capacity (test #'eql) (initial-size 0))
  (declare ((integer 0) initial-size)
           ((or null (integer 1)) capacity)
           ((or symbol function) test))
  ;; TODO - don't quite understand this
  (setf test (if (= 1 func-args-length)
                 (coerce test 'function)
                 #'eql))
  (make-caching-state :store (if capacity 
                                 (new-cache capacity test)
                                 (make-hash-table :test test :size initial-size))
                      :lock lock
                      :capacity capacity
                      :func-symbol func-symbol
                      :test test
                      :args-length func-args-length
                      :zero-arg-results :&memoized-item-not-found&))

(defmacro with-caching-state-lock (cs cache-form &optional hash-form)
  "Runs the body in the cs's lock, or plain if its lock is NIL. If the hash-form is omitted, the same form is run regardless of the store type."
  (alexandria:with-gensyms (lock worker)
    (alexandria:once-only (cs)
      `(let ((,lock (caching-state-lock ,cs)))
         ;; Wrap the body in a function that can be called from 2 places
         (flet ((,worker ()
                  ;; Unhygienic but useful value
                  (let ((store (caching-state-store ,cs)))
                    (declare (ignorable store))
                    ,(if hash-form
                         `(if (caching-state-capacity ,cs) ,cache-form ,hash-form)
                         cache-form))))
           (if ,lock
               (bt:with-lock-held (,lock)
                 (,worker))
               (,worker)))))))

(defun caching-state-get-zero-arg-results (caching-state)
  (with-caching-state-lock caching-state
    (caching-state-zero-arg-results caching-state)))

(defun caching-state-set-zero-arg-results (caching-state val)
  (with-caching-state-lock caching-state
    (setf (caching-state-zero-arg-results caching-state) val)))

(defun caching-state-lookup (caching-state key &optional (default :&memoized-item-not-found&))
  (with-caching-state-lock caching-state
    (cache-get-without-values store key default)
    (gethash key store default)))

(defun caching-state-put (caching-state key value)
  (with-caching-state-lock caching-state
    (cache-set store key value)
    (setf (gethash key store) value)))

(defun caching-state-clear (caching-state)
  (with-caching-state-lock caching-state
    (cache-clear store)
    (clrhash store)))

(defun caching-state-enter-multi-key-n (caching-state sxhash collisions results args-list)
  "[Cyc] Cache in CACHING-STATE under hash code SXHASH the fact that ARGS-LIST returns the list of values RESULTS"
  ;; Translates :&memoized-item-not-found& back to an empty list.
  ;; TODO - should we just test for EQ of that instead of LISTP?
  (unless (listp collisions)
    (setf collisions nil))
  (if (not args-list)
      (caching-state-set-zero-arg-results caching-state results)
      ;; TODO - original code directly hit the slot, not using the locking version.
      ;; That doesn't seem safe, can it do so?
      (caching-state-put caching-state sxhash (cons (list args-list results) collisions))))


;; TODO - detect no-arg as well?  will probably work (suboptimally) without it, though.
;; TODO - where is clearing of its memoization registered?
;; This macroexpansion should (assuming list-utilities' num-list-cached is being implemented):
;;  (defun num-list-cached (num start) ...)
;;  create the name *num-list-cached-caching-state* to hold the lazily instantiated cache
;;  hit the cache lookup by hashing the params without consing, and checking the list of collisions
;;  define the cache-miss expression to generate the value for the params


;; draw from calls in java:
;;  (create-global-caching-state-for-name name cs-variable (capacity nil) (test eql) (args-length auto) size)
;;  memoization_state.register_hl_store_cache_clear_callback(..) => :clear-when :hl-store-modified

;; kb-mapping-macros simple-term-assertion-list-filtered also registers a clearing parameter from the hl-store being cleared, and :CLEAR-WHEN is in the java stuff here, surrounded by other keywords that we're going to try to use.
(defmacro defun-memoized (name params (&key (capacity nil) (test 'eql) (initial-size 0) declare doc clear-when) &body calculate-cache-miss)
  "Define a memoized function.  The body can still have a docstring & declarations."
  (let ((varname (symbolicate "*" name "-CACHING-STATE*"))
        (clear-name (symbolicate "CLEAR-" name)))
    (alexandria:with-gensyms (cs key collisions results)
      `(progn
         (defvar ,varname nil)
         ;; Might as well create the table early, instead of always doing the lazy check as the Java code did.
         (toplevel
           (create-global-caching-state-for-name ',name ',varname ,capacity
                                                 ,(if (symbolp test) (list 'quote test) test)
                                                 ,(length params) ,initial-size)
           (note-globally-cached-function ',name)
           ,(when clear-when
              (case clear-when
                (:hl-store-modified `(register-hl-store-cache-clear-callback #',clear-name))
                (otherwise (error "Unknown defun-memoized :clear-when option: ~s" clear-when)))))
         ;; TODO - this would be the place to inject metrics on memoized functions
         (defun ,name ,params
           ,@(when declare `((declare ,@declare)))
           ,@(when doc (list doc))
           (let* ((,cs ,varname)
                  ;; Calculate the key by doing a non-consing hash construction
                  (,key (multi-hash ,@params))
                  ;; The initial lookup, which will return the list of hash collisions
                  (,collisions (caching-state-lookup ,cs ,key)))
             ;; Hash hit. Try to find a matching collision
             (when (not (eq ,collisions :&memoized-item-not-found&))
               ;;(format t "~&cache hash hit ~a ~s~%" ',name (list ,@params))
               (dolist (collision ,collisions)
                 ;; Each collision is (params results).  Try to match params.
                 (let ((args (first collision)))
                   (when (and ,@ (mapcar (lambda (param)
                                           `(,test ,param (pop args)))
                                         params))
                     (return-from ,name (second collision))))))
             ;; Hash miss, or collision not found.  Compute and store it.
             (let ((,results (multiple-value-list (progn ,@calculate-cache-miss))))
               ;;(format t "~&cache miss ~a ~s~%" ',name (list ,@params))
               (caching-state-enter-multi-key-n ,cs ,key ,collisions ,results (list ,@params))
               (caching-results ,results))))))))



  ;; TODO DESIGN - This doesn't deal with arg-list keys, just a singular key object passed to the hashtable.  Grepping the source code, these always seem to be symbol keys.  This probably maps a function to a cached-state, which seems like a pointless lookup, given that the function name itself can create a symbol-value unique to hold the caching state anyway.  I'm tempted to rip all this out.

(defstruct memoization-state
  store
  current-process
  lock
  name
  should-clone)

(defun create-memoization-state (&optional name lock should-clone (test #'eql))
  "[Cyc] Return a new memoization state suitable for WITH-MEMOIZATION-STATE"
  (declare ((or null string) name)
           ((or null bt:lock) lock)
           ((or symbol function) test))
  (when (and should-clone
             (not lock))
    (setf lock (bt:make-lock "Memoization state clone lock")))
  (make-memoization-state :name name
                          :lock lock
                          :store (make-hash-table :test test)
                          :current-process nil
                          :should-clone should-clone))

  ;; TODO - redundant names, clean it up

(defun new-memoization-state (&optional name lock should-clone (test #'eql))
  (create-memoization-state name lock should-clone test))

(defmacro with-memoization-state-lock (ms &body body)
  "Runs the body in the ms's lock, or plain if its lock is NIL."
  (alexandria:with-gensyms (lock worker)
    (alexandria:once-only (ms)
      `(let ((,lock (memoization-state-lock ,ms)))
         ;; Wrap the body in a function that can be called from 2 places
         (flet ((,worker ()
                  (let ((store (memoization-state-store ,ms)))
                    ,@body)))
           (if ,lock
               (bt:with-lock-held (,lock)
                 (,worker))
               (,worker)))))))

(defun memoization-state-lookup (memoization-state key &optional default)
  (with-memoization-state-lock memoization-state
    (gethash key store default)))

(defun memoization-state-put (memoization-state key value)
  (with-memoization-state-lock memoization-state
    (setf (gethash key store) value)))

(defun memoization-state-clear (memoization-state)
  (with-memoization-state-lock memoization-state
    (clrhash store)))

(defparameter *memoization-state* nil
  "[Cyc] Current memoization state. NIL indicates no memoization is occurring.")

(defun current-memoization-state ()
  "[Cyc] Return the current memoization state, or NIL if none."
  *memoization-state*)

(defun possibly-new-memoization-state ()
  (or *memoization-state*
      (create-memoization-state)))

(defun clear-all-memoization (state)
  (memoization-state-clear state))

(defglobal *memoized-functions* nil
  "[Cyc] The master list of all functiosn defined via define-memoized")

(defun note-memoized-function (function-symbol)
  (pushnew function-symbol *memoized-functions*)
  function-symbol)

(defglobal *globally-cached-functions* nil
  "[Cyc] The master list of all functions defined via define-cached or define-cached-new")

(defun note-globally-cached-function (function-symbol)
  (pushnew function-symbol *globally-cached-functions*)
  function-symbol)

(defun globally-cached-functions ()
  (remove-if-not #'fboundp *globally-cached-functions*))

(defun global-cache-variables ()
  (remove-if-not #'boundp (mapcar (lambda (name)
                                    ;; TODO - which package?
                                    (intern (format nil "*~a-CACHING-STATE*" name)))
                                  (globally-cached-functions))))

(defun global-cache-variable-values ()
  (mapcar #'symbol-value (global-cache-variables)))

(defun clear-all-globally-cached-functions ()
  (progress-dolist (caching-state (global-cache-variable-values) "Clearing all globally cached functions")
    (when caching-state
      (caching-state-clear caching-state))))

(deflexical *cache-clear-triggers* '(:hl-store-modified
                                     :genl-mt-modified
                                     :genl-preds-modified
                                     :genls-modified
                                     :isa-modified
                                     :quoted-isa-modified)
  "[Cyc] The list of possible triggers which can clear caches when they are triggered. Note that :GENL-PREDS-MODIFIED is also triggered on the addition or removal of a #$genlInverse assertion.")

  ;; TODO - arg ordering is different than create-caching-state

(defun create-global-caching-state-for-name (name cs-variable capacity test args-length size)
  (unless test
    (setf test #'eql))
  (bt:with-lock-held (*global-caching-lock*)
    (or (symbol-value cs-variable)
        (set cs-variable (create-caching-state (bt:make-lock (format nil "global caching lock for ~a" name))
                                               name args-length capacity test size)))))

(defun global-caching-variable-new (name)
  (intern (format nil "*~a-CACHING-STATE*" name)))

(defglobal *hl-store-cache-clear-callbacks* nil
  "[Cyc] The list of zero-arity function-spec-p's to funcall each time the HL store changes. These are intended to clear HL-store-dependent caches.")

(defun register-hl-store-cache-clear-callback (callback)
  "[Cyc] Registers CALLBACK as a function which will be funcalled each time the HL store changes. CALLBACK is a function-spec-p which should take zero arguments."
  (check-type callback 'function-spec-p)
  (pushnew callback *hl-store-cache-clear-callbacks*)
  callback)

(defun clear-hl-store-dependent-caches ()
  "[Cyc] Clears all HL store dependent caches, as registered by REGISTER-HL-STORE-CACHE-CLEAR-CALLBACK."
  (dolist (callback *hl-store-cache-clear-callbacks*)
    ;; Adding in both symbol & function object support, just in case.
    ;; Original only checked fboundp, which requires symbols and would error on functions.
    ;; However, that doesn't make much sense as it had to be function-spec-p on registration,
    ;;  and this function's fboundp thus would only fail if the symbol was FMAKUNBOUNDed after registeration.
    (when (function-spec-p callback)
      (funcall callback))))

(defglobal *mt-dependent-cache-clear-callbacks* nil
  "[Cyc] The list of zero-arity function-spec-p's to funcall each time the microtheory structure changes. These are intended to clear mt-dependent caches.")

(defun register-mt-dependent-cache-clear-callback (callback)
  "[Cyc] Registers CALLBACK as a function which will be funcalled each time the microtheory structure changes. CALLBACK should take zero arguments."
  (check-type callback 'function-spec-p)
  (pushnew callback *mt-dependent-cache-clear-callbacks*)
  callback)

(defparameter *suspend-clearing-mt-dependent-caches?* nil)

(defun clear-mt-dependent-caches? ()
  *suspend-clearing-mt-dependent-caches?*)

(defglobal *genl-preds-dependent-cache-clear-callbacks* nil
  "[Cyc] The list of zero-arity function-spec-p's to funcall each time the genlPreds structure changes. These are intended to clear mt-dependent caches.")
(defglobal *genls-dependent-cache-clear-callbacks* nil
  "[Cyc] The list of zero-arity function-spec-p's to funcall each time the genls structure changes. These are intended to clear mt-dependent caches.")
(defglobal *isa-dependent-cache-clear-callbacks* nil
  "[Cyc] The list of zero-arity function-spec-p's to funcall each time the isa structure changes. These are intended to clear mt-dependent caches.")
(defglobal *quoted-isa-dependent-cache-clear-callbacks* nil
  "[Cyc] The list of zero-arity function-spec-p's to funcall each time the quotedIsa structure changes. These are intended to clear mt-dependent caches.")

(defun* caching-results (results) (:inline t)
  "Returns the list of results as multiple-values."
  ;; Bypass the function call to values-list when there's only 1 result
  ;; TODO - can there be zero results? this code would return 0 results into 1 NIL
  (if (cdr results)
      (values-list results)
      (car results)))

(defconstant *caching-n-sxhash-composite-value* 167)

