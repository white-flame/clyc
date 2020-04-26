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


;; TODO DESIGN - these functions pass a ton of variables around, which is both slow and messy for readability. Might be able to speed up these call chains by judicious inlining, having a single set of dynamic bindings established on entry, or wrapping these utilities in a single closure.


(defglobal *hl-storage-modules* (new-set #'eq)
  "[Cyc] Set of HL storage modules (set-p of hl-module-p).")

(defglobal *predicate-generic-hl-storage-modules* (new-set #'eq)
  "[Cyc] Set of generic (non-predicate-specific) HL storage modules.")

(defglobal *predicate-specific-hl-storage-modules-table* (make-hash-table :test #'eq)
  "[Cyc] Mapping between predicates and a list of their HL storage modules.")

(defglobal *argument-type-specific-hl-storage-modules-table* (make-hash-table :test #'eq)
  "[Cyc] Mapping between argument types and a list of HL storage modules that handle that argument type.")

(defun hl-storage-module (name plist)
  "[Cyc] Declare and create a new HL storage module with name NAME and properties PLIST."
  (setup-hl-storage-module name plist))

(defun try-hl-add-modules (hl-modules argument-spec cnf mt direction variable-map)
  "[Cyc] Tests for applicability and attempts to add to the store.
Returns whether the addition was successful."
  (try-hl-storage-modules-int hl-modules argument-spec cnf mt direction variable-map :add))
    
(defun try-hl-remove-modules (hl-modules argument-spec cnf mt)
  "[Cyc] Tests for applicability and attempts to remove an argument from the store.
Returns whether the removal was successful."
  (try-hl-storage-modules-int hl-modules argument-spec cnf mt nil nil :remove))
    
(defun* hl-storage-modules-for-predicate-and-argument-type (predicate argument-type) (:inline t)
  (fast-intersection (hl-storage-modules-for-predicate predicate)
                     (hl-storage-modules-for-argument-type argument-type)))

(defun hl-storage-modules-for-predicate (predicate)
  (copy-list (gethash predicate *predicate-specific-hl-storage-modules-table*)))

(defun hl-storage-modules-for-argument-type (argument-type)
  (let ((genls (argument-type-genls argument-type)))
    (loop for genl in genls
       nconc (hl-storage-modules-for-just-argument-type genl))))

(defun hl-storage-modules-for-just-argument-type (argument-type)
  "[Cyc] Accessor analogous to indexing, does not implement inheritance.
Result is destructible"
  (copy-list (gethash argument-type *argument-type-specific-hl-storage-modules-table*)))

(defparameter *currently-executing-hl-storage-module* nil)

(defun try-hl-storage-modules-int (hl-modules argument-spec cnf mt direction variable-map action
                                   &optional default)
  "[Cyc] This assumes a partition rather than a covering; we could relax this to allow more than one of HL-MODULES to apply."
  (let ((success? nil))
    ;; TODO - assuming *resourcing-sbhl-marking-spaces-p* is related to memory areas/spaces and unused here.
    (multiple-value-bind (applicable-hl-modules dispreferred-hl-modules)
        (applicable-hl-storage-modules hl-modules argument-spec cnf mt direction variable-map)
      ;; All references to this were after the missing-larkc
      (declare (ignore dispreferred-hl-modules))
      (let ((sorted-applicable-hl-modules (sort-hl-storage-modules-by-cost applicable-hl-modules
                                                                           argument-spec
                                                                           cnf mt direction
                                                                           variable-map)))
        (csome (hl-module sorted-applicable-hl-modules success?)
          (setf success? (apply-hl-storage-module hl-module argument-spec
                                                  cnf mt direction variable-map
                                                  action default))
          (when success?
            (note-successful-hl-storage-module hl-module)))
        ;; outside CSOME loop
        (unless success?
          (missing-larkc 31649))))
    success?))

(defun apply-hl-storage-module (hl-module argument-spec cnf mt direction variable-map action default)
  (let ((*currently-executing-hl-storage-module* hl-module))
    (case action
      (:add (hl-storage-module-add hl-module argument-spec cnf mt direction variable-map default))
      (:remove (hl-storage-module-remove hl-module argument-spec cnf mt default))
      (:remove-all (missing-larkc 31641)))))

(defun hl-storage-module-applicable? (hl-module argument-spec cnf mt direction variable-map)
  (let ((applicable-func (hl-storage-module-applicability-func hl-module)))
    ;; TODO - funcalled symbol
    (if (fboundp applicable-func)
        (funcall applicable-func argument-spec cnf mt direction variable-map)
        (missing-larkc 31637))))

(defun applicable-hl-storage-modules (hl-modules argument-spec cnf mt direction variable-map)
  (let ((supplanted-hl-modules nil)
        (dispreferred-hl-modules nil)
        (applicable-hl-modules nil)
        (exclusive-found? nil))
    (csome (hl-module hl-modules exclusive-found?)
      (unless (member? hl-module supplanted-hl-modules)
        (when (or (alist-lookup-without-values dispreferred-hl-modules hl-module)
                  (hl-storage-module-applicable? hl-module argument-spec cnf mt direction variable-map))
          (let ((exclusive-func (hl-storage-module-exclusive-func hl-module)))
            (when (or (not exclusive-func)
                      (and (function-spec-p exclusive-func)
                           (funcall exclusive-func argument-spec cnf mt direction variable-map)))
              (when exclusive-func
                (missing-larkc 31652))
              (unless exclusive-found?
                (multiple-value-bind (applicable-hl-modules-4 dispreferred-hl-modules-5)
                    (update-dispreferred-hl-storage-modules-wrt-applicable-modules hl-module applicable-hl-modules dispreferred-hl-modules hl-modules argument-spec cnf mt direction variable-map)
                  (setf applicable-hl-modules applicable-hl-modules-4)
                  (setf dispreferred-hl-modules dispreferred-hl-modules-5)))
              (when (or exclusive-found?
                        (not (alist-lookup dispreferred-hl-modules hl-module)))
                (push hl-module applicable-hl-modules)))))))
    (values (nreverse applicable-hl-modules)
            (unless exclusive-found?
              dispreferred-hl-modules))))


(defun update-dispreferred-hl-storage-modules-wrt-applicable-modules (hl-module applicable-hl-modules dispreferred-hl-modules hl-modules argument-spec cnf mt direction variable-map)
  ;; TODO - java did an empty pcase over this value?
  (let ((newly-dispreferred-module-names (hl-storage-module-preferred-over-info hl-module)))
    (dolist (dispreferred-module-name newly-dispreferred-module-names)
      (let ((dispreferred-hl-module (find-hl-module-by-name dispreferred-module-name)))
        (when (member? dispreferred-hl-module hl-modules)
          (cond
            ((member? dispreferred-hl-module applicable-hl-modules)
             (setf applicable-hl-modules (delete dispreferred-hl-module applicable-hl-modules))
             (setf dispreferred-hl-modules
                   (alist-push dispreferred-hl-modules dispreferred-hl-module hl-module)))
            
            ((alist-lookup dispreferred-hl-modules dispreferred-hl-module)
             (setf dispreferred-hl-modules
                   (alist-push dispreferred-hl-modules dispreferred-hl-module hl-module)))
            
            ((hl-storage-module-applicable? dispreferred-hl-module argument-spec cnf mt direction variable-map)
             (setf dispreferred-hl-modules
                   (alist-push dispreferred-hl-modules dispreferred-hl-module hl-module))))))))
  (values applicable-hl-modules dispreferred-hl-modules))

(defun* sort-hl-storage-modules-by-cost (hl-modules argument-spec cnf mt direction variable-map) (:inline t)
  (declare (ignore argument-spec cnf mt direction variable-map))
  hl-modules)

(defun* hl-storage-module-add (hl-module argument-spec cnf mt direction variable-map &optional default) (:inline t)
  "[Cyc] If HL-MODULE has an :ADD property, the specified function is applied to ARGUMENT-SPEC, CNF, MT, DIRECTION, and VARIABLE-MAP. Otherwise, DEFAULT is returned."
  (if-let ((add-func (get-hl-storage-module-property hl-module :add)))
    (funcall add-func argument-spec cnf mt direction variable-map)
    default))

(defun* hl-storage-module-remove (hl-module argument-spec cnf mt &optional default) (:inline t)
  "[Cyc] If HL-MODULE has a :REMOVE property, the specified function is applied to ARGUMENT-SPEC, CNF, and MT. Otherwise, DEFAULT is returned."
  (if-let ((remove-func (get-hl-storage-module-property hl-module :remove)))
    (funcall remove-func argument-spec cnf mt)
    default))

(defun* hl-storage-module-argument-type (hl-module) (:inline t)
  (get-hl-storage-module-property hl-module :argument-type))

(defun* hl-storage-module-predicate (hl-module) (:inline t)
  (get-hl-storage-module-property hl-module :predicate))

(defun* hl-storage-module-applicability-func (hl-module) (:inline t)
  (multiple-value-bind (applicability-func default?) (get-hl-storage-module-property hl-module :applicability)
    (unless default?
      applicability-func)))

(defun* hl-storage-module-exclusive-func (hl-module) (:inline t)
  (get-hl-storage-module-property hl-module :exclusive))
  
(defun* hl-storage-module-preferred-over-info (hl-module) (:inline t)
  (get-hl-storage-module-property hl-module :preferred-over))

(defun* get-hl-storage-module-property (hl-module indicator) (:inline t)
  (hl-module-property hl-module indicator))

(defun reclassify-hl-storage-modules ()
  (clear-hl-storage-module-indexes)
  (rebuild-solely-specific-hl-storage-module-predicate-store)
  (do-set (hl-module *hl-storage-modules*)
    (classify-hl-storage-module hl-module
                                (hl-storage-module-predicate hl-module)
                                (hl-storage-module-argument-type hl-module))))

;; TODO - this list was directly referenced in the java ($list28), so should this really be defconstant?
(deflexical *hl-storage-module-properties* '(:pretty-name
                                             :module-subtype
                                             :module-source
                                             :argument-type
                                             :sense
                                             :direction
                                             :required-mt
                                             :predicate
                                             :any-predicates
                                             :applicability-pattern
                                             :applicability
                                             :supplants
                                             :exclusive
                                             :preferred-over
                                             :incompleteness
                                             :add
                                             :remove
                                             :remove-all
                                             :documentation))

(defun clear-hl-storage-module-indexes ()
  (setf *predicate-generic-hl-storage-modules* (new-set #'eq))
  (clrhash *predicate-specific-hl-storage-modules-table*))

(defun setup-hl-storage-module (name plist)
  (destructuring-bind (&key pretty-name module-subtype module-source argument-type sense direction required-mt predicate any-predicates applicability-pattern applicability supplants exclusive preferred-over incompleteness add remove remove-all documentation) plist
    ;; TODO - tons of check-type calls in here, depending on if the keyword was given, but nothing was done with them?  I guess this just verifies the option shapes/values without doing anything with them?
    (declare (ignore pretty-name module-subtype module-source argument-type sense direction required-mt predicate any-predicates applicability-pattern applicability supplants exclusive preferred-over incompleteness add remove remove-all documentation))
    (register-hl-storage-module name plist)))

(defun register-hl-storage-module (name plist)
  (let ((hl-module (progn
                     (setup-module name :storage plist)
                     *hl-storage-modules*)))
    (classify-hl-storage-module hl-module
                                (getf plist :predicate)
                                (getf plist :argument-type))
    hl-module))

(defun classify-hl-storage-module (hl-module predicate argument-type)
  (if predicate
      (register-predicate-specific-hl-storage-module hl-module predicate)
      (register-predicate-generic-hl-storage-module hl-module))
  (register-argument-type-specific-hl-storage-module hl-module argument-type))
  
(defun register-predicate-specific-hl-storage-module (hl-module predicate)
  ;; TODO - dictionary calls have the opposite arg ordering from hashtables and sets, change that when the utils move into hashtable-utilities.
  (dictionary-pushnew *predicate-specific-hl-storage-modules-table* predicate hl-module))

(defun register-predicate-generic-hl-storage-module (hl-module)
  (set-add hl-module *predicate-generic-hl-storage-modules*))

(defun register-argument-type-specific-hl-storage-module (hl-module argument-type)
  (dictionary-pushnew *argument-type-specific-hl-storage-modules-table* argument-type hl-module))

(defglobal *solely-specific-hl-storage-module-predicate-store* (new-set #'eq))

(defun* rebuild-solely-specific-hl-storage-module-predicate-store () (:inline t)
  (set-rebuild *solely-specific-hl-storage-module-predicate-store*))

(defun* register-solely-specific-hl-storage-module-predicate (predicate) (:inline t)
  "[Cyc] If you want the specific hl-storage modules for PREDICATE to supplant ALL generic hl-storage modules, then register this property."
  (set-add predicate *solely-specific-hl-storage-module-predicate-store*))

(defun* solely-specific-hl-storage-module-predicate? (predicate) (:inline t)
  (set-member? predicate *solely-specific-hl-storage-module-predicate-store*))

(defstruct (hl-assertion-spec (:type list)
                              (:constructor new-hl-assertion-spec
                                            (cnf mt &optional direction variable-map)))
  cnf
  mt
  direction
  variable-map)

(defstruct (hl-assertible (:type list)
                          (:constructor new-hl-assertible (hl-assertion-spec argument-spec)))
  hl-assertion-spec
  argument-spec)

(defun hl-add-assertible (hl-assertible)
  ;; TODO - probably should build an easy structure destructuring macro, when these aren't in order.
  (apply #'hl-add-argument hl-assertible))

(defun hl-add-argument (argument-spec cnf mt direction &optional variable-map)
  "[Cyc] Returns NIL if the argument specified by ARGUMENT-SPEC was /not/ added as an argument for CNF, and non-NIL otherwise."
  (janus-note-argument argument-spec cnf mt direction variable-map)
  ;; TODO - probably tear these out to avoid a bunch of always-false runtime checks.
  (when *recording-hl-transcript-operations?*
    (missing-larkc 32159))
  (when (hlmts-supported?)
    (setf mt (canonicalize-hlmt mt)))
  (hl-store-perform-action-int :add argument-spec cnf mt direction variable-map))

(defun hl-remove-argument (argument-spec cnf mt)
  (when *recording-hl-transcript-operations?*
    (missing-larkc 32160))
  (hl-store-perform-action-int :remove argument-spec cnf mt nil nil))

(defparameter *successful-hl-storage-modules* nil)

(defun note-successful-hl-storage-module (hl-module)
  (set-add hl-module *successful-hl-storage-modules*))

(defun hl-store-perform-action-int (action argument-spec cnf mt direction variable-map)
  (let ((success? nil)
        (solely-specific? nil)
        (argument-type (and argument-spec
                            (argument-spec-type argument-spec)))
        (*successful-hl-storage-modules* (new-set)))
    (when (atomic-clause-p cnf)
      (let ((predicate (atomic-cnf-predicate cnf))
            (predicate-specific-modules (if argument-type
                                            (hl-storage-modules-for-predicate-and-argument-type predicate argument-type)
                                            (hl-storage-modules-for-predicate predicate))))
        (setf solely-specific? (solely-specific-hl-storage-module-predicate? predicate))
        (when predicate-specific-modules
          (setf success? (hl-perform-action-with-storage-modules-int action predicate-specific-modules argument-spec cnf mt direction variable-map)))))
    (unless success?
      (unless solely-specific?
        (missing-larkc 31629)))
    success?))



(deflexical *robustly-remove-uncanonical-decontextualized-assertibles?* t
  "[Cyc] Whether the HL storage modules should robustly try to remove a decontextualized assertible from the MT given by the user in addition to removing it from its proper, decontextualized MT. This can happen for example when a predicate is asserted to be decontextualized _after_ some assertions have already been made with it in other MTs.")

(defun hl-perform-action-with-storage-modules-int (action hl-modules argument-spec cnf mt direction variable-map)
  (let ((actual-mt (possibly-convention-mt-for-decontextualized-cnf mt cnf)))
    (case action
      (:add (try-hl-add-modules hl-modules argument-spec cnf actual-mt direction variable-map))
      (:remove

       (let ((success? (try-hl-remove-modules hl-modules argument-spec cnf actual-mt))
             (robust-success? (and *robustly-remove-uncanonical-decontextualized-assertibles?*
                                   (not (hlmt-equal mt actual-mt))
                                   (try-hl-remove-modules hl-modules argument-spec cnf mt))))
         (or success? robust-success?)))
      (:remove-all (missing-larkc 31650))
      (otherwise (error "Unexpected HL storage action ~a" action)))))

(defun* hl-assert (cnf mt strength direction &optional variable-map) (:inline t)
  "[Cyc] Returns NIL if CNF was not stored at the HL in some fashion, and non-NIL otherwise.  If CNF is stored as an as assertion object, that assertion object (assertion-p) will be returned, but there is no guarantee that CNF will be stored as an assertion."
  (hl-add-argument (create-asserted-argument-spec strength)
                   cnf mt direction variable-map))

(defglobal *dummy-asserted-argument-spec* (create-asserted-argument-spec :unspecified))

(defun* hl-unassert (cnf mt) (:inline t)
  (hl-remove-argument *dummy-asserted-argument-spec* cnf mt))

