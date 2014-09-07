#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(define-condition interface-not-found (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Interface ~s requested but not found." (requested c))))
  (:documentation "Condition signalled if an interface was requested but could not be found."))

(define-condition interface-already-implemented (error)
  ((%requested :initarg :interface :initform (error "INTERFACE required.") :reader requested)
   (%current :initarg :current :initform (error "CURRENT required.") :reader current)
   (%new :initarg :new :initform (error "NEW required.") :reader new))
  (:report (lambda (c s) (format s "Interface ~s is already implemented by ~s. Attempting to set ~s."
                                 (requested c) (current c) (new c))))
  (:documentation "Condition signalled if an interface is already implemented by a different module than was attempted to register as an implementation."))

(defun interface (object)
  "Returns the interface package module as identified by the object.
See MODULE for more."
  (or (handler-case
          (let ((module (module object)))
            (when (interface-p module)
              module))
        (module-not-found (err)
          (declare (ignore err))))
      (error 'interface-not-found :requested object)))

(defun interface-p (object)
  "Returns T if the passed object is or names an interface, otherwise NIL."
  (handler-case
      (let ((module (module object)))
        (not (null (find-symbol "*IMPLEMENTATION*" module))))
    (module-not-found (err)
      (declare (ignore err)))))

(defun implementation (interface)
  "Returns the currently active implementation of the interface."
  (let ((interface (interface interface)))
    (symbol-value (find-symbol "*IMPLEMENTATION*" interface))))

(defun (setf implementation) (implementation interface)
  "Attempts to set the implementation of an interface.
Interfaces can only be implemented by modules and if a different
module already implements the interface a condition is signalled.
The following restarts are available:
 DELETE    Deletes the old implementation through DELETE-MODULE.
 OVERRIDE  Overrides the implementation and leaves the old module.
 ABORT     Aborts the setting of the implementation completely.

If the implementation setting was successful, TEST-INTERFACE is
called. If the implementation is set to NIL, RESET-INTERFACE is
called. "
  (with-simple-restart (abort "Abort the implementation set.")
    (if (setf (symbol-value (find-symbol "*IMPLEMENTATION*" interface))
              (when implementation
                (let ((module (module implementation)))
                  (let* ((interface (interface interface))
                         (current (implementation interface)))
                    (cond
                      ((not current))
                      ((eql current module))
                      (T (restart-case
                             (error 'interface-already-implemented :interface interface :current current :new module)
                           (delete ()
                             :report "Delete the current implementation and set the new one."
                             (delete-module current))
                           (override ()
                             :report "Override the implementation while leaving the old module intact."
                             (setf (module-storage current :implements)
                                   (delete interface (module-storage current :implements)))
                             (reset-interface interface)))))
                    module))))
        (test-interface interface)
        (reset-interface interface))))

(defun reset-interface (interface)
  "Resets the interface by redefining it with stubs as per its component definitions."
  (let ((interface (interface interface)))
    (warn "Resetting interface ~s" interface)
    (let ((*redefine* T))
      (with-muffled-warnings
        (funcall (module-storage interface 'interface-reset))))))

(defun test-interface (interface)
  "Tests the interface for definition conformity. See TEST-COMPONENTS."
  (let ((interface (interface interface)))
    (test-components interface (module-storage interface 'interface-definition))))

(defmacro expand-interface (interface)
  "Expands the given interface into its basic form."
  (setf interface (module interface))
  (let ((implementation-var (find-symbol "*IMPLEMENTATION*" interface))
        (implementation (find-symbol "IMPLEMENTATION" interface))
        (value (gensym "VALUE")))
    `(progn
       (defvar ,implementation-var NIL)
       (defun* ,implementation () ,implementation-var)
       (defun* (setf ,implementation) (,value)
         (setf (implementation ,interface) ,value)))))

(defmacro define-interface (name &body components)
  "Defines a new interface.

This defines a new module with the given name and nicks, as well
as a fully qualified interface identifier which is the name prefixed
by MODULARIZE.INT. . It then calls EXPAND-INTERFACE on the new module
and finally expands the component definitions as per EXPAND-COMPONENTS."
  (destructuring-bind (name &rest nicks) (if (listp name) name (list name))
    (let ((fqid (format NIL "MODULARIZE.INT.~a" name))
          (interface (gensym "INTERFACE")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (define-module ,name
           (:nicknames ,(make-symbol fqid) ,@(mapcar #'(lambda (s) (make-symbol (string s))) nicks))
           (:export #:*IMPLEMENTATION* #:IMPLEMENTATION)
           (:export ,@(loop for (type name &rest rest) in components
                            unless (listp name)
                              collect (make-symbol (string name)))))
         (expand-interface ,(string name))
         (let ((,interface (find-package ,(string name))))
           (setf (module-storage ,interface 'interface-definition)
                 ',components)
           (funcall
            (setf (module-storage ,interface 'interface-reset)
                  #'(lambda ()
                      (expand-components ,(string name) ,@components))))
           ,interface)))))

(defmacro define-interface-extension (name &body components)
  "Currently not implemented."
  (declare (ignore name components))
  (error "Not implemented."))

(defun generate-interface-stub (interface &optional (package *package*))
  "Generates a stub for the INTERFACE that you can use as an implementation starting point.
Symbols are interned into PACKAGE where appropriate."
  (let ((interface (interface interface)))
    (labels ((internalise (forms)
               (loop for form in forms
                     collect (typecase form
                               (list (internalise form))
                               (symbol (intern (symbol-name form) package))
                               (T form))))
             (definition-stub (definition)
               (destructuring-bind (func name &rest forms) definition
                 `(,func ,(typecase name
                            (symbol (find-symbol (symbol-name name) interface))
                            (list (list (first name) (find-symbol (symbol-name (second name)) interface)))
                            (T name))
                         ,@(internalise forms)))))
      (loop for definition in (module-storage interface 'interface-definition)
            collect (definition-stub definition)))))

(defun print-interface-stub (interface &optional (package *package*))
  "Print the stub of GENERATE-INTERFACE-STUB in a way that is easily copy-pastable."
  (let ((*print-case* :downcase))
    (format T "~{~s~^~%~%~}" (generate-interface-stub interface package))
    (values)))
