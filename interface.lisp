#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(define-condition interface-not-found (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Interface ~s requested but not found." (requested c)))))

(define-condition interface-already-implemented (error)
  ((%requested :initarg :interface :initform (error "INTERFACE required.") :reader requested)
   (%current :initarg :current :initform (error "CURRENT required.") :reader current)
   (%new :initarg :new :initform (error "NEW required.") :reader new))
  (:report (lambda (c s) (format s "Interface ~s is already implemented by ~s. Attempting to set ~s."
                                 (requested c) (current c) (new c)))))

(defun interface (object)
  (or (handler-case
          (let ((module (module object)))
            (when (interface-p module)
              module))
        (module-not-found (err)
          (declare (ignore err))))
      (error 'interface-not-found :requested object)))

(defun interface-p (object)
  (handler-case
      (let ((module (module object)))
        (not (null (find-symbol "*IMPLEMENTATION*" module))))
    (module-not-found (err)
      (declare (ignore err)))))

(defun implementation (interface)
  (let ((interface (interface interface)))
    (symbol-value (find-symbol "*IMPLEMENTATION*" interface))))

(defun (setf implementation) (implementation interface)
  (with-simple-restart (abort "Abort the implementation set.")
    (setf (symbol-value (find-symbol "*IMPLEMENTATION*" interface))
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
                         :report "Override the implementation while leaving the old module intact."))))
                module))))))

(defun reset-interface (interface)
  (let ((interface (interface interface)))
    (warn "Resetting interface ~s" interface)
    (funcall (module-storage interface 'interface-reset))))

(defmacro define-interface (name &body components)
  (let ((fqid (format NIL "MODULARIZE.INT.~a" name))
        (interface (gensym "INTERFACE")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-module ,name
         (:nicknames ,(make-symbol fqid))
         (:export #:*IMPLEMENTATION* #:IMPLEMENTATION)
         (:export ,@(loop for (type name &rest rest) in components
                          collect (make-symbol (string name)))))
       (let ((,interface (find-package ,(string name))))
         (funcall
          (setf (module-storage ,interface 'interface-reset)
                #'(lambda ()
                    (expand-interface ,(string name)
                      ,@components))))
         ,interface))))

(defmacro defimpl (name &rest args)
  (unless (eql (implementation (symbol-package name)) *package*)
    (error "~s is not implementation of ~s." *package* (symbol-package name)))
  (cond
    ((macro-function name)
     `(defmacro ,name ,@args))
    ((fboundp name)
     (etypecase (symbol-function name)
       (generic-function
        `(defmethod ,name ,@args))
       (function
        `(defun ,name ,@args))))))

(defmacro define-interface-function (name args &body body)
  `(defimpl ,name ,args ,@body))

(defmacro define-interface-macro (name args &body body)
  `(defimpl ,name ,args ,@body))

(defmacro define-interface-method (name &rest args)
  `(defimpl ,name ,@args))
