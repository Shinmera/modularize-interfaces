#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(define-component-expander (function f defun) (interface name lambda-list &optional documentation)
  (let ((name (intern-function-name interface name)))
    `(defun* ,name ,lambda-list
       ,@(when documentation (list documentation))
       (declare (ignore ,@(extract-lambda-vars lambda-list)))
       (error ,(format NIL "~s is not implemented!" name)))))

(define-component-tester (function f defun) (interface name lambda-list &optional documentation)
  (declare (ignore documentation))
  (function-lambda-matches (symbol-function (intern-function-name interface name)) lambda-list))

(define-component-expander (macro m defmacro) (interface name lambda-list &optional documentation)
  (let ((name (intern (string name) interface)))
    `(defmacro* ,name ,lambda-list
       ,@(when documentation (list documentation))
       (declare (ignore ,@(extract-lambda-vars lambda-list)))
       (error ,(format NIL "~s is not implemented!" name)))))

(define-component-tester (macro m defmacro) (interface name lambda-list &optional documentation)
  (declare (ignore documentation))
  (function-lambda-matches (symbol-function (intern (string name) interface)) lambda-list))

(define-component-expander (class c defclass) (interface name direct-superclasses direct-slots &body options)
  (let ((name (intern (string name) interface)))
    `(defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

(define-component-expander (generic gf defgeneric) (interface name lambda-list &body options)
  (let ((name (intern-function-name interface name)))
    `(defgeneric ,name ,lambda-list
       ,@options)))

(define-component-expander (method gm defmethod) (interface name lambda-list &optional documentation)
  (let ((name (intern-function-name interface name)))
    `(defmethod* ,name ,lambda-list
       ,@(when documentation (list documentation))
       (declare (ignore ,@(extract-lambda-vars lambda-list)))
       (error ,(format NIL "~s is not implemented!" name)))))




(defmacro defimpl (name &rest args)
  "Expands to I-DEFMACRO, I-DEFMETHOD or I-DEFUN depending on what kind of symbol it names."
  (cond
    ((macro-function name)
     `(i-defmacro ,name ,@args))
    ((fboundp name)
     (etypecase (symbol-function name)
       (generic-function
        `(i-defmethod ,name ,@args))
       (function
        `(i-defun ,name ,@args))))
    (T (error "The name fits neither macro, method or function?"))))

(defmacro i-defun (name args &body body)
  "Expands to an interface function definition."
  (unless (eql (implementation (symbol-package name)) *package*)
    (error "~s is not implementation of ~s." *package* (symbol-package name)))
  `(defun ,name ,args
     ,@(if (stringp (first body))
           body
           (cons (documentation name 'function) body))))

(defmacro i-defmacro (name args &body body)
  "Expands to an interface macro definition."
  (unless (eql (implementation (symbol-package name)) *package*)
    (error "~s is not implementation of ~s." *package* (symbol-package name)))
  `(defmacro ,name ,args
     ,@(if (stringp (first body))
           body
           (cons (documentation name 'function) body))))

(defmacro i-defmethod (name &rest args)
  "Expands to an interface method definition."
  (unless (eql (implementation (symbol-package name)) *package*)
    (error "~s is not implementation of ~s." *package* (symbol-package name)))
  `(defmethod ,name ,@args))
