#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(defun intern-function-name (package name)
  (if (listp name)
      (if (eql (car name) 'setf)
          (list 'setf (intern-function-name package (cdr name)))
          (error "Invalid name supplied."))
      (or (find-symbol (string name) package)
          (intern (string name) package))))

(define-component-expander (function f defun) (interface name lambda-list &optional documentation)
  (let ((name (intern-function-name interface name)))
    `(defun* ,name ,lambda-list
       ,@(list documentation)
       (error ,(format NIL "~s is not implemented!" name)))))

(define-component-expander (macro m defmacro) (interface name lambda-list &optional documentation)
  (let ((name (intern (string name) interface)))
    `(defmacro* ,name ,lambda-list
       ,@(list documentation)
       (error ,(format NIL "~s is not implemented!" name)))))

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
       ,@(list documentation)
       (error ,(format NIL "~s is not implemented!" name)))))
