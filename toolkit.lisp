#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(defvar *redefine* NIL)

(defmacro defun* (name args &body body)
  `(when (or *redefine* (not (fboundp ',name))) 
     (defun ,name ,args ,@body)))

(defmacro defmacro* (name args &body body)
  `(when (or *redefine* (not (fboundp ',name)))
     (defmacro ,name ,args ,@body)))

(defmacro defmethod* (name &rest args)
  (let ((qualifiers (loop for arg in args until (listp arg) collect arg))
        (lambda-list (loop for arg in args when (listp arg) do (return arg))))
    `(when (or *redefine* (not (find-method #',name ',qualifiers (mapcar #'find-class ',lambda-list))))
       (defmethod ,name ,@args))))

(defun make-keyword (name)
  (let ((name (string name)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))
