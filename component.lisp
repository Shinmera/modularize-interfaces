#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(defvar *component-alias* (make-hash-table :test 'eql))

(defgeneric expand-component (type args &key interface &allow-other-keys))

(defmacro define-component-expander (name (interface &rest arguments) &body body)
  (let* ((names (if (listp name) name (list name)))
         (name (first names))
         (args (gensym "ARGS")))
    `(progn
       (defmethod expand-component ((,(gensym "TYPE") (eql ,(make-keyword name))) ,args &key ,interface)
         (destructuring-bind ,arguments ,args
           ,@body))
       ,@(loop for alias in (rest names)
               collect `(setf (component-alias ',alias) ',name)))))

(defun component-alias (component)
  (gethash component *component-alias* component))

(defun (setf component-alias) (component alias)
  (setf (gethash (make-keyword alias) *component-alias*)
        (make-keyword component)))

(defun expand-components (interface component-defs)
  (loop for (type . args) in component-defs
        collect (expand-component (component-alias (make-keyword type)) args :interface interface)))

(defmacro expand-interface (interface &body component-defs)
  (setf interface (module interface))
  (let ((implementation-var (find-symbol "*IMPLEMENTATION*" interface))
        (implementation (find-symbol "IMPLEMENTATION" interface))
        (value (gensym "VALUE")))
    `(progn
       (defvar ,implementation-var NIL)
       (declaim (inline ,implementation))
       (defun* ,implementation () ,implementation-var)
       (defun* (setf ,implementation) (,value)
         (setf (implementation ,interface) ,value))
       ,@(expand-components interface component-defs))))
