#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(defvar *component-alias* (make-hash-table :test 'eql))

(define-condition component-test-failed (warning)
  ((%requested :initarg :interface :initform (error "INTERFACE required.") :accessor requested)
   (%component :initarg :component :initform (error "COMPONENT required.") :accessor component)
   (%result :initarg :result :initform NIL :accessor result))
  (:report (lambda (c s) (format s "Component test of ~s on ~s failed~:[.~;: ~:*~a~]"
                                 (component c) (requested c) (result c)))))

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

(defmacro expand-components (interface &body component-defs)
  (setf interface (module interface))
  `(progn
     ,@(loop for (type . args) in component-defs
             collect (expand-component (component-alias (make-keyword type)) args :interface interface))))

(defgeneric test-component (type args &key interface &allow-other-keys))
(defmethod test-component (type args &key)
  (values T "No test available; passing by default."))

(defmacro define-component-tester (name (interface &rest arguments) &body body)
  (let* ((names (if (listp name) name (list name)))
         (name (first names))
         (args (gensym "ARGS")))
    `(progn
       (defmethod test-component ((,(gensym "TYPE") (eql ,(make-keyword name))) ,args &key ,interface)
         (destructuring-bind ,arguments ,args
           ,@body))
       ,@(loop for alias in (rest names)
               collect `(setf (component-alias ',alias) ',name)))))

(defun test-components (interface component-defs)
  (loop for (type . args) in component-defs
        collect (multiple-value-bind (pass reason) (test-component (component-alias (make-keyword type)) args :interface interface)
                  (unless pass
                    (warn 'component-test-failed
                          :component (first args)
                          :interface interface
                          :result reason))
                  pass)))
