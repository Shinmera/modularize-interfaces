#|
 This file is a part of Modularize-Interfaces
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.lib.modularize.interfaces)

(defvar *component-alias* (make-hash-table :test 'eql)
  "Table of aliases for component names.")

(define-condition component-test-failed (warning)
  ((%requested :initarg :interface :initform (error "INTERFACE required.") :accessor requested)
   (%component :initarg :component :initform (error "COMPONENT required.") :accessor component)
   (%result :initarg :result :initform NIL :accessor result))
  (:report (lambda (c s) (format s "Component test of ~s on ~s failed~:[.~;: ~:*~a~]"
                                 (component c) (requested c) (result c))))
  (:documentation "Condition signalled when the test of an interface component failed for some reason."))

(defgeneric expand-component (type args &key interface &allow-other-keys)
  (:documentation "Generic function used to expand components into their proper forms."))

(defmacro define-component-expander (name (interface &rest arguments) &body body)
  "Defines a new component expander for interface definitions.

The NAME can be a list of names that will be aliased to the first one.
All names will be turned into keywords to allow for differing naming schemes
in the component definitions. INTERFACE should be a symbol to which the
interface package is bound. ARGUMENTS is the lambda-list used to deconstruct
the arguments of the component definition."
  (let* ((names (if (listp name) name (list name)))
         (name (first names))
         (args (gensym "ARGS")))
    `(progn
       (defmethod expand-component ((,(gensym "TYPE") (eql ,(make-keyword name))) ,args &key ,interface)
         (destructuring-bind ,arguments ,args
           ,@body))
       ,@(loop for alias in (rest names)
               collect `(setf (component-alias ',alias) ',name)))))

(defun component-alias (alias)
  "Returns the actual component name the given alias points to or itself if it is not resolved.."
  (gethash alias *component-alias* alias))

(defun (setf component-alias) (component alias)
  "Sets a new alias for the given component-name."
  (setf (gethash (make-keyword alias) *component-alias*)
        (make-keyword component)))

(defmacro expand-components (interface &body component-defs)
  "Expands the component definitions for the INTERFACE to their proper forms."
  (setf interface (module interface))
  `(progn
     ,@(loop for (type . args) in component-defs
             collect (expand-component (component-alias (make-keyword type)) args :interface interface))))

(defgeneric test-component (type args &key interface &allow-other-keys)
  (:documentation "Tests a component type for validity."))
(defmethod test-component (type args &key)
  (values T "No test available; passing by default."))

(defmacro define-component-tester (name (interface &rest arguments) &body body)
  "Defines a new testing method for a component type.

The structure of this macro is the same as for DEFINE-COMPONENT."
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
  "Tests the interface against the component-definitions to see if it matches.
That is to say, the passed component-defs tell what the interface should look like
and this function will test if the defined components match this definition
to a certain extent.

How well and thorough the tests are depends on the component itself and its
test function. Certain components may even not provide any tests at all."
  (loop for (type . args) in component-defs
        collect (multiple-value-bind (pass reason) (test-component (component-alias (make-keyword type)) args :interface interface)
                  (unless pass
                    (warn 'component-test-failed
                          :component (first args)
                          :interface interface
                          :result reason))
                  pass)))
