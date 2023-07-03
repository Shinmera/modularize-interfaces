(in-package #:cl-user)
(defpackage #:modularize-interfaces
  (:use #:cl #:modularize #:trivial-indent #:trivial-arguments #:lambda-fiddle)
  (:nicknames #:org.shirakumo.radiance.lib.modularize.interfaces #:interfaces)
  ;; component.lisp
  (:export
   #:expand-component
   #:define-component-expander
   #:define-component-tester
   #:component-alias)
  ;; interface.lisp
  (:export
   #:interface-not-found
   #:interface-already-implemented
   
   #:interface
   #:interface-p
   #:implementation
   #:test-interface
   #:reset-interface
   #:define-interface
   #:define-interface-extension

   #:generate-interface-stub
   #:print-interface-stub)
  ;; module.lisp
  (:export
   #:implements)
  ;; standard-components.lisp
  (:export
   #:defimpl
   #:i-defun
   #:i-defmacro
   #:i-defmethod)
  ;; toolkit.lisp
  (:export
   #:defun*
   #:defmacro*
   #:defmethod*))
