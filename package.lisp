#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:modularize-interfaces
  (:use #:cl #:modularize #:trivial-indent)
  (:nicknames #:org.tymoonnext.radiance.lib.modularize.interfaces #:interfaces)
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
