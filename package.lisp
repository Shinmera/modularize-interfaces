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
   #:component-alias)
  ;; interface.lisp
  (:export
   #:interface-not-found
   #:interface-already-implemented
   
   #:interface
   #:interface-p
   #:implementation
   #:reset-interface
   #:define-interface
   #:define-interface-extension)
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
