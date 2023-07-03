(in-package #:org.shirakumo.radiance.lib.modularize.interfaces)

(define-option-expander implements (package &rest interfaces)
  "Module option that allows modules to declare that they implement an itnerface.
This registers the module as the interface's implementation and puts the interface
onto its :IMPLEMENTS storage slot."
  (loop for identifier in interfaces
        for interface = (interface identifier)
        do (setf (implementation interface) package)
           (pushnew interface (module-storage package :implements))))

(define-delete-hook (module)
  "This both checks that when an interface is deleted it is removed from the
:IMPLEMENTS fields of the module that implements it and it resets the
implementation of an interface if a module that implements on is deleted."
  (when (interface-p module)
    (setf (module-storage (implementation module) :implements)
          (delete module (implementation module))))
  (loop for interface in (module-storage module :implements)
        do (setf (implementation interface) NIL)))

(defmethod asdf:perform :after ((op asdf:load-op) (module module))
  "Causes a TEST-INTERFACE to be performed on all interfaces listed in the module storage's :IMPLEMENTS field."
  (let ((module (module (virtual-module-name module))))
    (loop for interface in (module-storage module :implements)
          do (test-interface interface))))

(defun implements (module)
  "Returns a list of interfaces this module implements."
  (module-storage module :implements))
