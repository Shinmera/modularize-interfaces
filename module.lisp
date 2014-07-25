#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(define-option-expander implements (package &rest interfaces)
  `(progn ,@(loop for identifier in interfaces
                  for interface = (interface identifier)
                  collect `(progn (setf (implementation ,interface) ,package)
                                  (pushnew ,interface (module-storage ,package :implements))))))

(define-modularize-hook (module)
  )

(define-delete-hook (module)
  (when (interface-p module)
    (setf (module-storage (implementation module) :implements)
          (delete module (implementation module))))
  (loop for interface in (module-storage module :implements)
        do (setf (implementation interface) NIL)))

(defmethod asdf:operate :after ((op asdf:load-op) (module module) &key)
  (let ((module (module (virtual-module-name module))))
    (loop for interface in (module-storage module :implements)
          do (test-interface interface))))
