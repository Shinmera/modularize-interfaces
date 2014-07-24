#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(define-indentation define-interface (4 &rest (&whole 2 0 4 &body)))
(define-indentation defun* (4 &lambda &body))
(define-indentation defmacro* (4 &lambda &body))
(define-indentation defmethod* (4 &lambda &body))
