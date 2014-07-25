#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(define-module interfaces-test-implementation
  (:use #:cl #:interfaces)
  (:export #:greet)
  (:implements #:interfaces-test-interface))

(in-package #:interfaces-test-implementation)

(i-defun interfaces-test-interface:greet (&optional name)
  (format T "~&Hello, ~a!~%" (or name "you")))

(interfaces-test-interface:greet)
