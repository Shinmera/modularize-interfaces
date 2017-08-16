#|
 This file is a part of Modularize-Interfaces
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem interfaces-test-implementation
  :class "modularize:virtual-module"
  :defsystem-depends-on (:modularize)
  :serial T
  :components ((:file "interfaces-test-interface")
               (:file "interfaces-test-implementation"))
  :depends-on (:modularize-interfaces)
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Test module system for modularize-interfaces.")

