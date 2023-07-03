(defsystem interfaces-test-implementation
  :class "modularize:virtual-module"
  :defsystem-depends-on (:modularize)
  :serial T
  :components ((:file "interfaces-test-interface")
               (:file "interfaces-test-implementation"))
  :depends-on (:modularize-interfaces)
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Test module system for modularize-interfaces.")

