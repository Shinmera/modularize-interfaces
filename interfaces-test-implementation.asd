#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.modularize.interfaces.test-implementation.asdf
  (:use #:cl #:asdf))
(in-package :org.tymoonnext.radiance.lib.modularize.interfaces.test-implementation.asdf)

(defsystem interfaces-test-implementation
  :class "modularize:module"
  :defsystem-depends-on (:modularize)
  :serial T
  :components ((:file "interfaces-test-interface")
               (:file "interfaces-test-implementation"))
  :depends-on (:modularize-interfaces))

