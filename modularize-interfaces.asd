#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.modularize.interfaces.asdf
  (:use #:cl #:asdf))
(in-package :org.tymoonnext.radiance.lib.modularize.interfaces.asdf)

(defsystem modularize-interfaces
  :name "Modularize-Interfaces"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Programmatical interfaces extension for Modularize"
  :long-description ""
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "component")
               (:file "standard-components")
               (:file "interface")
               (:file "module")
               (:file "indent"))
  :depends-on (:modularize
               :trivial-indent))
