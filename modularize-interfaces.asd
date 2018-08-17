#|
 This file is a part of Modularize-Interfaces
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem modularize-interfaces
  :name "Modularize-Interfaces"
  :version "0.9.3"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Programmatical interfaces extension for Modularize"
  :homepage "https://Shinmera.github.io/modularize-interfaces/"
  :bug-tracker "https://github.com/Shinmera/modularize-interfaces/issues"
  :source-control (:git "https://github.com/Shinmera/modularize-interfaces.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "component")
               (:file "standard-components")
               (:file "interface")
               (:file "module")
               (:file "indent"))
  :depends-on (:modularize
               :trivial-indent
               :trivial-arguments
               :lambda-fiddle))
