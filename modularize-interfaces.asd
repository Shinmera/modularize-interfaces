(asdf:defsystem modularize-interfaces
  :name "Modularize-Interfaces"
  :version "0.9.3"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Programmatical interfaces extension for Modularize"
  :homepage "https://shinmera.com/docs/modularize-interfaces/"
  :bug-tracker "https://shinmera.com/project/modularize-interfaces/issues"
  :source-control (:git "https://shinmera.com/project/modularize-interfaces.git")
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
