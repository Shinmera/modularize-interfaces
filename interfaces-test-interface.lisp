(in-package #:org.shirakumo.radiance.lib.modularize.user)

(interfaces:define-interface interfaces-test-interface
  (defun greet (&optional name)
    "Greets the user."))
