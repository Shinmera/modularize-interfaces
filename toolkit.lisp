#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(defvar *redefine* NIL
  "Special variable that dictates whether already defined functions
should be redefined by defun*, defmacro*, defmethod*.")

(defmacro defun* (name args &body body)
  "Expands to a conditional function definition that only happens if
either *REDEFINE* is T or the function has not yet been defined."
  `(when (or *redefine* (not (fboundp ',name))) 
     (defun ,name ,args ,@body)))

(defmacro defmacro* (name args &body body)
  "Expands to a conditional macro definition that only happens if
either *REDEFINE* is T or the macro has not yet been defined."
  `(when (or *redefine* (not (fboundp ',name)))
     (defmacro ,name ,args ,@body)))

(defmacro defmethod* (name &rest args)
  "Expands to a conditional method definition that only happens if
either *REDEFINE* is T or the method has not yet been defined."
  (let ((qualifiers (loop for arg in args until (listp arg) collect arg))
        (lambda-list (loop for arg in args when (listp arg) do (return arg))))
    `(when (or *redefine* (not (find-method #',name ',qualifiers (mapcar #'find-class ',lambda-list))))
       (defmethod ,name ,@args))))

(defmacro with-muffled-warnings (&body body)
  "Calls the body with redefinition warnings muffled.

Currently only implemented on: SBCL"
  `(locally
       (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind
         (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
       ,@body)))

(defun make-keyword (name)
  "Returns the matching keyword for the NAME."
  (let ((name (string name)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))

(defun intern-function-name (package name)
  "Interns the function name into the package.
This has special handling for (SETF NAME)."
  (if (listp name)
      (if (eql (car name) 'setf)
          (list 'setf (intern-function-name package (second name)))
          (error "Invalid name supplied."))
      (or (find-symbol (string name) package)
          (intern (string name) package))))

(defun lambda-keyword-p (symbol)
  "Returns the symbol if it is a lambda-keyword symbol (the &-options)."
  (find symbol '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

(defun remove-aux-part (lambda-list)
  "Removes the &aux part of the lambda-list."
  (let ((position (position '&aux lambda-list)))
    (if position
        (subseq lambda-list 0 position)
        lambda-list)))

(defun flatten-lambda-list (lambda-list)
  "Flattens the lambda-list by replacing all lists within it with their respective first symbol."
  (loop with in-opt = NIL
        with results = ()
        for element in lambda-list
        do (cond ((or (member element '(&key &aux &optional)))
                  (push (setf in-opt element)
                        results))
                 (in-opt
                  (push (if (listp element) (car element) element)
                        results))
                 ((listp element)
                  (dolist (a element)
                    (push a results)))
                 (T (push element results)))
        finally (return (nreverse results))))

(defun extract-lambda-vars (lambda-list)
  "Extracts the symbols that name the variables in the lambda-list."
  (remove-if #'lambda-keyword-p (flatten-lambda-list (remove-aux-part lambda-list))))

(defun function-arguments (function)
  "Returns the lambda-list of the function if possible.
This is only implemented with: SBCL, SWANK, working FUNCTION-LAMBDA-EXPRESSION."
  #+sbcl (sb-introspect:function-lambda-list function)
  #+(and swank (not sbcl)) (swank-backend:arglist function)
  #-(and sbcl swank) (second (nth-value 2 (function-lambda-expression function))))

(defun function-lambda-matches (function lambda-list)
  "Returns T if the function matches the lambda-list in arguments.
As a secondary value it returns a reason as to why it may have failed the test."
  (let ((lambda-cur (remove-aux-part (function-arguments function)))
        (lambda-act (remove-aux-part lambda-list)))
    (cond
      ((/= (length lambda-cur) (length lambda-act))
       (values NIL "Lambda-lists do not match in length."))
      ((loop for cur in lambda-cur
             for act in lambda-act
             thereis (or (and (lambda-keyword-p cur)
                              (not (lambda-keyword-p act)))
                         (and (lambda-keyword-p act)
                              (not (eql act cur)))))
       (values NIL "Lambda-lists do not match in structure."))
      (T (values T "Test passed.")))))
