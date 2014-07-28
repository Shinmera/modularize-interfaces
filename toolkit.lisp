#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.interfaces)

(defvar *redefine* NIL)

(defmacro defun* (name args &body body)
  `(when (or *redefine* (not (fboundp ',name))) 
     (defun ,name ,args ,@body)))

(defmacro defmacro* (name args &body body)
  `(when (or *redefine* (not (fboundp ',name)))
     (defmacro ,name ,args ,@body)))

(defmacro defmethod* (name &rest args)
  (let ((qualifiers (loop for arg in args until (listp arg) collect arg))
        (lambda-list (loop for arg in args when (listp arg) do (return arg))))
    `(when (or *redefine* (not (find-method #',name ',qualifiers (mapcar #'find-class ',lambda-list))))
       (defmethod ,name ,@args))))

(defmacro with-muffled-warnings (&body body)
  `(locally
       (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind
         (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
       ,@body)))

(defun make-keyword (name)
  (let ((name (string name)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))

(defun intern-function-name (package name)
  (if (listp name)
      (if (eql (car name) 'setf)
          (list 'setf (intern-function-name package (second name)))
          (error "Invalid name supplied."))
      (or (find-symbol (string name) package)
          (intern (string name) package))))

(defun lambda-keyword-p (symbol)
  (find symbol '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

(defun remove-aux-part (lambda-list)
  (let ((position (position '&aux lambda-list)))
    (if position
        (subseq lambda-list 0 position)
        lambda-list)))

(defun flatten-lambda-list (lambda-list)
  (mapcar #'(lambda (a) (if (listp a) (car a) a)) lambda-list))

(defun extract-lambda-vars (lambda-list)
  (remove-if #'lambda-keyword-p (flatten-lambda-list (remove-aux-part lambda-list))))

(defun function-arguments (function)
  "Returns the lambda-list of the function if possible.
This is only implemented with: SBCL, SWANK, working FUNCTION-LAMBDA-EXPRESSION."
  #+sbcl (sb-introspect:function-lambda-list function)
  #+(and swank (not sbcl)) (swank-backend:arglist function)
  #-(and sbcl swank) (second (nth-value 2 (function-lambda-expression function))))

(defun function-lambda-matches (function lambda-list)
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
