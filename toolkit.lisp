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

(defun make-keyword (name)
  (let ((name (string name)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))

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

(defun required-lambda-vars (lambda-list)
  (loop for i in (if (eql '&whole (first lambda-list))
                     (cddr lambda-list)
                     lambda-list)
        until (lambda-keyword-p i)
        collect i))

(defun extract-macro-lambda-vars (macro-lambda-list)
  (loop with varlist = ()
        for i from 0 below (length macro-lambda-list)
        for arg in macro-lambda-list
        do (when (find arg '(&key &optional &rest &body &aux))
             (return (append varlist (extract-lambda-vars (nthcdr i macro-lambda-list)))))
           (unless (lambda-keyword-p arg)
             (if (listp arg)
                 (appendf varlist (extract-lambda-vars arg))
                 (appendf varlist (list arg))))
        finally (return varlist)))

(defun make-key-extensible (generic-lambda-list)
  (if (or (find '&rest generic-lambda-list)
          (find '&body generic-lambda-list))
      generic-lambda-list
      (if (find '&key generic-lambda-list)
          (append generic-lambda-list '(&allow-other-keys))
          (append generic-lambda-list '(&key &allow-other-keys)))))

(defun macro-lambda-list->generic-list (macro-lambda-list)
  (loop with in-required-args = T
        for arg in macro-lambda-list
        collect (cond
                  ((eql arg '&body)
                   (setf in-required-args NIL)
                   '&rest)
                  ((or (eql arg '&rest) (eql arg '&key) (eql arg '&optional))
                   (setf in-required-args NIL)
                   arg)
                  ((and in-required-args (listp arg))
                   (gensym (format NIL "~{~a~^-~}" (extract-lambda-vars arg))))
                  ((listp arg)
                   (car arg))
                  (T arg))))

(defun intern-list-symbols (list package)
  (loop for element in list
        collect (if (and (symbolp element) (not (keywordp element)) (not (null element)))
                    (intern (string-upcase element) package)
                    element)))
