(in-package #:org.shirakumo.radiance.lib.modularize.interfaces)

(define-component-expander (type deftype) (interface name lambda-list &rest body)
  (let ((name (intern (string name) interface)))
    `(deftype ,name ,lambda-list
       ,@body)))

(define-component-expander (function f defun) (interface name lambda-list &optional documentation)
  (let ((name (intern-function-name interface name)))
    `(defun* ,name ,lambda-list
       ,@(when documentation (list documentation))
       (declare (ignore ,@(extract-lambda-vars lambda-list)))
       (error ,(format NIL "~s is not implemented!" name)))))

(define-component-tester (function f defun) (interface name lambda-list &optional documentation)
  (declare (ignore documentation))
  (function-lambda-matches (intern-function-name interface name) lambda-list))

(define-component-expander (macro m defmacro) (interface name lambda-list &optional documentation)
  (let ((name (intern (string name) interface)))
    `(defmacro* ,name ,lambda-list
       ,@(when documentation (list documentation))
       (declare (ignore ,@(extract-lambda-vars lambda-list)))
       (error ,(format NIL "~s is not implemented!" name)))))

(define-component-tester (macro m defmacro) (interface name lambda-list &optional documentation)
  (declare (ignore documentation))
  (function-lambda-matches (intern (string name) interface) lambda-list))

(defun emit-class-def (macro name super slots options)
  (labels ((i (symb)
             (intern (string symb) (symbol-package name)))
           (int-opt (opts kw)
             (when (getf opts kw)
               (setf (getf opts kw) (i (getf opts kw))))))
    `(,macro ,name ,(loop for class in super
                          collect (or (find-symbol (string class) (symbol-package name))
                                      class))
             ,(loop for (slot . opts) in slots
                    for fixed-ops = (let ((opts (copy-list opts)))
                                      (int-opt opts :accessor)
                                      (int-opt opts :reader)
                                      (int-opt opts :writer)
                                      opts)
                    collect (list* (i slot) fixed-ops))
             ,@options)))

(define-component-expander (condition define-condition) (interface name direct-superclasses direct-slots &body options)
  (emit-class-def 'define-condition (intern (string name) interface)
                  direct-superclasses direct-slots options))

(define-component-expander (class c defclass) (interface name direct-superclasses direct-slots &body options)
  (emit-class-def 'defclass (intern (string name) interface)
                  direct-superclasses direct-slots options))

(define-component-expander (generic gf defgeneric) (interface name lambda-list &body options)
  (let ((name (intern-function-name interface name)))
    `(defgeneric ,name ,lambda-list
       ,@options)))

;; FIXME: Defmethods referring to interface classes in specialisation.

(define-component-expander (method gm defmethod) (interface name lambda-list &optional documentation)
  (let ((name (intern-function-name interface name)))
    `(defmethod* ,name ,lambda-list
       ,@(when documentation (list documentation))
       (declare (ignore ,@(extract-lambda-vars lambda-list)))
       (error ,(format NIL "~s is not implemented!" name)))))

(define-component-expander (variable v defvar) (interface name &optional (value NIL v-p) documentation)
  (let ((name (intern (string name) interface)))
    (if v-p
        `(defvar ,name ,value ,documentation)
        `(defvar ,name))))

(defun check-function-name (name)
  (etypecase name
    (list
     (assert (eql (first name) 'setf)
             () "~s is not a valid function name." name)
     (check-function-name (second name)))
    (symbol
     (assert (eql (implementation (symbol-package name)) *package*)
             () "~s is not an implementation of ~s." *package* (symbol-package name)))))

(defmacro defimpl (name &rest args)
  "Expands to I-DEFMACRO, I-DEFMETHOD or I-DEFUN depending on what kind of symbol it names."
  (cond
    ((macro-function name)
     `(i-defmacro ,name ,@args))
    ((fboundp name)
     (etypecase (symbol-function name)
       (generic-function
        `(i-defmethod ,name ,@args))
       (function
        `(i-defun ,name ,@args))))
    (T (error "The name fits neither macro, method or function?"))))

(defmacro i-defun (name args &body body)
  "Expands to an interface function definition."
  (check-function-name name)
  `(defun ,name ,args
     ,@(if (stringp (first body))
           body
           (cons (documentation name 'function) body))))

(defmacro i-defmacro (name args &body body)
  "Expands to an interface macro definition."
  (check-function-name name)
  `(defmacro ,name ,args
     ,@(if (stringp (first body))
           body
           (cons (documentation name 'function) body))))

(defmacro i-defmethod (name &rest args)
  "Expands to an interface method definition."
  (check-function-name name)
  `(defmethod ,name ,@args))
