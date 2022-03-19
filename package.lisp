(defpackage :extensible-compound-types
  (:use)
  (:export #:deftype
           #:undeftype

           #:define-type-expander
           #:typexpand-1
           #:typexpand
           #:typexpand-all
           #:typelet
           #:typelet*

           #+extensible-compound-types
           #:type
           #-extensible-compound-types
           #:extype

           #:typep
           #:subtypep
           #:%subtypep

           #:the
           #:check-type
           #:*the-skip-predicates*

           #:define-compound-type
           #:undefine-compound-type
           #:define-compound-type-compiler-macro
           #:undefine-compound-type-compiler-macro
           #:upgraded-cl-type
           #:%upgraded-cl-type

           #:unknown-type-specifier))

(defpackage :extensible-compound-types.impl
  (:use :cl :extensible-compound-types :alexandria)
  (:shadowing-import-from :extensible-compound-types
   #:deftype
   #:undeftype

   #:typexpand-1
   #:typexpand
   #:typexpand-all
   #:typelet
   #:typelet*

   #+extensible-compound-types
   #:type
   #-extensible-compound-types
   #:extype

   #:typep
   #:subtypep
   #:check-type
   #:the

   #:unknown-type-specifier)
  (:import-from :cl-environments.cltl2
                #:parse-macro
                #:define-declaration
                #:declaration-information))

(in-package :extensible-compound-types.impl)

(5am:def-suite :extensible-compound-types)
(5am:in-suite  :extensible-compound-types)

(define-declaration extype (args)
  (destructuring-bind (type &rest vars) args
    (values :variable
            (mapcar (lambda (var)
                      (list var 'extype type))
                    vars))))

(defun atomic-type-specifier-p (type)   ; TODO: Take ENV
  (and (atom type)
       (or (find-class type nil)
           (member type '(t nil)))))

(define-condition unknown-type-specifier (error)
  ((type-specifier :initarg :type))
  (:report (lambda (c s)
             (with-slots (type-specifier) c
               (format s "Unknown type-specifier ~S" type-specifier)))))

(defparameter *type-expanders* (make-hash-table))

(defun type-expander (name)
  (multiple-value-bind (expander existsp)
      (gethash name *type-expanders*)
    (if existsp
        expander
        (error 'unknown-type-specifier :type name))))

(defun (setf type-expander) (lambda name)
  (if lambda
      (setf (gethash name *type-expanders*) lambda)
      (remhash name *type-expanders*)))

(defmacro define-type-expander (name lambda-list &body body &environment env)
  "Used by TYPEXPAND-1 and TYPEXPAND to expand the types.
These in turn are used by TYPEP and SUBTYPEP."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (type-expander ',name)
           ,(parse-macro name lambda-list body env))))

(defmacro deftype (name lambda-list &body body &environment env)
  "Useful for defining type aliases, example: (DEFTYPE INT32 () '(SIGNED-BYTE 32))"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (type-expander ',name)
           ,(parse-macro name
                         lambda-list
                         body
                         env))))

;;; TODO: Could introduce a TMAKUNBOUND
(defmacro undeftype (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (type-expander ',name) nil)))

;; FIXME: Semantics of typexpand-1, typexpand, typexpand-all
(defun typexpand-1 (type &optional env)
  "Returns two values: EXPANSION and EXPANDEDP"
  (let* ((atomp (atom type))
         (type-name (if (atom type) type (car type)))
         (classp (and atomp (find-class type-name nil env))))
    (if classp
        type
        (funcall (type-expander type-name)
                 (if atomp (list type) type)
                 env))))

(defun typexpand (type &optional env)
  (let ((expansion (typexpand-1 type env)))
    (if (equal expansion type)
        expansion
        (typexpand expansion env))))

(5am:def-test define-and-undefine-types ()
  (eval `(progn
           (deftype float32 () 'single-float)
           (deftype f32 () 'float32)
           (5am:is (eq 'float32 (typexpand-1 'f32)))
           (5am:is (eq 'single-float (typexpand-1 'float32)))
           (5am:is (eq 'single-float (typexpand 'f32)))
           (undeftype f32)
           (undeftype float32)
           (5am:signals unknown-type-specifier (typexpand 'f32))
           (5am:signals unknown-type-specifier (typexpand 'float32)))))
