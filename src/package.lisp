(defpackage :extensible-compound-types
  (:use)
  (:export #:deftype
           #:undeftype

           #:typexpand-1
           #:typexpand
           #:typexpand-all
           #:typelet
           #:typelet*

           #+extensible-compound-types
           #:type
           #:extype

           #:typep
           #:subtypep
           #:%subtypep
           #:intersect-type-p
           #:%intersect-type-p

           #:type=
           #:supertypep

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
   #:type=
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

(in-nomine:define-namespace extype)

#+extensible-compound-types
(in-nomine:define-namespace type)

(define-declaration extype (args)
  (destructuring-bind (type &rest vars) args
    (values :variable
            (mapcar (lambda (var)
                      (list var 'extype type))
                    vars))))

#+extensible-compound-types
(define-declaration type (args)
  (destructuring-bind (type &rest vars) args
    (values :variable
            (append (mapcar (lambda (var)
                              (list var 'type type))
                            vars)
                    (mapcar (lambda (var)
                              (list var 'extype type))
                            vars)))))

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

(defmacro deftype (name lambda-list &body body &environment env)
  "Useful for defining type aliases, example: (DEFTYPE INT32 () '(SIGNED-BYTE 32))"
  (multiple-value-bind (body decl doc) (parse-body body :documentation t)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (type-expander ',name)
             ,(parse-macro name
                           lambda-list
                           (append decl body)
                           env))
       #-extensible-compound-types
       (setf (symbol-extype ',name) ',lambda-list)
       #+extensible-compound-types
       (setf (symbol-type ',name) ',lambda-list)
       #-extensible-compound-types
       (setf (cl:documentation ',name 'extype) ,doc)
       #+extensible-compound-types
       (setf (cl:documentation ',name 'type) ,doc))))

;;; TODO: Could introduce a TMAKUNBOUND
(defmacro undeftype (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (type-expander ',name) nil)))

(defun typexpand-1 (type &optional env)
  "Returns two values: EXPANSION and EXPANDEDP"
  (let* ((atomp (atom type))
         (type-name (if (atom type) type (car type)))
         (classp (and atomp (find-class type-name nil env)))
         (expander (ignore-some-conditions (unknown-type-specifier)
                     (type-expander type-name)))
         (expansion (cond (expander
                           (funcall expander
                                    (if atomp (list type) type)
                                    env))
                          (classp
                           type)
                          ((null type)
                           nil)
                          (t
                           type))))
    (if (and classp
             (listp expansion)
             (null (cdr expansion)))
        (values (first expansion) (not (equal type (first expansion))))
        (values expansion (not (equal expansion type))))))

(defun typexpand (type &optional env)
  (let ((expansion (typexpand-1 type env)))
    (if (equal expansion type)
        (values expansion nil)
        (values (typexpand expansion env) t))))

(5am:def-test define-and-undefine-types ()
  (eval `(progn
           (deftype float32 () 'single-float)
           (deftype f32 () 'float32)
           (5am:is (eq 'float32 (typexpand-1 'f32)))
           (5am:is (eq 'single-float (typexpand-1 'float32)))
           (5am:is (eq 'single-float (typexpand 'f32)))
           (undeftype f32)
           (undeftype float32))))