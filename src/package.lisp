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
           #+extensible-compound-types
           #:ftype
           #:exftype

           #:type-specifier-p
           #:typep
           #:subtypep
           #:%subtypep
           #:intersect-type-p
           #:%intersect-type-p
           #:intersection-null-p
           #:specializing-type-name-p

           #:type=
           #:supertypep

           #:the
           #:check-type
           #:*the-skip-predicates*

           #:type-name-parameters

           #:define-compound-type
           #:undefine-compound-type
           #:define-compound-type-compiler-macro
           #:undefine-compound-type-compiler-macro
           #:upgraded-cl-type
           #:%upgraded-cl-type

           #:*excluded-packages-for-cl-deftype*
           #:define-mutually-exclusive-types

           #:unknown-type-specifier)
  (:import-from :trivial-types
                #:character-designator))

(defpackage :extensible-compound-types.impl
  (:use :cl-environments-cl :extensible-compound-types :alexandria)
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
   #+extensible-compound-types
   #:ftype

   #:extype
   #:exftype

   #:typep
   #:subtypep
   #:type=
   #:check-type
   #:the

   #:unknown-type-specifier)
  (:shadow #:find-class)
  (:import-from :trivial-types
                #:character-designator)
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
  ;; On CCL, args starts with DECL-NAME while not using CL-ENVIRONMENTS-CL
  ;; Other times, it starts with the appropriate args
  (destructuring-bind (type &rest vars) (optima:match args
                                          ((list* 'extype args)
                                           args)
                                          (_ args))
    (values :variable
            (mapcar (lambda (var)
                      (list var 'extype type))
                    vars))))

(define-declaration exftype (args)
  ;; On CCL, args starts with DECL-NAME while not using CL-ENVIRONMENTS-CL
  ;; Other times, it starts with the appropriate args
  (destructuring-bind (type &rest vars) (optima:match args
                                          ((list* 'exftype args)
                                           args)
                                          (_ args))
    (values :function
            (mapcar (lambda (var)
                      (list var 'exftype type))
                    vars))))

#+extensible-compound-types
(define-declaration type (args)
  ;; On CCL, args starts with DECL-NAME while not using CL-ENVIRONMENTS-CL
  ;; Other times, it starts with the appropriate args
  (destructuring-bind (type &rest vars) (optima:match args
                                          ((list* 'type args)
                                           args)
                                          (_ args))
    (values :variable
            (append (mapcar (lambda (var)
                              (list var 'type type))
                            vars)
                    (mapcar (lambda (var)
                              (list var 'extype type))
                            vars)))))

#+extensible-compound-types
(define-declaration ftype (args)
  ;; On CCL, args starts with DECL-NAME while not using CL-ENVIRONMENTS-CL
  ;; Other times, it starts with the appropriate args
  (destructuring-bind (type &rest vars) (optima:match args
                                          ((list* 'ftype args)
                                           args)
                                          (_ args))
    (values :function
            (append (mapcar (lambda (var)
                              (list var 'exftype type))
                            vars)
                    (mapcar (lambda (var)
                              (list var 'ftype type))
                            vars)))))

(defun find-class (name &optional errorp environment)
  #-sbcl
  (if errorp
      (cl:find-class name t environment)
      (ignore-errors (cl:find-class name nil environment)))
  #+sbcl
  (cl:find-class name errorp environment))

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

(defun type-expander (name &optional (error-if-not-exists t))
  (multiple-value-bind (expander existsp)
      (gethash name *type-expanders*)
    (cond (existsp
           expander)
          (error-if-not-exists
           (error 'unknown-type-specifier :type name))
          (t
           nil))))

(defun (setf type-expander) (lambda name)
  (if lambda
      (setf (gethash name *type-expanders*) lambda)
      (remhash name *type-expanders*)))

(defparameter *type-name-parameters* (make-hash-table))

(defun type-name-parameters (name &optional (error-if-not-exists t))
  (multiple-value-bind (name-parameters existsp)
      (gethash name *type-name-parameters*)
    (cond (existsp
           name-parameters)
          ((find-class name nil)
           name)
          (error-if-not-exists
           (error 'unknown-type-specifier :type name))
          (t
           nil))))

(defun (setf type-name-parameters) (name-parameters name)
  (if name-parameters
      (setf (gethash name *type-name-parameters*) name-parameters)
      (remhash name *type-name-parameters*)))



(defvar *excluded-packages-for-cl-deftype*
  (mapcar #'find-package '(:cl :alexandria :trivial-types :sb-kernel))
  "EXTENSIBLE-COMPOUND-TYPES:DEFTYPE avoids adding a CL:DEFTYPE if the NAME is
a symbol in package excluded in this list.")
(cl:declaim (type list *excluded-packages-for-cl-deftype*))

(defun ignore-all-form-from-lambda-list (lambda-list)
  (let ((vars (loop :for state := 'required
                    :for form :in lambda-list
                    :if (member form (cons 'required lambda-list-keywords))
                      :do (setq state form)
                    :else
                      :appending (etypecase form
                                   (symbol (list form))
                                   (list (ecase (length form)
                                           (1 (list (first form)))
                                           (2 (list (first form)))
                                           (3 (list (first form)
                                                    (third form)))))))))
    `(declare (ignore ,@vars))))

(defmacro deftype (name lambda-list &body body &environment env)
  "Useful for defining type aliases, example: (DEFTYPE INT32 () '(SIGNED-BYTE 32))

Depending on the value of *EXCLUDED-PACKAGES-FOR-CL-DEFTYPE*,
also adds a CL:DEFTYPE with the expansion being determined by UPGRADED-CL-TYPE"
  (multiple-value-bind (body decl doc) (parse-body body :documentation t)
    (with-gensyms (form)
      `(progn
         ,(unless (member (symbol-package name) *excluded-packages-for-cl-deftype*)
            `(cl:deftype ,name (&whole ,form ,@lambda-list)
               ,@(when doc `(,doc))
               ,(ignore-all-form-from-lambda-list lambda-list)
               (upgraded-cl-type ,form)))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (type-name-parameters ',name) '(,name ,@lambda-list))
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
           (setf (cl:documentation ',name 'type) ,doc)
           t)))))

;;; TODO: Could introduce a TMAKUNBOUND
(defmacro undeftype (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (type-expander ',name) nil)))

(defun type-specifier-p (object &optional env)
  (let ((object (typexpand object env)))
    (typecase object
      (symbol
       (let* ((classp   (find-class object nil env))
              (expander (type-expander object nil)))
         (or classp
             (cond ((and expander
                         (specializing-type-name-p object))
                    t)
                   ((and expander
                         (not (specializing-type-name-p object)))
                    nil)
                   (t
                    nil)))))
      (list
       (let* ((type-name       (car object))
              (name-parameters (type-name-parameters type-name nil))
              (num-required    (if (listp name-parameters)
                                   (loop :for i :from 0
                                         :for parameter :in (rest name-parameters)
                                         :if (member parameter lambda-list-keywords)
                                           :do (return i)
                                         :finally (return i))
                                   0))
              (num-supplied    (etypecase object
                                 (atom 0)
                                 (list (1- (length object))))))
         (and (type-expander type-name nil)
              (<= num-required num-supplied)))))))

(defun typexpand-1 (type &optional env)
  "Returns two values: EXPANSION and EXPANDEDP"
  (when (member type '(cl:t cl:nil))
    (return-from typexpand-1 type))
  (let* ((atomp (atom type))
         (type-name (if (atom type)
                        #-ecl type
                        #+ecl (if (eq type 'c::gen-bool) 'boolean type)
                        (car type)))
         (classp (and atomp (find-class type-name nil env)))
         (expander (type-expander type-name nil))
         (expansion (cond ((eq expander 'compound-type-nonexpander)
                           (return-from typexpand-1 (values type nil)))
                          (expander
                           (funcall expander
                                    (if atomp (list type) type)
                                    env))
                          (classp
                           type)
                          ((null type)
                           nil)
                          (t
                           type))))
    ;; FIXME: Why do we need EXPANSION as well as CLASSP?
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
