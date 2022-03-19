(in-package :extensible-compound-types.impl)

(5am:in-suite :extensible-compound-types)

(defparameter *compound-type-lambdas* (make-hash-table))
(defun compound-type-lambda (type)
  (multiple-value-bind (lambda existsp)
      (if (atom type)
          (gethash type *compound-type-lambdas*)
          (gethash (first type) *compound-type-lambdas*))
    (if existsp
        lambda
        (error 'unknown-type-specifier :type type))))
(defun (setf compound-type-lambda) (lambda type)
  (if (null lambda)
      (if (atom type)
          (remhash type *compound-type-lambdas*)
          (remhash (first type) *compound-type-lambdas*))
      (if (atom type)
          (setf (gethash type *compound-type-lambdas*) lambda)
          (setf (gethash (first type) *compound-type-lambdas*) lambda))))

(defparameter *compound-type-lambda-expressions* (make-hash-table))
(defun compound-type-lambda-expression (type)
  (multiple-value-bind (lambda existsp)
      (if (atom type)
          (gethash type *compound-type-lambda-expressions*)
          (gethash (first type) *compound-type-lambda-expressions*))
    (if existsp
        lambda
        (error 'unknown-type-specifier :type type))))
(defun (setf compound-type-lambda-expression) (lambda type)
  (if (null lambda)
      (if (atom type)
          (remhash type *compound-type-lambda-expressions*)
          (remhash (first type) *compound-type-lambda-expressions*))
      (if (atom type)
          (setf (gethash type *compound-type-lambda-expressions*) lambda)
          (setf (gethash (first type) *compound-type-lambda-expressions*) lambda))))

(defmacro define-compound-type (name (object-name &rest lambda-list) &body body)
  "EXTENSIBLE-COMPOUND-TYPES:TYPEP relies on these whenever TYPE supplied is non-atomic.
FIXME: Should also rely on them when atomic, since FIXNUM type is atomic"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (compound-type-lambda-expression ',name)
           '(lambda (,object-name ,@lambda-list) ,@body))
     (setf (compound-type-lambda ',name)
           (lambda (,object-name ,@lambda-list) ,@body))))
(defun undefine-compound-type (name)
  (setf (compound-type-lambda-expression name) nil)
  (setf (compound-type-lambda name) nil))

(defparameter *compound-type-compiler-macros* (make-hash-table))
(defun compound-type-compiler-macro (type)
  (multiple-value-bind (lambda existsp)
      (if (atom type)
          (gethash type *compound-type-compiler-macros*)
          (gethash (first type) *compound-type-compiler-macros*))
    (when existsp
      lambda)))
(defun (setf compound-type-compiler-macro) (lambda type)
  (if (null lambda)
      (if (atom type)
          (remhash type *compound-type-compiler-macros*)
          (remhash (first type) *compound-type-compiler-macros*))
      (if (atom type)
          (setf (gethash type *compound-type-compiler-macros*) lambda)
          (setf (gethash (first type) *compound-type-compiler-macros*) lambda))))

(defmacro define-compound-type-compiler-macro (name (object-name &rest lambda-list)
                                               &body body
                                               &environment env)
  "EXTENSIBLE-COMPOUND-TYPES:TYPEP relies on these whenever TYPE supplied is non-atomic."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (compound-type-compiler-macro ',name)
           ,(parse-macro name (cons object-name lambda-list) body env))))
(defun undefine-compound-type-compiler-macro (name)
  (setf (compound-type-compiler-macro name) nil))

(defun typep (object type &optional environment)
  "Like CL:TYPEP if TYPE is an ATOM; but otherwise uses COMPOUND-TYPE-LAMBDA to determine TYPEP."
  (declare (ignorable environment))
  (let ((type (typexpand type environment)))
    (if (atom type)
        (cl:typep object type)
        (apply (compound-type-lambda type)
               object
               (rest type)))))

(define-compiler-macro typep (&whole form object-form type-form &optional env-form &environment env)
  (let ((optimize (and (> (second (assoc 'speed (declaration-information 'optimize env)))
                          (second (assoc 'debug (declaration-information 'optimize env))))
                       (> (second (assoc 'speed (declaration-information 'optimize env)))
                          (second (assoc 'safety (declaration-information 'optimize env)))))))
    (compiler-macro-notes:with-notes
        (form nil
         :unwind-on-signal t
         :optimization-note-condition optimize)
      (unless (and (constantp type-form)
                   (constantp env-form))
        (signal 'compiler-macro-notes:optimization-failure-note
                :datum "Cannot determine TYPE and ENV from their compile time forms:~%  ~S~%  ~S"
                :args (list type-form env-form)))
      (let ((type (typexpand (eval type-form) (eval env-form))))
        (cond ((not optimize)
               form)
              ((atom type)
               `(cl:typep ,object-form ',type))
              (t
               (let* ((cm   (compound-type-compiler-macro type))
                      (compound-type-lambda-form `(,(compound-type-lambda-expression type)
                                                   ,object-form
                                                   ,@(loop :for arg :in (if (atom type)
                                                                            nil
                                                                            (rest type))
                                                           :collect `(quote ,arg)))))
                 (if cm
                     (funcall cm compound-type-lambda-form env)
                     compound-type-lambda-form))))))))

(5am:def-test typep ()
  (handler-bind ((warning #'muffle-warning))
    (eval `(progn
             (cl:defstruct pair a b)
             (define-compound-type pair (o &optional (type-a t) (type-b t))
               (and (cl:typep o 'pair)
                    (with-slots (a b) o
                      (and (cl:typep a type-a)
                           (cl:typep b type-b)))))
             (define-type-expander pair (&optional (type-a nil type-a-p) (type-b nil type-b-p))
               (cond ((and type-a-p type-b-p)
                      `(pair ,(typexpand-1 type-a)
                             ,(typexpand-1 type-b)))
                     (type-a-p
                      `(pair ,(typexpand-1 type-a)))
                     (type-b-p
                      `(pair cl:* ,(typexpand-1 type-b)))
                     (t
                      '(pair))))
             (5am:is-true  (typep (make-pair) 'pair))
             (5am:is-true  (typep (make-pair) '(pair)))
             (5am:is-true  (typep (make-pair :a 42 :b "hello") '(pair)))
             (5am:is-true  (typep (make-pair :a 42 :b "hello") '(pair number)))
             (5am:is-true  (typep (make-pair :a 42 :b "hello") '(pair number string)))
             (5am:is-false (typep (make-pair :a 42 :b "hello") '(pair string number)))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (undefine-compound-type 'pair))
             (5am:signals unknown-type-specifier (typep (make-pair) '(pair)))))))

