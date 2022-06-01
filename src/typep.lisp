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

(defun compound-type-nonexpander (expr env)
  (declare (ignore env))
  expr)

(defmacro define-compound-type (name-spec (object-name &rest lambda-list) &body body)
  "EXTENSIBLE-COMPOUND-TYPES:TYPEP relies on these whenever TYPE supplied is
  non-atomic and non-class.

NAME-SPEC can be either NAME or (NAME &KEY (NON-NULL T) (CL-TYPE T))

  NON-NULL being non-NIL indicates that no matter what the arguments
    in lambda-list the type is always not nil.
  DEFINE-COMPOUND-TYPE also automatically creates a CL:DEFTYPE using
    UPGRADED-CL-TYPE. However, if the NAME is a symbol in one of the packages
    *EXCLUDED-PACKAGES-FOR-CL-DEFTYPE*, then a CL:DEFTYPE is not created. This
    can be overriden by supplying the value of CL-TYPE.

Note: Whenever possible, it is recommended to use EXTENSIBLE-COMPOUND-TYPES:DEFTYPE
  and only use EXTENSIBLE-COMPOUND-TYPES:DEFINE-COMPOUND-TYPE as a last resort.
  Use of DEFINE-COMPOUND-TYPE also entails getting the %SUBTYPEP and %INTERSECT-TYPE-P
  methods correct.
"
  (destructuring-bind (name &key (non-null t) (cl-type t cl-type-p)) (ensure-list name-spec)
    (let ((doc (nth-value 2 (parse-body body :documentation t)))
          (form (gensym "FORM")))
      `(progn
         ,(when (if cl-type-p
                    cl-type
                    (not (member (symbol-package name) *excluded-packages-for-cl-deftype*)))
            `(cl:deftype ,name (&whole ,form ,@lambda-list)
               ,(ignore-all-form-from-lambda-list lambda-list)
               (upgraded-cl-type ,form)))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (compound-type-lambda-expression ',name)
                 '(lambda (,object-name ,@lambda-list) ,@body))
           (setf (compound-type-lambda ',name)
                 (lambda (,object-name ,@lambda-list) ,@body))
           (setf (type-expander ',name) 'compound-type-nonexpander)
           #-extensible-compound-types
           (setf (symbol-extype ',name) ',lambda-list)
           #+extensible-compound-types
           (setf (symbol-type ',name) ',lambda-list)
           ,(unless non-null
              `(defmethod %subtypep ((t1-name (eql ',name)) (t2-name (eql nil)) type1 type2 &optional env)
                 (declare (ignore t1-name t2-name type1 type2 env))
                 (values nil t)))
           #-extensible-compound-types
           (setf (cl:documentation ',name 'extype)
                 ,(format nil "~S is a BASIC COMPOUND TYPE~%~A" name doc))
           #+extensible-compound-types
           (setf (cl:documentation ',name 'type)
                 ,(format nil (if doc
                                  "~S is a PRIMITIVE COMPOUND TYPE~%~A"
                                  "~S is a PRIMITIVE COMPOUND TYPE")
                          name doc))
           t)))))

(defun undefine-compound-type (name)
  (setf (compound-type-lambda-expression name) nil)
  (setf (compound-type-lambda name) nil)
  (setf (type-expander name) nil)

  #-extensible-compound-types
  (extype-makunbound name)
  #+extensible-compound-types
  (type-makunbound name)
  ;; FIXME: Undefine methods specialized on this type
  ;; ,(unless non-null
  ;;    `(defmethod %subtypep ((t1-name (eql ',name)) (t2-name (eql nil)) type1 type2 &optional env)
  ;;       (declare (ignore t1-name t2-name type1 type2 env))
  ;;       (values nil t)))
  #-extensible-compound-types
  (setf (cl:documentation name 'extype) nil)
  #+extensible-compound-types
  (setf (cl:documentation name 'type) nil))

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
  (let* ((type   (typexpand type environment))
         (atomp  (atom type))
         (classp (and atomp (find-class type nil environment))))
    (cond ((eq t type)
           t)
          ((eq nil type)
           nil)
          (classp
           (cl:typep object type))
          (t
           (apply (compound-type-lambda type)
                  object
                  (rest type))))))

(define-compiler-macro typep (&whole form object-form type-form &optional env-form &environment env)
  (let ((optimize (and (> (second (assoc 'speed (declaration-information 'optimize env)))
                          (second (assoc 'debug (declaration-information 'optimize env))))
                       (> (second (assoc 'speed (declaration-information 'optimize env)))
                          (second (assoc 'safety (declaration-information 'optimize env)))))))
    (compiler-macro-notes:with-notes
        (form env
         :unwind-on-signal t
         :optimization-note-condition optimize)
      (unless (and (constantp type-form)
                   (constantp env-form))
        (signal 'compiler-macro-notes:optimization-failure-note
                :datum "Cannot determine TYPE and ENV from their compile time forms:~%  ~S~%  ~S"
                :args (list type-form env-form)))
      (let* ((type   (typexpand (eval type-form) (eval env-form)))
             (atomp  (atom type))
             (classp (and atomp (find-class type nil (eval env-form)))))
        (cond ((not optimize)
               form)
              ((eq t type)
               t)
              ((eq nil type)
               nil)
              (classp
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
             (define-compound-type (pair :cl-type nil) (o &optional (type-a t) (type-b t))
               (and (cl:typep o 'pair)
                    (with-slots (a b) o
                      (and (cl:typep a type-a)
                           (cl:typep b type-b)))))
             (5am:is-true  (typep (make-pair) 'pair))
             (5am:is-true  (typep (make-pair) '(pair)))
             (5am:is-true  (typep (make-pair :a 42 :b "hello") '(pair)))
             (5am:is-true  (typep (make-pair :a 42 :b "hello") '(pair number)))
             (5am:is-true  (typep (make-pair :a 42 :b "hello") '(pair number string)))
             (5am:is-false (typep (make-pair :a 42 :b "hello") '(pair string number)))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (undefine-compound-type 'pair))
             (5am:signals unknown-type-specifier (typep (make-pair) '(pair)))))))

