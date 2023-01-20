(in-package :extensible-compound-types.impl)

(5am:in-suite :extensible-compound-types)

(defun typep (object type &optional environment)
  (declare (ignorable environment))
  (with-extype-name-and-expansion (name exp) type
    (apply (extype-lambda (extype-structure name))
           object
           (etypecase exp
             (atom nil)
             (cons (rest exp))))))

(define-compiler-macro typep
    (&whole form object-form type-form &optional env-form &environment env)
  (let ((optimize (and (> (env-speed env) (env-debug env))
                       (> (env-speed env) (env-safety env)))))
    (compiler-macro-notes:with-notes
        (form env
              :unwind-on-signal t
              :optimization-note-condition optimize)
      (labels ((type-form-failure ()
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Cannot determine TYPE from its compile time form:~%  ~S~%"
                         :args (list type-form)))
               (env-form-failure ()
                 (signal 'compiler-macro-notes:optimization-failure-note
                         :datum "Cannot determine ENV from its compile time form:~%  ~S~%"
                         :args (list env-form)))
               (may-be-constant-type-value ()
                 (cond ((and (constantp type-form env)
                             (constantp env-form env))
                        (typexpand (eval type-form) (eval env-form)))
                       ((constantp env-form env)
                        (type-form-failure))
                       (t
                        (env-form-failure)))))
        (let* ((expansion (if (find-package :cl-form-types)
                              (let ((type-form-type (uiop:symbol-call '#:cl-form-types
                                                                      '#:nth-form-type
                                                                      type-form env 0 t t))
                                    (env-form-value (if (constantp env-form)
                                                        (eval env-form)
                                                        (type-form-failure))))
                                (if (and (not (type= t type-form-type))
                                         (null env-form-value))
                                    (typexpand (optima:ematch (typexpand type-form-type env)
                                                 ((list 'eql type) type)
                                                 ((list 'member type) type)
                                                 (_ (may-be-constant-type-value)))
                                               env-form-value)
                                    (type-form-failure)))
                              (may-be-constant-type-value)))
               (name       (type-name expansion)))
          ;; Reaching this point means that TYPE-FORM-FAILURE was not called
          ;; and therefore the TYPE is a constant type
          (cond ((not optimize)
                 form)
                (t
                 (with-slots (compiler-macro lambda-expression)
                     (extype-structure name)
                   (let ((lambda-form
                           `(,lambda-expression
                             ,object-form
                             ,@(loop :for arg :in (if (atom expansion)
                                                      nil
                                                      (rest expansion))
                                     :collect `(quote ,arg)))))
                     (cond (compiler-macro
                            (funcall compiler-macro lambda-form env))
                           (t
                            lambda-form)))))))))))

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
