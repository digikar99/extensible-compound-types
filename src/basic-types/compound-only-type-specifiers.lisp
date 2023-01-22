(in-package :extensible-compound-types.impl)

;; FIXME: These should not be valid as ATOMIC TYPE SPECIFIERS

(define-compound-type(and :specializing nil :cl-type nil) (o &rest type-specifiers)
  (every (lambda (type) (typep o type))
         type-specifiers))

(define-cl-type-for-extype and (type env)
  `(and ,@(mapcar (lambda (type) (upgraded-cl-type type env)) (rest type))))

(define-compound-type-compiler-macro and (o-form &rest type-specifier-forms)
  (once-only (o-form)
    `(cl:and ,@(loop :for type-specifier-form :in type-specifier-forms
                     :collect `(typep ,o-form ,type-specifier-form)))))

(define-type t () `(and))

(define-compound-type (or :specializing nil :cl-type nil) (o &rest type-specifiers)
  (declare (dynamic-extent type-specifiers))
  (loop :for type :in type-specifiers
          :thereis (typep o type)))

(define-cl-type-for-extype or (type env)
  `(or ,@(mapcar (lambda (type) (upgraded-cl-type type env)) (rest type))))

(define-compound-type-compiler-macro or (o-form &rest type-specifier-forms)
  (once-only (o-form)
    `(cl:or ,@(loop :for type-specifier-form :in type-specifier-forms
                    :collect `(typep ,o-form ,type-specifier-form)))))

(define-type nil () `(or))



(define-compound-type (eql :specializing nil :cl-type nil) (o object)
  (cl:eql o object))



(define-type member (&rest objects)
  `(or ,@(loop :for o :in objects :collect `(eql ,o))))



(define-compound-type (not :specializing nil :cl-type nil) (o typespec)
  (not (typep o typespec)))

(define-cl-type-for-extype not (type env)
  `(not ,(upgraded-cl-type (second type) env)))

(define-compound-type (satisfies :specializing nil :cl-type nil) (o predicate-name)
  (funcall (fdefinition predicate-name) o))

(define-subtypep-lambda (satisfies nil) (type1 type2 env)
  (declare (ignore type1 type2 env))
  (values nil nil))

(define-compound-type (values :specializing nil :cl-type nil) (o &rest type-specifiers)
  (typep o (first type-specifiers)))

(define-cl-type-for-extype values (type env)
  `(values ,@(mapcar (lambda (type)
                       (if (member type lambda-list-keywords)
                           type
                           (upgraded-cl-type type env)))
                     (rest type))))
