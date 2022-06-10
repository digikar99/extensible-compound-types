(in-package :extensible-compound-types.impl)

;; FIXME: These should not be valid as ATOMIC TYPE SPECIFIERS

(define-compound-type (and :non-null nil) (o &rest type-specifiers)
  (every (lambda (type) (typep o type))
         type-specifiers))

(define-compound-type-compiler-macro and (o-form &rest type-specifier-forms)
  (once-only (o-form)
    `(cl:and ,@(loop :for type-specifier-form :in type-specifier-forms
                     :collect `(typep ,o-form ,type-specifier-form)))))

(define-compound-type or (o &rest type-specifiers)
  (declare (dynamic-extent type-specifiers))
  (loop :for type :in type-specifiers
          :thereis (typep o type)))

(define-compound-type-compiler-macro or (o-form &rest type-specifier-forms)
  (once-only (o-form)
    `(cl:or ,@(loop :for type-specifier-form :in type-specifier-forms
                    :collect `(typep ,o-form ,type-specifier-form)))))

(define-compound-type eql (o object)
  (cl:eql o object))

(define-compound-type (member :non-null nil) (o &rest objects)
  (member o objects :test #'cl:eql))

(define-compound-type not (o typespec)
  (not (typep o typespec)))

(define-compound-type (satisfies :non-null nil) (o predicate-name)
  (funcall (fdefinition predicate-name) o))

(defmethod %subtypep ((t1-name (eql 'satisfies)) (t2-name (eql nil)) type1 type2
                      &optional env)
  (declare (ignore t1-name t2-name type1 type2 env))
  (values nil nil))

(define-compound-type (values :non-null nil) (o &rest type-specifiers)
  (typep o (first type-specifiers)))

