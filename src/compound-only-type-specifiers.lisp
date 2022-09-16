(in-package :extensible-compound-types.impl)

;; FIXME: These should not be valid as ATOMIC TYPE SPECIFIERS

(define-compound-type (and :non-null nil) (o &rest type-specifiers)
  (every (lambda (type) (typep o type))
         type-specifiers))

(defmethod %upgraded-cl-type ((name (eql 'and)) type &optional env)
  `(and ,@(mapcar (lambda (type) (upgraded-cl-type type env)) (rest type))))

(define-compound-type-compiler-macro and (o-form &rest type-specifier-forms)
  (once-only (o-form)
    `(cl:and ,@(loop :for type-specifier-form :in type-specifier-forms
                     :collect `(typep ,o-form ,type-specifier-form)))))



(define-compound-type (or :specializing nil) (o &rest type-specifiers)
  (declare (dynamic-extent type-specifiers))
  (loop :for type :in type-specifiers
          :thereis (typep o type)))

(defmethod %upgraded-cl-type ((name (eql 'or)) type &optional env)
  `(or ,@(mapcar (lambda (type) (upgraded-cl-type type env)) (rest type))))

(define-compound-type-compiler-macro or (o-form &rest type-specifier-forms)
  (once-only (o-form)
    `(cl:or ,@(loop :for type-specifier-form :in type-specifier-forms
                    :collect `(typep ,o-form ,type-specifier-form)))))



(define-compound-type (eql :specializing nil) (o object)
  (cl:eql o object))



(deftype member (&rest objects)
  `(or ,@(loop :for o :in objects :collect `(eql ,o))))



(define-compound-type (not :specializing nil) (o typespec)
  (not (typep o typespec)))

(defmethod %upgraded-cl-type ((name (eql 'not)) type &optional env)
  `(not ,(upgraded-cl-type (second type) env)))

(define-compound-type (satisfies :non-null nil :specializing nil) (o predicate-name)
  (funcall (fdefinition predicate-name) o))

(defmethod %subtypep ((t1-name (eql 'satisfies)) (t2-name (eql nil)) type1 type2
                      &optional env)
  (declare (ignore t1-name t2-name type1 type2 env))
  (values nil nil))

(define-compound-type (values :non-null nil :specializing nil) (o &rest type-specifiers)
  (typep o (first type-specifiers)))

(defmethod %upgraded-cl-type ((name (eql 'values)) type &optional env)
  `(values ,@(mapcar (lambda (type)
                       (if (member type lambda-list-keywords)
                           type
                           (upgraded-cl-type type env)))
                     (rest type))))
