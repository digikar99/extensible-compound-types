(in-package :extensible-compound-types.impl)

;; FIXME: These should not be valid as ATOMIC TYPE SPECIFIERS

(define-compound-type and (o &rest type-specifiers)
  (every (lambda (type) (typep o type))
         type-specifiers))

(define-compound-type-compiler-macro and (o-form &rest type-specifier-forms)
  (once-only (o-form)
    `(cl:and ,@(loop :for type-specifier-form :in type-specifier-forms
                     :collect `(typep ,o-form ,type-specifier-form)))))

(defun flatten-type-specifier-combination (combination type-specifier &optional env)
  (flet ((typexpand-may-be (type-specifier)
           (if (atom type-specifier)
               (typexpand type-specifier env)
               type-specifier)))
    (let ((ts (typexpand-may-be type-specifier)))
      (assert (and (listp ts)
                   (eq combination (first ts)))
              (combination ts))
      (cons combination
            (loop :for type :in (rest ts)
                  :for expanded-type := (typexpand-may-be type)
                  :if (and (listp expanded-type)
                           (eq combination (first expanded-type)))
                    :appending (rest (flatten-type-specifier-combination combination expanded-type))
                  :else
                    :collect type)))))

(defun simplify-and-type (and-type-specifier &optional env)
  (let ((ts (flatten-type-specifier-combination 'and and-type-specifier env))
        (non-redundant-types ()))
    (loop :for ts1 :in (sort (copy-list (rest ts)) #'subtypep)
          :do (unless (loop :for ts2 :in non-redundant-types
                              :thereis (supertypep ts1 ts2 env))
                (push ts1 non-redundant-types)))
    (if (null (cdr non-redundant-types))
        (car non-redundant-types)
        (cons 'and non-redundant-types))))

(defmethod %subtypep ((t1-name (eql 'and)) t2-name type1 type2 &optional env)
  (let ((type1 (simplify-and-type type1)))
    (cond ((and (listp type1)
                (eq 'and (first type1)))
           (values (some (lambda (type)
                           (subtypep type type2 env))
                         (rest type1))
                   t))
          (t
           (values nil nil)))))

(defmethod %subtypep (t1-name (t2-name (eql 'and)) type1 type2 &optional env)
  (let ((type2 (simplify-and-type type2)))
    (cond ((and (listp type2)
                (eq 'and (first type2)))
           (values (every (lambda (type)
                           (subtypep type1 type env))
                          (rest type2))
                   t))
          (t
           (values nil nil)))))

(defmethod %subtypep ((t1-name (eql 'and)) (t2-name (eql 'and)) type1 type2 &optional env)
  ;; TODO: Write this
  (let ((type1 (simplify-and-type type1))
        (type2 (simplify-and-type type2)))
    (cond ((and (listp type1)
                (eq 'and (first type1)))
           (values (some (lambda (type)
                           (subtypep type type2 env))
                         (rest type1))
                   t))
          (t
           (values nil nil)))))

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

(define-compound-type member (o &rest objects)
  (member o objects :test #'cl:eql))

(define-compound-type not (o typespec)
  (not (typep o typespec)))

(define-compound-type satisfies (o predicate-name)
  (funcall (fdefinition predicate-name) o))

