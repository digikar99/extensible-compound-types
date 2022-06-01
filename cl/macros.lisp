(in-package :extensible-compound-types-cl.impl)

(defmacro excl:lambda (args &body body &environment env)
  (multiple-value-bind (rem-body decl doc-string) (a:parse-body body :documentation t)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      (multiple-value-bind (type-decl remaining-decls-2)
          #+extensible-compound-types (extract-declaration decl 'ex:type)
          #-extensible-compound-types nil
        #+extensible-compound-types
        (setq extype-decl (cond ((and extype-decl type-decl)
                                 (append extype-decl
                                         (rest type-decl)))
                                (type-decl
                                 type-decl)
                                (extype-decl
                                 extype-decl)
                                (nil
                                 nil))
              remaining-decls (append remaining-decls
                                      remaining-decls-2))
        `(clel:lambda ,args
           ,(or doc-string "")
           ,@(remove-if #'null
                        (list* (cl-type-declarations extype-decl env)
                               extype-decl
                               remaining-decls))
           ,@(prepare-extype-checks extype-decl)
           ,@rem-body)))))

(5am:def-test excl:lambda ()
  (5am:is-true (eval `(excl:lambda (x) "" (declare (type integer x)) x)))
  (5am:is-true (eval `(excl:lambda (x) ""
                        x)))
  (5am:is-true (eval `(excl:lambda (x) "" (declare (excl:extype integer x)) x))))

(defmacro excl:defun (name lambda-list &body body &environment env)
  `(clel:defun ,name ,@(rest (macroexpand-1 `(excl:lambda ,lambda-list ,@body) env))))

(defmacro excl:defmacro (name lambda-list &body body &environment env)
  `(clel:defmacro ,@(rest (macroexpand-1 `(excl:defun ,name ,lambda-list ,@body) env))))

(defmacro excl:define-compiler-macro (name lambda-list &body body &environment env)
  `(clel:define-compiler-macro ,@(rest (macroexpand-1 `(excl:defun ,name ,lambda-list ,@body) env))))

(defmacro excl:defmethod (name &rest args &environment env)
  (if (listp (first args))
      `(clel:defmethod ,@(rest (macroexpand-1 `(excl:defun ,name ,@args) env)))
      `(clel:defmethod ,name ,(first args)
         ,@(nthcdr 2 (macroexpand-1 `(excl:defun ,name ,@(rest args)) env)))))

(defmacro excl:define-condition (name (&rest parent-types) (&rest slot-specs)
                                 &body options
                                 &environment env)
  `(clel:define-condition ,name ,parent-types
     ,slot-specs
     ,@(let ((report-value (second (assoc :report options))))
         (if (and (listp report-value)
                  (member (first report-value) '(cl:lambda excl:lambda clel:lambda)))
             (cons `(:report (cl:lambda
                                 ,@(rest (macroexpand-1 `(excl:lambda ,@(rest report-value)) env))))
                   (remove :report options :key #'first))
             options))))

(defmacro excl:destructuring-bind (lambda-list expression &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(clel:destructuring-bind ,lambda-list ,expression
         ,@(remove-if #'null
                      (list* (cl-type-declarations extype-decl env)
                             extype-decl
                             remaining-decls))
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:multiple-value-bind (vars value-form &body body &environment env)
  `(clel:multiple-value-bind
         ,@(rest (macroexpand-1 `(excl:destructuring-bind ,vars ,value-form ,@body) env))))

(defmacro excl:do (varlist endlist &body body &environment env)
  `(clel:do ,@(rest (macroexpand-1 `(excl:destructuring-bind ,varlist ,endlist ,@body) env))))

(defmacro excl:do* (varlist endlist &body body &environment env)
  `(clel:do* ,@(rest (macroexpand-1 `(excl:do ,varlist ,endlist ,@body) env))))

(defmacro excl:do-all-symbols ((var &optional (result-form nil result-form-p))
                               &body body-decls &environment env)
  `(clel:do-all-symbols ,(if result-form-p
                           `(,var ,result-form)
                           `(,var))
     ,@(decl-and-type-check-body body-decls env)))

(defmacro excl:do-external-symbols ((var
                                     &optional (package (quote cl:*package*) packagep)
                                       (result-form nil result-form-p))
                                    &body body-decls &environment env)
  `(clel:do-external-symbols ,(cond ((and packagep result-form-p)
                                   `(,var ,package ,result-form))
                                  (packagep
                                   `(,var ,package))
                                  (t
                                   `(,var)))
     ,@(decl-and-type-check-body body-decls env)))

(defmacro excl:dolist ((var list &optional (result-form nil result-form-p))
                       &body body &environment env)
  `(clel:dolist ,(if result-form-p
                   `(,var ,list ,result-form)
                   `(,var ,list))
     ,@(decl-and-type-check-body body env)))

(defmacro excl:dotimes ((var count &optional (result-form nil result-form-p))
                        &body body &environment env)
  `(clel:dotimes ,(if result-form-p
                    `(,var ,count ,result-form)
                    `(,var ,count))
     ,@(decl-and-type-check-body body env)))

(defmacro excl:handler-case (form &rest cases &environment env)
  `(clel:handler-case ,form
     ,@(loop :for case :in cases
             :collect (destructuring-bind (typespec lambda-list &body body)
                          case
                        `(,typespec ,lambda-list ,@(decl-and-type-check-body body env))))))

(defmacro excl:restart-case (expression &body clauses &environment env)
  `(clel:restart-case ,expression
     ,@(loop :for clause :in clauses
             :collect
             (destructuring-bind (case-name arg-list &body body) clause
               `(,case-name
                 ,arg-list
                 ,@(loop :for body-form :in body
                         :with keywords-end-p := nil
                         :for i :from 0
                         :collect
                         (if (or keywords-end-p
                                 (evenp i))
                             body-form
                             (if (and (listp body-form)
                                      (member (first body-form)
                                              '(cl:lambda excl:lambda clel:lambda)))
                                 `(clel:lambda ,@(rest
                                                (macroexpand-1
                                                 `(excl:lambda ,@(rest body-form))
                                                 env)))
                                 body-form))))))))

(defmacro excl:with-slots (slots instance &body body &environment env)
  `(clel:with-slots ,slots ,instance
     ,@(decl-and-type-check-body body env)))
