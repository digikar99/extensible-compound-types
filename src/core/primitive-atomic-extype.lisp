(in-package :extensible-compound-types.impl)

(defmacro define-primitive-atomic-extype (name &body body)
  "If the first element of BODY is a SYMBOL then, then defines NAME as
equivalent to the CL class corresponding to the symbol.

Otherwise BODY should destructure into ((OBJECT-VAR) &BODY BODY)"
  (declare (type symbol name))
  (optima:match body
    ((list (and (cl:type symbol) class-name))
     (with-gensyms (object-var type-specifier)
       `(with-eval-always
          (setf (extype-structure ',name)
                (make-primitive-atomic-extype
                 :name ',name
                 :documentation nil
                 :arg-list nil
                 :lambda (named-lambda ,name (,object-var)
                           (cl:typep ,object-var ',class-name))
                 :lambda-expression
                 '(lambda (,object-var) (cl:typep ,object-var ',class-name))
                 :compiler-macro nil
                 :to-cl-type '(lambda (,type-specifier) ',class-name)))
          (namespace-value-and-doc-set
           ',name
           nil
           ,(format nil
                    "~A names a PRIMITIVE-ATOMIC-EXTYPE~%"
                    name)))))
    ((list* (list object-var) body)
     (multiple-value-bind (rem-body decl doc) (parse-body body :documentation t)
       `(with-eval-always
          (setf (extype-structure ',name)
                (make-primitive-atomic-extype
                 :name ',name
                 :documentation ,doc
                 :arg-list nil
                 :lambda (named-lambda ,name (,object-var) ,@decl ,@rem-body)
                 :lambda-expression '(lambda (,object-var) ,@decl ,@rem-body)
                 :compiler-macro nil))
          (namespace-value-and-doc-set
           ',name
           nil
           ,(if doc
                (format nil
                        "~A names a PRIMITIVE-ATOMIC-EXTYPE~%~A"
                        name
                        doc)
                (format nil
                        "~A names a PRIMITIVE-ATOMIC-EXTYPE~%"
                        name))))))))
