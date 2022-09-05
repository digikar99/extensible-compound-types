(in-package :extensible-compound-types-cl.impl)

(excl:deftype excl:function (&optional arg-typespec value-typespec)
  `(cl:function ,arg-typespec ,value-typespec))

(cl:deftype excl:function (&optional arg-typespec value-typespec)
  `(cl:function ,arg-typespec ,value-typespec))

(cl:defun excl:compile (name &optional definition)
  (if definition
      (cl:compile name (optima:match definition
                         ((list* 'cl:lambda _)
                          definition)
                         (_
                          (eval
                           (cl-environments.cltl2::enclose-form
                            (cl-environments:macroexpand-1 definition))))))
      (cl:compile name)))

(do-external-symbols (s (find-package :extensible-compound-types-cl))
  (setf (documentation s 'cl:function)
        (documentation (find-symbol (symbol-name s) :cl) 'cl:function)))
