(in-package :extensible-compound-types-cl.impl)

(excl:deftype excl:function (&optional (arg-typespec 'cl:*) (value-typespec 'cl:*))
  `(cl:function ,arg-typespec ,value-typespec))

(cl:deftype excl:function (&optional (arg-typespec 'cl:*) (value-typespec 'cl:*))
  `(cl:function ,arg-typespec ,value-typespec))

(cl:defun excl:compile (name &optional definition)
  "Like CL:COMPILE but more robust for handling edge cases"
  (if definition
      (cl:compile name (optima:match definition
                         ((list* 'cl:function definition)
                          definition)
                         ((list* 'cl:lambda _)
                          definition)
                         #+sbcl
                         ((list* 'sb-int:named-lambda _)
                          definition)
                         (_
                          #+sbcl
                          (cl-environments:macroexpand-1 definition)
                          #-sbcl
                          (eval
                           (cl-environments.cltl2::enclose-form
                            (cl-environments:macroexpand-1 definition))))))
      (cl:compile name)))

(cl:defun excl:augment-environment
    (env &key variable symbol-macro function macro declare)
  (let ((upgraded-cl-type-declarations
          (loop :for decl :in declare
                :collect decl
                :nconcing
                (case (first decl)
                  (ex:extype (list `(cl:type ,(ex:upgraded-cl-type (second decl))
                                             ,@(nthcdr 2 decl))))
                  #+extensible-compound-types
                  (ex:type (list `(ex:extype ,(second decl) ,@(nthcdr 2 decl))
                                 `(cl:type ,(ex:upgraded-cl-type (second decl))
                                           ,@(nthcdr 2 decl))))))))
    (cl-environments:augment-environment env
                                         :variable variable
                                         :function function
                                         :symbol-macro symbol-macro
                                         :macro macro
                                         :declare upgraded-cl-type-declarations)))

(let ((exct-cl-package (find-package :extensible-compound-types-cl)))
  (do-external-symbols (s exct-cl-package)
    (when (eq exct-cl-package
              (symbol-package s))
      (setf (documentation s 'cl:function)
            (documentation (find-symbol (symbol-name s) :cl) 'cl:function)))))
