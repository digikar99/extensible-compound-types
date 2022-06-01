(in-package :extensible-compound-types-cl.impl)

(defun extract-declaration (declarations name)
  "Returns two values:
- extracted declaration
- remaining declarations"
  (loop :for decl :in declarations
        :with required-decl-specs := ()
        :for rem-decl
          := (loop :for decl-spec :in (rest decl)
                   :if (eq name (first decl-spec))
                     :do (push decl-spec required-decl-specs)
                   :else
                     :collect decl-spec)
        :if rem-decl
          :collect (cons 'declare rem-decl)
            :into remaining-decls
        :finally (return (values (if (null required-decl-specs)
                                     ()
                                     `(declare ,@required-decl-specs))
                                 (if (null remaining-decls)
                                     ()
                                     remaining-decls)))))

(5am:def-test extract-declaration ()
  (5am:is (equalp '(nil nil)
                  (multiple-value-list (extract-declaration '((declare)) 'excl:extype))))
  (5am:is (equalp '(nil ((declare (type integer x))))
                  (multiple-value-list
                   (extract-declaration '((declare (type integer x))) 'excl:extype)))))

(defun prepare-extype-checks (extype-decl)
  (a:mappend (lambda (extype-decl)
               (destructuring-bind (extype &rest vars) (rest extype-decl)
                 (loop :for var :in vars
                       :collect `(ex:the ,extype ,var))))
             (rest extype-decl)))

(defun cl-type-declarations (extype-decl &optional env)
  (if (null extype-decl)
      ()
      `(declare ,@(loop :for (extype-decl ex:extype . vars) :in (rest extype-decl)
                        :collect `(cl:type ,(ex:upgraded-cl-type ex:extype env) ,@vars)))))


(defun decl-and-type-check-body (body &optional env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(,@(remove-if #'null
                     (list* (cl-type-declarations extype-decl env)
                            extype-decl
                            remaining-decls))
        ,@(prepare-extype-checks extype-decl)
        ,@rem-body))))

;;; There are three steps to this:
;;; 1. Process declarations to extract EX:EXTYPE declarations.
;;; 2. Convert EX:EXTYPE declarations to CL:TYPE declarations.
;;; 3. Add initial EX:EXTYPE checks. This is done using EX:THE by PREPARE-EXTYPE-CHECKS.
(defmacro excl:let (bindings &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(clel:let ,bindings
         ,@(remove-if #'null
                      (list* (cl-type-declarations extype-decl env)
                             extype-decl
                             remaining-decls))
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:let* (bindings &body body &environment env)
  `(clel:let* ,@(rest (macroexpand-1 `(excl:let ,bindings ,@body) env))))

(defmacro excl:locally (&body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(clel:locally
           ,@(remove-if #'null
                        (list* (cl-type-declarations extype-decl env)
                               extype-decl
                               remaining-decls))
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defun process-function-definition (definition)
  (rest (macroexpand-1 `(excl:defun ,@definition))))

(defmacro excl:flet (definitions &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(clel:flet ,(mapcar #'process-function-definition definitions)
         ,@(remove-if #'null
                      (list* (cl-type-declarations extype-decl env)
                             extype-decl
                             remaining-decls))
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:labels (definitions &body body &environment env)
  `(clel:labels ,@(rest (macroexpand-1 `(excl:flet ,definitions ,@body) env))))

(defmacro excl:macrolet (definitions &body body &environment env)
  `(clel:macrolet ,@(rest (macroexpand-1 `(excl:flet ,definitions ,@body) env))))

(defmacro excl:symbol-macrolet (macrobindings &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(clel:symbol-macrolet ,macrobindings
         ,@(remove-if #'null
                      (list* (cl-type-declarations extype-decl env)
                             extype-decl
                             remaining-decls))
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:function (thing)
  `(cl:function ,(if (and (listp thing)
                          (string= 'lambda (first thing)))
                     (macroexpand-1 `(excl:lambda ,@(rest thing)))
                     thing)))
