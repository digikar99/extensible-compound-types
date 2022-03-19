(defpackage :extensible-compound-types-cl
  (:use :extensible-compound-types)

  ;; Special Forms
  (:export
;;; Commented out ones do not need a rewrite
   ;; #:block
   ;; #:catch
   ;; #:eval-when
   #:flet
   #:function
   ;; #:go
   ;; #:if
   #:labels
   #:let
   #:let*
   ;; #:load-time-value
   #:locally
   #:macrolet
   ;; #:multiple-value-call
   ;; #:multiple-value-prog1
   ;; #:progn
   ;; #:progv
   ;; #:quote
   ;; #:return-from
   ;; #:setq
   #:symbol-macrolet
   ;; #:tagbody
   #:the
   ;; #:throw
   ;; #:unwind-protect
   )

  ;; Macros
  (:export
   #:defgeneric
   #:define-compiler-macro
   #:define-method-combination
   #:define-setf-expander
   #:defmacro
   #:defmethod
   #:defsetf
   #:deftype
   #:defun
   #:destructuring-bind
   #:do
   #:do*
   #:do-all-symbols
   #:do-external-symbols
   #:do-symbols
   #:dotimes
   ;; #:flet
   #:handler-case
   ;; #:labels
   #:lambda
   ;; #:let
   ;; #:let*
   ;; #:locally
   ;; #:macrolet
   #:multiple-value-bind
   #:pprint-logical-block
   #:prog
   #:prog*
   #:restart-case
   ;; #:symbol-macrolet
   #:with-accessors
   #:with-hash-table-iterator
   #:with-input-from-string
   #:with-open-file
   #:with-open-stream
   #:with-output-to-string
   #:with-package-iterator
   #:with-slots
   ))

(defpackage :extensible-compound-types-cl.impl
  (:use :cl)
  (:local-nicknames (:excl :extensible-compound-types-cl)
                    (:ex :extensible-compound-types)
                    (:a :alexandria)))

(in-package :extensible-compound-types-cl.impl)

(defun extract-declaration (declarations name)
  "Returns two values:
- extracted declaration
- remaining declarations"
  (loop :for decl :in declarations
        :with required-decl-specs := ()
        :collect (cons 'declare
                       (loop :for decl-spec :in (rest decl)
                             :if (eq name (first decl-spec))
                               :do (push decl-spec required-decl-specs)
                             :else
                               :collect decl-spec))
          :into remaining-decls
        :finally (return (values (if (null required-decl-specs)
                                     ()
                                     `(declare ,@required-decl-specs))
                                 (if (null remaining-decls)
                                     ()
                                     remaining-decls)))))

(defun prepare-bindings (bindings extype-decl)
  (loop :for binding :in bindings
        :for var := (if (atom binding)
                        binding
                        (first binding))
        :for value := (if (and (listp binding)
                               (rest binding))
                          (second binding)
                          nil)
        :for extype := (or (second (find var (rest extype-decl)
                                         :key #'cddr
                                         :test #'member))
                           t)
        :collect `(,var (ex:the ,extype ,value))))

(defun prepare-extype-checks (vars extype-decl)
  (loop :for var :in vars
        :for extype := (or (second (find var (rest extype-decl)
                                         :key #'cddr
                                         :test #'member))
                           t)
        :collect `(ex:the ,extype ,var)))

(defun cl-type-declarations (extype-decl &optional env)
  (if (null extype-decl)
      ()
      `(declare ,@(loop :for (extype-decl ex:extype . vars) :in (rest extype-decl)
                        :collect `(cl:type ,(ex:upgraded-cl-type ex:extype env) ,@vars)))))


;;; There are three steps to this:
;;; 1. Process declarations to extract EX:EXTYPE declarations.
;;; 2. Convert EX:EXTYPE declarations to CL:TYPE declarations.
;;; 3. Add initial EX:EXTYPE checks. This is done using EX:THE below.
(defmacro excl:let (bindings &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(cl:let ,(prepare-bindings bindings extype-decl)
         ,(cl-type-declarations extype-decl env)
         ,extype-decl
         ,@remaining-decls
         ,@rem-body))))

(defmacro excl:let* (bindings &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(cl:let* ,(prepare-bindings bindings extype-decl)
         ,(cl-type-declarations extype-decl env)
         ,extype-decl
         ,@remaining-decls
         ,@rem-body))))

(defmacro excl:locally (&body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(cl:locally
           ,(cl-type-declarations extype-decl env)
         ,extype-decl
         ,@remaining-decls
         ;; ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:the (value-type form)
  `(ex:the ,value-type ,form))

(defmacro excl:lambda (args &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(cl:lambda ,args
         ,(cl-type-declarations extype-decl env)
         ,extype-decl
         ,@remaining-decls
         ;; FIXME: Extract variables from ARGS
         ,@(prepare-extype-checks args extype-decl)
         ,@rem-body))))

(defmacro excl:defun (name lambda-list &body body &environment env)
  (multiple-value-bind (rem-body decl doc-string) (a:parse-body body :documentation t)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(cl:defun ,name ,lambda-list
         ,doc-string
         ,(cl-type-declarations extype-decl env)
         ,extype-decl
         ,@remaining-decls
         ;; FIXME: Extract variables from LAMBDA-LIST
         ,@(prepare-extype-checks lambda-list extype-decl)
         ,@rem-body))))

(defun process-function-definition (definition)
  )

(defmacro excl:flet (definitions &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(cl:flet ,definitions
         ,(cl-type-declarations extype-decl env)
         ,extype-decl
         ,@remaining-decls
         ,@rem-body))))
