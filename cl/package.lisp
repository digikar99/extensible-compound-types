(defpackage :extensible-compound-types-cl
  (:use :cl :extensible-compound-types)

  #.`(:export ,@(let (symbols)
                  (do-external-symbols (s (find-package :cl))
                    (push s symbols))
                  symbols))

  #.`(:shadowing-import-from :extensible-compound-types
                             ,@(let (symbols)
                                 (do-external-symbols (s (find-package :extensible-compound-types))
                                   (push s symbols))
                                 symbols))

  #.`(:export
      ,@(let (symbols)
          (do-external-symbols (s (find-package :extensible-compound-types))
            (push s symbols))
          symbols))

  ;; Special Forms
  (:shadow
   ;; Commented out ones do not need a rewrite, since they do not incorporate DECLARATIONS
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
   ;; #:the
   ;; #:throw
   ;; #:unwind-protect
   )

  ;; Macros that incorporate declarations
  (:shadow
   ;;  #:defgeneric
   #:define-compiler-macro
   #:define-condition
   ;;  #:define-method-combination
   ;;  #:define-setf-expander
   #:defmacro
   #:defmethod
   ;;  #:defsetf
   ;; #:deftype
   #:defun
   #:destructuring-bind
   #:do
   #:do*
   #:do-all-symbols
   #:do-external-symbols
   #:do-symbols
   #:dolist
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
   ;; #:pprint-logical-block
   ;;  #:prog
   ;;  #:prog*
   #:restart-case
   ;;  ;; #:symbol-macrolet
   ;;  #:with-accessors
   ;;  #:with-hash-table-iterator
   ;;  #:with-input-from-string
   ;;  #:with-open-file
   ;;  #:with-open-stream
   ;;  #:with-output-to-string
   ;;  #:with-package-iterator
    #:with-slots
   )
  )

(defpackage :extensible-compound-types-cl.impl
  (:use :cl)
  (:local-nicknames (:excl :extensible-compound-types-cl)
                    (:ex :extensible-compound-types)
                    (:a :alexandria)))
