(defpackage :extensible-compound-types-interfaces
  (:use)
  (:import-from :cl #:sequence)
  (:export #:interface
           #:define-interface
           #:define-interface-instance

           #:eq
           #:=
           #:/=

           #:sequence
           #:seq-ref
           #:len

           #:array-like
           #:element-type
           #:dimensions-and-strides
           #:row-major-iterator
           #:dimensions))

(defpackage :extensible-compound-types-interfaces.impl
  (:use :polymorphic-functions
        :extensible-compound-types-cl)
  (:import-from :extensible-compound-types.impl
                #:derived-extype
                #:with-eval-always
                #:extype-structure
                #:namespace-value-and-doc-set)
  (:import-from :extensible-compound-types-interfaces
                #:interface
                #:define-interface
                #:define-interface-instance

                #:array-like
                #:element-type
                #:dimensions-and-strides
                #:dimensions
                #:row-major-iterator)
  (:import-from :alexandria
                #:with-gensyms
                #:assoc-value
                #:set-equal
                #:non-negative-integer)
  (:local-nicknames (:interfaces :extensible-compound-types-interfaces)))
