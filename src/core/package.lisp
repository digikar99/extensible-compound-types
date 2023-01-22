(defpackage :extensible-compound-types
  (:use)
  (:export #:define-type
           #:deftype
           #:undeftype

           #:typexpand-1
           #:typexpand
           #:typexpand-all
           #:typelet
           #:typelet*

           #+extensible-compound-types
           #:type
           #:extype
           #+extensible-compound-types
           #:ftype
           #:exftype

           #:type-specifier-p
           #:typep
           #:subtypep
           #:intersect-type-p
           #:intersection-null-p
           #:specializing-type-name-p

           #:type=
           #:supertypep

           #:the
           #:check-type
           #:*the-skip-predicates*

           #:type-name-parameters

           #:define-compound-type
           #:undefine-compound-type
           #:define-compound-type-compiler-macro
           #:undefine-compound-type-compiler-macro
           #:upgraded-cl-type
           #:define-cl-type-for-extype

           #:*excluded-packages-for-cl-deftype*
           #:define-mutually-exclusive-types

           #:unknown-type-specifier

           #:specializing
           #:define-specializing-type
           #:define-orthogonally-specializing-type
           #:orthogonally-specializing-type-specifier-p

           #:clhs-class-from-object
           #:clhs-class-from-type-spec)
  (:import-from :trivial-types
                #:character-designator))

(defpackage :extensible-compound-types.impl
  (:use :cl-environments-cl :extensible-compound-types :alexandria)
  (:shadowing-import-from :extensible-compound-types
   #:deftype
   #:undeftype

   #:typexpand-1
   #:typexpand
   #:typexpand-all
   #:typelet
   #:typelet*

   #+extensible-compound-types
   #:type
   #+extensible-compound-types
   #:ftype

   #:extype
   #:exftype

   #:typep
   #:subtypep
   #:type=
   #:check-type
   #:the

   #:unknown-type-specifier)
  (:shadow #:find-class
           #:named-lambda)
  (:import-from :trivial-types
                #:character-designator)
  (:import-from :cl-environments.cltl2
                #:parse-macro
                #:define-declaration
                #:declaration-information))

(in-package :extensible-compound-types.impl)

(5am:def-suite :extensible-compound-types)
(5am:in-suite  :extensible-compound-types)