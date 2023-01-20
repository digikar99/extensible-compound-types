
(defsystem "extensible-compound-types"
  :depends-on ("alexandria"
               "cl-environments"
               "compiler-macro-notes"
               "fiveam"
               "in-nomine"
               "optima"
               (:feature (:not :swank) "swank")
               "trivial-types")
  :description "EXTENSIBLE-COMPOUND-TYPES for user-defined compound-types like (array &optional element-type dimension-spec)"
  :author "Shubhamkar B. Ayare (digikar)"
  :version "0.0.0" ; alpha
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:module "core"
                :components ((:file "package")
                             (:file "namespaces")
                             (:file "declarations")
                             (:file "utils")
                             (:file "extype-structures")
                             (:file "derived-extype")
                             (:file "primitive-atomic-extype")
                             (:file "primitive-compound-extype")
                             (:file "subtypep-table")
                             (:file "intersect-type-p-table")
                             (:file "typep")
                             (:file "upgraded-cl-type")
                             (:file "misc")))
               (:file "class-specializers")
               (:module "basic-types"
                :components ((:file "compound-only-type-specifiers")
                             (:file "compound-only-subtypep")
                             (:file "compound-only-intersect-type-p")
                             (:file "cl-compound-types")
                             (:file "clhs-classes")
                             (:file "designators"))))
  :perform (test-op (o c)
             (eval (read-from-string "(LET ((5AM:*ON-FAILURE* :DEBUG)
                                            (5AM:*ON-ERROR* :DEBUG))
                                        (5AM:RUN! :EXTENSIBLE-COMPOUND-TYPES))"))))
