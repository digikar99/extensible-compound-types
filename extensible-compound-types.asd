
(defsystem "extensible-compound-types"
  :depends-on ("alexandria"
               "cl-environments"
               "compiler-macro-notes"
               "fiveam"
               "in-nomine"
               "trivial-types")
  :description "EXTENSIBLE-COMPOUND-TYPES for user-defined compound-types like (array &optional element-type dimension-spec)"
  :author "Shubhamkar B. Ayare (digikar)"
  :version "0.0.0" ; alpha
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "typep")
               (:file "others")
               (:file "cl-compound-types")
               (:file "compound-only-type-specifiers")
               (:file "intersect-type-p")
               (:file "subtypep")
               (:file "designators"))
  :perform (test-op (o c)
             (eval (read-from-string "(LET ((5AM:*ON-FAILURE* :DEBUG)
                                            (5AM:*ON-ERROR* :DEBUG))
                                        (5AM:RUN! :EXTENSIBLE-COMPOUND-TYPES))"))))
