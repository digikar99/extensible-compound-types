
(defsystem "extensible-compound-types"
  :depends-on ("fiveam"
               "alexandria"
               "cl-environments"
               "compiler-macro-notes")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "typep")
               (:file "others")
               (:file "cl-compound-types")
               (:file "compound-only-type-specifiers")
               (:file "intersect-type-p")
               (:file "subtypep")))
