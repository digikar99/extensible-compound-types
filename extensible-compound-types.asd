
(defsystem "extensible-compound-types"
  :depends-on ("fiveam"
               "alexandria"
               "cl-environments"
               "compiler-macro-notes")
  :serial t
  :components ((:file "package")
               (:file "typep")
               (:file "others")
               (:file "cl-compound-types")
               (:file "compound-only-type-specifiers")))
