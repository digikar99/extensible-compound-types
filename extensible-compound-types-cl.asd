
(defsystem "extensible-compound-types-cl"
  :depends-on ("extensible-compound-types"
               "alexandria"
               "fiveam")
  :serial t
  :pathname "cl"
  :components ((:file "package")
               (:file "special-forms")
               (:file "macros")
               (:file "misc")))
