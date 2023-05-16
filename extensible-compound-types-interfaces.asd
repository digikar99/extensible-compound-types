(defsystem "extensible-compound-types-interfaces"
  :pathname "src/interfaces"
  :depends-on ("extensible-compound-types-cl"
               "polymorphic-functions")
  :serial t
  :components ((:file "package")
               (:file "interface")
               (:file "eq")
               (:file "sequence")
               (:file "array-like")
               (:file "iterable")
               (:file "field")
               (:file "iterator")))
