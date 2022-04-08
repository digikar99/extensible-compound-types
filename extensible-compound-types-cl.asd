
(defsystem "extensible-compound-types-cl"
  :depends-on ("extensible-compound-types"
               "alexandria"
               "fiveam")
  :description "Shadowing CL package for EXTENSIBLE-COMPOUND-TYPES"
  :author "Shubhamkar B. Ayare (digikar)"
  :version "0.0.0" ; alpha
  :license "MIT"
  :serial t
  :pathname "cl"
  :components ((:file "package")
               (:file "special-forms")
               (:file "macros")
               (:file "misc")))
