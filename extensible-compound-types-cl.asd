(defsystem "extensible-compound-types-cl"
  :depends-on ("alexandria"
               "cl-form-types"
               "extensible-compound-types"
               "fiveam")
  :defsystem-depends-on ("asdf-system-connections")
  :description "Shadowing CL package for EXTENSIBLE-COMPOUND-TYPES"
  :author "Shubhamkar B. Ayare (digikar)"
  :version "0.0.0" ; alpha
  :license "MIT"
  :serial t
  :pathname #p"src/cl/"
  :components ((:file "package")
               (:file "special-forms")
               (:file "macros")
               (:file "extras")
               (:file "misc")))

(defsystem-connection "extensible-compound-types-cl/magicl"
  :requires ("magicl" "extensible-compound-types-cl")
  :pathname #p"src/connections/"
  :components ((:file "magicl")))
