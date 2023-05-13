(defsystem "extensible-compound-types-cl"
  :depends-on ("alexandria"
               "cl-form-types"
               "extensible-compound-types"
               "fiveam")
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

(defsystem "extensible-compound-types-cl/specializable-structs"
  :depends-on ("cl-form-types"
               "cl-ppcre"
               "extensible-compound-types-cl"
               "fiveam"
               "polymorphic-functions")
  :pathname #p"src/cl/"
  :components ((:file "specializable-structs")))

