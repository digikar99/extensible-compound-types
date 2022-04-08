(in-package :extensible-compound-types-cl.impl)

(excl:deftype excl:function (&optional arg-typespec value-typespec)
  `(cl:function ,arg-typespec ,value-typespec))

(cl:deftype excl:function (&optional arg-typespec value-typespec)
  `(cl:function ,arg-typespec ,value-typespec))

(do-external-symbols (s (find-package :extensible-compound-types-cl))
  (setf (documentation s 'cl:function)
        (documentation (find-symbol (symbol-name s) :cl) 'cl:function)))
