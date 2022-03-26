(in-package :extensible-compound-types-cl.impl)

(excl:deftype excl:function (&optional arg-typespec value-typespec)
  `(cl:function ,arg-typespec ,value-typespec))

(cl:deftype excl:function (&optional arg-typespec value-typespec)
  `(cl:function ,arg-typespec ,value-typespec))
