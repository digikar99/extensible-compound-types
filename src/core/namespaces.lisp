(in-package :extensible-compound-types.impl)

(in-nomine:define-namespace extype
  :documentation "NAMESPACE for EXTENSIBLE-COMPOUND-TYPES.
Equivalent to EXTENSIBLE-COMPOUND-TYPES:TYPE")

#+extensible-compound-types
(in-nomine:define-namespace type
  :documentation "NAMESPACE for EXTENSIBLE-COMPOUND-TYPES.
Equivalent to EXTENSIBLE-COMPOUND-TYPES:EXTYPE")

(defun namespace-value-and-doc-set (name value doc)
  (declare (cl:type symbol name)
           (cl:type string doc))
  #-extensible-compound-types
  (setf (symbol-extype name) value)
  #+extensible-compound-types
  (setf (symbol-type name) value)
  #-extensible-compound-types
  (setf (cl:documentation name 'extype) doc)
  #+extensible-compound-types
  (setf (cl:documentation name 'type) doc))

(defun namespace-value-and-doc-rem (name)
  (declare (cl:type symbol name))
  #-extensible-compound-types
  (extype-makunbound name)
  #+extensible-compound-types
  (type-makunbound name)
  ;; FIXME: Undefine methods specialized on this type
  ;; ,(unless non-null
  ;;    `(defmethod %subtypep ((t1-name (eql ',name)) (t2-name (eql nil)) type1 type2 &optional env)
  ;;       (declare (ignore t1-name t2-name type1 type2 env))
  ;;       (values nil t)))
  #-extensible-compound-types
  (setf (cl:documentation name 'extype) nil)
  #+extensible-compound-types
  (setf (cl:documentation name 'type) nil))
