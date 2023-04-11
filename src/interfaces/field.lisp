(in-package :extensible-compound-types-interfaces.impl)

(define-interface field ()
  (add      (field field) field)
  (subtract (field field) field)
  (divide   (field field) field)
  (multiply (field field) field)
  (one-like  (field) field)
  (zero-like (field) field))

(defpolymorph interfaces:1+ ((field field)) field
  (add field (one-like field)))

(defpolymorph interfaces:1- ((field field)) field
  (subtract field (one-like field)))

(define-interface-instance field integer
  (add      (x y) (cl:+ x y))
  (subtract (x y) (cl:- x y))
  (multiply (x y) (cl:* x y))
  (divide   (x y) (cl:/ x y))
  (one-like   (x) (declare (ignore x)) 1)
  (zero-like  (x) (declare (ignore x)) 0))

(define-interface-instance field single-float
  (add      (x y) (cl:+ x y))
  (subtract (x y) (cl:- x y))
  (multiply (x y) (cl:* x y))
  (divide   (x y) (cl:/ x y))
  (one-like   (x) (declare (ignore x)) 1.0f0)
  (zero-like  (x) (declare (ignore x)) 0.0f0))

(define-interface-instance field double-float
  (add      (x y) (cl:+ x y))
  (subtract (x y) (cl:- x y))
  (multiply (x y) (cl:* x y))
  (divide   (x y) (cl:/ x y))
  (one-like   (x) (declare (ignore x)) 1.0d0)
  (zero-like  (x) (declare (ignore x)) 0.0d0))
