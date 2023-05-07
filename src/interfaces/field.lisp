(in-package :extensible-compound-types-interfaces.impl)

(define-interface field ()

  (add      (field field) field
      ""
      (a b)
    (cl:+ a b))

  (subtract (field field) field
      ""
      (a b)
    (cl:- a b))

  (divide   (field field) field
      ""
      (a b)
    (cl:/ a b))

  (multiply (field field) field
      ""
      (a b)
    (cl:* a b))

  (one-like  (field) field)

  (zero-like (field) field)

  (interfaces:1+ (field) field
      ""
      (f)
    (add f (one-like f)))

  (interfaces:1- (field) field
      ""
      (f)
    (subtract f (one-like f))))



(define-polymorphic-function interfaces:+ (&rest fields) :overwrite t)
(defpolymorph interfaces:+ ((f field)) field f)
(defpolymorph interfaces:+ ((f1 field) (f2 field)) field (add f1 f2))

(defpolymorph (interfaces:+ :inline t)
    ((f1 field) (f2 field) &rest args)
    field
  (reduce #'add args :initial-value (add f1 f2)))

(define-interface-instance field integer
  (divide   (x y) (nth-value 0 (cl:floor x y)))
  (one-like   (x) (declare (ignore x)) 1)
  (zero-like  (x) (declare (ignore x)) 0))

(define-interface-instance field rational
  (divide   (x y) (cl:/ x y))
  (one-like   (x) (declare (ignore x)) 1)
  (zero-like  (x) (declare (ignore x)) 0))

(define-interface-instance field single-float
  (divide   (x y) (cl:/ x y))
  (one-like   (x) (declare (ignore x)) 1.0f0)
  (zero-like  (x) (declare (ignore x)) 0.0f0))

(define-interface-instance field double-float
  (divide   (x y) (cl:/ x y))
  (one-like   (x) (declare (ignore x)) 1.0d0)
  (zero-like  (x) (declare (ignore x)) 0.0d0))

(define-interface-instance field (complex single-float)
  (divide   (x y) (cl:/ x y))
  (one-like   (x) (declare (ignore x)) #c(1.0f0 0.0f0))
  (zero-like  (x) (declare (ignore x)) #c(0.0f0 0.0f0)))

(define-interface-instance field (complex double-float)
  (divide   (x y) (cl:/ x y))
  (one-like   (x) (declare (ignore x)) #c(1.0d0 0.0f0))
  (zero-like  (x) (declare (ignore x)) #c(0.0d0 0.0f0)))
