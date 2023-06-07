(in-package :extensible-compound-types-cl)

(macrolet ((def (type)
             `(define-orthogonally-specializing-type ,type () ())))
  (def magicl:polynomial)
  (def magicl:abstract-tensor)

  (def magicl::vector)
  (def magicl:vector/single-float)
  (def magicl:vector/double-float)
  (def magicl:vector/complex-single-float)
  (def magicl:vector/complex-double-float)

  (def magicl:matrix)
  (def magicl:matrix/single-float)
  (def magicl:matrix/double-float)
  (def magicl:matrix/complex-single-float)
  (def magicl:matrix/complex-double-float)

  (def magicl:tensor)
  (def magicl:tensor/single-float)
  (def magicl:tensor/double-float)
  (def magicl:tensor/complex-single-float)
  (def magicl:tensor/complex-double-float))
