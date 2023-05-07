(in-package :extensible-compound-types-interfaces.impl)

(define-interface interfaces:eq ()
  (interfaces:=  (interfaces:eq interfaces:eq) boolean)
  (interfaces:/= (interfaces:eq interfaces:eq) boolean
      ""
      (a b)
    (not (interfaces:= a b))))

(define-interface-instance interfaces:eq number
  (interfaces:=  (a b) (=  a b))
  (interfaces:/= (a b) (/= a b)))

(define-interface-instance interfaces:eq character
  (interfaces:=  (a b) (char=  a b))
  (interfaces:/= (a b) (char/= a b)))

(define-interface-instance interfaces:eq string
  (interfaces:=  (a b) (string=  a b))
  (interfaces:/= (a b) (string/= a b)))

(define-interface-instance interfaces:eq symbol
  (interfaces:=  (a b) (eq a b)))
