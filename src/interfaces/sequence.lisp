(in-package :extensible-compound-types-interfaces.impl)

(define-interface (interfaces:sequence :cl-type nil) ()
  (interfaces:seq-ref        (sequence non-negative-integer))
  ((setf interfaces:seq-ref) (t sequence non-negative-integer))
  (interfaces:len (sequence) non-negative-integer))

(define-interface-instance interfaces:sequence list
  (interfaces:seq-ref (list n)
    (or (nth n list)
        (let ((len (length list)))
          (if (<= len n)
              (error "Trying to access element ~D in a list of length ~D"
                     n len)
              nil))))
  ((setf interfaces:seq-ref) (new-value list n)
    (setf (nth n list) new-value))
  (interfaces:len (list) (length list)))

(define-interface-instance interfaces:sequence vector
  (interfaces:seq-ref (vec n)
                      (aref vec n))
  ((setf interfaces:seq-ref) (new-value vec n)
   (setf (aref vec n) new-value))
  (interfaces:len (vec) (length vec)))
