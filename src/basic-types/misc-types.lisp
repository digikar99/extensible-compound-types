(in-package :extensible-compound-types.impl)

(define-orthogonally-specializing-type compiler-macro-notes:optimization-failure-note
    () ())
(define-orthogonally-specializing-type compiler-macro-notes:note () ())

(define-type non-negative-integer () '(integer 0))
