(in-package :extensible-compound-types.impl)

(with-eval-always
  (labels ((add-to-list-maintaining-subtypep (class classes)
             (cond ((null classes)
                    (list class))
                   ((cl:subtypep class (first classes))
                    (cons class classes))
                   (t
                    (setf (rest classes)
                          (cons (first classes)
                                (add-to-list-maintaining-subtypep
                                 class (rest classes))))))))

    (alexandria:define-constant +clhs-classes+
        (let ((class-list
                '(arithmetic-error                  generic-function    simple-error
                  array                             hash-table          simple-type-error
                  bit-vector                        integer             simple-warning
                  broadcast-stream                  list                standard-class
                  built-in-class                    logical-pathname    standard-generic-function
                  cell-error                        method              standard-method
                  character                         method-combination  standard-object
                  class                             null                storage-condition
                  complex                           number              stream
                  concatenated-stream               package             stream-error
                  condition                         package-error       string
                  cons                              parse-error         string-stream
                  control-error                     pathname            structure-class
                  division-by-zero                  print-not-readable  structure-object
                  echo-stream                       program-error       style-warning
                  end-of-file                       random-state        symbol
                  error                             ratio               synonym-stream
                  file-error                        rational            t
                  file-stream                       reader-error        two-way-stream
                  float                             readtable           type-error
                  floating-point-inexact            real                unbound-slot
                  floating-point-invalid-operation  restart             unbound-variable
                  floating-point-overflow           sequence            undefined-function
                  floating-point-underflow          serious-condition   vector
                  function                          simple-condition    warning)))
          (loop :with sorted-class-list := (list (car class-list))
                :for class :in (rest class-list)
                :do (setq sorted-class-list
                          (add-to-list-maintaining-subtypep class sorted-class-list))
                :finally (return sorted-class-list)))
      :test #'equal)))

(macrolet ((def ()
             `(progn
                ,@(loop :for n :in +clhs-classes+
                        :if (not (extype-structure n nil))
                          :collect
                        `(define-orthogonally-specializing-type ,n () ())))))
  (def))

(deftype class () 'standard-class)
