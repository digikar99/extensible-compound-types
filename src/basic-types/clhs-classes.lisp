(in-package :extensible-compound-types.impl)

(alexandria:define-constant +clhs-classes+
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
              :finally (return sorted-class-list))))
  :test #'equal)

(macrolet ((def ()
             `(progn
                ,@(loop :for n :in +clhs-classes+
                        :if (not (extype-structure n nil))
                          :collect
                        `(define-orthogonally-specializing-type ,n () ())))))
  (def))

(define-type class () 'standard-class)

(defun clhs-class-from-type-spec (type-spec)
  (flet ((%clhs-class-from-type-spec (type-spec)
           (loop :for class :in +clhs-classes+
                 :if (subtypep type-spec class)
                   :do (return class))))
    (let* ((type-spec (typexpand type-spec)))
      (optima:match type-spec
        ((list* 'specializing class-name _)
         class-name)
        ((list 'and)
         t)
        ((list* class-name _)
         (if (member class-name '(or and not satisfies eql member))
             (%clhs-class-from-type-spec type-spec)
             class-name))
        ((variable class-name)
         class-name)))))

(defun clhs-class-from-object (object)
  (let* ((object-class (class-of object))
         (object-class-name (class-name object-class)))
    (if (eq (find-package :cl)
            (symbol-package object-class-name))
        (loop :for class :in +clhs-classes+
              :if (cl:subtypep object-class class)
                :do (return class))
        object-class-name)))
