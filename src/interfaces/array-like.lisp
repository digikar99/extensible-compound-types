(in-package :extensible-compound-types-interfaces.impl)

(defun max-type (type-1 type-2)
  (cond ((subtypep type-1 type-2)
         type-2)
        ((subtypep type-2 type-1)
         type-1)
        ((or (alexandria:type= type-1 'double-float)
             (alexandria:type= type-2 'double-float))
         'double-float)
        ((or (alexandria:type= type-1 'single-float)
             (alexandria:type= type-2 'single-float))
         'single-float)
        ;; At this point, none of the types are floats
        ;; FIXME: Operate better on impl with other float types
        ((and (subtypep type-1 '(unsigned-byte *))
              (subtypep type-2 '(signed-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-1 `(signed-byte ,num-bits))
                 :do (return-from max-type `(signed-byte ,num-bits))
               :finally (return-from max-type 'single-float)))
        ((and (subtypep type-1 '(signed-byte *))
              (subtypep type-2 '(unsigned-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-2 `(signed-byte ,num-bits))
                 :do (return-from max-type `(signed-byte ,num-bits))
               :finally (return-from max-type 'single-float)))
        (t
         (error "Don't know how to find MAX-TYPE of ~S and ~S" type-1 type-2))))

(define-interface array-like ()
  (dimensions-and-strides (array-like) list)
  (element-type (array-like) (or symbol cons))
  (row-major-iterator (array-like)))

(define-interface-instance array-like t
  (dimensions-and-strides (object) (declare (ignore object)) (values nil nil))
  (element-type (object) (type-of object))
  (row-major-iterator (object) (values object nil)))

(define-interface-instance array-like cons
  (dimensions-and-strides (cons)
    (multiple-value-bind (rest-dimensions rest-strides)
        (if (typep (car cons) 'array-like)
            (dimensions-and-strides (car cons))
            (values nil nil))
      (let ((current-dimension (length cons)))
        (values (cons current-dimension
                      rest-dimensions)
                (cons (cl:* current-dimension (or (first rest-strides) 1))
                      rest-strides)))))
  (element-type (cons)
    (if (< 0 (length cons))
        (loop :with max-type := nil
              :for elt :in cons
              :do (setq max-type
                        (max-type max-type (if (typep elt 'array-like)
                                               (element-type elt)
                                               (type-of elt))))
              :finally (return max-type))
        'null))
  (row-major-iterator (cons)
     (let ((iterators (mapcar (lambda (elt)
                                (if (typep elt 'array-like)
                                    (row-major-iterator elt)
                                    elt))
                              cons)))
       (lambda ()
         (let ((iterator (first iterators)))
           (if (functionp iterator)
               (multiple-value-bind (value morep) (funcall iterator)
                 (unless morep
                   (setq iterators (rest iterators)))
                 (values value morep))
               (progn
                 (setq iterators (rest iterators))
                 (values iterator (if iterators t nil)))))))))

(define-interface-instance array-like (and cl:array (not string))
  (dimensions-and-strides (array)
    (values (cl:array-dimensions array)
            (nreverse (loop :for stride := 1
                            :for dim :in (nreverse (cl:array-dimensions array))
                            :collect stride
                            :do (setf stride (cl:* dim stride))))))
  (element-type (array)
    (cl:array-element-type array))
  (row-major-iterator (array)
    (let ((total-size-1 (1- (cl:array-total-size array)))
          (row-major-index -1))
      (lambda ()
        (incf row-major-index)
        (values (cl:row-major-aref array row-major-index)
                (not (cl:= total-size-1 row-major-index)))))))
