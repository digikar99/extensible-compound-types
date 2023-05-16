(in-package :extensible-compound-types-interfaces.impl)

(define-interface collector ()
  (collect (collector &rest t) collector))

(define-interface-instance collector list
  (collect (list &rest new-elements)
    (if (null (cdr new-elements))
        (cons (car new-elements) list)
        (error "A LIST collector can only collect a single element at a time."))))

(defstruct queue storage end-of-storage)

(defmethod print-object ((o queue) s)
  (format s "#Q~S" (queue-storage o)))

(define-interface-instance collector queue
  (collect (queue &rest new-elements)
    (if (null (cdr new-elements))
        (let ((end-of-storage (queue-end-of-storage queue)))
          (etypecase end-of-storage
            (cons
             (setf (cdr end-of-storage)
                   (list (car new-elements)))
             (setf (queue-end-of-storage queue)
                   (cdr (queue-end-of-storage queue))))
            (null
             (setf (queue-storage queue)        (list (car new-elements)))
             (setf (queue-end-of-storage queue) (queue-storage queue))))
          queue)
        (error "A QUEUE collector can only collect a single element at a time."))))
