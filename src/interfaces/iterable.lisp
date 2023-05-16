(in-package :extensible-compound-types-interfaces.impl)

(define-interface iterable ()
  (at        (iterable t) t)
  (first-key (iterable)   (values t boolean &optional))
  (next-key  (iterable t) (values t boolean &optional)))

(define-interface-instance iterable list
  (at (list key)
    (declare (ignore list))
    (car key))
  (first-key (list)
    (values list (not (null list))))
  (next-key (list key)
    (declare (ignore list))
    (values (cdr key) (not (null (cdr key))))))

(define-interface-instance iterable vector
  (at (vector index)
    (aref vector index))
  (first-key (vector)
    (values 0 (not (zerop (length vector)))))
  (next-key (vector index)
    (declare (type fixnum index))
    (values (1+ index) (< (1+ index) (length vector)))))

#|

(defun map-vector/iterable (function vector)
  (declare (optimize speed)
           (type (vector t) vector))
  (interfaces:map-to-list function vector))

(defun map-vector/native (function vector)
  (declare (optimize speed)
           (type (vector t) vector))
  (loop :for elt :across vector
        :do (funcall function elt)))

|#

(defpolymorph interfaces:map-to-collector (collector function (it iterable)) t
  (with-interface-instances ((collector collector))
    (loop :with key := (first-key it)
          :do (setq collector
                    (collect collector
                      (multiple-value-call
                          function
                        (locally (declare (optimize (safety 0)))
                          (at it key)))))
              (multiple-value-bind (%key %validp)
                  (next-key it key)
                (if %validp
                    (setq key %key)
                    (return-from interfaces:map-to-collector collector))))))

(defpolymorph interfaces:filter-to-collector (collector function (it iterable)) t
  (with-interface-instances ((collector collector))
    (loop :with key := (first-key it)
          :do (imlet ((value (locally (declare (optimize))
                               (at it key))))
                (when (funcall function value)
                  (setq collector (collect collector value))))
              (multiple-value-bind (%key %validp)
                  (next-key it key)
                (if %validp
                    (setq key %key)
                    (return-from interfaces:filter-to-collector collector))))))
