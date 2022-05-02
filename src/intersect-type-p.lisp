(in-package :extensible-compound-types.impl)

(defmethod %intersect-type-p ((t1-name (eql 'or)) (t2-name (eql 'or))
                              type1 type2 &optional env)
  (let ((t1 (simplify-or-type type1))
        (t2 (simplify-or-type type2)))
    (loop :with all-known-p := t
          :for t1 :in t1
          :while all-known-p
          :do (loop :for t2 :in t2
                    :while all-known-p
                    :do (multiple-value-bind (intersectp knownp)
                            (intersect-type-p t1 t2 env)
                          (cond ((and intersectp knownp)
                                 (return-from %intersect-type-p (values t t)))
                                ((not knownp)
                                 (setq all-known-p nil)))))
          :finally (return-from %intersect-type-p (if all-known-p
                                                      (values nil t)
                                                      (values nil nil))))))

(defmethod %intersect-type-p ((t1-name (eql 'or)) t2-name
                              type1 type2 &optional env)
  (let ((t1 (simplify-or-type type1)))
    (cond ((and (listp t1)
                (eq 'or (first t1)))
           (values (some (lambda (t1)
                           (multiple-value-bind (intersectp knownp)
                               (intersect-type-p t1 type2 env)
                             (if knownp
                                 intersectp
                                 (return-from %intersect-type-p (values nil nil)))))
                         (rest t1))
                   t))
          (t
           (multiple-value-bind (intersectp knownp)
               (intersect-type-p t1 type2 env)
             (values intersectp knownp))))))

(defmethod %intersect-type-p (t2-name (t1-name (eql 'or))
                              type2 type1 &optional env)
  (multiple-value-bind (intersectp knownp)
      (%intersect-type-p t1-name t2-name type1 type2 env)
    (values intersectp knownp)))

;; TODO: Abstract into a DEFINE-MUTUALLY-EXCLUSIVE-TYPES macro
(macrolet ((def (type1 type2)
             `(progn
                (defmethod %intersect-type-p ((t1 (eql ',type1)) (t2 (eql ',type2))
                                              type1 type2 &optional env)
                  (declare (ignore t1 t2 type1 type2 env))
                  (values nil t))
                (defmethod %intersect-type-p ((t1 (eql ',type2)) (t2 (eql ',type1))
                                              type1 type2 &optional env)
                  (declare (ignore t1 t2 type1 type2 env))
                  (values nil t))
                (defmethod %subtypep ((n1 (eql ',type1)) (n2 (eql ',type2)) t1 t2 &optional env)
                  (declare (ignore n1 n2 t1 t2 env))
                  (values nil t))
                (defmethod %subtypep ((n1 (eql ',type2)) (n2 (eql ',type1)) t1 t2 &optional env)
                  (declare (ignore n1 n2 t1 t2 env))
                  (values nil t)))))
  (def list array)
  (def symbol array))
