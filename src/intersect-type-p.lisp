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
