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
                             ;; We are only looking for *some* type-specifier to intersect
                             (and knownp intersectp)))
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

(defmethod %intersect-type-p ((n1 (eql 'and)) n2 t1 t2 &optional env)
  (declare (ignore n1 n2))
  (let ((t1 (simplify-and-type t1)))
    (cond ((and (listp t1)
                (eq 'and (first t1)))
           (multiple-value-bind (null knownp) (apply #'intersection-null-p env (rest t1))
             (cond ((and null knownp)
                    (values t t))
                   (t
                    (loop :for t1-list :on (rest t1)
                          :for t11 := (car t1-list)
                          :with all-known-p := t
                          :with all-intersect-p := t
                          :while (and all-known-p all-intersect-p)
                          :do (loop :for t12 :in (rest t1-list)
                                    :while (and all-known-p all-intersect-p)
                                    :do (multiple-value-bind (null knownp)
                                            (intersection-null-p env t11 t12 t2)
                                          (cond ((not knownp)
                                                 (setq all-known-p nil))
                                                (null
                                                 (setq all-intersect-p nil)))))
                          :finally (return (cond ((not all-intersect-p)
                                                  (values nil t))
                                                 ((not all-known-p)
                                                  (values nil nil))
                                                 (t
                                                  (values t t)))))))))
          (t
           (multiple-value-bind (intersectp knownp) (intersect-type-p t1 t2 env)
             (values intersectp knownp))))))

(defmethod %intersect-type-p (t2-name (t1-name (eql 'and))
                              type2 type1 &optional env)
  (multiple-value-bind (intersectp knownp)
      (%intersect-type-p t1-name t2-name type1 type2 env)
    (values intersectp knownp)))

(defmethod %intersect-type-p ((n1 (eql 'not)) n2 t1 t2 &optional env)
  ;; Simplify, by removing the NOT
  (optima.extra:let-match (((list _ ts) t1))
    (multiple-value-bind (s1 k1) (subtypep ts t2 env)
      (multiple-value-bind (s2 k2) (subtypep t2 ts env)
        (multiple-value-bind (intersectp knownp) (intersect-type-p ts t2 env)
          (declare (ignore intersectp))
          (return-from %intersect-type-p
            (cond ((and k1 s1 k2 s2)
                   ;; They are TYPE=
                   (values nil t))
                  ((and k1 s1 k2 (not s2))
                   ;; TS is a proper subtype of T2
                   (values t t))
                  ((and k1 (not s1) k2 s2)
                   ;; T2 is a proper subtype of TS
                   (values nil t))
                  ((or (and k1 k2) knownp)
                   ;; Neither is a proper subtype
                   (values t t))
                  (t
                   (values nil nil)))))))))

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
  (def symbol array)
  (def character array)
  (def character symbol)
  (def character list)
  (def character integer)

  (def function array)
  (def integer list)
  (def real list)
  (def float list)
  (def single-float list)
  (def double-float list)
  (def short-float list)
  (def long-float list))
