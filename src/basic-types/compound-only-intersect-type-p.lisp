(in-package :extensible-compound-types.impl)

(define-intersect-type-p-lambda (or or) (type1 type2 env)
  (let ((t1 (simplify-or-type type1))
        (t2 (simplify-or-type type2)))
    (optima:match t1
      ((list* 'or _))
      (_ (setq t1 (list 'or t1))))
    (optima:match t2
      ((list* 'or _))
      (_ (setq t2 (list 'or t2))))
    (loop :with all-known-p := t
          :for t1 :in (rest t1)
          :while all-known-p
          :do (loop :for t2 :in (rest t2)
                    :while all-known-p
                    :do (multiple-value-bind (intersectp knownp)
                            (intersect-type-p t1 t2 env)
                          (cond ((and intersectp knownp)
                                 (return-from intersect-type-p (values t t)))
                                ((not knownp)
                                 (setq all-known-p nil)))))
          :finally (return-from intersect-type-p (if all-known-p
                                                      (values nil t)
                                                      (values nil nil))))))

(define-intersect-type-p-lambda (or nil) (type1 type2 env)
  (let ((t1 (simplify-or-type type1)))
    (cond ((and (listp t1)
                (eq 'or (first t1)))
           (loop :for t1 :in (rest t1)
                 :with all-known-p := cl:t
                 :do (multiple-value-bind (intersectp knownp)
                         (intersect-type-p t1 type2 env)
                       ;; We are only looking for *some* type-specifier to intersect
                       (cond ((and knownp intersectp)
                              (return-from intersect-type-p
                                (values t t)))
                             ((not knownp)
                              (setq all-known-p cl:nil))))
                 :finally (return (values nil all-known-p))))
          (t
           (multiple-value-bind (intersectp knownp)
               (intersect-type-p t1 type2 env)
             (values intersectp knownp))))))

(define-intersect-type-p-lambda (nil or) (type2 type1 env)
  (multiple-value-bind (intersectp knownp)
      (intersect-type-p type1 type2 env)
    (values intersectp knownp)))

(define-intersect-type-p-lambda (and nil) (t1 t2 env)
  (let ((t1 (simplify-and-type t1)))
    (cond ((and (listp t1)
                (eq 'and (first t1)))
           (multiple-value-bind (null knownp) (apply #'intersection-null-p env (rest t1))
             (cond ((and null knownp)
                    (values t t))
                   (t
                    (when (some (lambda (t1)
                                  (multiple-value-bind (null knownp)
                                      (intersection-null-p env t1 t2)
                                    (and null knownp)))
                                (rest t1))
                      (return-from intersect-type-p (values nil t)))
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

(define-intersect-type-p-lambda (nil and) (type2 type1 env)
  (with-extype-name-and-expansion (name exp) type2
    (let ((fn1 (primitive-intersect-type-p-lambda 'and name))
          (fn2 (primitive-intersect-type-p-lambda 'and nil)))
      (when fn1
        (multiple-value-bind (intersectp knownp)
            (funcall fn1 type1 type2 env)
          (when knownp
            (return-from intersect-type-p (values intersectp knownp)))))
      (when fn2
        (multiple-value-bind (intersectp knownp)
            (funcall fn2 type1 type2 env)
          (when knownp
            (return-from intersect-type-p (values intersectp knownp)))))
      (values nil nil))))

(define-intersect-type-p-lambda (not nil) (t1 t2 env)
  ;; Simplify, by removing the NOT
  (optima.extra:let-match (((list _ ts) t1))
    (multiple-value-bind (s1 k1) (subtypep ts t2 env)
      (multiple-value-bind (s2 k2) (subtypep t2 ts env)
        (multiple-value-bind (intersectp knownp) (intersect-type-p ts t2 env)
          (declare (ignore intersectp))
          (return-from intersect-type-p
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

(define-intersect-type-p-lambda (nil not) (t1 t2 env)
  (multiple-value-bind (intersectp knownp)
      (intersect-type-p t2 t1 env)
    (if knownp
        (values intersectp knownp)
        (values nil nil))))

(define-intersect-type-p-lambda (eql nil) (t1 t2 env)
  (assert (and (listp t1) (null (cddr t1))) (t1))
  (values (typep (second t1) t2 env)
          t))

(define-intersect-type-p-lambda (nil eql) (t1 t2 env)
  (assert (and (listp t2) (null (cddr t2))) (t2))
  (values (typep (second t2) t1 env)
          t))

;; (define-mutually-exclusive-types
;;   cons (null symbol) character single-float double-float complex function pathname
;;   (array simple-array)
;;   (integer rational)
;;   structure-object
;;   standard-object)
