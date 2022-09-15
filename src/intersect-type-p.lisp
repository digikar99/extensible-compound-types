(in-package :extensible-compound-types.impl)

(defmethod %intersect-type-p ((t1-name (eql 'or)) (t2-name (eql 'or))
                              type1 type2 &optional env)
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
           (loop :for t1 :in (rest t1)
                 :with all-known-p := cl:t
                 :do (multiple-value-bind (intersectp knownp)
                         (intersect-type-p t1 type2 env)
                       ;; We are only looking for *some* type-specifier to intersect
                       (cond ((and knownp intersectp)
                              (return-from %intersect-type-p
                                (values t t)))
                             ((not knownp)
                              (setq all-known-p cl:nil))))
                 :finally (return (values nil all-known-p))))
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
                    (when (some (lambda (t1)
                                  (multiple-value-bind (null knownp)
                                      (intersection-null-p env t1 t2)
                                    (and null knownp)))
                                (rest t1))
                      (return-from %intersect-type-p (values nil t)))
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
                   (call-next-method)))))))))

(defmethod %intersect-type-p (n1 (n2 (eql 'not)) t1 t2 &optional env)
  (multiple-value-bind (intersectp knownp)
      (%intersect-type-p n2 n1 t2 t1 env)
    (if knownp
        (values intersectp knownp)
        (call-next-method))))

(defmethod %intersect-type-p ((n1 (eql 'eql)) n2 t1 t2 &optional env)
  (declare (ignore n1 n2))
  (assert (and (listp t1) (null (cddr t1))) (t1))
  (values (typep (second t1) t2 env)
          t))

(defmethod %intersect-type-p (n1 (n2 (eql 'eql)) t1 t2 &optional env)
  (declare (ignore n1 n2))
  (assert (and (listp t2) (null (cddr t2))) (t2))
  (values (typep (second t2) t1 env)
          t))

(macrolet ((def (type)
             `(defmethod %intersect-type-p
                  ((t1 (eql ',type)) (t2 (eql ',type)) type1 type2 &optional env)
                (declare (ignore t1 t2))
                (destructuring-bind (&optional (elt1 'cl:*) (dr1 'cl:*)) (rest (ensure-list type1))
                  (destructuring-bind (&optional (elt2 'cl:*) (dr2 'cl:*)) (rest (ensure-list type2))
                    (let ((dim-rank-intersect-p (or (and (eq 'cl:* dr1)
                                                         (eq 'cl:* dr2))
                                                    (eq 'cl:* dr1)
                                                    (eq 'cl:* dr2)
                                                    (and (numberp dr1)
                                                         (numberp dr2)
                                                         (= dr1 dr2))
                                                    (and (numberp dr1)
                                                         (listp dr2)
                                                         (= dr1 (length dr2)))
                                                    (and (listp dr1)
                                                         (numberp dr2)
                                                         (= dr2 (length dr1)))
                                                    (and (listp dr1)
                                                         (listp dr2)
                                                         (= (length dr1) (length dr2))
                                                         (loop :for d1 :in dr1
                                                               :for d2 :in dr2
                                                               :always (or (eq 'cl:* d1)
                                                                           (eq 'cl:* d2)
                                                                           (= d1 d2)))))))
                      (cond ((and (eq 'cl:* elt1)
                                  (eq 'cl:* elt2))
                             (values dim-rank-intersect-p t))
                            ((or (eq 'cl:* elt1)
                                 (eq 'cl:* elt2))
                             (values dim-rank-intersect-p t))
                            (t
                             (values (and (type= elt1 elt2 env)
                                          dim-rank-intersect-p)
                                     t)))))))))
  (def array)
  (def simple-array))

(defmethod %intersect-type-p
    ((t1 (eql 'array)) (t2 (eql 'simple-array)) type1 type2 &optional env)
  (%intersect-type-p t1 t1 type1 `(array ,@(rest (ensure-list type2))) env))
(defmethod %intersect-type-p
    ((t1 (eql 'simple-array)) (t2 (eql 'array)) type1 type2 &optional env)
  (%intersect-type-p t2 t2 `(array ,@(rest (ensure-list type1))) type2 env))

(defmethod %intersect-type-p
    ((t1 (eql 'array)) t2 type1 type2 &optional env)
  (declare (ignore t1 type1))
  ;; T2 is guaranteed to be type-expanded and be a symbol
  (if (and (or (symbolp type2)
               (and (listp type2)
                    (car type2)
                    (null (cdr type2))))
           (find-class t2 nil env))
      (multiple-value-bind (nullp knownp)
          (cl:subtypep `(and array ,t2) nil env)
        (values (not nullp) knownp))
      (call-next-method)))
(defmethod %intersect-type-p
    (t1 (t2 (eql 'array)) type1 type2 &optional env)
  (%intersect-type-p t2 t1 type2 type1 env))

(defmethod %intersect-type-p
    ((t1 (eql 'simple-array)) t2 type1 type2 &optional env)
  (declare (ignore t1 type1))
  ;; T2 is guaranteed to be type-expanded and be a symbol
  (if (and (or (symbolp type2)
               (and (listp type2)
                    (car type2)
                    (null (cdr type2))))
           (find-class t2 nil env))
      (multiple-value-bind (nullp knownp)
          (cl:subtypep `(and simple-array ,t2) nil env)
        (values (not nullp) knownp))
      (call-next-method)))
(defmethod %intersect-type-p
    (t1 (t2 (eql 'simple-array)) type1 type2 &optional env)
  (%intersect-type-p t2 t1 type2 type1 env))


(defmacro define-mutually-exclusive-types (&body types &environment env)
  (loop :for type :in types
        :do (assert (and (symbolp type)
                         (or (find-class type nil env)
                             (compound-type-lambda type)))
                    ()
                    "Expected~%  ~S~%to be a symbol and a primitive compound type specifier defined using DEFINE-COMPOUND-TYPE~%but~%  (COMPOUND-TYPE-LAMBDA ~S)~%did not return non-NIL indicating the absence of an appropriate DEFINE-COMPOUND-TYPE form"
                    type type))
  (flet ((def (type1 type2)
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

    `(progn
       ,@(loop :for (t1 . rest) :on types
               :appending
               (loop :for t2 :in rest
                     :collect (def t1 t2))))))

(define-mutually-exclusive-types
  cons array symbol character rational single-float double-float complex function)

(define-mutually-exclusive-types
  cons simple-array symbol character rational single-float double-float complex function)

(define-mutually-exclusive-types
  cons array symbol character integer single-float double-float complex function)

(define-mutually-exclusive-types
  cons simple-array symbol character integer single-float double-float complex function)

(macrolet ((def (type)
             `(define-mutually-exclusive-types
                ,type null)))

  (def cons)
  (def array)
  (def simple-array)
  (def character)
  (def integer)
  (def rational)
  (def float)
  (def single-float)
  (def double-float)
  #-sbcl
  (def short-float)
  #-sbcl
  (def long-float))
