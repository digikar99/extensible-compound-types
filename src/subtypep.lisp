(in-package :extensible-compound-types.impl)

(5am:in-suite :extensible-compound-types)

(defun flatten-type-specifier-combination (combination type-specifier &optional env)
  (flet ((typexpand-may-be (type-specifier)
           (if (atom type-specifier)
               (typexpand type-specifier env)
               type-specifier)))
    (let ((ts (typexpand-may-be type-specifier)))
      (assert (and (listp ts)
                   (eq combination (first ts)))
              (combination ts))
      (cons combination
            (loop :for type :in (rest ts)
                  :for expanded-type := (typexpand-may-be type)
                  :if (and (listp expanded-type)
                           (eq combination (first expanded-type)))
                    :appending (rest (flatten-type-specifier-combination combination expanded-type))
                  :else
                    :collect type)))))

(defun simplify-collection-type (collection-name type-specifier &optional env)
  (let ((ts (flatten-type-specifier-combination collection-name type-specifier env))
        (non-redundant-types ()))
    (loop :for ts1 :in (sort (copy-list (rest ts)) #'subtypep)
          :do (unless (loop :for ts2 :in non-redundant-types
                              :thereis (supertypep ts1 ts2 env))
                (push ts1 non-redundant-types)))
    (if (null (cdr non-redundant-types))
        (car non-redundant-types)
        (cons collection-name non-redundant-types))))

(defun simplify-and-type (and-type-specifier &optional env)
  (simplify-collection-type 'and and-type-specifier env))

(defun simplify-or-type (or-type-specifier &optional env)
  (simplify-collection-type 'or or-type-specifier env))

(defun simplify-member-type (member-type-specifier &optional env)
  (simplify-collection-type 'member member-type-specifier env))

(defmethod %subtypep ((t1-name (eql 'and)) t2-name type1 type2 &optional env)
  (let ((type1 (simplify-and-type type1)))
    (cond ((and (listp type1)
                (eq 'and (first type1)))
           (multiple-value-bind (null knownp) (apply #'intersection-null-p env (rest type1))
             (cond ((and knownp null)
                    (values t t))
                   ;; ((not knownp)
                   ;;  (values nil nil))
                   (t
                    (values (some (lambda (type)
                                    (multiple-value-bind (subtypep knownp)
                                        (subtypep type type2 env)
                                      ;; We want at least one type to be a known subtype
                                      (and knownp subtypep)))
                                  (rest type1))
                            t)))))
          (t
           (multiple-value-bind (subtypep knownp) (subtypep type1 type2 env)
             (values subtypep knownp))))))

(defmethod %subtypep (t1-name (t2-name (eql 'and)) type1 type2 &optional env)
  (let ((type2 (simplify-and-type type2)))
    (cond ((and (listp type2)
                (eq 'and (first type2)))
           (values (every (lambda (type)
                           (subtypep type1 type env))
                          (rest type2))
                   t))
          (t
           (subtypep type1 type2 env)))))

(defmethod %subtypep ((t1-name (eql 'and)) (t2-name (eql nil)) type1 type2 &optional env)
  (let ((type1 (simplify-and-type type1)))
    (cond ((and (listp type1)
                (eq 'and (first type1)))
           (apply #'intersection-null-p env (rest type1)))
          (t
           (multiple-value-bind (subtypep knownp) (subtypep type1 type2 env)
             (values subtypep knownp))))))

(5am:def-test subtypep-and ()
  (5am:is-true  (subtypep '(and string) 'string))
  (5am:is-true  (subtypep 'string '(and string)))
  (5am:is-true  (subtypep '(and string integer) 'string))
  (5am:is-true  (subtypep '(and string integer) 'simple-string))
  (5am:is-false (subtypep 'simple-string '(and string integer)))
  (5am:is-true  (subtypep '(and simple-string integer) '(and string integer))))

(defmethod %subtypep ((t1-name (eql 'or)) t2-name type1 type2 &optional env)
  (let ((type1 (simplify-or-type type1)))
    (cond ((and (listp type1)
                (eq 'or (first type1)))
           (values (every (lambda (type)
                            (subtypep type type2 env))
                          (rest type1))
                   t))
          (t
           (subtypep type1 type2 env)))))

(defmethod %subtypep (t1-name (t2-name (eql 'or)) type1 type2 &optional env)
  (let ((type2 (simplify-or-type type2)))
    (cond ((and (listp type2)
                (eq 'or (first type2)))
           (values (some (lambda (type)
                           (subtypep type1 type env))
                         (rest type2))
                   t))
          (t
           (subtypep type1 type2 env)))))

(5am:def-test subtypep-or ()
  (5am:is-true  (subtypep '(or string simple-string) 'string))
  (5am:is-true  (subtypep '(or string) 'string))
  (5am:is-false (subtypep '(or string) 'simple-string))
  (5am:is-true  (subtypep 'simple-string '(or string integer))))

(defmethod %subtypep ((t1-name (eql 'not)) t2-name t1 t2 &optional env)
  ;; Removing the NOT
  (optima.extra:let-match (((list _ ts) t1))
    (multiple-value-bind (intersectp knownp) (intersect-type-p ts t2 env)
      (cond ((and intersectp knownp)
             (values nil t))
            (t
             (call-next-method))))))

(defmethod %subtypep (t1-name (t2-name (eql 'not)) t1 t2 &optional env)
  ;; Removing the NOT
  (optima.extra:let-match (((list _ ts) t2))
    (multiple-value-bind (intersectp knownp) (intersect-type-p ts t1 env)
      (cond ((and (not intersectp) knownp)
             (values t t))
            (knownp
             (values nil t))
            (t
             (call-next-method))))))

(defmethod %subtypep ((t1-name (eql 'eql)) (t2-name (eql 'eql)) type1 type2 &optional env)
  (declare (ignore t1-name t2-name env))
  (assert (listp type1) (type1))
  (assert (listp type2) (type2))
  (values (eql (second type1) (second type2))
          t))

(defmethod %subtypep ((t1-name (eql 'eql)) t2-name type1 type2 &optional env)
  (declare (ignore t1-name t2-name))
  (assert (listp type1) (type1))
  (values (typep (second type1) type2 env)
          t))

(defmethod %subtypep ((t1-name (eql 'member)) (t2-name (eql 'member)) type1 type2 &optional env)
  (declare (ignore t1-name t2-name env))
  (assert (listp type1) (type1))
  (assert (listp type2) (type2))
  (values (subsetp (rest type1) (rest type2) :test #'eql)
          t))

(defmethod %subtypep ((t1-name (eql 'member)) t2-name type1 type2 &optional env)
  (declare (ignore t1-name))
  (assert (listp type1) (type1))
  (values (every (lambda (obj) (typep obj type2 env)) (rest type1))
          t))

(defmethod %subtypep ((t1 (eql 'array)) (t2 (eql 'array)) type1 type2 &optional env)
  (declare (ignore t1 t2 env))
  (labels ((dim-subtype-p (dim1 dim2)
             (cond ((and (atom dim1) (atom dim2))
                    (or (eq dim2 'cl:*)
                        (and (not (eq dim1 'cl:*))
                             (= dim1 dim2))))
                   ((and (atom dim2) (eq dim2 'cl:*))
                    t)
                   ((atom dim1)
                    nil)
                   ((= (length dim1) (length dim2))
                    (every #'dim-subtype-p dim1 dim2))
                   (t
                    nil))))
    (destructuring-bind (&optional (elt1 'cl:*) (dim1 'cl:*)) (rest (ensure-list type1))
      (destructuring-bind (&optional (elt2 'cl:*) (dim2 'cl:*)) (rest (ensure-list type2))
        (let* ((dim1 (if (integerp dim1)
                         (make-list dim1 :initial-element 'cl:*)
                         dim1))
               (dim2 (if (integerp dim2)
                         (make-list dim2 :initial-element 'cl:*)
                         dim2))
               (dim-subtype-p (dim-subtype-p dim1 dim2)))
          (cond ((and (eq 'cl:* elt1) (eq 'cl:* elt2))
                 (values dim-subtype-p t))
                ((eq 'cl:* elt1)
                 ;; TYPE1 is specific; TYPE2 is not
                 (values nil t))
                ((eq 'cl:* elt2)
                 (values dim-subtype-p t))
                ((type= (second type1) (second type2))
                 (values dim-subtype-p t))
                (t
                 (values nil t))))))))

(defmethod %subtypep ((t1 (eql 'array)) (t2 (eql 'simple-array)) type1 type2 &optional env)
  (declare (ignore t1 t2 type1 type2 env))
  (values nil t))

(defmethod %subtypep ((t1 (eql 'simple-array)) (t2 (eql 'array)) type1 type2 &optional env)
  (declare (ignore t1 t2))
  (%subtypep 'array 'array
             `(array ,@(rest (ensure-list type1)))
             `(array ,@(rest (ensure-list type2))) env))

(defmethod %subtypep ((t1 (eql 'simple-array)) (t2 (eql 'simple-array)) type1 type2 &optional env)
  (declare (ignore t1 t2))
  (%subtypep 'array 'array
             `(array ,@(rest (ensure-list type1)))
             `(array ,@(rest (ensure-list type2))) env))

(defmethod %subtypep ((n1 (eql 'array)) (n2 (eql 'sequence)) t1 t2 &optional env)
  (multiple-value-bind (subtypep knownp) (subtypep t1 'vector env)
    (values subtypep knownp)))

(defmethod %subtypep ((n1 (eql 'simple-array)) (n2 (eql 'sequence)) t1 t2 &optional env)
  (multiple-value-bind (subtypep knownp) (subtypep t1 'simple-vector env)
    (values subtypep knownp)))

(defmethod %subtypep ((n1 (eql 'complex)) (n2 (eql 'complex)) t1 t2 &optional env)
  (declare (ignore n1 n2 env))
  (destructuring-bind (c1 &optional (elt1 'cl:*)) (ensure-list t1)
    (declare (ignore c1))
    (destructuring-bind (c2 &optional (elt2 'cl:*)) (ensure-list t2)
      (declare (ignore c2))
      (cond ((and (eq 'cl:* elt1) (eq 'cl:* elt2))
             (values t t))
            ((eq 'cl:* elt1)
             ;; TYPE2 is specific; TYPE1 is not
             (values nil t))
            ((eq 'cl:* elt2)
             (values t t))
            (t
             (multiple-value-bind (type= knownp) (type= elt1 elt2)
               (values type= knownp)))))))

(defmethod %subtypep ((n1 (eql 'complex)) n2 t1 t2 &optional env)
  (declare (ignore n1 t1))
  (if (and (or (symbolp t2)
               (and (listp t2)
                    (car t2)
                    (null (cdr t2))))
           (find-class n2 nil env))
      (multiple-value-bind (subtypep knownp)
          (cl:subtypep 'complex t2 env)
        (values subtypep knownp))
      (call-next-method)))

(defmethod %subtypep (n1 (n2 (eql 'complex)) t1 t2 &optional env)
  (declare (ignore n2 t2))
  (if (and (or (symbolp t1)
               (and (listp t1)
                    (car t1)
                    (null (cdr t1))))
           (find-class n1 nil env))
      (multiple-value-bind (subtypep knownp)
          (cl:subtypep t1 'complex env)
        (values subtypep knownp))
      (call-next-method)))

#+sbcl
(progn
  (defmethod %subtypep ((n1 (eql 'short-float)) n2 t1 t2 &optional env)
    (declare (ignore n1 n2))
    (subtypep `(single-float ,@(rest (ensure-list t1))) t2 env))
  (defmethod %subtypep (n1 (n2 (eql 'short-float)) t1 t2 &optional env)
    (declare (ignore n1 n2))
    (subtypep t1 `(single-float ,@(rest (ensure-list t1))) env))
  (defmethod %subtypep ((n1 (eql 'long-float)) n2 t1 t2 &optional env)
    (declare (ignore n1 n2))
    (subtypep `(double-float ,@(rest (ensure-list t1))) t2 env))
  (defmethod %subtypep (n1 (n2 (eql 'long-float)) t1 t2 &optional env)
    (declare (ignore n1 n2))
    (subtypep t1 `(double-float ,@(rest (ensure-list t2))) env)))

(defmethod %subtypep ((n1 (eql 'integer)) (n2 (eql 'rational)) t1 t2 &optional env)
  (subtypep `(rational ,@(rest (ensure-list t1))) t2 env))

(macrolet ((def (subtype supertype)
             `(progn
                (defmethod %subtypep ((n1 (eql ',subtype)) (n2 (eql ',supertype))
                                      t1 t2 &optional env)
                  (declare (ignore n1 n2 t1 t2 env))
                  (values t t))
                (defmethod %subtypep ((n1 (eql ',supertype)) (n2 (eql ',subtype))
                                      t1 t2 &optional env)
                  (declare (ignore n1 n2 t1 t2 env))
                  (values nil t)))))
  (def standard-char base-char)
  (def standard-char character)
  (def base-char character))
