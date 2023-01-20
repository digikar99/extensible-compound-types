(in-package :extensible-compound-types.impl)

(defun flatten-type-specifier-combination (combination type-specifier &optional env)
  "Linearizes a nest type-specifier combined using COMBINATION"
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

(defun simplify-and-type (and-type-specifier &optional env)
  (let ((ts (flatten-type-specifier-combination 'and and-type-specifier env))
        (non-redundant-types ()))
    (loop :for ts1 :in (sort (copy-list (rest ts)) #'subtypep)
          :do (unless (loop :for ts2 :in non-redundant-types
                              :thereis (supertypep ts1 ts2 env))
                (push ts1 non-redundant-types)))
    (case (length non-redundant-types)
      (0 `(and))
      (1 (car non-redundant-types))
      (t `(and ,@non-redundant-types)))))

(defun simplify-or-type (or-type-specifier &optional env)
  (let ((ts (flatten-type-specifier-combination 'or or-type-specifier env))
        (non-redundant-types ()))
    (loop :for ts1 :in (sort (copy-list (rest ts)) #'subtypep)
          :do (unless (loop :for ts2 :in non-redundant-types
                              :thereis (subtypep ts1 ts2 env))
                (push ts1 non-redundant-types)))
    (case (length non-redundant-types)
      (0 `(or))
      (1 (car non-redundant-types))
      (t `(or ,@non-redundant-types)))))

(define-subtypep-lambda (and nil) (type1 type2 env)
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

(define-subtypep-lambda (nil and) (type1 type2 env)
  (let ((type2 (simplify-and-type type2)))
    (cond ((and (listp type2)
                (eq 'and (first type2)))
           (values (every (lambda (type)
                            (subtypep type1 type env))
                          (rest type2))
                   t))
          (t
           (subtypep type1 type2 env)))))

;; (defmethod %subtypep ((t1-name (eql 'and)) (t2-name (eql nil)) type1 type2 &optional env)
;;   (let ((type1 (simplify-and-type type1)))
;;     (cond ((and (listp type1)
;;                 (eq 'and (first type1)))
;;            (apply #'intersection-null-p env (rest type1)))
;;           (t
;;            (multiple-value-bind (subtypep knownp) (subtypep type1 type2 env)
;;              (values subtypep knownp))))))

(5am:def-test subtypep-and ()
  (5am:is-true  (subtypep '(and string) 'string))
  (5am:is-true  (subtypep 'string '(and string)))
  (5am:is-true  (subtypep '(and string integer) 'string))
  (5am:is-true  (subtypep '(and string integer) 'simple-string))
  (5am:is-false (subtypep 'simple-string '(and string integer)))
  (5am:is-true  (subtypep '(and simple-string integer) '(and string integer))))

(define-subtypep-lambda (or nil) (type1 type2 env)
  (let ((type1 (simplify-or-type type1)))
    (cond ((and (listp type1)
                (eq 'or (first type1)))
           (values (every (lambda (type)
                            (subtypep type type2 env))
                          (rest type1))
                   t))
          (t
           (subtypep type1 type2 env)))))

(define-subtypep-lambda (nil or) (type1 type2 env)
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



(define-subtypep-lambda (not nil) (t1 t2 env)
  ;; Removing the NOT
  (optima.extra:let-match (((list _ ts) t1))
    (multiple-value-bind (intersectp knownp) (intersect-type-p ts t2 env)
      (cond ((and intersectp knownp)
             (values nil t))
            (t
             (values nil nil))))))

(define-subtypep-lambda (nil not) (t1 t2 env)
  ;; Removing the NOT
  (optima.extra:let-match (((list _ ts) t2))
    (multiple-value-bind (intersectp knownp) (intersect-type-p ts t1 env)
      (cond ((and (not intersectp) knownp)
             (values t t))
            (knownp
             (values nil t))
            (t
             (values nil nil))))))


(define-subtypep-lambda (eql eql) (type1 type2 env)
  (declare (ignore env))
  (assert (listp type1) (type1))
  (assert (listp type2) (type2))
  (values (eql (second type1) (second type2))
          t))

(define-subtypep-lambda (eql nil) (type1 type2 env)
  (assert (listp type1) (type1))
  (values (typep (second type1) type2 env)
          t))
