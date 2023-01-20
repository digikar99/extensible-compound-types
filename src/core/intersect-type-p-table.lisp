(in-package :extensible-compound-types.impl)

(5am:in-suite  :extensible-compound-types)

(defvar *primitive-intersect-type-p-table* (make-hash-table :test #'equal)
  "Maps a CONS cell with the INTERSECT-TYPE-P function corresponding to those primitive types.")

(defun primitive-intersect-type-p-lambda (name1 name2)
  (gethash (cons name1 name2) *primitive-intersect-type-p-table*))

(defun (setf primitive-intersect-type-p-lambda) (intersect-type-p-lambda name1 name2)
  (if intersect-type-p-lambda
      (setf (gethash (cons name1 name2) *primitive-intersect-type-p-table*)
            intersect-type-p-lambda)
      (remhash (cons name1 name2) *primitive-intersect-type-p-table*)))

(defun intersect-type-p-lambda (type-specifier-1 type-specifier-2 &optional environment)
  (declare (ignore environment))
  (with-extype-name-and-expansion-2
      (n1 n2 t1 t2)
      type-specifier-1 type-specifier-2
    (or (primitive-intersect-type-p-lambda n1 n2)
        (primitive-intersect-type-p-lambda n1 nil)
        (primitive-intersect-type-p-lambda nil n2))))

(defun all-intersect-type-p-lambda (type-specifier-1 type-specifier-2 &optional environment)
  (declare (ignore environment))
  (with-extype-name-and-expansion-2
      (n1 n2 t1 t2)
      type-specifier-1 type-specifier-2
    (remove-if #'null
               (list (primitive-intersect-type-p-lambda n1 n2)
                     (primitive-intersect-type-p-lambda n1 nil)
                     (primitive-intersect-type-p-lambda nil n2)))))

(defun intersect-type-p (type-specifier-1 type-specifier-2 &optional environment)
  (if (equal type-specifier-1 type-specifier-2)
      (values t t)
      (with-extype-name-and-expansion-2
          (n1 n2 t1 t2)
          type-specifier-1 type-specifier-2
        (loop :for intersect-type-p-lambda
                :in (all-intersect-type-p-lambda n1 n2 environment)
              :do (multiple-value-bind (intersect-type-p knownp)
                      (funcall intersect-type-p-lambda t1 t2)
                    (when knownp
                      (return (values intersect-type-p knownp))))
              :finally (return (values nil nil))))))

(defmacro define-intersect-type-p-lambda
    ((name1 name2) (expansion1-var expansion2-var env-var) &body body)
  (cl:check-type name1 symbol)
  (cl:check-type name2 symbol)
  (cl:check-type expansion1-var symbol)
  (cl:check-type expansion2-var symbol)
  (multiple-value-bind (rem-forms decl) (parse-body body)
    `(with-eval-always
       (setf (primitive-intersect-type-p-lambda ',name1 ',name2)
             (list-named-lambda (intersect-type-p ,name1 ,name2)
                 (,expansion1-var ,expansion2-var &optional ,env-var)
               ,@decl
               (block intersect-type-p
                 ,@rem-forms))))))

(defun undefined-intersect-type-p-relations ()
  (let* ((primitive-extype-names (primitive-extype-names))
         (possible-intersect-type-p-relations
           (loop :for n1 :in primitive-extype-names
                 :nconcing (loop :for n2 :in primitive-extype-names
                                 :if (not (primitive-intersect-type-p-lambda n1 n2))
                                   :collect (cons n1 n2)))))
    (remove-if (lambda (cons)
                 (primitive-intersect-type-p-lambda (car cons) (cdr cons)))
               possible-intersect-type-p-relations)))

(defun intersection-null-p (env &rest type-specifiers)
  ;; FIXME: Make a distinction between NIL and NULL
  ;; Intersection is NULL if at least one type specifier is a subtype of NIL.
  ;; Intersection is also NULL if the intersection of any two type specifiers is NULL.
  ;; However, even if the intersection of all 2-combinations of type specifiers is non-NULL,
  ;; the intersection can still be NULL because each combination might intersect at a different place.

  ;; First check if some type is known to be NIL
  (let ((all-known-p t))
    (loop :for ts :in type-specifiers
          :do (multiple-value-bind (subtypep knownp)
                  (subtypep ts nil env)
                (cond ((and knownp subtypep)
                       (return-from intersection-null-p (values t t)))
                      ((not knownp)
                       (setq all-known-p nil)))))
    (when all-known-p
      ;; Since all types are known to be non-NIL, if some type is T, then
      ;; intersection is non-NIL
      (loop :for ts :in type-specifiers
            :do (multiple-value-bind (tp knownp)
                    (type= ts t env)
                  (cond ((and knownp tp)
                         (return-from intersection-null-p (values nil t)))
                        ((not knownp)
                         (setq all-known-p nil)))))))
  (loop :for (type1 . rest) :on type-specifiers
        :with all-known-p := t
        :while all-known-p
        :do (loop :for type2 :in rest
                  :while all-known-p
                  :do (multiple-value-bind (intersectp knownp)
                          (intersect-type-p type1 type2 env)
                        (cond ((and knownp (not intersectp))
                               (return-from intersection-null-p (values t t)))
                              ((not knownp)
                               (setq all-known-p nil)))))
        :finally (return-from intersection-null-p
                   (cond ((null type-specifiers)
                          (values nil t))
                         ((null (cdr type-specifiers)) ; there's just one specifier
                          (values nil t))
                         ((and all-known-p
                               (null (cddr type-specifiers)))
                          (values nil t))
                         ((or all-known-p t)
                          ;; Either the intersection of three or more types is NULL
                          ;; OR we don't even know all the intersections of two types
                          (values nil nil))))))

(5am:def-test intersection-null-p ()
  (5am:is-false (intersection-null-p nil 'vector))
  (5am:is-false (intersection-null-p nil 'simple-string 'string))
  (5am:is-false (intersection-null-p nil 'string 'simple-string))
  (5am:is-true  (intersection-null-p nil 'string 'integer))
  (5am:is-true  (intersection-null-p nil 'string 'integer 'simple-string))
  (5am:is-true  (intersection-null-p nil 'string 'integer 'float))
  (5am:is-false (intersection-null-p nil
                                     '(or string number)
                                     '(or string symbol))))
