(in-package :extensible-compound-types.impl)

(defvar *primitive-subtypep-table* (make-hash-table :test #'equal)
  "Maps a CONS cell with the SUBTYPEP function corresponding to those primitive types.")

(defun primitive-subtypep-lambda (name1 name2)
  (gethash (cons name1 name2) *primitive-subtypep-table*))

(defun (setf primitive-subtypep-lambda) (subtypep-lambda name1 name2)
  (if subtypep-lambda
      (setf (gethash (cons name1 name2) *primitive-subtypep-table*)
            subtypep-lambda)
      (remhash (cons name1 name2) *primitive-subtypep-table*)))

(defun subtypep-lambda (type-specifier-1 type-specifier-2 &optional environment)
  (declare (ignore environment))
  (with-extype-name-and-expansion-2
      (n1 n2 t1 t2)
      type-specifier-1 type-specifier-2
    (or (primitive-subtypep-lambda n1 n2)
        (primitive-subtypep-lambda n1 nil)
        (primitive-subtypep-lambda nil n2))))

(defun all-subtypep-lambda (type-specifier-1 type-specifier-2 &optional environment)
  (declare (ignore environment))
  (with-extype-name-and-expansion-2
      (n1 n2 t1 t2)
      type-specifier-1 type-specifier-2
    (remove-if #'null
               (list (primitive-subtypep-lambda n1 n2)
                     (primitive-subtypep-lambda n1 nil)
                     (primitive-subtypep-lambda nil n2)))))

(defun subtypep (type-specifier-1 type-specifier-2 &optional environment)
  (if (equal type-specifier-1 type-specifier-2)
      (values t t)
      (with-extype-name-and-expansion-2
          (n1 n2 t1 t2)
          type-specifier-1 type-specifier-2
        (loop :for subtypep-lambda :in (all-subtypep-lambda n1 n2 environment)
              :do (multiple-value-bind (subtypep knownp)
                      (funcall subtypep-lambda t1 t2)
                    (when knownp
                      (return (values subtypep knownp))))
              :finally (return (values nil nil))))))

(defmacro define-subtypep-lambda
    ((name1 name2) (expansion1-var expansion2-var env-var) &body body)
  "Use NIL in place of NAME1 or NAME2 if a default is to be defined."
  (cl:check-type name1 symbol)
  (cl:check-type name2 symbol)
  (cl:check-type expansion1-var symbol)
  (cl:check-type expansion2-var symbol)
  (multiple-value-bind (rem-forms decl) (parse-body body)
    `(with-eval-always
       (setf (primitive-subtypep-lambda ',name1 ',name2)
             (list-named-lambda (subtypep ,name1 ,name2)
                 (,expansion1-var ,expansion2-var &optional ,env-var)
               ,@decl
               (block subtypep
                 ,@rem-forms))))))

(defun undefined-subtypep-relations ()
  (let* ((primitive-extype-names (primitive-extype-names))
         (possible-subtypep-relations
           (loop :for n1 :in primitive-extype-names
                 :nconcing (loop :for n2 :in primitive-extype-names
                                 :if (not (primitive-subtypep-lambda n1 n2))
                                   :collect (cons n1 n2)))))
    (remove-if (lambda (cons)
                 (primitive-subtypep-lambda (car cons) (cdr cons)))
               possible-subtypep-relations)))

(defun supertypep (type1 type2 &optional environment)
  (subtypep type2 type1 environment))

(defun type= (type1 type2 &optional environment)
  (multiple-value-bind (s1 k1) (subtypep type1 type2 environment)
    (multiple-value-bind (s2 k2) (subtypep type2 type1 environment)
      (cond ((and s1 k1 s2 k2)
             (values t t))
            ((and k1 k2)
             (values nil t))
            (t
             (values nil nil))))))
