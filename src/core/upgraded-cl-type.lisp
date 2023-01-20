(in-package :extensible-compound-types.impl)

(defmacro define-cl-type-for-extype (extype-name (type-specifier-var env-var) &body body)
  "Defines how to convert any given primitive extype specifier
with EXTYPE-NAME to the corresponding CL type specifier."
  (cl:check-type extype-name symbol)
  (cl:check-type type-specifier-var symbol)
  (cl:check-type env-var symbol)
  (with-gensyms (extype-structure)
    `(with-eval-always
       (let ((,extype-structure (extype-structure ',extype-name)))
         (assert (and ,extype-structure
                      (cl:typep ,extype-structure 'primitive-extype)))
         (setf (extype-to-cl-type ,extype-structure)
               (lambda (,type-specifier-var ,env-var) ,@body))))))

(defun upgraded-cl-type (type-specifier &optional environment)
  (with-extype-name-and-expansion (name exp) type-specifier
    (let* ((extype-structure (extype-structure name nil))
           (fn (when extype-structure
                 (extype-to-cl-type extype-structure))))
      (if fn
          (funcall fn exp environment)
          exp))))
