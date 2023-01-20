(in-package :extensible-compound-types.impl)

(5am:in-suite :extensible-compound-types)

(cl:deftype type-specifier () `(or symbol cons))

(defstruct (extype-structure
            (:predicate extypep)
            (:copier nil)
            (:constructor nil)
            (:conc-name extype-))
  name
  documentation
  arg-list)

(defmethod print-object ((o extype-structure) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (if-let (arg-list (extype-arg-list o))
      (format stream "~S ~A" (extype-name o) arg-list)
      (format stream "~S" (extype-name o)))))

(defstruct (derived-extype
            (:include extype-structure)
            (:conc-name derived-extype-))
  "CLHS calls these abbreviating type specifiers, and are defined using DEFTYPE"
  expander)

(defstruct (primitive-extype
            (:include extype-structure)
            (:conc-name extype-)
            (:copier nil)
            (:constructor nil))
  lambda
  lambda-expression
  to-cl-type)

(defstruct (primitive-atomic-extype
            (:include primitive-extype)
            (:conc-name extype-)
            (:copier nil)))

(defstruct (primitive-compound-extype
            (:include primitive-extype)
            (:conc-name extype-)
            (:copier nil))
  compiler-macro
  specializing-p)

(define-condition unknown-type-specifier (error)
  ((type-specifier :initarg :type))
  (:report (lambda (c s)
             (with-slots (type-specifier) c
               (format s "Unknown extype-specifier ~S" type-specifier)))))

(declaim (inline type-name))
(defun type-name (type)
  (declare (cl:type type-specifier type))
  (etypecase type
    (symbol type)
    (cons (car type))))

(defvar *extype-structure-table* (make-hash-table))

(defun extype-structure (type-specifier &optional (error-if-not-exists t))
  (declare (cl:type type-specifier type-specifier))
  (or (gethash (type-name type-specifier)
               *extype-structure-table*)
      (if error-if-not-exists
          (error 'unknown-type-specifier :type type-specifier)
          nil)))

(defvar *warn-on-type-redefinition* nil)

(defun (setf extype-structure) (new-extype-structure type-specifier)

  (declare (cl:type type-specifier type-specifier)
           (cl:type (or null extype-structure) new-extype-structure))

  (let ((name (type-name type-specifier)))

    (when (null new-extype-structure)
      (remhash name *extype-structure-table*)
      (return-from extype-structure nil))

    (when-let (old-extype-structure (gethash name *extype-structure-table*))

      (when *warn-on-type-redefinition*
        (simple-style-warning "Redefining extype ~S~%" name)
        (unless (eq (class-of old-extype-structure)
                    (class-of new-extype-structure))
          (simple-style-warning "~S was originally ~S but is being redefined to be a ~S~%"
                                name
                                (class-name (class-of old-extype-structure))
                                (class-name (class-of new-extype-structure)))))

      (when (and (eq (find-class 'primitive-compound-extype)
                     (class-of new-extype-structure))
                 (eq (class-of old-extype-structure)
                     (class-of new-extype-structure)))
        (setf (extype-compiler-macro new-extype-structure)
              (extype-compiler-macro old-extype-structure))))

    (setf (gethash name *extype-structure-table*)
          new-extype-structure)))

(defun primitive-extype-names ()
  (mapcar (lambda (extype-structure)
            (extype-name extype-structure))
          (remove-if-not (lambda (extype-structure)
                           (cl:typep extype-structure 'primitive-extype))
                         (hash-table-values *extype-structure-table*))))
