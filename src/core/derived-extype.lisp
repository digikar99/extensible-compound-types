(in-package :extensible-compound-types.impl)

(5am:in-suite  :extensible-compound-types)

(defvar *excluded-packages-for-cl-deftype*
  (mapcar #'find-package '(:cl :alexandria :trivial-types :sb-kernel))
  "EXTENSIBLE-COMPOUND-TYPES:DEFTYPE avoids adding a CL:DEFTYPE if the NAME is
a symbol in package excluded in this list.")
(cl:declaim (type list *excluded-packages-for-cl-deftype*))

(defmacro deftype (name lambda-list &body body &environment env)
  "Useful for defining type aliases, example: (DEFTYPE INT32 () '(SIGNED-BYTE 32))

Depending on the value of *EXCLUDED-PACKAGES-FOR-CL-DEFTYPE*,
also adds a CL:DEFTYPE with the expansion being determined by UPGRADED-CL-TYPE"
  (multiple-value-bind (rem-body decl doc) (parse-body body :documentation t)

    (with-gensyms (form)
      `(progn

         ,(unless (member (symbol-package name) *excluded-packages-for-cl-deftype*)
            `(cl:deftype ,name (&whole ,form ,@lambda-list)
               ,@(when doc `(,doc))
               ,(ignore-all-form-from-lambda-list lambda-list)
               (upgraded-cl-type ,form)))

         (with-eval-always
           (setf (extype-structure ',name)
                 (make-derived-extype
                  :name ',name
                  :documentation ,doc
                  :arg-list ',lambda-list
                  :expander ,(parse-macro name
                                          lambda-list
                                          (append decl rem-body)
                                          env)))
           (namespace-value-and-doc-set
            ',name
            ',lambda-list
            ,(if doc
                 (format nil
                         "~A names a DERIVED-EXTYPE~%~A"
                         name
                         doc)
                 (format nil
                         "~A names a DERIVED-EXTYPE~%"
                         name))))))))

;;; TODO: Could introduce a TMAKUNBOUND
(defmacro undeftype (name)
  `(with-eval-always
     (setf (extype-structure ',name) nil)))

(defun typexpand-1 (type &optional env)
  "Returns two values: EXPANSION and EXPANDEDP"
  (let* ((atomp (atom type))
         (type-name (if (atom type)
                        #-ecl type
                        #+ecl (if (eq type 'c::gen-bool) 'boolean type)
                        (car type)))
         (extype-structure (extype-structure type-name nil)))
    (optima:match extype-structure
      ((derived-extype expander)
       (let ((expansion (funcall expander (if atomp (list type) type) env)))
         (values expansion
                 (not (equal expansion type)))))
      (_
       (values type nil)))))

(defun typexpand (type &optional env)
  (let ((expansion (typexpand-1 type env)))
    (if (equal expansion type)
        (values expansion nil)
        (values (typexpand expansion env) t))))


(5am:def-test define-and-undefine-types ()
  (eval `(progn
           (deftype float32 () 'single-float)
           (deftype f32 () 'float32)
           (5am:is (eq 'float32 (typexpand-1 'f32)))
           (5am:is (eq 'single-float (typexpand-1 'float32)))
           (undeftype f32)
           (undeftype float32))))

(defmacro with-extype-name-and-expansion ((name-var expansion-var) type &body body)
  (cl:check-type expansion-var symbol)
  (cl:check-type name-var symbol)
  (once-only (type)
    `(let* ((,expansion-var (typexpand ,type))
            (,name-var (type-name ,expansion-var)))
       ,@body)))

(defmacro with-extype-name-and-expansion-2
    ((name1-var name2-var expansion1-var expansion2-var) type1 type2 &body body)
  `(with-extype-name-and-expansion (,name1-var ,expansion1-var) ,type1
     (with-extype-name-and-expansion (,name2-var ,expansion2-var) ,type2
       ,@body)))
