(in-package :extensible-compound-types.impl)

(define-condition unknown-class-specializer (unknown-type-specifier)
  ()
  (:report (lambda (c s)
             (with-slots (type-specifier) c
               (format s "No specializer defined for class with name ~S" type-specifier)))))

(defstruct (class-specializer
            (:include derived-extype)
            (:conc-name cs-)
            (:copier nil))
  "A CLASS-SPECIALIZER can be used to restrict the type to only
certain members of the class. For example, (INTEGER 0 5) denotes
only those members of the class INTEGER that are between 0 and 5.

Similarly, (ARRAY SINGLE-FLOAT) denotes only those ARRAY that
have the element-type as SINGLE-FLOAT.

All these types expand into the SPECIALIZING PRIMITIVE-COMPOUND-TYPE"
  class-name
  lambda
  lambda-expression
  to-cl-type
  subtypep-lambda
  intersectp-lambda)

(defstruct (orthogonal-class-specializer
            (:include class-specializer)
            (:conc-name ocs-)
            (:copier nil))
  ;; TODO: Trivia/optima
  "An ORTHOGONAL-CLASS-SPECIALIZER is an EXTYPE
whose each argument specializes independently of each other.
For example (ARRAY element-type) is an orthogonally specializing
class specializer, while (INTEGER low high) is not.

An orthogonally specializing class specializer has some nice
properties; in particular, subtypep and intersectp methods
are automatically defined for them.

For our purposes, we also assume that every not-* value of the
type parameters results in a different non-intersecting
type specifier. If or not this is necessary for the notion of
orthogonality seems unclear. Orthogonality was intended as
between parameters rather than within parameters."
  slots)

(defun equalize-list-lengths/2 (l1 l2 &optional default)
  (let ((len1 (length l1))
        (len2 (length l2)))
    (cond ((= len1 len2)
           (values l1 l2))
          ((< len1 len2)
           (values (nconc l1 (make-list (- len2 len1) :initial-element default))
                   l2))
          ((> len1 len2)
           (values l1
                   (nconc l2 (make-list (- len1 len2) :initial-element default)))))))

(defun orthogonal-subtypep-lambda (expansion1-args expansion2-args)
  (multiple-value-bind (l1 l2)
      (equalize-list-lengths/2 expansion1-args expansion2-args 'cl:*)
    (values (loop :for a1 :in l1
                  :for a2 :in l2
                  :always (cond ((eq a2 'cl:*)
                                 t)
                                ((atom a2)
                                 (equal a1 a2))
                                ((atom a1)
                                 nil)
                                (t
                                 (orthogonal-subtypep-lambda a1 a2))))
            t)))

(defun orthogonal-intersect-type-p-lambda (expansion1-args expansion2-args)
  (multiple-value-bind (l1 l2)
      (equalize-list-lengths/2 expansion1-args expansion2-args 'cl:*)
    (values (loop :for a1 :in l1
                  :for a2 :in l2
                  :always (cond ((or (eq a2 'cl:*)
                                     (eq a1 'cl:*))
                                 t)
                                ((and (consp a1)
                                      (consp a2))
                                 (orthogonal-intersect-type-p-lambda a1 a2))
                                (t
                                 (equal a1 a2))))
            t)))

(defun tree-equal-or-* (tree1 tree2)
  (cond ((eq tree2 'cl:*)
         t)
        ((atom tree2)
         (equal tree1 tree2))
        ((atom tree1)
         nil)
        (t
         (and (tree-equal-or-* (car tree1) (car tree2))
              (tree-equal-or-* (cdr tree1) (cdr tree2))))))

(defvar *class-specializers* (make-hash-table))

(defun class-specializer (class-name &optional (error-if-not-exists t))
  (declare (cl:type symbol class-name))
  (multiple-value-bind (specializer existsp)
      (gethash class-name *class-specializers*)
    (cond (existsp
           specializer)
          (error-if-not-exists
           (error 'unknown-class-specializer :type class-name))
          (t
           nil))))

(defun (setf class-specializer) (specializer class-name)
  (declare (cl:type symbol class-name)
           (cl:type (or class-specializer null) specializer))
  (if specializer
      (setf (gethash class-name *class-specializers*) specializer)
      (remhash class-name *class-specializers*)))

(define-compound-type specializing (o class-name &rest args)
  (and (cl:typep o class-name)
       (apply (cs-lambda (class-specializer class-name)) o args)))

(defun lambda-list-declarations-from-args (lambda-list arg-forms &optional env)
  (let* ((keyword-pos (position '&key lambda-list))
         (keyword-parameters (when keyword-pos
                               (subseq lambda-list (1+ keyword-pos)))))
    (flet ((form-type-decl (form parameter)
             (let* ((extype (cl-form-types:nth-form-type form env 0 t t))
                    (type (upgraded-cl-type extype)))
               `((extype ,extype ,parameter)
                 (cl:type ,type ,parameter))))
           (keyword (name)
             (find name keyword-parameters :test #'string=)))
      `(declare ,@(loop :with state := nil
                        :while arg-forms
                        :for parameter :in lambda-list
                        :nconcing
                        (if (member parameter lambda-list-keywords)
                            (progn (setq state parameter) nil)
                            (let ((decl
                                    (case state
                                      ((nil)
                                       `(,@(form-type-decl (first arg-forms) parameter)))
                                      (&optional
                                       (let ((parameter (etypecase parameter
                                                          (symbol parameter)
                                                          (cons (first parameter)))))
                                         `(,@(form-type-decl (first arg-forms)
                                                             parameter))))
                                      (&key
                                       (let ((name  (first arg-forms))
                                             (value (second arg-forms)))
                                         `(,@(form-type-decl value
                                                             (keyword name))))))))
                              (setq arg-forms (case state
                                                (&key (nthcdr 2 arg-forms))
                                                (t (cdr arg-forms))))
                              decl)))))))

(define-compound-type-compiler-macro specializing
    (&whole form o-form class-name-form &rest arg-forms &environment env)
  (let* ((class-name-form-type
           (cl-form-types:nth-form-type class-name-form env))
         (class-name (optima:match class-name-form-type
                       ((list 'eql class-name)
                        class-name)
                       ((list 'member class-name)
                        class-name)
                       (_
                        t)))
         (parent-form `(typep ,o-form
                              (specializing ,class-name-form ,@arg-forms))))
    (compiler-macro-notes:with-notes
        (parent-form env
                     :name '(:extype specializing)
                     :unwind-on-signal nil)
      (if (eq t class-name)
          (progn
            (signal 'compiler-macro-notes:optimization-failure-note
                    :datum "Unable to derive CLASS-NAME from type~%  ~S~%of form~%  ~S"
                    :args (list class-name-form-type class-name-form))
            form)
          (with-gensyms (o)
            (let ((o-form-type (cl-form-types:nth-form-type o-form env)))
              `(let ((,o ,o-form))
                 (declare (extype ,o-form-type ,o)
                          (cl:type ,(upgraded-cl-type o-form-type) ,o))
                 (and (cl:typep ,o ',class-name)
                      (,(optima:ematch (cs-lambda-expression
                                        (class-specializer class-name))
                          ((list* lambda lambda-list body)
                           `(,lambda ,lambda-list
                              ,(lambda-list-declarations-from-args
                                lambda-list (cons `(cl:the ,o-form-type ,o)
                                                  arg-forms)
                                env)
                              ,@body)))
                       ,o
                       ,@arg-forms)))))))))

(define-subtypep-lambda (specializing specializing) (exp1 exp2 env)
  (declare (ignore env))
  (destructuring-bind (class1 &rest args1) (rest exp1)
    (declare (ignore args1))
    (destructuring-bind (class2 &rest args2) (rest exp2)
      (cond ((eq class1 class2)
             (let ((specializer (class-specializer class1)))
               (funcall (cs-subtypep-lambda specializer) (rest exp1) (rest exp2))))
            ((and (subclassp class1 class2)
                  (orthogonal-subtypep-lambda (make-list (length args2)
                                                         :initial-element 'cl:*)
                                              args2))
             (values t t))
            ((subclassp class2 class1)
             (values nil t))
            ((and (not (subclassp class1 class2))
                  (not (subclassp class2 class1)))
             (values nil t))
            ((not (eq class1 class2))
             (values nil nil))))))

(define-intersect-type-p-lambda (specializing specializing) (exp1 exp2 env)
  (declare (ignore env))
  (destructuring-bind (class1 &rest args1) (rest exp1)
    (declare (ignore args1))
    (destructuring-bind (class2 &rest args2) (rest exp2)
      (declare (ignore args2))
      (cond ((and (not (subclassp class1 class2))
                  (not (subclassp class2 class1)))
             (values nil t))
            ((not (eq class1 class2))
             (values nil nil))
            (t
             ;; Both classes are EQ
             (let ((specializer (class-specializer class1)))
               (funcall (cs-intersectp-lambda specializer) (rest exp1) (rest exp2))))))))

(define-cl-type-for-extype specializing (type-spec env)
  (declare (ignore env))
  (symbol-if-possible
   (let ((class-name (first (rest type-spec))))
     (funcall (cs-to-cl-type (class-specializer class-name)) (rest type-spec)))))

(defmacro define-specializing-type
    (extype-name-spec (object-var &rest lambda-list)
     &body
       (typep-form
        &key subtypep-lambda
          intersect-type-p-lambda
          (to-cl-type ''symbol-if-possible)))
  "The OBJECT-VAR and the parameters in LAMBDA-LIST should be treated
as read-only parameters. The read-only nature is used for optimization."
  (with-gensyms (whole specializer)
    (destructuring-bind (extype-name &key (class extype-name))
        (ensure-list extype-name-spec)
      `(with-eval-always
         (let ((,specializer
                 (make-class-specializer
                  :name ',extype-name
                  :documentation nil
                  :arg-list ',lambda-list
                  :expander
                  ,(parse-macro nil
                                `(&whole ,whole ,@lambda-list)
                                `(,(ignore-all-form-from-lambda-list lambda-list)
                                  `(specializing ,',class ,@(rest ,whole))))
                  :class-name ',class
                  :lambda (lambda (,object-var ,@lambda-list)
                            ,typep-form)
                  :lambda-expression '(lambda (,object-var ,@lambda-list)
                                       (and (cl:typep ,object-var ',class)
                                        ,typep-form))
                  :subtypep-lambda ,subtypep-lambda
                  :intersectp-lambda ,intersect-type-p-lambda
                  :to-cl-type ,to-cl-type)))
           (namespace-value-and-doc-set ',extype-name ',lambda-list "")
           (setf (class-specializer ',class) ,specializer)
           (setf (extype-structure ',extype-name) ,specializer))))))

(defmacro define-orthogonally-specializing-type
    (extype-name-spec slot-lambda-list
     &body (slot-type-specs &key (to-cl-type ''symbol-if-possible)))
  "EXTYPE-NAME-SPEC
  can be
    EXTYPE
  or
    (EXTYPE &KEY (CLASS EXTYPE))

  EXTYPE is the name of the type, and CLASS is he name of
  the class whose specialization is being defined.

SLOT-LAMBDA-LIST
  is a lambda-list containing the names of the slots

Each element of SLOT-TYPE-SPECS should be of the form
  (SLOT-NAME &KEY (ACCESSOR 'SLOT-VALUE))

An orthogonally specializing type is a class specializer is an EXTYPE
whose each argument specializes independently of each other.
For example (ARRAY element-type) is an orthogonally specializing
class specializer, while (INTEGER low high) is not.

An orthogonally specializing class specializer has some nice
properties; in particular, subtypep and intersectp methods
are automatically defined for them.

For our purposes, we further assume that every non-* value of the
type parameters results in a different non-intersecting
type specifier. Originally, orthogonality was intended as
between parameters rather than within parameters. However, because
orthogonal type specifiers have been put to use as parametric types
in polymorphic-functions, and they require that the type specifiers
not only be orthogonal but also unique, we carry over the uniqueness
property to the orthogonal type specifiers themselves. The crux of the
problem is to be able to derive unique type parameters given an
object's value.
"
  ;; TODO: Define a TRIVIA/OPTIMA pattern for this type
  (with-gensyms (whole object-var specializer)
    (let ((slot-type-forms
            (loop :for slot-type-spec :in slot-type-specs
                  :collect (destructuring-bind
                               (slot-name &key (accessor 'slot-value) nested)
                               slot-type-spec
                             `(or (eql ,slot-name 'cl:*)
                                  ,(if nested
                                       `(tree-equal-or-* (,accessor ,object-var) ,slot-name)
                                       `(equal (,accessor ,object-var) ,slot-name))))))
          (slot-type-specs
            (loop :for slot-type-spec :in slot-type-specs
                  :collect (destructuring-bind
                               (slot-name &key (accessor 'slot-value) nested)
                               slot-type-spec
                             `(list ',slot-name
                                    :accessor ',accessor
                                    :accessor-function
                                    ,(if (and (listp accessor)
                                              (eq 'cl:lambda (first accessor)))
                                         accessor
                                         (list 'quote accessor))
                                    :nested ',nested)))))

      ;; The restriction of only-&OPTIONAL occurs because, at least in the
      ;; case of "parametric-types" (for, say, use in polymorphic-functions),
      ;; we need a "unique" way to recover the type.
      (assert (subsetp (intersection slot-lambda-list lambda-list-keywords)
                       '(&optional))
              ()
              "SLOT-LAMBDA-LIST can only have &OPTIONAL arguments.")
      ;; TODO: Add options for inheritance?
      (destructuring-bind (extype-name &key (class extype-name))
          (ensure-list extype-name-spec)
        `(with-eval-always
           (let ((,specializer
                   (make-orthogonal-class-specializer
                    :name ',extype-name
                    :documentation nil
                    :arg-list ',slot-lambda-list
                    :expander
                    ,(parse-macro nil
                                  `(&whole ,whole ,@slot-lambda-list)
                                  `(,(ignore-all-form-from-lambda-list slot-lambda-list)
                                    `(specializing ,',class ,@(rest ,whole))))
                    :class-name ',class
                    :lambda
                    (lambda (,object-var ,@slot-lambda-list)
                      (and (cl:typep ,object-var ',class)
                           (locally (declare (type ,class ,object-var))
                             (and ,@slot-type-forms))))
                    :lambda-expression
                    `(lambda (,',object-var ,@',slot-lambda-list)
                       (and (cl:typep ,',object-var ',',class)
                            (locally (declare (type ,',class ,',object-var))
                              (and ,@',slot-type-forms))))
                    :subtypep-lambda 'orthogonal-subtypep-lambda
                    :intersectp-lambda 'orthogonal-intersect-type-p-lambda
                    :to-cl-type ,to-cl-type
                    :slots (list ,@slot-type-specs))))
             (namespace-value-and-doc-set ',extype-name ',slot-lambda-list "")
             (setf (class-specializer ',class) ,specializer)
             (setf (extype-structure ',extype-name) ,specializer)))))))


(defun specializing-type-specifier-p (object)
  (and (type-specifier-p object)
       (with-extype-name-and-expansion (name exp) object
         (eq name 'specializing))))

(defun orthogonally-specializing-type-specifier-p (object)
  (and (type-specifier-p object)
       (with-extype-name-and-expansion (name exp) object
         (and (eq name 'specializing)
              (orthogonal-class-specializer-p
               (class-specializer (second exp)))))))

(defun orthogonally-specializing-type-of (object)
  "Returns the ORTHOGONALLY-SPECIALIZING-TYPE of the object."
  (with-extype-name-and-expansion (name exp) (clhs-class-from-object object)
    (assert (eq name 'specializing))
    (let ((ocs (class-specializer (second exp))))
      (cl:check-type ocs orthogonal-class-specializer)
      (with-slots (name arg-list slots) ocs
        (flet ((accessor (slot-name)
                 (getf (assoc-value slots slot-name) :accessor-function)))
          (cons name
                (loop :for slot-name :in arg-list
                      :nconc (optima:match slot-name
                               ((list slot-name _)
                                (list (funcall (accessor slot-name) object)))
                               (_
                                (if (eq slot-name '&optional)
                                    nil
                                    (list (funcall (accessor slot-name) object))))))))))))
