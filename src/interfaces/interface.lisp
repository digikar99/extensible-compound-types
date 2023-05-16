(in-package :extensible-compound-types-interfaces.impl)

(define-condition unknown-interface ()
  ()
  (:report (lambda (c s)
             (with-slots (type-specifier) c
               (format s "No interface with name ~S" type-specifier)))))

(define-condition incompatible-interface-instance ()
  ((actual   :initarg :actual)
   (expected :initarg :expected))
  (:report (lambda (c s)
             (with-slots (actual expected) c
               (format s "Expected all of the following to be implemented~%~{  ~S~^~%~}~%but implemented the following~%~{  ~S~^~%~}" expected actual)))))

(defstruct (%interface
            (:include derived-extype)
            (:conc-name interface-)
            (:copier nil)
            (:constructor make-interface))
  "An INTERFACE defines the external interface to classes with varying
internal interfaces."
  required-functions
  default-functions
  instances
  dependencies)

(defvar *interfaces* (make-hash-table))

(defun interface (interface-name &optional (error-if-not-exists t))
  (declare (cl:type symbol interface-name))
  (multiple-value-bind (interface existsp)
      (gethash interface-name *interfaces*)
    (cond (existsp
           interface)
          (error-if-not-exists
           (error 'unknown-interface :type interface-name))
          (t
           nil))))

(defun (setf interface) (interface interface-name)
  (declare (cl:type symbol interface-name)
           (cl:type (or %interface null) interface))
  (if interface
      (setf (gethash interface-name *interfaces*) interface)
      (remhash interface-name *interfaces*)))

(define-compound-type interface (o interface-name)
  (let ((instances (interface-instances (interface interface-name))))
    (if (member o instances :test #'typep)
        t
        nil)))

(define-subtypep-lambda (interface interface) (exp1 exp2 env)
  (declare (ignore env))
  (equal exp1 exp2))

(define-subtypep-lambda (nil interface) (exp1 exp2 env)
  (declare (ignore env))
  (with-slots (instances) (interface (second exp2))
    (let ((some-unknown-p nil))
      (dolist (instance instances)
        (multiple-value-bind (subtypep knownp)
            (subtypep exp1 instance)
          (when (and knownp subtypep)
            (return-from subtypep (values t t)))
          (when (not knownp) (setq some-unknown-p t))))
      (if some-unknown-p
          (values nil nil)
          (values nil t)))))

(define-cl-type-for-extype interface (type-spec env)
  (declare (ignore env))
  (let ((interface-name (second type-spec)))
    `(satisfies ,(interface-name-p interface-name))))

(defun interface-instance-from-type (interface type)
  "Returns an upgraded type from TYPE that actually has a defined
interface instance with name INTERFACE.

For instance, while (EQL NIL) does not have an interface-instance,
LIST as obtained from the following function call does have an interface-instance.

  (INTERFACE-INSTANCE-FROM-TYPE 'COLLECTOR '(EQL NIL)) ;=> LIST

"
  (let ((instances (interface-instances (if (%interface-p interface)
                                            interface
                                            (interface interface)))))
    (loop :for instance :in instances
          :if (subtypep type instance)
            :do (return-from interface-instance-from-type instance))))

(defun interface-instance-from-object (interface object)
  "Returns an upgraded type from OBJECT that actually has a defined
interface instance with name INTERFACE.

For instance, while NIL does not have an interface-instance, but
LIST as obtained from the following function call does have an interface-instance.

  (INTERFACE-INSTANCE-FROM-OBJECT 'COLLECTOR NIL) ;=> LIST

"
  (let ((instances (interface-instances (if (%interface-p interface)
                                            interface
                                            (interface interface)))))
    (loop :for instance :in instances
          :if (typep object instance)
            :do (return-from interface-instance-from-object instance))))

(defmacro with-interface-instances (bindings &body body &environment env)
  "Each BINDING should be of the form (VAR INTERFACE-NAME)

Rebinds each VAR so that its type is derived using
  INTERFACE-INSTANCE-FROM-TYPE with TYPE obtained from the lexical environment.

This is closely related to the notion of principal types in ML-like languages."
  `(let (,@(loop :for (var interface) :in bindings
                 :collect `(,var ,var)))
     (declare ,@(loop :for (var interface) :in bindings
                      :collect
                      (let ((var-type (cl-form-types:nth-form-type var env 0 t t)))
                        (list 'type
                              (or (interface-instance-from-type interface var-type) t)
                              var))))
     ,@body))

(defun interface-name-p (interface-name)
  (intern (let ((name (symbol-name interface-name)))
            (if (find #\- name)
                (uiop:strcat name "-P")
                (uiop:strcat name "P")))))

(deftype list-of (&rest types)
  (if types
      `(cons ,(first types) (list-of ,@(rest types)))
      'null))

(defun interface-lambda-list-from-type-list (type-list)
  (let ((total-type-count ())
        (used-type-count  ()))
    (flet ((name (type)
             (etypecase type
               ((eql t) (intern "VALUE"))
               (symbol type)
               (cons (intern (format nil "~{~A~^/~}" type))))))
      (loop :for type :in type-list
            :do (let ((name (name type)))
                  (unless (assoc-value total-type-count name)
                    (setf (assoc-value total-type-count name) 0)
                    (setf (assoc-value used-type-count name)  0))
                  (incf (assoc-value total-type-count name))))
      (loop :for type :in type-list
            :collect (let ((name (name type)))
                       (incf (assoc-value used-type-count name))
                       (if (= 1 (assoc-value total-type-count name))
                           name
                           (intern (format nil (etypecase type
                                                 (cons "~{~A~^/~}/~A")
                                                 (symbol "~A/~A"))
                                           type
                                           (assoc-value used-type-count name)))))))))

(defmacro define-interface (interface-name dependencies &rest interface-functions)
  "Defines a extype INTERFACE-NAME whose instances defined using
DEFINE-INTERFACE-INSTANCE are then subtypes of INTERFACE-NAME.

INTERFACE-NAME can be a symbol or a list of the form
  (INTERFACE-NAME &KEY (CL-TYPE T))

Each of INTERFACE-FUNCTIONS should be a list of the form
  (FUNCTION-NAME TYPE-LIST &OPTIONAL RETURN-TYPE DOCUMENTATION
     DEFAULT-LAMBDA-LIST &BODY DEFAULT-BODY)
If DEFAULT-LAMBDA-LIST and DEFAULT-BODY is not provided,
then it will need to be necessarily provided using DEFINE-INTERFACE-INSTANCE

DEPENDENCIES is currently unused.
"
  (destructuring-bind (interface-name &key (cl-type t))
      (alexandria:ensure-list interface-name)
    (cl:check-type interface-name symbol)
    (multiple-value-bind (doc interface-functions)
        (if (stringp (first interface-functions))
            (values (first interface-functions) (rest interface-functions))
            (values nil interface-functions))
      (loop :for interface-function-name :in interface-functions
            :do (cl:check-type interface-name (or symbol (list-of (eql cl:setf) symbol))))
      (let* ((interface-functions
               (loop :for interface-function :in interface-functions
                     :collect (case (length interface-function)
                                (2 (nconc interface-function '(t)))
                                (t interface-function))))
             (required-functions
               (loop :for interface-function :in interface-functions
                     :if (cl:<= (length interface-function) 4)
                     :collect interface-function))
             (default-functions
               (loop :for interface-function :in interface-functions
                     :if (cl:>  (length interface-function) 4)
                     :collect interface-function))
             (interface-name-p (interface-name-p interface-name)))
        (with-gensyms (interface object)
          `(with-eval-always
             (declaim (inline ,interface-name-p))
             (defun ,interface-name-p (,object)
               (typep ,object ',interface-name))
             ,(when cl-type
                `(cl:deftype ,interface-name () '(satisfies ,interface-name-p)))
             (let ((,interface
                     (make-interface :name ',interface-name
                                     :documentation ,doc
                                     :arg-list ()
                                     :expander (lambda (&rest args)
                                                 (declare (ignore args))
                                                 '(interface ,interface-name))
                                     :required-functions ',required-functions
                                     :default-functions  ',default-functions
                                     :dependencies ',dependencies)))
               ;; FIXME: Handle the case of redefinition
               (namespace-value-and-doc-set
                ',interface-name ()
                ,(format nil "A type corresponding to an interface formed by:~%~{  ~S~^~%~}"
                         interface-functions))
               (setf (extype-structure ',interface-name) ,interface)
               (setf (interface ',interface-name) ,interface)
               ;; FIXME: Validate dependencies
               ,@(loop :for interface-function :in interface-functions
                       :nconcing
                       (destructuring-bind (fn-name type-list return-type
                                            &optional doc &body body)
                           interface-function
                         (declare (ignore body))
                         `((polymorphic-functions:define-polymorphic-function
                               ;; TODO: What about multi-argument interfaces
                               ,fn-name ,(interface-lambda-list-from-type-list type-list)
                             :overwrite t
                             :documentation
                             ,(format nil "~A~%~%Part of the interface ~S.~%This interface includes:~%~{  ~S~^~%~}" (or doc "") interface-name interface-functions))
                           (declaim (cl:ftype ,(upgraded-cl-type
                                                `(function ,type-list ,return-type))
                                              ,fn-name))
                           (declaim (exftype (function ,type-list ,return-type)
                                             ,fn-name))))))))))))

(defun interface-instance-lambda-list (interface-name instance-type lambda-list type-list)
  (loop :with state := :required
        :for variable :in lambda-list
        :for type :in type-list
        :if (member variable lambda-list-keywords)
          :collect (setq state variable)
        :else
          :collect (if (eq state '&rest)
                       variable
                       (optima:match variable
                         ((list* variable rest)
                          (list* `(,variable ,(traverse-tree
                                               type
                                               (lambda (node)
                                                 (cond ((eq interface-name node)
                                                        instance-type)
                                                       (t
                                                        node)))))
                                 rest))
                         (_
                          `(,variable ,(traverse-tree type
                                                      (lambda (node)
                                                        (cond ((eq interface-name node)
                                                               instance-type)
                                                              (t
                                                               node))))))))))

(defun interface-return-type (interface-name instance-type return-type)
  (traverse-tree return-type
                 (lambda (node)
                   (cond ((eq interface-name node)
                          instance-type)
                         (t
                          node)))))

(defmacro define-interface-instance
    (interface-name type &body interface-function-definitions)
  "Each of INTERFACE-FUNCTION-DEFINITIONS should be of the form
  (NAME LAMBDA-LIST &BODY BODY)
"
  (let* ((interface
           (interface interface-name))
         (implemented-function-names
           (mapcar #'first interface-function-definitions))
         (required-functions
           (interface-required-functions interface))
         (default-functions
           (interface-default-functions interface))
         (all-functions
           (append required-functions default-functions))
         (remaining-functions
           (set-difference all-functions interface-function-definitions
                           :key #'first :test #'equal))
         (required-function-names
           (mapcar #'first required-functions)))
    (unless (subsetp required-function-names implemented-function-names :test #'equal)
      ;; TODO: Make the error more informative
      (error 'incompatible-interface-instance
             :expected required-function-names :actual implemented-function-names))
    `(with-eval-always
       ;; FIXME: A better way to do this than just pushing
       ;;   Insert INTERFACE-NAME at the right place, because
       ;;   INTERFACE-INSTANCE-FROM-OBJECT and -FROM-TYPE exist
       (pushnew ',type (interface-instances (interface ',interface-name)))
       ,@(loop :for interface-function-definition
                 :in interface-function-definitions
               :collect (destructuring-bind (name lambda-list &body body)
                            interface-function-definition
                          (destructuring-bind (type-list return-type &rest doc-body)
                              (assoc-value all-functions name :test #'equal)
                            (declare (ignore doc-body))
                            `(polymorphic-functions:defpolymorph (,name :inline t)
                                 ,(interface-instance-lambda-list
                                   interface-name type lambda-list type-list)
                                 ,(interface-return-type
                                   interface-name type return-type)
                               ,@body))))
       ,@(loop :for default-function :in remaining-functions
               :collect (destructuring-bind (name type-list return-type
                                             &optional doc lambda-list &body body)
                            default-function
                          `(polymorphic-functions:defpolymorph (,name :inline t)
                               ,(interface-instance-lambda-list
                                 interface-name
                                 type
                                 lambda-list
                                 type-list)
                               ,(interface-return-type
                                 interface-name type return-type)
                             ,doc
                             ,@body))))))
