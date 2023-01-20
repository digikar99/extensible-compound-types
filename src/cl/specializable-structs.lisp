(defpackage :extensible-compound-types-cl/specializable-structs
  (:use :extensible-compound-types-cl :polymorphic-functions)
  (:export #:define-specializable-struct
           #:undefine-specializable-struct
           #:make-struct))

(in-package :extensible-compound-types-cl/specializable-structs)
(5am:def-suite :extensible-compound-types-cl/specializable-structs)
(5am:in-suite :extensible-compound-types-cl/specializable-structs)

(define-polymorphic-function make-struct (name &rest args) :overwrite t)

(defmacro lm (&rest vars-body)
  `(lambda ,(butlast vars-body) ,@(last vars-body)))
(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun parametric-type-parameter-p (s)
  (let* ((name (symbol-name s))
         (len  (length name)))
    (and (char= #\< (elt name 0))
         (char= #\> (elt name (1- len))))))
(pushnew 'parametric-type-parameter-p *parametric-type-symbol-predicates*)

(defun skip-if-parametric-type-parameter (value-type form env)
  (declare (ignore value-type env))
  (if (and *compiler-macro-expanding-p*
           (symbolp form)
           (parametric-type-parameter-p form))
      cl:t
      cl:nil))
(pushnew 'skip-if-parametric-type-parameter *the-skip-predicates*)

(defun subtypep-specializable-struct (slot-names t1 t2 type1 type2 &optional env)
  (if (not (eq t1 t2))
      (values nil nil)
      (let* ((specializers-1 (if (listp type1) (rest type1) ()))
             (specializers-1 (append specializers-1
                                     (make-list (- (length slot-names) (length specializers-1))
                                                :initial-element cl:t)))
             (specializers-2 (if (listp type2) (rest type2) ()))
             (specializers-2 (append specializers-2
                                     (make-list (- (length slot-names) (length specializers-2))
                                                :initial-element cl:t))))
        (loop :for t1 :in specializers-1
              :for t2 :in specializers-2
              :do (multiple-value-bind (subtypep knownp)
                      (subtypep t1 t2 env)
                    (cond ((and knownp (not subtypep))
                           (return-from subtypep-specializable-struct
                             (values nil t)))
                          ((not knownp)
                           (return-from subtypep-specializable-struct
                             (values nil nil)))))
              :finally (return-from subtypep-specializable-struct
                         (values t t))))))

(defun intersect-type-p-specializable-struct (slot-names t1 t2 type1 type2 &optional env)
  (if (not (eq t1 t2))
      (values nil nil)
      (or (subtypep-specializable-struct slot-names t1 t2 type1 type2 env)
          (subtypep-specializable-struct slot-names t2 t1 type2 type1 env)
          (let* ((specializers-1 (if (listp type1) (rest type1) ()))
                 (specializers-1 (append specializers-1
                                         (make-list (- (length slot-names) (length specializers-1))
                                                    :initial-element cl:t)))
                 (specializers-2 (if (listp type2) (rest type2) ()))
                 (specializers-2 (append specializers-2
                                         (make-list (- (length slot-names) (length specializers-2))
                                                    :initial-element cl:t))))
            (loop :for t1 :in specializers-1
                  :for t2 :in specializers-2
                  :do (multiple-value-bind (intersectp knownp)
                          (intersect-type-p t1 t2 env)
                        (cond ((and knownp (not intersectp))
                               (return-from intersect-type-p-specializable-struct
                                 (values nil t)))
                              ((not knownp)
                               (return-from intersect-type-p-specializable-struct
                                 (values nil nil)))))
                  :finally (return-from intersect-type-p-specializable-struct
                             (values t t)))))))


(defmacro define-specializable-struct (name-and-options &body slot-descriptions)
  "Like CL:DEFSTRUCT, but also defines additional functions and methods to
play nice with parametric polymorphs and type-checking in POLYMORPHIC-FUNCTIONS,
to allow for specifying type specializers for structures."
  ;; FIXME: allow for NAME-AND-OPTIONS to be a list

  (let* ((name (etypecase name-and-options
                 (list (first name-and-options))
                 (symbol name-and-options)))
         (slot-names (loop :for slot :in slot-descriptions
                           :collect (etypecase slot
                                      (symbol slot)
                                      (list (first slot)))))
         (slot-type-vars (loop :for slot-name :in slot-names
                               :collect (gensym (strcat "TYPE-" (symbol-name slot-name)))))
         (slot-type-parameters (loop :for slot-name :in slot-names
                                     :collect (intern (strcat "<" (symbol-name slot-name) ">")))))

    (alexandria:with-gensyms (o env parameter accessor slot-name elt-type pos
                                t1 t2 type type1 type2 type-car type-cdr type-parameter type-specifier)

      `(eval-when (:compile-toplevel :load-toplevel :execute)

         (cl:defstruct ,name-and-options ,@slot-descriptions)

         (define-compound-type (,name :cl-type nil :specializing t)
             (,o &optional ,@(mapcar (lm slot `(,slot 'cl:t))
                                     slot-type-vars))
           ,(format nil "A specializing compound-type that allows the specification of the
types of the values stored in the slots of the structure-class ~S" name)

           (and (cl:typep ,o ',name)
                (with-slots ,slot-names ,o
                  (and ,@(mapcar (lm slot-name slot-type-var
                                     `(typep ,slot-name ,slot-type-var))
                                 slot-names slot-type-vars)))))

         (defmethod %upgraded-cl-type ((,name (eql ',name)) ,type &optional ,env)
           (declare (ignore ,type ,env))
           ',name)

         ;; TODO: SUBTYPEP, INTERSECT-TYPE-P

         (defmethod %subtypep ((,t1 (eql ',name)) (,t2 (eql ',name)) ,type1 ,type2 &optional ,env)
           (subtypep-specializable-struct ',slot-names ,t1 ,t2 ,type1 ,type2 ,env))

         (defmethod %intersect-type-p
             ((,t1 (eql ',name)) (,t2 (eql ',name)) ,type1 ,type2 &optional ,env)
           (intersect-type-p-specializable-struct ',slot-names ,t1 ,t2 ,type1 ,type2 ,env))

         (defmethod %deparameterize-type ((,name (eql ',name)) ,type-specifier &optional ,env)
           (declare (ignore ,type-specifier ,env))
           ,name)

         (defmethod parametric-type-run-time-lambda-body ((,type-car (eql ',name)) ,type-cdr ,parameter)
           (let ((,accessor (loop :for ,type-parameter :in ,type-cdr
                                  :for ,slot-name :in ',slot-names
                                  :if (eq ,parameter ,type-parameter)
                                    :do (return (intern (strcat ,(symbol-name name)
                                                                "-" (symbol-name ,slot-name)))))))
             `(cl:lambda (,',name)
                (declare (optimize speed)
                         (type ,',name ,',name))
                ;; FIXME: One needs a wrapper around TYPE-OF, since TYPE-OF may not
                ;; return what one expects; example:
                ;; (TYPE-OF 1) ;=> BIT
                (type-of (,,accessor ,',name)))))

         (defmethod parametric-type-compile-time-lambda-body
             ((,type-car (eql ',name)) ,type-cdr ,parameter)
           (let* ((,pos (position ,parameter ,type-cdr)))
             (if (and ,parameter (not ,pos))
                 (error "Unknown case")
                 `(cl:lambda (,',elt-type)
                    (or (nth ,,pos (if (listp ,',elt-type)
                                       (rest ,',elt-type)
                                       nil))
                        cl:t)))))

         (defpolymorph (make-struct :inline t) ((,name (cl:eql ,name))
                                                &key ,@(mapcar (lm slot-name slot-type-param
                                                                   `((,slot-name
                                                                      ,slot-type-param)))
                                                               slot-names
                                                               slot-type-parameters))
             (,name ,@slot-type-parameters)
           (declare (ignore ,name))
           (,(intern (strcat "MAKE-" (symbol-name name)))
            ,@(loop :for slot-name :in slot-names
                    :collect (intern (symbol-name slot-name) :keyword)
                    :collect slot-name)))

         ,@(loop :for slot-name :in slot-names
                 :for slot-type-param :in slot-type-parameters
                 :collect `(defpolymorph (,(intern (strcat "SLOT-" (string slot-name)))
                                          :inline t)
                               ((,name (,name ,@slot-type-parameters)))
                               ,slot-type-param
                             (,(intern (strcat (string name) "-" (string slot-name)))
                              ,name)))))))


(defun compiled-code-size (compiled-function &optional print-disassembly)
  #+sbcl
  (let ((disassembly (with-output-to-string (*standard-output*)
                       (disassemble compiled-function))))
    (when print-disassembly (format t disassembly))
    (cl-ppcre:do-register-groups (size) ("[^|\s]; Size\\: ([0-9]*)" disassembly)
      (return-from compiled-code-size (parse-integer size))))
  #-sbcl
  (error "This function is only implemented on SBCL"))

(5am:def-test pair ()
  (polymorphic-functions::ignoring-error-output
    (eval `(define-specializable-struct pair a b))
    (5am:is-true
     (nth-value 2 (compile nil `(lambda ()
                                  (declare (optimize speed))
                                  (let ((x (make-struct 'pair :a "h" :b "o")))
                                    (declare (type (pair fixnum fixnum) x))
                                    x)))))
    (5am:is-false
     (nth-value 2 (compile nil `(lambda ()
                                  (declare (optimize speed))
                                  (let ((x (make-struct 'pair :a 1 :b 2)))
                                    (declare (type (pair fixnum fixnum) x))
                                    x)))))
    (eval `(progn
             (define-polymorphic-function slot-a (object) :overwrite t)
             (defpolymorph slot-a ((o (pair <a> <b>))) <a>
               (pair-a o))
             (define-polymorphic-function slot-b (object) :overwrite t)
             (defpolymorph slot-b ((o (pair <a> <b>))) <b>
               (pair-b o))))

    (5am:is (= 26 (compiled-code-size (compile nil `(lambda (o)
                                                      (declare (type (pair fixnum fixnum) o)
                                                               (optimize speed))
                                                      (cl:+ (pair-a o)
                                                            (pair-b o)))))))
    (5am:is (= 59 (compiled-code-size (compile nil `(lambda (o)
                                                      (declare (type (pair fixnum fixnum) o)
                                                               (optimize speed))
                                                      (cl:+ (slot-a o)
                                                            (slot-b o)))))))
    (5am:is (= 63 (compiled-code-size (compile nil `(lambda (o)
                                                      (declare (type (pair single-float single-float) o)
                                                               (optimize speed))
                                                      (cl:+ (slot-a o)
                                                            (slot-b o)))))))

    ;; Extreme
    (eval `(progn
             (define-polymorphic-function add-pair (a b) :overwrite t)
             (defpolymorph add-pair ((a (pair <a> <b>)) (b (pair <a> <b>))) (pair <a> <b>)
               (make-pair :a
                          (cl:+ (slot-a a)
                                (slot-a b))
                          :b
                          (cl:+ (slot-b a)
                                (slot-b b))))))
    (5am:is (= 217 (compiled-code-size
                    (compile nil `(lambda (x y)
                                    (declare (optimize speed)
                                             (type (pair single-float double-float) x y))
                                    (add-pair x y))))))))
