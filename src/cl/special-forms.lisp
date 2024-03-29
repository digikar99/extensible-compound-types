(in-package :extensible-compound-types-cl.impl)

(defun macroexpand-until (predicate form &optional env)
  "Calls MACROEXPAND-1 on FORM until it is a list which
starts with the symbol specified by CAR"
  (loop :until (funcall predicate form)
        :for expansion := (macroexpand-1 form env)
        :do (setq form expansion)
            (sleep 1)
        :finally (return form)))

(defun macroexpand-until-car (car form &optional env)
  "Calls MACROEXPAND-1 on FORM until it is a list which
starts with the symbol specified by CAR"
  (cl:check-type car symbol)
  (loop :until (and (listp form)
                    (eq car (car form)))
        :for expansion := (macroexpand-1 form env)
        :do (setq form expansion)
        :finally (return form)))

(defun type-decl-from-bindings (bindings env &key parallel)
  (if parallel
      (loop :for binding :in bindings
            :nconcing
            (multiple-value-bind (var form)
                (if (symbolp binding)
                    (values binding nil)
                    (values-list binding))
              (unless
                  #+sbcl (and (symbol-package var)
                              (sb-ext:package-locked-p (symbol-package var)))
                  #-sbcl ()
                  (let ((form-type (cl-form-types:nth-form-type form env 0 t #-ccl t
                                                                             #+ccl nil)))
                    (cond ((eq cl:t form-type)
                           ())
                          (t
                           `((ex:extype ,form-type ,var)
                             (cl:type ,(ex:upgraded-cl-type form-type env) ,var))))))))
      (loop :with augmented-env := nil
            :with form-type-env := env
            :for binding :in bindings
            :nconcing
            (multiple-value-bind (var form)
                (if (symbolp binding)
                    (values binding nil)
                    (values-list binding))
              (unless #+sbcl (and (symbol-package var)
                                  (sb-ext:package-locked-p (symbol-package var)))
                      #-sbcl ()
                      (let ((form-type (cl-form-types:nth-form-type
                                        form form-type-env 0 t #-ccl t #+ccl nil)))
                        (cond ((eq cl:t form-type)
                               ())
                              (t
                               (let ((decl
                                       `((ex:extype ,form-type ,var)
                                         (cl:type ,(ex:upgraded-cl-type
                                                    form-type form-type-env)
                                                  ,var))))
                                 (setq augmented-env
                                       (augment-environment
                                        augmented-env
                                        :variable (list var)
                                        :declare decl))
                                 (setq form-type-env
                                       (augment-environment
                                        form-type-env
                                        :variable (list var)
                                        :declare decl))
                                 decl)))))))))

(defun special-variable-p (var &optional env)
  (eq :special (nth-value 0 (variable-information var env))))

(defun extract-declarations (declarations &rest names)
  "Returns two values:
- extracted declaration
- remaining declarations"
  (let ((ignored-variables (loop :for decl :in declarations
                                 :appending
                                 (loop :for decl-spec :in (rest decl)
                                       :if (eq 'cl:ignore (first decl-spec))
                                         :appending (rest decl-spec)))))
    (loop :for decl :in declarations
          :with required-decl-specs := ()
          :for rem-decl
            := (loop :for decl-spec :in (rest decl)
                     :if (member (first decl-spec) names)
                       :do (push `(,(first decl-spec)
                                   ,(second decl-spec)
                                   ,@(set-difference (cddr decl-spec) ignored-variables))
                                 required-decl-specs)
                     :else
                       :collect decl-spec)
          :if rem-decl
            :nconcing rem-decl
              :into remaining-decls
          :finally (return (values (if (null required-decl-specs)
                                       ()
                                       `(declare ,@required-decl-specs))
                                   (if (null remaining-decls)
                                       ()
                                       `(declare ,@remaining-decls)))))))

(5am:def-test extract-declarations ()
  (5am:is (equalp '(nil nil)
                  (multiple-value-list (extract-declarations '((declare)) 'excl:extype))))
  (5am:is (equalp '(nil ((declare (type integer x))))
                  (multiple-value-list
                   (extract-declarations '((declare (type integer x))) 'excl:extype))))
  (5am:is (equalp '((declare (excl:extype integer)) ((declare (ignore x))))
                  (multiple-value-list
                   (extract-declarations '((declare (excl:extype integer x) (ignore x))) 'excl:extype))))
  (5am:is (equalp '((declare (excl:extype integer y)) ((declare (ignore x))))
                  (multiple-value-list
                   (extract-declarations '((declare (excl:extype integer x y) (ignore x)))
                                         'excl:extype)))))

(defvar excl:*disable-extype-checks* nil
  "If non-NIL then macros in EXTENSIBLE-COMPOUND-TYPES-CL emit only
the declarations but no CHECK-TYPE statements.")

(defun prepare-extype-checks (extype-decl &optional env)
  "ENV should be passed only if the declarations pertain to already bound variables,
rather than variables with new bindings."
  (a:mappend
   (lambda (extype-decl)
     (destructuring-bind (extype &rest vars) (rest extype-decl)
       (when extype                   ; skip over NIL type specifiers by mistake
         (loop :for var :in vars
               :do (when env
                     (a:when-let (old-type (variable-type var env))
                       (multiple-value-bind (intersectp knownp)
                           (ex:intersect-type-p extype old-type env)
                         (cond ((and knownp (not intersectp))
                                (error 'simple-type-error
                                       :format-control
                                       "~%~S was derived to be of type~%  ~S~%but is being declared to be of type~%  ~S~%but the two types do not intersect"
                                       :format-arguments (list var old-type extype)))
                               ((and knownp intersectp)
                                (setq extype `(and ,extype ,old-type)))))))
               :collect (if excl:*disable-extype-checks*
                            nil
                            `(ex:check-type ,var ,extype))))))
   (rest extype-decl)))

(defun extype-declarations (decl &optional env)
  "Returns two values: EXTYPE-DECL and REMAINING-DECL"
  (declare (ignore env))
  (extract-declarations decl 'ex:extype #+extensible-compound-types 'ex:type))

(defun cl-type-declarations (extype-decl &optional env)
  (if (null extype-decl)
      ()
      `(declare ,@(loop :for (extype-decl ex:extype . vars) :in (rest extype-decl)
                        :collect (let ((vars
                                         (if (and (member :sbcl cl:*features*)
                                                  (member "polymorphic-functions"
                                                          (asdf:already-loaded-systems)
                                                          :test #'string=)
                                                  (symbol-value (find-symbol
                                                                 "*COMPILER-MACRO-EXPANDING-P*"
                                                                 :polymorphic-functions)))
                                             ;; Try to avoid checks for type parameters,
                                             ;; since we must have already checked for them.
                                             (loop :for var :in vars
                                                   :if (not
                                                        (funcall (find-symbol
                                                                  "PARAMETRIC-TYPE-SYMBOL-P"
                                                                  :polymorphic-functions)
                                                                 var))
                                                     :collect var)
                                             vars)))
                                   `(cl:type ,(ex:upgraded-cl-type ex:extype env) ,@vars))))))


(defun decl-and-type-check-body (body &optional env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declarations decl 'ex:extype)
      `(,@(remove-if #'null
                     (list (cl-type-declarations extype-decl env)
                           extype-decl
                           remaining-decls))
        ,@(prepare-extype-checks extype-decl)
        ,@rem-body))))

;;; There are three steps to this:
;;; 1. Process declarations to extract EX:EXTYPE declarations.
;;; 2. Convert EX:EXTYPE declarations to CL:TYPE declarations.
;;; 3. Add initial EX:EXTYPE checks. This is done using EX:CHECK-TYPE by PREPARE-EXTYPE-CHECKS.
(defmacro excl:let (bindings &body body &environment env)
  (let* ((form-type-declarations
           (loop :for binding :in bindings
                 :nconcing
                 (multiple-value-bind (var form)
                     (if (symbolp binding)
                         (values binding nil)
                         (values-list binding))
                   (unless
                       #+sbcl (and (symbol-package var)
                                   (sb-ext:package-locked-p (symbol-package var)))
                       #-sbcl ()
                     (let ((form-type (cl-form-types:nth-form-type form env 0 t #-ccl t
                                                                                #+ccl nil)))
                       (cond ((eq cl:t form-type)
                              ())
                             (t
                              `((ex:extype ,form-type ,var)
                                (cl:type ,(ex:upgraded-cl-type form-type env) ,var)))))))))
         (vars (mapcar #'third form-type-declarations))
         (augmented-env (augment-environment nil
                                             :variable vars
                                             :declare form-type-declarations)))
    (multiple-value-bind (rem-body decl) (a:parse-body body)
      (multiple-value-bind (extype-decl remaining-decls)
          (extype-declarations decl)
        `(clel:let ,bindings
           ,@(remove-if #'null
                        (list (cl-type-declarations extype-decl env)
                              extype-decl
                              remaining-decls))
           ;; Passing AUGMENTED-ENV to PREPARE-EXTYPE-CHECKS to check for types of FORMs
           ,@(prepare-extype-checks extype-decl augmented-env)
           ,@rem-body)))))

(defmacro excl:let* (bindings &body body &environment env)
  (let* ((augmented-env
           (loop :with augmented-env := nil
                 :with form-type-env := env
                 :for binding :in bindings
                 :do (multiple-value-bind (var form)
                         (if (symbolp binding)
                             (values binding nil)
                             (values-list binding))
                       (unless #+sbcl (and (symbol-package var)
                                           (sb-ext:package-locked-p (symbol-package var)))
                               #-sbcl ()
                         (let ((form-type (cl-form-types:nth-form-type
                                           form form-type-env 0 t #-ccl t #+ccl nil)))
                           (cond ((eq cl:t form-type)
                                  ())
                                 (t
                                  (setq augmented-env
                                        (augment-environment
                                         augmented-env
                                         :variable (list var)
                                         :declare
                                         `((ex:extype ,form-type ,var)
                                           (cl:type ,(ex:upgraded-cl-type form-type form-type-env)
                                                    ,var))))
                                  (setq form-type-env
                                        (augment-environment
                                         form-type-env
                                         :variable (list var)
                                         :declare
                                         `((ex:extype ,form-type ,var)
                                           (cl:type ,(ex:upgraded-cl-type form-type form-type-env)
                                                    ,var)))))))))
                 :finally (return augmented-env))))
    (multiple-value-bind (rem-body decl) (a:parse-body body)
      (multiple-value-bind (extype-decl remaining-decls)
          (extype-declarations decl)
        `(clel:let* ,bindings
           ,@(remove-if #'null
                        (list (cl-type-declarations extype-decl env)
                              extype-decl
                              remaining-decls))
           ;; Passing AUGMENTED-ENV to PREPARE-EXTYPE-CHECKS to check for types of FORMs
           ,@(prepare-extype-checks extype-decl augmented-env)
           ,@rem-body)))))

(defmacro excl:locally (&body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extype-declarations decl)
      `(clel:locally
           ,@(remove-if #'null
                        (list (cl-type-declarations extype-decl env)
                              extype-decl
                              remaining-decls))
         ;; Passing ENV to PREPARE-EXTYPE-CHECKS since these are old bindings
         ,@(prepare-extype-checks extype-decl env)
         ,@rem-body))))

(defun process-function-definition (definition)
  (rest (macroexpand-1 `(excl:defun ,@definition))))

(defmacro excl:flet (definitions &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extype-declarations decl)
      `(clel:flet ,(mapcar #'process-function-definition definitions)
         ,@(remove-if #'null
                      (list (cl-type-declarations extype-decl env)
                            extype-decl
                            remaining-decls))
         ;; Not passing ENV to PREPARE-EXTYPE-CHECKS since these are essentially new bindings
         ;; FIXME: Should check for which variables are parameters and which are not
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:labels (definitions &body body &environment env)
  `(clel:labels ,@(rest (macroexpand-1 `(excl:flet ,definitions ,@body) env))))

(defmacro excl:macrolet (definitions &body body &environment env)
  `(clel:macrolet ,@(rest (macroexpand-1 `(excl:flet ,definitions ,@body) env))))

(defmacro excl:symbol-macrolet (macrobindings &body body &environment env)
  (declare (ignore env))
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extype-declarations decl)
      `(clel:symbol-macrolet ,macrobindings
         ,@(remove-if #'null
                      (list (cl-type-declarations extype-decl)
                            extype-decl
                            remaining-decls))
         ;; Not passing ENV to PREPARE-EXTYPE-CHECKS since these are essentially new bindings
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:function (thing)
  `(cl:function ,(if (and (listp thing)
                          (string= 'lambda (first thing)))
                     (macroexpand-1 `(excl:lambda ,@(rest thing)))
                     thing)))
