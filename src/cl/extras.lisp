(in-package #:extensible-compound-types-cl.impl)

(defmacro excl:imlet (bindings &body body &environment env)
  "IMMUTABLE-LET: Like CL:LET, but automatically adds type declarations
corresponding to the BINDINGS. Expects the variable bindings to be immutable."
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
                       (let ((form-type (cl-form-types:nth-form-type
                                         form env 0 t #-ccl t #+ccl nil)))
                         (cond ((eq cl:t form-type)
                                ())
                               (t
                                `((ex:extype ,form-type ,var)
                                  (cl:type ,(ex:upgraded-cl-type form-type env) ,var)))))))))
         (vars (mapcar #'third form-type-declarations))
         (augmented-env (augment-environment nil
                                             :variable vars
                                             :declare form-type-declarations)))
    (multiple-value-bind (rem-body decl) (alexandria:parse-body body)
      (multiple-value-bind (extype-decl remaining-decls)
          (extype-declarations decl)
        `(cl:let ,bindings
           (declare ,@form-type-declarations)
           ,@(remove-if #'null
                        (list (cl-type-declarations extype-decl env)
                              extype-decl
                              remaining-decls))
           ,@(prepare-extype-checks extype-decl augmented-env)
           ,@rem-body)))))

(defmacro excl:imlet* (bindings &body body &environment env)
  "IMMUTABLE-LET*: Like CL:LET*, but automatically adds type declarations
corresponding to the BINDINGS. Expects the variable bindings to be immutable."
  (let* ((form-type-declarations ())
         (augmented-env
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
                               (let* ((form-type (cl-form-types:nth-form-type
                                                  form form-type-env 0 t #-ccl t #+ccl nil))
                                      (decl `((ex:extype ,form-type ,var)
                                              (cl:type ,(ex:upgraded-cl-type form-type
                                                                             form-type-env)
                                                       ,var))))
                                 (setf form-type-declarations
                                       (nconc decl form-type-declarations))
                                 (cond ((eq cl:t form-type)
                                        ())
                                       (t
                                        (setq augmented-env
                                              (augment-environment
                                               augmented-env
                                               :variable (list var)
                                               :declare decl))
                                        (setq form-type-env
                                              (augment-environment
                                               form-type-env
                                               :variable (list var)
                                               :declare decl)))))))
                 :finally (return augmented-env))))
    (multiple-value-bind (rem-body decl) (alexandria:parse-body body)
      (multiple-value-bind (extype-decl remaining-decls)
          (extype-declarations decl)
        `(cl:let* ,bindings
           (declare ,@(nreverse form-type-declarations))
           ,@(remove-if #'null
                        (list (cl-type-declarations extype-decl env)
                              extype-decl
                              remaining-decls))
           ;; Passing AUGMENTED-ENV to PREPARE-EXTYPE-CHECKS to check for types of FORMs
           ,@(prepare-extype-checks extype-decl augmented-env)
           ,@rem-body)))))

;; (defmacro excl:imlet+ (bindings &body body)
;;   (error "Not implemented yet."))
