(in-package :extensible-compound-types-cl.impl)

(defun extract-declaration (declarations name)
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
                     :if (eq name (first decl-spec))
                       :do (push `(,name ,(second decl-spec)
                                         ,@(set-difference (cddr decl-spec) ignored-variables))
                                 required-decl-specs)
                     :else
                       :collect decl-spec)
          :if rem-decl
            :collect (cons 'declare rem-decl)
              :into remaining-decls
          :finally (return (values (if (null required-decl-specs)
                                       ()
                                       `(declare ,@required-decl-specs))
                                   (if (null remaining-decls)
                                       ()
                                       remaining-decls))))))

(5am:def-test extract-declaration ()
  (5am:is (equalp '(nil nil)
                  (multiple-value-list (extract-declaration '((declare)) 'excl:extype))))
  (5am:is (equalp '(nil ((declare (type integer x))))
                  (multiple-value-list
                   (extract-declaration '((declare (type integer x))) 'excl:extype))))
  (5am:is (equalp '((declare (excl:extype integer)) ((declare (ignore x))))
                  (multiple-value-list
                   (extract-declaration '((declare (excl:extype integer x) (ignore x))) 'excl:extype))))
  (5am:is (equalp '((declare (excl:extype integer y)) ((declare (ignore x))))
                  (multiple-value-list
                   (extract-declaration '((declare (excl:extype integer x y) (ignore x)))
                                        'excl:extype)))))

(defun prepare-extype-checks (extype-decl)
  (a:mappend (lambda (extype-decl)
               (destructuring-bind (extype &rest vars) (rest extype-decl)
                 (loop :for var :in vars
                       :collect `(ex:check-type ,var ,extype))))
             (rest extype-decl)))

(defun extype-declarations (decl)
  "Returns two values: EXTYPE-DECL and REMAINING-DECL"
  (multiple-value-bind (extype-decl remaining-decls)
      (extract-declaration decl 'ex:extype)
    (multiple-value-bind (type-decl remaining-decls-2)
        #+extensible-compound-types (extract-declaration decl 'ex:type)
      #-extensible-compound-types nil
      #+extensible-compound-types
      (setq extype-decl (cond ((and extype-decl type-decl)
                               (append extype-decl
                                       (rest type-decl)))
                              (type-decl
                               type-decl)
                              (extype-decl
                               extype-decl)
                              (nil
                               nil))
            remaining-decls (append remaining-decls
                                    remaining-decls-2))
      (values extype-decl remaining-decls))))

(defun cl-type-declarations (extype-decl &optional env)
  (if (null extype-decl)
      ()
      `(declare ,@(loop :for (extype-decl ex:extype . vars) :in (rest extype-decl)
                        :collect `(cl:type ,(ex:upgraded-cl-type ex:extype env) ,@vars)))))


(defun decl-and-type-check-body (body &optional env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extract-declaration decl 'ex:extype)
      `(,@(remove-if #'null
                     (list* (cl-type-declarations extype-decl env)
                            extype-decl
                            remaining-decls))
        ,@(prepare-extype-checks extype-decl)
        ,@rem-body))))

;;; There are three steps to this:
;;; 1. Process declarations to extract EX:EXTYPE declarations.
;;; 2. Convert EX:EXTYPE declarations to CL:TYPE declarations.
;;; 3. Add initial EX:EXTYPE checks. This is done using EX:CHECK-TYPE by PREPARE-EXTYPE-CHECKS.
(defmacro excl:let (bindings &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extype-declarations decl)
      `(clel:let ,bindings
         ,@(remove-if #'null
                      (list* (cl-type-declarations extype-decl env)
                             extype-decl
                             remaining-decls))
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:let* (bindings &body body &environment env)
  `(clel:let* ,@(rest (macroexpand-1 `(excl:let ,bindings ,@body) env))))

(defmacro excl:locally (&body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extype-declarations decl)
      `(clel:locally
           ,@(remove-if #'null
                        (list* (cl-type-declarations extype-decl env)
                               extype-decl
                               remaining-decls))
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defun process-function-definition (definition)
  (rest (macroexpand-1 `(excl:defun ,@definition))))

(defmacro excl:flet (definitions &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extype-declarations decl)
      `(clel:flet ,(mapcar #'process-function-definition definitions)
         ,@(remove-if #'null
                      (list* (cl-type-declarations extype-decl env)
                             extype-decl
                             remaining-decls))
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:labels (definitions &body body &environment env)
  `(clel:labels ,@(rest (macroexpand-1 `(excl:flet ,definitions ,@body) env))))

(defmacro excl:macrolet (definitions &body body &environment env)
  `(clel:macrolet ,@(rest (macroexpand-1 `(excl:flet ,definitions ,@body) env))))

(defmacro excl:symbol-macrolet (macrobindings &body body &environment env)
  (multiple-value-bind (rem-body decl) (a:parse-body body)
    (multiple-value-bind (extype-decl remaining-decls)
        (extype-declarations decl)
      `(clel:symbol-macrolet ,macrobindings
         ,@(remove-if #'null
                      (list* (cl-type-declarations extype-decl env)
                             extype-decl
                             remaining-decls))
         ,@(prepare-extype-checks extype-decl)
         ,@rem-body))))

(defmacro excl:function (thing)
  `(cl:function ,(if (and (listp thing)
                          (string= 'lambda (first thing)))
                     (macroexpand-1 `(excl:lambda ,@(rest thing)))
                     thing)))
