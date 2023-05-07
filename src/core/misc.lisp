(in-package :extensible-compound-types.impl)

(defun type-specifier-p (object &optional environment)
  (declare (ignore environment))
  (if (and (cl:typep object 'type-specifier)
           (extype-structure object nil))
      t
      nil))

(defvar *the-skip-predicates* nil
  "A list of function-designators. THE avoids checking the VALUE-TYPE
of FORM if at least one of the predicates returns a non-NIL
value. This is useful for optimization purposes.

Each predicate should take three arguments: VALUE-TYPE FORM ENV")

(defun speed-more-safety-less-p (value-type form env)
  (declare (ignore value-type form))
  (let ((optimize-decl (declaration-information 'optimize env)))
    (if (> (second (assoc 'speed optimize-decl))
           (second (assoc 'safety optimize-decl)))
        'cl:the
        nil)))

(pushnew 'speed-more-safety-less-p *the-skip-predicates*)

(defmacro the (value-type form &environment env)

  "At runtime, signals a TYPE-ERROR unless (TYPEP FORM VALUE-TYPE) holds.

Necessary: A check has to be present at the \"top-level\".  The
runtime check can be avoided for optimization purposes if at least one
predicate in *THE-SKIP-PREDICATES* returns non-NIL. Specifically, if at least
one predicate returns CL:T, then no check is performed, otherwise if all
predicates return CL:NIL but some return CL:THE, then the form is surrounded
in CL:THE with UPGRADED-CL-TYPE, if all return CL:NIL, then full check is done."

  (cond ((eq t value-type)
         form)

        ;; In some cases we want to avoid checks altogether
        ((loop :for predicate :in *the-skip-predicates*
                 :thereis (eq 'cl:t (funcall predicate value-type form env)))
         ;; SBCL treats type declarations as assertions, but also plays nicer
         ;; with user defined declarations; so we should omit the CL:TYPE and CL:FTYPe
         ;; declarations on SBCL
         (optima:match value-type
           ((list* 'cl:values _)
            (with-gensyms (fn)
              `(flet ((,fn () ,form))
                 (declare (exftype (function () ,value-type) ,fn)
                          #-sbcl (cl:ftype (function () ,(upgraded-cl-type value-type)) ,fn)
                          (inline ,fn))
                 (,fn))))
           (_
            (with-gensyms (var)
              `(let ((,var ,form))
                 (declare (extype ,value-type ,var)
                          #-sbcl (cl:type ,(upgraded-cl-type value-type) ,var))
                 ,var)))))

        ((loop :for predicate :in *the-skip-predicates*
                 :thereis (eq 'cl:the (funcall predicate value-type form env)))
         (optima:match value-type
           ((list* 'cl:values _)
            (with-gensyms (fn)
              `(flet ((,fn () ,form))
                 (declare (exftype (function () ,value-type) ,fn)
                          (cl:ftype (function () ,(upgraded-cl-type value-type)) ,fn)
                          (inline ,fn))
                 (,fn))))
           (_
            (with-gensyms (var)
              `(let ((,var ,form))
                 (declare (extype ,value-type ,var)
                          (cl:type ,(upgraded-cl-type value-type) ,var))
                 ,var)))))

        (t
         (optima:match value-type
           ((list* 'cl:values value-types)
            ;; TODO: Optimize this. Even a multiple-value-call with lambda would involve
            ;; making a list and then calling values-list on it in the general case
            (let* ((optional-position (position '&optional value-types))
                   (rest-position     (position '&rest value-types))
                   (no-more-values    (and optional-position
                                           (= (1+ optional-position)
                                              (length value-types)))))
              (setq value-types (remove '&optional value-types))
              (setq value-types (remove '&allow-other-keys value-types))
              (with-gensyms (i fn type form-value
                               original-num-values original-form-value-list
                               form-value-list)
                `(flet ((,fn ()
                          (let* ((,original-form-value-list (multiple-value-list ,form))
                                 (,original-num-values (length ,original-form-value-list))
                                 (,form-value-list (nconc ,original-form-value-list
                                                          (make-list ,(or optional-position
                                                                          rest-position
                                                                          (length value-types))
                                                                     :initial-element nil))))
                            (loop :for ,form-value :in ,form-value-list
                                  :for ,i :from 0
                                  :for ,type :in ',value-types
                                  :do (when (eq ,type '&rest)
                                        (setq ,type ',(lastcar value-types)))
                                      (unless (typep ,form-value ,type)
                                        (error 'simple-type-error
                                               :format-control
                                               "~A value of form (0-indexed) ~%  ~S~%is~%  ~S~%not of expected type~%  ~S"
                                               :format-arguments
                                               (list (ecase ,i
                                                       (1 "1st")
                                                       (2 "2nd")
                                                       (3 "3rd")
                                                       (t (format nil "~Ath" ,i)))
                                                     ',form
                                                     ,form-value
                                                     ,type))))
                            ,(when no-more-values
                               `(unless (<= ,original-num-values ,optional-position)
                                  (error 'simple-type-error
                                         :format-control
                                         "Expected at most ~D value(s) but~%  ~S~%returned ~D values: ~S"
                                         :format-arguments (list ,optional-position
                                                                 ',form
                                                                 ,original-num-values
                                                                 (subseq ,original-form-value-list
                                                                         0 ,original-num-values)))))
                            (values-list (subseq ,form-value-list 0 ,original-num-values)))))
                   (declare (exftype (function () ,value-type) ,fn)
                            (cl:ftype (function () ,(upgraded-cl-type value-type)) ,fn)
                            (inline ,fn))
                   (,fn)))))
           (_
            (with-gensyms (form-value)
              `(let ((,form-value ,form))
                 (declare (cl:type ,(upgraded-cl-type value-type env) ,form-value)
                          (extype ,value-type ,form-value))
                 (if (typep ,form-value ',value-type)
                     ,form-value
                     (error 'simple-type-error
                            :format-control
                            "Value of form~%  ~S~%is~%  ~S~%not of expected type~%  ~S"
                            :format-arguments (list ',form
                                                    ,form-value
                                                    ',value-type))))))))))

(defmacro check-type (place type &optional type-string)
  (declare (ignore type-string))
  (cl:check-type place symbol)
  `(the ,type ,place))
