(in-package :extensible-compound-types.impl)

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
         `(cl:the ,(upgraded-cl-type value-type env) ,form))

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
                                               :format-arguments (list (ecase ,i
                                                                         (1 "1st")
                                                                         (2 "2nd")
                                                                         (3 "3rd")
                                                                         (t (format nil "~Ath" ,i)))
                                                                       ',form
                                                                       ,form-value
                                                                       ,type))))
                            ,(when no-more-values
                               `(unless (< ,original-num-values ,optional-position)
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

;;; Think what happens when one defines a subclass / substructure
;;; Nothing. We are not implementing parametric types. We are implementing compound types.
;;; It is up to the user to implement the correct subtypep relation in the case of
;;; using compound types as parametric-types.



(defparameter *exclusive-types-table* (make-hash-table))

(defmacro define-mutually-exclusive-types (&body types &environment env)
  (flet ((ensure-compound-type (type)
           (assert (and (symbolp type)
                        (or (find-class type nil env)
                            (compound-type-lambda type)))
                   ()
                   "Expected~%  ~S~%to be a symbol and a primitive compound type specifier defined using DEFINE-COMPOUND-TYPE~%but~%  (COMPOUND-TYPE-LAMBDA ~S)~%did not return non-NIL indicating the absence of an appropriate DEFINE-COMPOUND-TYPE form"
                   type type)))
    (loop :for type :in types
          :do (etypecase type
                (list (mapc #'ensure-compound-type type))
                (atom (ensure-compound-type type))))
    (let ((types (loop :for type :in types :collect (ensure-list type))))
      `(progn
         ,@(apply #'map-product
                  (lambda (&rest types)
                    `(progn
                       ,@(loop :for type :in types
                               :collect `(unionf (gethash ',type *exclusive-types-table*)
                                                 ',(remove type types)))))
                  types)))))

(defun subtypep (type1 type2 &optional environment)
  "Behaves like CL:SUBTYPEP when type1 and type2 are atomic type
specifiers, corresponding to a CLASS; but when either is a list, calls
the generic-function %SUBTYPEP to determine the SUBTYPEP relation."
  (let* ((type1 (typexpand type1 environment))
         (type2 (typexpand type2 environment))
         (atom1p  (atom type1))
         (atom2p  (atom type2))
         (class1p (and atom1p (find-class type1 nil environment)))
         (class2p (and atom2p (find-class type2 nil environment))))
    (cond ((or (eq nil type1)
               (eq t type2))
           (values t t))
          ;; We can't say anything in this case, because may be the type-intersection is NULL
          ;; ((and (eq nil type2)
          ;;       (not (eq nil type1)))
          ;;  (values nil t))
          ((equalp type1 type2)
           (values t t))
          ((and class1p (or class2p (eql nil type2)))
           (cl:subtypep type1 type2 environment))
          (t
           (let* ((type1-name (if atom1p
                                  type1
                                  (car type1)))
                  (type2-name (if atom2p
                                  type2
                                  (car type2)))
                  ;; Simplify types
                  (type1 (if (and (listp type1)
                                  (loop :for arg :in (rest type1)
                                        :always (eq 'cl:* arg)))
                             type1-name
                             type1))
                  (type2 (if (and (listp type2)
                                  (loop :for arg :in (rest type2)
                                        :always (eq 'cl:* arg)))
                             type2-name
                             type2)))
             (if (or (member type1-name (gethash type2-name *exclusive-types-table*))
                     (member type2-name (gethash type1-name *exclusive-types-table*)))
                 (values nil t)
                 (%subtypep type1-name type2-name type1 type2 environment)))))))

(defgeneric %subtypep (t1-name t2-name type1 type2 &optional env))

(defmethod %subtypep (t1-name t2-name type1 type2 &optional env)
  (declare (ignore type1))
  (cond ((and (if (atom type2) t (null (rest type2)))
              (specializing-type-name-p t1-name))
         (cl:subtypep t1-name t2-name env))
        ((and (if (atom type1) t (null (rest type1)))
              (find-class t1-name nil env)
              (specializing-type-name-p t2-name))
         (multiple-value-bind (subtypep knownp)
             (cl:subtypep t1-name t2-name env)
           (cond ((and knownp (not subtypep))
                  (values nil t))
                 ;; TYPE2 is necessarily a list
                 ((next-method-p)
                  (call-next-method))
                 (t
                  (values nil nil)))))
        ((next-method-p)
         (call-next-method))
        (t
         (values nil nil))))

(defun supertypep (type1 type2 &optional environment)
  (subtypep type2 type1 environment))

(defun type= (type1 type2 &optional environment)
  (multiple-value-bind (s1 k1) (subtypep type1 type2 environment)
    (multiple-value-bind (s2 k2) (subtypep type2 type1 environment)
      (cond ((and s1 k1 s2 k2)
             (values t t))
            ((and k1 k2)
             (values nil t))
            (t
             (values nil nil))))))

(defun upgraded-cl-type (type-specifier &optional environment)
  "If TYPE-SPECIFIER is a non-ATOM, uses %UPGRADED-CL-TYPE to upgraded to a CL type.
The default unspecialized method corresponding to (T T) returns the type as it is."
  (let* ((type-specifier (typexpand type-specifier environment))
         (atomp  (atom type-specifier))
         (classp (and atomp (find-class type-specifier nil environment))))
    (cond (classp
           type-specifier)
          ((member type-specifier '(t nil))
           type-specifier)
          (t
           (let ((type-specifier (ensure-list type-specifier)))
             (%upgraded-cl-type (car type-specifier) type-specifier environment))))))

(defgeneric %upgraded-cl-type (type-name type &optional env))

(defmethod %upgraded-cl-type (type-name type &optional env)
  (declare (ignore type-name env))
  type)

(defun intersect-type-p (type1 type2 &optional env)
  "Usually, intersection can be tested for by calling (SUBTYPEP '(AND TYPE1 TYPE2) NIL).
However, this itself requires knowing whether or not TYPE1 and TYPE2 intersect.
For instance, the types
- (OR STRING NUMBER) and (OR STRING SYMBOL)
- (ARRAY * 1) and (ARRAY SINGLE-FLOAT *)

TODO: Improve documentation for this."
  ;; If the types are mutually exclusive, then their intersection is NIL
  (let* ((type1 (typexpand type1 env))
         (type2 (typexpand type2 env))
         (name1 (if (listp type1) (first type1) type1))
         (name2 (if (listp type2) (first type2) type2)))
    (when (or (member name2 (gethash name1 *exclusive-types-table*))
              (member name1 (gethash name2 *exclusive-types-table*)))
      (return-from intersect-type-p (values nil t))))
  ;; If either is a subtype of NIL, then intersection is NIL
  (when (or (subtypep type1 nil env)
            (subtypep type2 nil env))
    (return-from intersect-type-p (values nil t)))
  ;; May be first is a SUBTYPE of the second
  (multiple-value-bind (subtypep knownp)
      (subtypep type1 type2)
    (when (and knownp subtypep)
      (return-from intersect-type-p (values t t))))
  ;; May be second is a SUBTYPE of the first
  (multiple-value-bind (subtypep knownp)
      (subtypep type2 type1)
    (when (and knownp subtypep)
      (return-from intersect-type-p (values t t))))
  ;; May be neither is a subtype, yet they intersect.
  (let* ((type1 (typexpand type1 env))
         (type2 (typexpand type2 env))
         (atom1p  (atom type1))
         (atom2p  (atom type2))
         (class1p (and atom1p (find-class type1 nil env)))
         (class2p (and atom2p (find-class type2 nil env))))
    (cond ((and class1p class2p)
           (multiple-value-bind (intersection-null-p knownp)
               (cl:subtypep `(and ,type1 ,type2) nil env)
             (values (not intersection-null-p) knownp)))
          (t
           (let* ((type1-name (if atom1p
                                  type1
                                  (car type1)))
                  (type2-name (if atom2p
                                  type2
                                  (car type2)))
                  (type1 (if (and (listp type1)
                                  (loop :for arg :in (rest type1)
                                        :always (eq 'cl:* arg)))
                             type1-name
                             type1))
                  (type2 (if (and (listp type2)
                                  (loop :for arg :in (rest type2)
                                        :always (eq 'cl:* arg)))
                             type2-name
                             type2)))
             (%intersect-type-p type1-name type2-name type1 type2 env))))))

(defgeneric %intersect-type-p (type1-name type2-name type1 type2 &optional env)
  (:documentation "INTERSECT-TYPE-P guarantees that this generic function is called only
after both TYPE1 and TYPE2 are expanded."))

(defmethod %intersect-type-p (type1-name type2-name type1 type2 &optional env)
  (declare (ignore type1-name type2-name))
  (cond ((equalp type1 type2)
         (values t t))
        ((and (atom type1)
              (atom type2)
              (find-class type1 nil env)
              (find-class type2 nil env))
         (multiple-value-bind (subtypep knownp)
             (cl:subtypep `(and ,type1 ,type2) nil env)
           (if knownp
               (values (not subtypep) knownp)
               (values nil nil))))
        (t
         (values nil nil))))

(defun intersection-null-p (env &rest type-specifiers)
  ;; FIXME: Make a distinction between NIL and NULL
  ;; Intersection is NULL if at least one type specifier is a subtype of NIL.
  ;; Intersection is also NULL if the intersection of any two type specifiers is NULL.
  ;; However, even if the intersection of all 2-combinations of type specifiers is non-NULL,
  ;; the intersection can still be NULL because each combination might intersect at a different place.

  ;; First check if some type is known to be NIL
  (let ((all-known-p t))
    (loop :for ts :in type-specifiers
          :do (multiple-value-bind (subtypep knownp)
                  (subtypep ts nil env)
                (cond ((and knownp subtypep)
                       (return-from intersection-null-p (values t t)))
                      ((not knownp)
                       (setq all-known-p nil)))))
    (when all-known-p
      ;; Since all types are known to be non-NIL, if some type is T, then
      ;; intersection is non-NIL
      (loop :for ts :in type-specifiers
            :do (multiple-value-bind (tp knownp)
                    (type= ts t env)
                  (cond ((and knownp tp)
                         (return-from intersection-null-p (values nil t)))
                        ((not knownp)
                         (setq all-known-p nil)))))))
  (loop :for (type1 . rest) :on type-specifiers
        :with all-known-p := t
        :while all-known-p
        :do (loop :for type2 :in rest
                  :while all-known-p
                  :do (multiple-value-bind (intersectp knownp)
                          (intersect-type-p type1 type2 env)
                        (cond ((and knownp (not intersectp))
                               (return-from intersection-null-p (values t t)))
                              ((not knownp)
                               (setq all-known-p nil)))))
        :finally (return-from intersection-null-p
                   (cond ((null type-specifiers)
                          (values t t))
                         ((null (cdr type-specifiers)) ; there's just one specifier
                          (values nil t))
                         ((and all-known-p
                               (null (cddr type-specifiers)))
                          (values nil t))
                         ((or all-known-p t)
                          ;; Either the intersection of three or more types is NULL
                          ;; OR we don't even know all the intersections of two types
                          (values nil nil))))))

(5am:def-test intersection-null-p ()
  (5am:is-false (intersection-null-p nil 'vector))
  (5am:is-false (intersection-null-p nil 'simple-string 'string))
  (5am:is-false (intersection-null-p nil 'string 'simple-string))
  (5am:is-true  (intersection-null-p nil 'string 'integer))
  (5am:is-false (intersection-null-p nil 'string 'integer 'simple-string))
  (5am:is-true  (intersection-null-p nil 'string 'integer 'float))
  (5am:is-false (intersection-null-p nil
                                     '(or string number)
                                     '(or string symbol))))

