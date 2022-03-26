(in-package :extensible-compound-types.impl)

(defvar *the-skip-predicates* nil
  "A list of function-designators. THE avoids checking the VALUE-TYPE
of FORM if at least one of the predicates returns a non-NIL
value. This is useful for optimization purposes.

Each predicate should take three arguments: VALUE-TYPE FORM ENV")

(defun speed-more-safety-less-p (value-type form env)
  (declare (ignore value-type form))
  (let ((optimize-decl (declaration-information 'optimize env)))
    (> (second (assoc 'speed optimize-decl))
       (second (assoc 'safety optimize-decl)))))

(pushnew 'speed-more-safety-less-p *the-skip-predicates*)

(defmacro the (value-type form &environment env)
  "At runtime, signals a TYPE-ERROR unless (TYPEP FORM VALUE-TYPE) holds.

Necessary: A check has to be present at the \"top-level\".  The
runtime check can be avoided for optimization purposes if at least one
predicate in *THE-SKIP-PREDICATES* returns non-NIL."
  (cond ((eq t value-type)
         form)
        ((loop :for predicate :in *the-skip-predicates*
                 :thereis (funcall predicate value-type form env))
         `(cl:the ,(upgraded-cl-type value-type env) ,form))
        (t
         (with-gensyms (form-value)
           `(let ((,form-value ,form))
              (declare (cl:type ,(upgraded-cl-type value-type env) ,form-value)
                       (extype ,value-type ,form-value))
              (if (typep ,form-value ',value-type) ; FIXME: Handle VALUES type
                  ,form-value
                  (error 'type-error :datum ,form-value
                                     :expected-type ',value-type
                                     :context ',form)))))))

(defmacro check-type (place type &optional type-string)
  (cl:check-type place symbol)
  `(cl:assert (typep ,place ',type) () ,type-string))

;;; Think what happens when one defines a subclass / substructure
;;; Nothing. We are not implementing parametric types. We are implementing compound types.
;;; It is up to the user to implement the correct subtypep relation in the case of
;;; using compound types as parametric-types.

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
           (let ((type1-name (if atom1p
                                 type1
                                 (car type1)))
                 (type2-name (if atom2p
                                 type2
                                 (car type2))))
             (%subtypep type1-name type2-name type1 type2 environment))))))

(defgeneric %subtypep (t1-name t2-name type1 type2 &optional env))

(defmethod %subtypep (t1-name t2-name type1 type2 &optional env)
  (declare (ignore t1-name t2-name type1 type2 env))
  (values nil nil))

(defun supertypep (type1 type2 &optional environment)
  (subtypep type2 type1 environment))

(defun type= (type1 type2 &optional environment)
  (and (subtypep type1 type2 environment)
       (subtypep type2 type1 environment)))

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
           (%upgraded-cl-type (car type-specifier) type-specifier environment)))))

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
           (let ((type1-name (if atom1p
                                 type1
                                 (car type1)))
                 (type2-name (if atom2p
                                 type2
                                 (car type2))))
             (%intersect-type-p type1-name type2-name type1 type2 env))))))

(defgeneric %intersect-type-p (type1-name type2-name type1 type2 &optional env))

(defmethod %intersect-type-p (type1-name type2-name type1 type2 &optional env)
  (declare (ignore type1-name type2-name env))
  (if (equalp type1 type2)
      (values t t)
      (values nil nil)))

(defun intersection-null-p (env &rest type-specifiers)
  (loop :for (type1 . rest) :on type-specifiers
        :with all-known-p := t
        :while all-known-p
        :do (loop :for type2 :in rest
                  :while all-known-p
                  :do (multiple-value-bind (intersectp knownp)
                          (intersect-type-p type1 type2 env)
                        (cond ((and knownp intersectp)
                               (return-from intersection-null-p (values nil t)))
                              ((not knownp)
                               (setq all-known-p nil)))))
        :finally (if all-known-p
                     (return-from intersection-null-p (values t t))
                     (return-from intersection-null-p (values nil nil)))))

(5am:def-test intersection-null-p ()
  (5am:is-false (intersection-null-p nil 'simple-string 'string))
  (5am:is-false (intersection-null-p nil 'string 'simple-string))
  (5am:is-true  (intersection-null-p nil 'string 'integer))
  (5am:is-false (intersection-null-p nil 'string 'integer 'simple-string))
  (5am:is-true  (intersection-null-p nil 'string 'integer 'float))
  (5am:is-false (intersection-null-p nil
                                     '(or string number)
                                     '(or string symbol))))

