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
         form)
        (t
         (with-gensyms (form-value)
           `(let ((,form-value ,form))
              (if (typep ,form-value ',value-type) ; TODO: Handle VALUES type
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
  "Behaves like CL:SUBTYPEP when type1 and type2 are atomic type specifiers,
but when either is a list, calls the generic-function %SUBTYPEP to determine
the SUBTYPEP relation.

%SUBTYPEP is also called when CL:SUBTYPEP returns NIL as the second value."
  (let ((type1 (typexpand type1 environment))
        (type2 (typexpand type2 environment)))
    (multiple-value-bind (subtypep knownp)
        (if (and (atom type1) (atom type2))
            (cl:subtypep type1 type2 environment)
            (values nil nil))
      (if knownp
          (values subtypep t)
          (let ((type1-name (if (atom type1)
                                type1
                                (car type1)))
                (type2-name (if (atom type2)
                                type2
                                (car type2))))
            (%subtypep type1-name type2-name type1 type2 environment))))))

(defgeneric %subtypep (t1-name t2-name type1 type2 &optional env))

(defun supertypep (type1 type2 &optional environment)
  (subtypep type2 type1 environment))

(defun upgraded-cl-type (type-specifier &optional environment)
  "If TYPE-SPECIFIER is a non-ATOM, uses %UPGRADED-CL-TYPE to upgraded to a CL type.
The default unspecialized method corresponding to (T T) returns the type as it is."
  (etypecase type-specifier
    (list (%upgraded-cl-type (car type-specifier) type-specifier environment))
    (atom type-specifier)))

(defgeneric %upgraded-cl-type (type-name type &optional env))

(defmethod %upgraded-cl-type (type-name type &optional env)
  (declare (ignore type-name env))
  type)
