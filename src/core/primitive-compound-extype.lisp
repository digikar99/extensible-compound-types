(in-package :extensible-compound-types.impl)

(5am:in-suite :extensible-compound-types)

(defun specializing-type-name-p (name)
  (declare (type symbol name))
  (extype-specializing-p (extype-structure name)))

(defmacro define-compound-type
    (name-spec (object-var &rest lambda-list) &body body)
  "EXTENSIBLE-COMPOUND-TYPES:TYPEP relies on these whenever TYPE supplied is
  non-atomic and non-class.

NAME-SPEC can be either NAME or the list

    (NAME &KEY (CL-TYPE T) (SPECIALIZING T))

  DEFINE-COMPOUND-TYPE also automatically creates a CL:DEFTYPE using
    UPGRADED-CL-TYPE. However, if the NAME is a symbol in one of the packages
    *EXCLUDED-PACKAGES-FOR-CL-DEFTYPE*, then a CL:DEFTYPE is not created. This
    can be overriden by supplying the value of CL-TYPE.
  If SPECIALIZING is non-NIL, then it is assumed that the NAME names a structure/class,
    and that any list form of the type specifier is a SUBTYPE of the atom form.
    This holds for types such as ARRAY, SIMPLE-ARRAY, INTEGER,
    but is violated for types such as OR, NOT, MEMBER.
    If SPECIALIZING is NIL, then it is also implicitly assumed that only the list
    form of the type specifier is valid and its atomic form is invalid.

Check-list for implementing a new compound type:
  - Implement a DEFINE-COMPOUND-TYPE
  - If CL-TYPE option was supplied as NIL, then optionally define an appropriate
    CL:DEFTYPE form.
  - Compulsory: Implement a %UPGRADED-CL-TYPE method specializing on (EQL NAME)
    that returns a valid CL type specifier, with no ultimate dependency
    on EXTENSIBLE-COMPOUND-TYPES
  - Optional: Implement a DEFINE-COMPOUND-TYPE-COMPILER-MACRO for optimizing TYPEP
  - Optional: Appropriate %SUBTYPEP methods and %INTERSECT-TYPE-P methods
  - Optional: DEFINE-MUTUALLY-EXCLUSIVE-TYPES

The CL type specifiers are emitted for optimization purposes by
EXTENSIBLE-COMPOUND-TYPES-CL and friends. Because the native CL compiler
understands these specifiers, no additional work needs to be done if these
type specifiers are generated appropriately.

Note: Whenever possible, it is recommended to use EXTENSIBLE-COMPOUND-TYPES:DEFTYPE
  and only use EXTENSIBLE-COMPOUND-TYPES:DEFINE-COMPOUND-TYPE as a last resort.
  Use of DEFINE-COMPOUND-TYPE also entails getting the %SUBTYPEP and %INTERSECT-TYPE-P
  methods correct.
"
  (destructuring-bind (name &key (specializing t) (cl-type t cl-type-p))
      (ensure-list name-spec)
    (assert (not (member name '(extype type cl:type))) ()
          "Illegal to define a type named ~S" name)
    (multiple-value-bind (rem-body decl doc) (parse-body body :documentation t)
      `(with-eval-always
         ,(when (if cl-type-p
                    cl-type
                    (not (member (symbol-package name) *excluded-packages-for-cl-deftype*)))
            (with-gensyms (form)
              `(cl:deftype ,name (&whole ,form ,@lambda-list)
                 ,@(when doc `(,doc))
                 ,(ignore-all-form-from-lambda-list lambda-list)
                 (upgraded-cl-type ,form))))
         (setf (extype-structure ',name)
               (make-primitive-compound-extype
                :name ',name
                :documentation ,doc
                :arg-list ',lambda-list
                :lambda (lambda (,object-var ,@lambda-list)
                          ,@decl ,@rem-body)
                :lambda-expression
                '(lambda (,object-var ,@lambda-list) ,@decl ,@rem-body)
                :compiler-macro nil
                :specializing-p ,specializing))
         (namespace-value-and-doc-set
          ',name
          ',lambda-list
          ,(if doc
               (format nil
                       "~A names a PRIMITIVE-COMPOUND-EXTYPE~%~A"
                       name
                       doc)
               (format nil
                       "~A names a PRIMITIVE-COMPOUND-EXTYPE~%"
                       name)))
         ',name))))

(defun undefine-compound-type (name)
  (setf (extype-structure name) nil)
  (namespace-value-and-doc-rem name)
  name)

(defmacro define-compound-type-compiler-macro
    (name (object-name &rest lambda-list)
     &body body
     &environment env)
  (declare (cl:type symbol name object-name))
  (with-gensyms (extype-structure)
    `(with-eval-always
       (let ((,extype-structure (extype-structure ',name)))
         (setf (extype-compiler-macro ,extype-structure)
               ,(parse-macro name (cons object-name lambda-list) body env))))))

(defun undefine-compound-type-compiler-macro (name)
  (let ((extype-structure (extype-structure name)))
    (setf (extype-compiler-macro extype-structure) nil)))
