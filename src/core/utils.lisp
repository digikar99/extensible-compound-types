(in-package :extensible-compound-types.impl)

(defun find-class (name &optional errorp environment)
  #-sbcl
  (if errorp
      (cl:find-class name t environment)
      (ignore-errors (cl:find-class name nil environment)))
  #+sbcl
  (cl:find-class name errorp environment))

(defmacro named-lambda (name lambda-list &body body)
  `(flet ((,name ,lambda-list ,@body))
     (function ,name)))

(defmacro list-named-lambda (name lambda-list &body body &environment env)
  (declare (cl:type list name)
           (ignorable env))
  #+sbcl
  `(sb-int:named-lambda ,name ,lambda-list
     ,@body)
  #+ccl
  `(ccl:nfunction ,name (cl:lambda ,lambda-list ,@body))
  #-(or sbcl ccl)
  (let ((function-name (etypecase name
                         (cons (car name))
                         (atom name))))
    `(flet ((,function-name ,lambda-list ,@body))
       #',function-name)))

(defmacro with-eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defun class-instance-p (class-name object)
  (declare (type symbol class-name))
  (eq (class-name (find-class object))
      class-name))

(defun class-instance-p-lambda-expression (class-name)
  (declare (type symbol class-name))
  (with-gensyms (object)
    `(lambda (,object)
       (cl:typep ,object ',class-name))))

(defun subclassp (class-or-name-1 class-or-name-2)
  "Returns non-NIL if first is a SUBCLASS of the second"
  (let ((class1 (etypecase class-or-name-1
                  (symbol (find-class class-or-name-1))
                  (class  class-or-name-1)))
        (class2 (etypecase class-or-name-2
                  (symbol (find-class class-or-name-2))
                  (class  class-or-name-2))))
    (closer-mop:ensure-finalized class1)
    (closer-mop:ensure-finalized class2)
    (nth-value 0
               (ends-with-subseq (closer-mop:class-precedence-list class2)
                                 (closer-mop:class-precedence-list class1)))))

(defun env-speed (environment)
  (second (assoc 'speed (declaration-information 'optimize environment))))

(defun env-debug (environment)
  (second (assoc 'debug (declaration-information 'optimize environment))))

(defun env-safety (environment)
  (second (assoc 'safety (declaration-information 'optimize environment))))

(defun ignore-all-form-from-lambda-list (lambda-list)
  (let ((vars (loop :for state := 'required
                    :for form :in lambda-list
                    :if (member form (cons 'required lambda-list-keywords))
                      :do (setq state form)
                    :else
                      :appending (etypecase form
                                   (symbol (list form))
                                   (list (ecase (length form)
                                           (1 (list (first form)))
                                           (2 (list (first form)))
                                           (3 (list (first form)
                                                    (third form)))))))))
    `(declare (ignore ,@vars))))

(defun type-name (type-specifier)
  (etypecase type-specifier
    (atom type-specifier)
    (cons (car type-specifier))))

(defun symbol-if-possible (cons-or-symbol)
  (if (and (listp cons-or-symbol)
           (null (cdr cons-or-symbol)))
      (first cons-or-symbol)
      cons-or-symbol))
