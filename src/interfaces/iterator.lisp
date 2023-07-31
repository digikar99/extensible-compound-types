(in-package #:extensible-compound-types-interfaces.impl)

;;; Idea credits: commander-trashdin / Andrew

(with-eval-always
  (defstruct interfaces:iterator
    "A base class for defining lazy iterators. Define using DEFINE-ITERATOR.
Use using ITERATOR-NEXT."))
(define-polymorphic-function interfaces:iterator-next (interfaces:iterator) :overwrite t
  :documentation "This takes in an iterator and returns two values: the second value should
be NIL only when the iterator has reached the end of iteration.

Polymorphs for this should be defined using DEFINE-ITERATOR rather than directly.")

(defmacro interfaces:define-iterator (name (slots specializers) read-only-slots &body body)
  "Defines a lazy iterator NAME that can be used by repeatedly calling ITERATOR-NEXT

Effectively, this defines
- a structure
- an orthogonally-specializing-type for the structure:
    see EXTENSIBLE-COMPOUND-TYPES:DEFINE-ORTHOGONALLY-SPECIALIZING-TYPE
    for more details about this
- a constructor wrapper around the base structure using the ortho-type
- an ITERATOR-NEXT polymorph and its compiler macro with body BODY specializing on NAME

The BODY will have access to the SLOTS through the WITH-SLOTS macro.
BODY should return two values
- the first value should be the value the iterator was expected to return,
while the second value should be a non-NIL if the first value is part of the
iterator. In particular, if the second value is NIL, the iterator would be
assumed to have ended.

Examples

```lisp

  (define-iterator list-iterator ((list) ()) ()
    (if list
        (values (let ((first (car list)))
                  (setf list (cdr list))
                  first)
                t)
        (values nil nil)))

  (define-iterator map-iterator
      ((function iterator) (function iterator))
      (function iterator)
    (multiple-value-bind (elt validp) (iterator-next iterator)
      (values (when validp (funcall function elt)) validp)))

  (define-iterator filter-iterator
      ((filter-function iterator) (filter-function iterator))
      (filter-function iterator)
    (loop do
      (multiple-value-bind (elt validp) (iterator-next iterator)
        (cond ((and validp
                    (funcall filter-function elt))
               (return (values elt t)))
              ((not validp)
               (return (values nil nil)))))))
```

See src/interfaces/iterator.lisp for more examples.
"
  (let ((make-function-name (intern (uiop:strcat "MAKE-" (symbol-name name))))
        (env (gensym "ENV")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (inline ,make-function-name))
       (cl:defstruct (,name (:include interfaces:iterator)
                            (:constructor ,make-function-name ,slots))
         ,@slots)
       (defmethod print-object ((object ,name) stream)
         (print-unreadable-object (object stream :type t :identity t)))
       (define-orthogonally-specializing-type ,name
           (&optional ,@(loop :for s :in specializers :collect `(,s 'cl:*)))
         (,@(loop :for s :in specializers :collect
                  `(,s :accessor (cl:lambda (o)
                                   (orthogonally-specializing-type-of
                                    (,(intern (uiop:strcat (string name) "-"
                                                           (string s)))
                                     o))))))
         :to-cl-type (cl:lambda (type) (declare (ignore type)) ',name))
       (defun ,name ,slots
         (,make-function-name ,@slots))
       (define-compiler-macro ,name (,@slots &environment ,env)
         (declare (ignorable ,env))
         `(the (,',name ,,@(loop :for s :in specializers
                                 :collect `(cl-form-types:nth-form-type ,s ,env 0 t t)))
               (,',make-function-name ,,@slots)))
       ,@(with-gensyms (s st iterator iterator-type env type-decl specializer-types)
           `((defpolymorph (interfaces:iterator-next :inline t) ((,iterator ,name)) t
               (let (,@(loop :for s :in read-only-slots
                             :collect `(,s (,(intern (uiop:strcat (string name) "-"
                                                                  (string s)))
                                            ,iterator))))
                 (with-slots ,(set-difference slots read-only-slots) ,iterator
                   ,@body)))
             (defpolymorph-compiler-macro interfaces:iterator-next (,name)
                 (,iterator &environment ,env)
               (let* ((,iterator-type (cl-form-types:nth-form-type ,iterator ,env 0 t t))
                      (,type-decl
                        (optima:match (typexpand ,iterator-type)
                          ((list* 'specializing ',name ,specializer-types)
                           (loop :for ,s :in ',specializers
                                 :for ,st :in ,specializer-types
                                 :nconcing `((extype ,,st ,,s)
                                             (cl:type ,(upgraded-cl-type ,st) ,,s)))))))
                 `(let (,@(loop :for ,s :in ',read-only-slots
                                :collect `(,,s (,(intern (uiop:strcat (string ',name) "-"
                                                                      (string ,s)))
                                                ,,iterator))))
                    (declare ,@,type-decl)
                    (with-slots ,',(set-difference slots read-only-slots) ,,iterator
                      ,@',body)))))))))

(interfaces:define-iterator interfaces:list-iterator ((list) ()) ()
  (if list
      (values (let ((first (car list)))
                (setf list (cdr list))
                first)
              t)
      (values nil nil)))

(interfaces:define-iterator interfaces:vector-iterator ((vector index) (vector)) (vector)
  (locally (declare (type fixnum index))
    (if (cl:< index (length vector))
        (values (let ((elt (aref vector index)))
                  (incf index)
                  elt)
                t)
        (values nil nil))))

(interfaces:define-iterator interfaces:map-iterator
    ((function iterator) (function iterator))
    (function iterator)
  (multiple-value-bind (elt validp) (interfaces:iterator-next iterator)
    (values (when validp (funcall function elt)) validp)))

(interfaces:define-iterator interfaces:filter-iterator
    ((filter-function iterator) (filter-function iterator))
    (filter-function iterator)
  (loop do
    (multiple-value-bind (elt validp) (interfaces:iterator-next iterator)
      (cond ((and validp
                  (funcall filter-function elt))
             (return (values elt t)))
            ((not validp)
             (return (values nil nil)))))))

(defpolymorph interfaces:do-iterator (iterator) (values)
  (loop do
    (multiple-value-bind (elt validp) (interfaces:iterator-next iterator)
      (declare (ignore elt))
      (unless validp (return (values))))))

#|

(defun iterate/iterator (fn1 fn2 l)
  (declare (optimize speed)
           (type list l))
  (imlet* ((it1 (interfaces:list-iterator l))
           (it2 (interfaces:filter-iterator fn1 it1))
           (it3 (interfaces:map-iterator fn2 it2)))
    (declare (dynamic-extent it1 it2 it3))
    (let (a)
      (loop do
        (multiple-value-bind (elt validp) (interfaces:iterator-next it3)
          (setq a elt)
          (when (null validp) (return))))
      a)))

(defun iterate/native (fn1 fn2 l)
  (declare (optimize speed)
           (type list l))
  (let (a)
    (loop :for elt :in l
          :do (when (funcall fn1 elt)
                (setq a (funcall fn2 elt))))
    a))

|#

(defpolymorph interfaces:reduce-iterator (function iterator initial-value)
    (values t &optional)
  (loop with final-value = initial-value
        do (multiple-value-bind (elt validp) (interfaces:iterator-next iterator)
             (if (not validp)
                 (return final-value)
                 (setq final-value (funcall function final-value elt))))))

#|

(defun reduce/iterator (fn l iv)
  (declare (optimize speed)
           (type (simple-array single-float 1) l))
  (imlet ((a (interfaces:vector-iterator l 0)))
    (declare (dynamic-extent a))
    (interfaces:reduce-iterator fn a iv)))

(defun reduce/native (fn l iv)
  (declare (optimize speed)
           (type (simple-array single-float 1) l))
  (reduce fn l :initial-value iv))

|#
