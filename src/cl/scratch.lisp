(in-package :polymorphic-functions)

(defun parametric-type-run-time-lambda-body (type-car type-cdr type-parameter)
  (with-gensyms (object)
    (let ((type-pattern (traverse-tree `(,type-car ,@type-cdr)
                                       (lambda (node)
                                         (cond ((eq node type-parameter)
                                                type-parameter)
                                               ((and (symbolp node)
                                                     (parametric-type-symbol-p node))
                                                '_)
                                               (t
                                                node))))))
      `(cl:lambda (,object)
         (type-of
          (trivia:let-match ((,type-pattern ,object))
            ,type-parameter))))))

(defun parametric-type-compile-time-lambda-body (type-car type-cdr type-parameter)
  (with-gensyms (object-type)
    (let ((type-pattern (traverse-tree `(,type-car ,@type-cdr)
                                       (lambda (node)
                                         (cond ((eq node type-parameter)
                                                (values type-parameter t))
                                               ((and (symbolp node)
                                                     (parametric-type-symbol-p node))
                                                (values '_ t))
                                               ((and (symbolp node)
                                                     (member node
                                                             '(cl:list cl:quote _)))
                                                (values node t))
                                               ((symbolp node)
                                                (values `',node
                                                        t))
                                               ((member (first node)
                                                        '(cl:list cl:quote))
                                                node)
                                               (t
                                                (cons 'list
                                                      node)))))))
      `(cl:lambda (,object-type)
         (trivia:let-match ((,type-pattern ,object-type))
           (print ,type-parameter))))))

(in-package :extensible-compound-types-cl/specializable-structs)

(defstruct
    (%pair (:conc-name pair-)
           (:constructor make-pair)
           (:copier copy-pair)
           (:predicate pair-p))
  a b)

(define-compound-type pair
    (o &optional (a-type 'cl:*) (b-type 'cl:*))
  (and (pair-p o)
       (or (eq a-type 'cl:*) (typep (pair-a o) a-type))
       (or (eq b-type 'cl:*) (typep (pair-b o) b-type))))

(defmethod %upgraded-cl-type ((name (eql 'pair)) type &optional env)
  (declare (ignore name type env))
  '%pair)

(defmethod %subtypep ((n1 (eql 'pair)) (n2 (eql 'pair)) t1 t2 &optional env)
  (subtypep-specializable-struct '(a b) n1 n2 t1 t2 env))

(defmethod %intersect-type-p ((n1 (eql 'pair)) (n2 (eql 'pair)) t1 t2 &optional env)
  (intersect-type-p-specializable-struct '(a b) n1 n2 t1 t2 env))

(trivia:defpattern pair (&optional (a-pattern-or-type '_) (b-pattern-or-type '_))
  (alexandria:with-gensyms (pair)
    `(trivia:guard1 (,pair :type %pair)
                    (pair-p ,pair)
                    (pair-a ,pair)
                    ,a-pattern-or-type
                    (pair-b ,pair)
                    ,b-pattern-or-type)))

(trivia:match
    (make-pair :a (make-pair :a 1.0d0 :b 2.0d0)
               :b 2.0f0)
  ((pair (pair <aa>) <b>)
   (list <aa> <b>)))

(define-polymorphic-function foo (a b) :overwrite t)
(defpolymorph (foo :inline t)
    ((a (pair <a> <b>))
     (b (pair <a> <b>)))
    t
  (declare (ignorable <a> <b>))
  (list a b))

(foo (make-pair :a 1.0f0 :b 1.0f0)
     (make-pair :a 2.0f0 :b 2.0f0))
;=> works

(foo (make-pair :a 1.0d0 :b 1.0f0)
     (make-pair :a 2.0f0 :b 2.0f0))
;=> no-applicable-polymorph

(disassemble
 (lambda (a b)
   (declare (optimize speed)
            (type (pair single-float single-float) a b))
   (foo a b)))
;=> inlined

(lambda (a b)
  (declare (optimize speed)
           (type (pair single-float single-float) a)
           (type (pair single-float double-float) b))
  (foo a b))
;=> no-applicable-polymorph

(define-polymorphic-function bar (a b) :overwrite t)
(defpolymorph (bar :inline t)
    ((a (pair (pair <a> <c>) <b>))
     (b (pair (pair <a> <c>) <b>)))
    t
  (declare (ignorable <a> <c> <b>))
  (list a b))

(bar (make-pair :a (make-pair :a 1.0f0 :b 1.0f0) :b 1.0f0)
     (make-pair :a (make-pair :a 1.0f0 :b 1.0f0) :b 2.0f0))
;=> works

(bar (make-pair :a (make-pair :a 1.0f0 :b 1.0f0) :b 1.0f0)
     (make-pair :a 3.0d0 :b 2.0f0))
;=> no-applicable-polymorph

(disassemble
 (lambda (a b)
   (declare (optimize speed)
            (type (pair (pair single-float single-float)
                        single-float)
                  a b))
   (bar a b)))
;=> inlined
