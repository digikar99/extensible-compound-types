(in-package :extensible-compound-types.impl)

(5am:in-suite :extensible-compound-types)

(define-type trivial-types:string-designator () `(or character string symbol))
(define-type alexandria:string-designator () `(or character string symbol))

(defun character-designator-p (object)
  (and (typep object 'alexandria:string-designator)
       (= 1 (length (string object)))))
(define-type character-designator ()
  `(or character
       (string 1)
       (and symbol (satisfies character-designator-p))))


(defun function-designator-p (object)
  (or (symbolp object)
      (functionp object)
      (and (listp object)
           (eq 'setf (first object))
           (second object)
           (null (cddr object)))))
(define-type trivial-types:function-designator ()
  `(or symbol
       function
       (and list (satisfies function-designator-p))))

;; (define-compound-type character-designator (o)
;;   (or (characterp o)
;;       (and (stringp o)
;;            (= 1 (length o)))
;;       (and (symbolp o)
;;            (= 1 (length (symbol-name o))))))

;; (macrolet ((def (n2)
;;              `(progn
;;                 (defmethod %subtypep ((n1 (eql 'character-designator)) (n2 (eql ',n2))
;;                                       t1 t2 &optional env)
;;                   (declare (ignore n1 n2 t1 t2 env))
;;                   (values nil t))
;;                 (defmethod %intersect-type-p ((n1 (eql 'character-designator)) (n2 (eql ',n2))
;;                                               t1 t2 &optional env)
;;                   (declare (ignore n1 n2 t1 t2 env))
;;                   (values t t)))))
;;   (def character)
;;   (def symbol)
;;   (def string)
;;   (def sequence))

;; (defmethod %subtypep ((n1 (eql 'character)) (n2 (eql 'character-designator)) t1 t2
;;               &optional env)
;;     (declare (ignore n1 n2 t1 t2 env))
;;     (values t t))

;; (defmethod %subtypep ((n1 (eql 'character-designator)) (n2 (eql 'or))
;;                       t1 t2 &optional env)
;;   (declare (ignore n1 n2 t1))
;;   (multiple-value-bind (subtypep knownp) (subtypep '(or character symbol string) t2 env)
;;     (values subtypep knownp)))

;; (defmethod %subtypep ((n1 (eql 'or)) (n2 (eql 'character-designator))
;;                       t1 t2 &optional env)
;;   (declare (ignore n1 n2 t2))
;;   (multiple-value-bind (subtypep knownp) (subtypep t1 '(or character (string 1)) env)
;;     (values subtypep knownp)))

(5am:def-test character-designator ()
  (5am:is (equal '(nil t) (multiple-value-list (subtypep 'character-designator 'character))))
  (5am:is (equal '(nil t) (multiple-value-list (subtypep 'character-designator 'symbol))))
  (5am:is (equal '(nil t) (multiple-value-list (subtypep 'character-designator 'string))))
  (5am:is (equal '(nil t) (multiple-value-list (subtypep 'character-designator 'sequence))))
  (5am:is (equal '(t t)   (multiple-value-list (subtypep 'character 'character-designator))))

  (5am:is (equal '(nil t) (multiple-value-list (subtypep 'character-designator
                                                         '(or character symbol)))))
  (5am:is (equal '(t t)   (multiple-value-list (subtypep 'character-designator
                                                         '(or character symbol string)))))
  (5am:is (equal '(nil t) (multiple-value-list (subtypep '(or character string)
                                                         'character-designator))))
  (5am:is (equal '(t t)   (multiple-value-list (subtypep '(or character (string 1))
                                                         'character-designator)))))
