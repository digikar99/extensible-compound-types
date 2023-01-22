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
