(in-package :extensible-compound-types.impl)

(define-compound-type base-char (o) (cl:typep o 'base-char))

(macrolet ((def (type)
             `(progn
                ;; (deftype ,type (&optional (lower-limit 'cl:*) (upper-limit 'cl:*))
                ;;   `(,',type ,lower-limit ,upper-limit))
                (define-compound-type ,type (o &optional (lower-limit 'cl:*) (upper-limit 'cl:*))
                  (and (cl:typep o ',type)
                       (cond ((eq 'cl:* lower-limit)
                              t)
                             ((eq 'cl:* upper-limit)
                              (cl:check-type lower-limit ,type)
                              (cl:< (cl:the ,type lower-limit) o))
                             (t
                              (cl:check-type lower-limit ,type)
                              (cl:check-type upper-limit ,type)
                              (cl:<= (cl:the ,type lower-limit)
                                     o
                                     (cl:the ,type upper-limit))))))
                (defmethod %subtypep ((n1 (eql ',type)) (n2 (eql ',type)) t1 t2 &optional env)
                  (declare (ignore env))
                  (let ((t1 (ensure-list t1))
                        (t2 (ensure-list t2)))
                    (destructuring-bind (&optional (low1 'cl:*) (high1 'cl:*)) (rest t1)
                      (destructuring-bind (&optional (low2 'cl:*) (high2 'cl:*)) (rest t2)
                        (cond ((and (eq low1 'cl:*) (eq low2 'cl:*))
                               (values t t))
                              ((eq low2 'cl:*) ; low1 is specified but low2 isn't
                               (values t t))
                              ((eq low1 'cl:*) ; low2 is specified but low1 isn't
                               (values nil t))
                              ((and (< low1 low2) ; both are specified
                                    (eq high1 'cl:*)
                                    (eq high2 'cl:*))
                               (values nil t))
                              ((and (>= low1 low2) ; both are specified
                                    (eq high1 'cl:*)
                                    (eq high2 'cl:*))
                               (values t t))
                              ((and (>= low1 low2) ; both are specified
                                    (eq high2 'cl:*)) ; high1 is specified but high2 is not
                               (values t t))
                              ((and (>= low1 low2) ; both are specified
                                    (eq high1 'cl:*)) ; high2 is specified but high1 is not
                               (values nil t))
                              ((and (>= low1 low2) ; everything is specified
                                    (<= high1 high2))
                               (values t t))
                              (t
                               (values nil t))))))))))
  ;; On SBCL, about 3-6 times faster than "completely reducing to CL:TYPE check as below"

  ;; TODO: Can this be simplified?
  (def integer)
  (def rational)
  (def single-float)
  (def double-float)
  #-sbcl
  (def short-float)
  #-sbcl
  (def long-float))

(defun equivalent-num-type-form (num-type low high)
  `(,num-type ,(if (eq 'cl:* low)
                   low
                   (case num-type
                     (integer (cl:ceiling low))
                     (rational (cl:rationalize low))
                     (t (cl:coerce low num-type))))
              ,(if (eq 'cl:* high)
                   high
                   (case num-type
                     (integer (cl:floor high))
                     (rational (cl:rationalize high))
                     (t (cl:coerce high num-type))))))

(deftype float (&optional (low 'cl:*) (high 'cl:*))
  #+sbcl
  `(or ,(equivalent-num-type-form 'single-float low high)
       ,(equivalent-num-type-form 'double-float low high))
  #-sbcl
  (error "FLOAT not implemented"))

(deftype short-float (&optional (low 'cl:*) (high 'cl:*))
  #+sbcl
  (equivalent-num-type-form 'single-float low high)
  #-sbcl
  (error "SHORT-FLOAT not implemented on ~S" (lisp-implementation-type)))

(deftype long-float (&optional (low 'cl:*) (high 'cl:*))
  #+sbcl
  (equivalent-num-type-form 'double-float low high)
  #-sbcl
  (error "LONG-FLOAT not implemented on ~S" (lisp-implementation-type)))

(deftype real (&optional (low 'cl:*) (high 'cl:*))
  `(or ,(equivalent-num-type-form 'integer low high)
       ,(equivalent-num-type-form 'rational low high)
       ,(equivalent-num-type-form 'float low high)))

(deftype signed-byte (&optional (s 'cl:* s-supplied-p))
  (if (not s-supplied-p)
      'integer
      (progn
        (cl:check-type s non-negative-fixnum)
        (let ((low (- (expt 2 (1- s))))
              (high (1- (expt 2 (1- s)))))
          `(integer ,low ,high)))))

(deftype unsigned-byte (&optional (s 'cl:* s-supplied-p))
  (if (not s-supplied-p)
      `(integer 0)
      (progn
        (cl:check-type s non-negative-fixnum)
        (let ((low 0)
              (high (1- (expt 2 s))))
          `(integer ,low ,high)))))

(deftype fixnum () `(integer ,most-negative-fixnum ,most-positive-fixnum))
(deftype mod (n) `(integer 0 ,(1- n)))
(deftype bit () `(integer 0 1))

(define-compound-type complex (o &optional (typespec 'cl:*))
  (and (complexp o)
       (or (eq 'cl:* typespec)
           (typep (realpart o) typespec))))
(define-compound-type-compiler-macro complex (&whole form o &optional typespec)
  (unless (constantp typespec)
    (return-from complex form))
  (let ((type (typexpand (eval typespec))))
    (once-only (o)
      `(and (complexp ,o)
            ,(if (eq 'cl:* typespec)
                 t
                 `(typep (realpart ,o) ',type))))))

(deftype number () `(or real complex))
(deftype ratio () `(and rational (not integer)))

;;; This is slower; see above for the faster version
;; (macrolet ((def (type)
;;              `(define-compound-type ,type (o &optional (lower-limit 'cl:*) (upper-limit 'cl:*))
;;                 (declare (type ,type o)
;;                          (optimize speed))
;;                 (cl:typep o (list ',type lower-limit upper-limit)))))
;;   (def integer)
;;   (def float)
;;   (def single-float)
;;   (def double-float)
;;   (def short-float)
;;   (def long-float))

(macrolet ((def (type)
             `(progn
                (define-compound-type ,type (o
                                             &optional (element-type 'cl:*)
                                             (dimension-spec 'cl:*))
                  (and (cl:typep o ',type)
                       (locally (declare (type ,type o))
                         (or (eq 'cl:* element-type)
                             (type= element-type
                                    (array-element-type o))))
                       (locally (declare (type ,type o))
                         (or (eq 'cl:* dimension-spec)
                             (if (atom dimension-spec)
                                 (= (the fixnum dimension-spec) (array-rank o))
                                 (loop :for d1 :in dimension-spec
                                       :for d2 :in (array-dimensions o)
                                       :always (or (eq 'cl:* d1)
                                                   (= (the fixnum d1)
                                                      (the fixnum d2))))))))))))
  (def array)
  (def simple-array))

(deftype vector (&optional (element-type 'cl:*) (size 'cl:*))
  `(array ,element-type (,size)))
(deftype simple-vector (&optional (size 'cl:*))
  `(simple-array t ,size))

(deftype string (&optional (size 'cl:*))
  ;; FIXME: See http://www.lispworks.com/documentation/lw70/CLHS/Body/t_string.htm#string
  ;; We need SUBTYPES of CHARACTER
  `(or (array base-char (,size))
       (array character (,size))))
(deftype simple-string (&optional (size 'cl:*))
  ;; FIXME: See http://www.lispworks.com/documentation/lw70/CLHS/Body/t_string.htm#string
  ;; We need SUBTYPES of CHARACTER
  `(or (simple-array base-char (,size))
       (simple-array character (,size))))
(deftype base-string (&optional (size 'cl:*))
  `(array base-char (,size)))
(deftype simple-base-string (&optional (size 'cl:*))
  `(simple-array base-char (,size)))

(deftype bit-vector (&optional (size 'cl:*))
  `(array bit (,size)))
(deftype simple-bit-vector (&optional (size 'cl:*))
  `(simple-array bit (,size)))

(define-compound-type cons (o &optional (car-typespec 'cl:*) (cdr-typespec 'cl:*))
  (and (consp o)
       (or (eq 'cl:* car-typespec)
           (typep (car o) car-typespec))
       (or (eq 'cl:* cdr-typespec)
           (typep (cdr o) cdr-typespec))))
(deftype list () `(or cons null))

(define-compound-type function (o &optional arg-typespec value-typespec)
  (declare (ignore arg-typespec value-typespec))
  (functionp o))

(define-compound-type keyword (o)
  (and (symbolp o)
       (eq (find-package :keyword)
           (symbol-package o))))

(define-compound-type standard-char (o) (cl:typep o 'standard-char))
(define-compound-type base-char (o) (cl:typep o 'base-char))
(define-compound-type character (o) (characterp o))
(deftype extended-char () `(and character (not base-char)))

(deftype boolean () `(member t nil))

#+sbcl
(define-compound-type sb-kernel:extended-sequence (o)
  (cl:typep o 'sb-kernel:extended-sequence))

;; (macrolet ((def (type)
;;              `(progn
;;                 (define-compound-type ,type (o &optional (size 'cl:*))
;;                   (declare (optimize speed))
;;                   (and (cl:typep o ',type)
;;                        (or (eq 'cl:* size)
;;                            (progn
;;                              (cl:check-type size non-negative-fixnum)
;;                              (= size (length o)))))))))
;;   (def string)
;;   (def simple-string))
;; (define-compound-type string (o &optional (size 'cl:*))
;;   (declare (optimize speed))
;;   (and (cl:typep o 'string)
;;        (or (eq 'cl:* size)
;;            (progn
;;              (cl:check-type size non-negative-fixnum)
;;              (= size (length o))))))

;; (defun my-type-p (o type)
;;   (cl:typep o type))

;; (defun my-string-p (o &optional (size 'cl:*))
;;   (declare (optimize speed))
;;   (and (cl:typep o 'string)
;;        (or (eq 'cl:* size)
;;            (progn
;;              (cl:check-type size non-negative-fixnum)
;;              (= size (length o))))))
