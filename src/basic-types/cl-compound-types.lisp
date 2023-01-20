(in-package :extensible-compound-types.impl)

(deftype null () `(eql nil))
;; FIXME: Remove references to NULL

(macrolet ((def (type)
             `(progn
                ;; It is slower to CONS up lists and then pass them to CL:TYPEP
                ;; (deftype ,type (&optional (lower-limit 'cl:*) (upper-limit 'cl:*))
                ;;   `(,',type ,lower-limit ,upper-limit))
                (define-specializing-type ,type
                    (o &optional (lower-limit 'cl:*) (upper-limit 'cl:*))

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
                                     (cl:the ,type upper-limit)))))

                  :subtypep-lambda
                  (list-named-lambda (subtypep ,type ,type) (t1 t2)
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
                                 (values nil t)))))))

                  :intersect-type-p-lambda
                  (list-named-lambda (intersect-type-p ,type ,type) (t1 t2)
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
                                   (values nil t)))))))))))
  ;; On SBCL, about 3-6 times faster than "completely reducing to CL:TYPE check as below"

  ;; TODO: Can this be simplified?
  (def integer)
  (def rational) ; starting with ratio does not work, since ratio type does not have low or high
  (def single-float)
  (def double-float)
  #-(or sbcl ccl ecl)
  (def short-float)
  #+ecl
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
  #+(or sbcl ccl ecl)
  `(or ,(equivalent-num-type-form 'single-float low high)
       ,(equivalent-num-type-form 'double-float low high)
       #+ecl ,(equivalent-num-type-form 'long-float low high))
  #-(or sbcl ccl ecl)
  (error "FLOAT not implemented"))

(deftype short-float (&optional (low 'cl:*) (high 'cl:*))
  #+(or sbcl ccl ecl)
  (equivalent-num-type-form 'single-float low high)
  #-(or sbcl ccl ecl)
  (error "SHORT-FLOAT not implemented on ~S" (lisp-implementation-type)))

#-ecl
(deftype long-float (&optional (low 'cl:*) (high 'cl:*))
  #+(or sbcl ccl)
  (equivalent-num-type-form 'double-float low high)
  #-(or sbcl ccl)
  (error "LONG-FLOAT not implemented on ~S" (lisp-implementation-type)))

(deftype real (&optional (low 'cl:*) (high 'cl:*))
  `(or ,(equivalent-num-type-form 'integer low high)
       ,(equivalent-num-type-form 'rational low high)
       ,(equivalent-num-type-form 'float low high)))

(deftype signed-byte (&optional (s 'cl:*))
  (if (eq s 'cl:*)
      'integer
      (progn
        (cl:check-type s non-negative-fixnum)
        (let ((low (- (expt 2 (1- s))))
              (high (1- (expt 2 (1- s)))))
          `(integer ,low ,high)))))

(deftype unsigned-byte (&optional (s 'cl:*))
  (if (eq s 'cl:*)
      `(integer 0)
      (progn
        (cl:check-type s non-negative-fixnum)
        (let ((low 0)
              (high (1- (expt 2 s))))
          `(integer ,low ,high)))))

(deftype fixnum () `(integer ,most-negative-fixnum ,most-positive-fixnum))
(deftype bignum () `(and integer (not fixnum)))
(deftype mod (n) `(integer 0 ,(1- n)))
(deftype bit () `(integer 0 1))

(deftype ratio () `(and rational (not integer)))


(define-orthogonally-specializing-type complex (&optional (typespec 'cl:*))
  ((typespec :accessor (lambda (c) (class-name (class-of (realpart c)))))))

(deftype number () `(or real complex))

(define-orthogonally-specializing-type (%array :class array)
    (&optional (element-type 'cl:*) (rank 'cl:*) (dimensions 'cl:*) (simple-p 'cl:*))
  ;; This isn't technically orthogonal. It is orthogonal only because
  ;; the exposed API through ARRAY and SIMPLE-ARRAY makes it so

  ((element-type :accessor array-element-type)
   (rank :accessor array-rank)
   (dimensions :accessor array-dimensions :nested t)
   (simple-p :accessor (lambda (o) (cl:typep o 'cl:simple-array))))

  :to-cl-type (lambda (type-spec)
                (destructuring-bind
                    (&optional (element-type 'cl:*) (rank 'cl:*)
                       (dimensions 'cl:*) (simple-p 'cl:*))
                    (rest (ensure-list type-spec))
                  `(,(if (eq simple-p t)
                         'simple-array
                         'array)
                    ,element-type
                    ,(if (consp dimensions)
                         dimensions
                         rank)))))

(deftype array (&optional (element-type 'cl:*) (dim/rank 'cl:*))
  (if (consp dim/rank)
      `(%array ,element-type ,(length dim/rank) ,dim/rank cl:*)
      `(%array ,element-type ,dim/rank cl:* cl:*)))
(deftype simple-array (&optional (element-type 'cl:*) (dim/rank 'cl:*))
  (if (consp dim/rank)
      `(%array ,element-type ,(length dim/rank) ,dim/rank t)
      `(%array ,element-type ,dim/rank cl:* t)))

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

(define-orthogonally-specializing-type cons
    (&optional (car-typespec 'cl:*) (cdr-typespec 'cl:*))
  ((car-typespec :accessor (lambda (c) (class-name (class-of (car c)))))
   (cdr-typespec :accessor (lambda (c) (class-name (class-of (cdr c)))))))

(deftype list () `(or cons null))

(define-compound-type function (o &optional (arg-typespec 'cl:*) (value-typespec 'cl:*))
  (let ((name (swank/backend:function-name #'+)))
    (cond ((and (eq 'cl:* arg-typespec)
                (eq 'cl:* value-typespec))
           (functionp o))
          ((symbolp name)
           (let ((ftype (assoc 'cl:ftype
                               (nth-value 2 (function-information name)))))
             (if (null ftype)
                 (values nil nil)
                 (subtypep ftype (list 'function arg-typespec value-typespec)))))
          (t
           (values nil nil)))))

(define-orthogonally-specializing-type (%symbol :class symbol)
    (&optional (package-name 'cl:*) (length 'cl:*))
  ((package-name :accessor (lambda (s) (package-name (symbol-package s))))
   (length :accessor (lambda (s) (length (symbol-name s))))))

(deftype symbol () `%symbol)
(deftype keyword () '(%symbol "KEYWORD"))

(define-orthogonally-specializing-type (%char :class character)
    (&optional (base-char-p 'cl:*))
  ((base-char-p :accessor (lambda (o) (cl:typep o 'base-char)))))

(define-compound-type standard-char (o) (cl:typep o 'standard-char))
;; BASE-CHAR is the UPGRADED-ARRAY-ELEMENT-TYPE of STANDARD-CHAR
;; All the three compound-types for characters have implemented %SUBTYPEP
;; methods in subtypep.lisp

(deftype base-char () `(%char t))
(deftype extended-char () `(%char nil))
(deftype character () `%char)

(deftype boolean () `(member t nil))

;; FIXME: While incorporating sequences that are not lists or vectors,
;; one needs to think about *which* sequence protocol to support.
;; Eg: the one in generic-cl, the one in lisp-polymorph, the one in
;; trivial-extensible-sequences, the one in extensible-sequences,
;; or something else?!
(deftype sequence () `(or vector list))


#+sbcl
(define-orthogonally-specializing-type sb-c::lvar () ())

;; (defun my-type-p (o type)
;;   (cl:typep o type))

;; (defun my-string-p (o &optional (size 'cl:*))
;;   (declare (optimize speed))
;;   (and (cl:typep o 'string)
;;        (or (eq 'cl:* size)
;;            (progn
;;              (cl:check-type size non-negative-fixnum)
;;              (= size (length o))))))
