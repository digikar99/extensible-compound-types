(in-package :extensible-compound-types.impl)

(define-type null () `(eql nil))
;; FIXME: Remove references to NULL

(macrolet ((def (type)
             `(progn
                ;; It is slower to CONS up lists and then pass them to CL:TYPEP
                ;; (define-type ,type (&optional (lower-limit 'cl:*) (upper-limit 'cl:*))
                ;;   `(,',type ,lower-limit ,upper-limit))
                (define-specializing-type ,type
                    (o &optional (lower-limit 'cl:*) (upper-limit 'cl:*))

                  (and (cl:typep o ',type)
                       (cond ((eq 'cl:* lower-limit)
                              t)
                             ((eq 'cl:* upper-limit)
                              (cl:check-type lower-limit ,type)
                              (cl:<= (cl:the ,type lower-limit) o))
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
                                  ((or (<= low1 low2 high1 high2) ; everything is specified
                                       (<= low2 low1 high2 high1)
                                       (<= low2 low1 high1 high2)
                                       (<= low1 low2 high2 high1))
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

(define-type float (&optional (low 'cl:*) (high 'cl:*))
  #+(or sbcl ccl ecl)
  `(or ,(equivalent-num-type-form 'single-float low high)
       ,(equivalent-num-type-form 'double-float low high)
       #+ecl ,(equivalent-num-type-form 'long-float low high))
  #-(or sbcl ccl ecl)
  (error "FLOAT not implemented"))

(define-type short-float (&optional (low 'cl:*) (high 'cl:*))
  #+(or sbcl ccl ecl)
  (equivalent-num-type-form 'single-float low high)
  #-(or sbcl ccl ecl)
  (error "SHORT-FLOAT not implemented on ~S" (lisp-implementation-type)))

#-ecl
(define-type long-float (&optional (low 'cl:*) (high 'cl:*))
  #+(or sbcl ccl)
  (equivalent-num-type-form 'double-float low high)
  #-(or sbcl ccl)
  (error "LONG-FLOAT not implemented on ~S" (lisp-implementation-type)))

(define-type real (&optional (low 'cl:*) (high 'cl:*))
  `(or ,(equivalent-num-type-form 'integer low high)
       ,(equivalent-num-type-form 'rational low high)
       ,(equivalent-num-type-form 'float low high)))

(define-type signed-byte (&optional (s 'cl:*))
  (if (eq s 'cl:*)
      'integer
      (progn
        (cl:check-type s non-negative-fixnum)
        (let ((low (- (expt 2 (1- s))))
              (high (1- (expt 2 (1- s)))))
          `(integer ,low ,high)))))

(define-type unsigned-byte (&optional (s 'cl:*))
  (if (eq s 'cl:*)
      `(integer 0)
      (progn
        (cl:check-type s non-negative-fixnum)
        (let ((low 0)
              (high (1- (expt 2 s))))
          `(integer ,low ,high)))))

(define-type fixnum () `(integer ,most-negative-fixnum ,most-positive-fixnum))
(define-type bignum () `(and integer (not fixnum)))
(define-type mod (n) `(integer 0 ,(1- n)))
(define-type bit () `(integer 0 1))

(define-type ratio () `(and rational (not integer)))


(define-orthogonally-specializing-type complex (&optional (typespec 'cl:*))
  ((typespec :accessor (lambda (c) (class-name (class-of (realpart c)))))))

(define-type number () `(or real complex))

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

(define-type array (&optional (element-type 'cl:*) (dim/rank 'cl:*))
  (if (consp dim/rank)
      `(%array ,element-type ,(length dim/rank) ,dim/rank cl:*)
      `(%array ,element-type ,dim/rank cl:* cl:*)))
(define-type simple-array (&optional (element-type 'cl:*) (dim/rank 'cl:*))
  (etypecase dim/rank
    (cons
     `(%array ,element-type ,(length dim/rank) ,dim/rank t))
    (integer
     `(%array ,element-type ,dim/rank ,(make-list dim/rank :initial-element 'cl:*) t))
    ((eql cl:*)
     `(%array ,element-type ,dim/rank cl:* t))))

(define-type vector (&optional (element-type 'cl:*) (size 'cl:*))
  `(array ,element-type (,size)))
(define-type simple-vector (&optional (size 'cl:*))
  `(simple-array t (,size)))

(define-type string (&optional (size 'cl:*))
  ;; FIXME: See http://www.lispworks.com/documentation/lw70/CLHS/Body/t_string.htm#string
  ;; We need SUBTYPES of CHARACTER
  `(or (array base-char (,size))
       (array character (,size))))

(define-type simple-string (&optional (size 'cl:*))
  ;; FIXME: See http://www.lispworks.com/documentation/lw70/CLHS/Body/t_string.htm#string
  ;; We need SUBTYPES of CHARACTER
  `(or (simple-array base-char (,size))
       (simple-array character (,size))))
(define-type base-string (&optional (size 'cl:*))
  `(array base-char (,size)))
(define-type simple-base-string (&optional (size 'cl:*))
  `(simple-array base-char (,size)))

(define-type bit-vector (&optional (size 'cl:*))
  `(array bit (,size)))
(define-type simple-bit-vector (&optional (size 'cl:*))
  `(simple-array bit (,size)))

(define-specializing-type cons (o &optional (car-typespec 'cl:*) (cdr-typespec 'cl:*))
  (and (or (eql car-typespec 'cl:*)
           (typep (car o) car-typespec))
       (or (eql cdr-typespec 'cl:*)
           (typep (cdr o) cdr-typespec)))
  :subtypep-lambda
  (lambda (t1 t2)
    (destructuring-bind (&optional (car1 'cl:*) (cdr1 'cl:*)) (rest (ensure-list t1))
      (destructuring-bind (&optional (car2 'cl:*) (cdr2 'cl:*)) (rest (ensure-list t2))
        (cond ((eql car2 'cl:*)
               (values t t))
              ((eql car1 'cl:*)
               (values nil t))
              ((eql cdr2 'cl:*)
               (multiple-value-bind (subtypep knownp)
                   (subtypep car1 car2)
                 (if knownp
                     (values subtypep t)
                     (values nil nil))))
              ((eql cdr1 'cl:*)
               (values nil t))
              (t
               (multiple-value-bind (car-subtypep car-knownp)
                   (subtypep car1 car2)
                 (multiple-value-bind (cdr-subtypep cdr-knownp)
                     (subtypep cdr1 cdr2)
                   (if (and car-knownp cdr-knownp)
                       (values (and car-subtypep cdr-subtypep) t)
                       (values nil nil)))))))))
  :intersect-type-p-lambda
  (lambda (t1 t2)
    (destructuring-bind (&optional (car1 'cl:*) (cdr1 'cl:*)) (rest (ensure-list t1))
      (destructuring-bind (&optional (car2 'cl:*) (cdr2 'cl:*)) (rest (ensure-list t2))
        (cond ((or (eql car1 'cl:*) (eql car2 'cl:*)
                   (eql cdr1 'cl:*) (eql cdr2 'cl:*))
               (values t t))
              (t
               (multiple-value-bind (car-intersectp car-knownp)
                   (intersect-type-p car1 car2)
                 (multiple-value-bind (cdr-intersectp cdr-knownp)
                     (intersect-type-p cdr1 cdr2)
                   (if (and car-knownp cdr-knownp)
                       (values (and car-intersectp cdr-intersectp) t)
                       (values nil nil)))))))))
  :to-cl-type
  (lambda (typespec)
    (destructuring-bind (&optional (car 'cl:*) (cdr 'cl:*))
        (rest (ensure-list typespec))
      `(cons ,(if (eql 'cl:* car)
                  'cl:*
                  (upgraded-cl-type car))
             ,(if (eql 'cl:* cdr)
                  'cl:*
                  (upgraded-cl-type cdr))))))

(define-type list () `(or cons null))

(define-compound-type (function :cl-type nil)
    (o &optional (arg-typespec 'cl:*) (value-typespec 'cl:*))
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
(define-cl-type-for-extype function (type-spec env)
  (declare (ignore env))
  (let ((state nil))
    (flet ((may-be-upgraded-cl-type (typespec)
             (cond ((member typespec lambda-list-keywords)
                    (setq state typespec))
                   ((eq state '&key)
                    `(,(first typespec) ,(upgraded-cl-type (second typespec))))
                   (t
                    (upgraded-cl-type typespec)))))
      (destructuring-bind (&optional (arg-typespecs 'cl:*) (value-typespec 'cl:*))
          (rest (ensure-list type-spec))
        (if (eq arg-typespecs 'cl:*)
            `(cl:function cl:* cl:*)
            `(cl:function (,@(mapcar #'may-be-upgraded-cl-type arg-typespecs))
                          ,(optima:match value-typespec
                             ((list 'cl:values value-typespecs)
                              (setq state nil)
                              `(cl:values ,@(mapcar #'may-be-upgraded-cl-type
                                                    value-typespecs)))
                             ((quote cl:*)
                              'cl:*)
                             (_
                              (may-be-upgraded-cl-type value-typespec)))))))))

(define-orthogonally-specializing-type (%symbol :class symbol)
    (&optional (package-name 'cl:*) (length 'cl:*))
  ((package-name :accessor (lambda (s) (package-name (symbol-package s))))
   (length :accessor (lambda (s) (length (symbol-name s))))))

(define-type symbol () `%symbol)
(define-type keyword () '(%symbol "KEYWORD"))

(define-orthogonally-specializing-type (%char :class character)
    (&optional (base-char-p 'cl:*))
  ((base-char-p :accessor (lambda (o) (cl:typep o 'base-char)))))

(define-type standard-char ()
  `(member #\Newline #\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\0
           #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\? #\@ #\A #\B #\C
           #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V
           #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i
           #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\|
           #\} #\~))
;; BASE-CHAR is the UPGRADED-ARRAY-ELEMENT-TYPE of STANDARD-CHAR
;; All the three compound-types for characters have implemented %SUBTYPEP
;; methods in subtypep.lisp

(define-type base-char () `(%char t))
(define-type extended-char () `(%char nil))
(define-type character () `%char)

(define-type boolean () `(member t nil))

;; FIXME: While incorporating sequences that are not lists or vectors,
;; one needs to think about *which* sequence protocol to support.
;; Eg: the one in generic-cl, the one in lisp-polymorph, the one in
;; trivial-extensible-sequences, the one in extensible-sequences,
;; or something else?!
(define-type sequence () `(or vector list))

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
