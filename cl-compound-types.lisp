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
                                     (cl:the ,type upper-limit)))))))))
  ;; On SBCL, about 3-6 times faster than "completely reducing to CL:TYPE check as below"

  ;; TODO: Can this be simplified?
  (def integer)
  (def float)
  (def real)
  (def single-float)
  (def double-float)
  (def short-float)
  (def long-float))

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
(deftype mod (n) `(integer 0 ,n))

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

(deftype rational (&optional (lower-limit 'cl:*) (upper-limit 'cl:*))
  `(and (real ,lower-limit ,upper-limit)
        (not (float ,lower-limit ,upper-limit))))
(deftype ratio ()
  `(and rational (not integer)))


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

(define-compound-type function (o &optional arg-typespec value-typespec)
  (declare (ignore arg-typespec value-typespec))
  (functionp o))

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
