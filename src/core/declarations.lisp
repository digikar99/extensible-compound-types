(in-package :extensible-compound-types.impl)

(define-declaration extype (args)
  ;; On CCL, args starts with DECL-NAME while not using CL-ENVIRONMENTS-CL
  ;; Other times, it starts with the appropriate args
  (destructuring-bind (type &rest vars) (optima:match args
                                          ((list* 'extype args)
                                           args)
                                          (_ args))
    (values :variable
            (mapcar (lambda (var)
                      (list var 'extype type))
                    vars))))

(define-declaration exftype (args)
  ;; On CCL, args starts with DECL-NAME while not using CL-ENVIRONMENTS-CL
  ;; Other times, it starts with the appropriate args
  (destructuring-bind (type &rest vars) (optima:match args
                                          ((list* 'exftype args)
                                           args)
                                          (_ args))
    (values :function
            (mapcar (lambda (var)
                      (list var 'exftype type))
                    vars))))

#+extensible-compound-types
(define-declaration type (args)
  ;; On CCL, args starts with DECL-NAME while not using CL-ENVIRONMENTS-CL
  ;; Other times, it starts with the appropriate args
  (destructuring-bind (type &rest vars) (optima:match args
                                          ((list* 'type args)
                                           args)
                                          (_ args))
    (values :variable
            (append (mapcar (lambda (var)
                              (list var 'type type))
                            vars)
                    (mapcar (lambda (var)
                              (list var 'extype type))
                            vars)))))

#+extensible-compound-types
(define-declaration ftype (args)
  ;; On CCL, args starts with DECL-NAME while not using CL-ENVIRONMENTS-CL
  ;; Other times, it starts with the appropriate args
  (destructuring-bind (type &rest vars) (optima:match args
                                          ((list* 'ftype args)
                                           args)
                                          (_ args))
    (values :function
            (append (mapcar (lambda (var)
                              (list var 'exftype type))
                            vars)
                    (mapcar (lambda (var)
                              (list var 'ftype type))
                            vars)))))
