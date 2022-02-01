#lang racket
(define make-k
  (lambda (v) v)
  )
(define apply-k (lambda (k v) (k v)))
(define snlist-recur
  (lambda (seed item-proc list-proc)
    (letrec ([helper
              (lambda (ls)
                (if (null? ls)
                    seed
                    (let ([c (car ls)])
                      (if (or (pair? c) (null? c))
                          (list-proc (helper c) (helper (cdr ls)))
                          (item-proc c (helper (cdr ls)))))))]))))

(define cps-snlist-recur
  (lambda (base-value item-proc-cps list-proc-cps)
    (letrec
        ([helper (lambda (ls k)
                   (if (null? ls)
                       (apply-k k seed)
                       (let ([c (car s)]
                             (if (or (pair? c) (null? c))
                                 (helper (c) (make-k (helper-car)
                                                     (helper (cdr ls) (make-k (lambda (helper-cdr)
                                                                                (list-proc-cps helper-car helper-cdr k)
                                                                                )))))
                                 (helper (cdr ls) (make-k (lambda (helper-cdr)
                                                            (item-proc-cps c helper-cdr k)))))))))]))))
                   
                                 ; you fill in the details.