#lang racket
(define (apply-k k v)
  (k v))
(define (make-k v) v)
(define (1st-cps l k)
  (apply-k k (car l) ))


(define member?-cps
  (lambda (item ls k)
    (cond
      [(null? ls) (apply-k k #f)]
      [(eq? (car ls) item) (apply-k k #t)]
      [else (member?-cps item (cdr ls) k)])))
(define set-of-cps
  (lambda (s k)
    (cond [(null? s)
           (apply-k k '())]
          [else
           (set-of-cps (cdr s) (make-k (lambda (set-cdr)
                                         (member?-cps (car s) (cdr s) (make-k (lambda (member-cdr)
                                                                             (if member-cdr
                                                                                 set-cdr
                                                                                 (apply-k k (cons (car s) set-cdr)))))))))
                                                                                 
           ]))
  )
(define map-cps
  (lambda (p ls k)
    (if (null? ls)
        (apply-k k '())
        (map-cps p (cdr ls) (make-k (lambda (map-cdr)
                                      (apply-k k (cons (p (car ls)) map-cdr))))))))
(define domain-cps
  (lambda (ls k)
    (set-of-cps (map-cps 1st-cps ls))))
