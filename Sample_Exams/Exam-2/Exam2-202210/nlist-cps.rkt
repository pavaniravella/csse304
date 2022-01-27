#lang racket
(require "chez-init.rkt")
(define apply-k
  (lambda (k v)
    (k v)))

(define make-k    ; lambda is the "real" continuation 
  (lambda (v) v))
(define-datatype continuation continuation? )
(define apply-k-ds
  (lambda (k v)
    (if (procedure? k)
        (k v)
        (cases continuation k))
    ))
(define nlist-filter
  (lambda (pred? s)
    (cond [(null? s) '()]
          [(number? (car s))
           (if (pred? (car s))
               (cons (car s)
                     (nlist-filter pred? (cdr s)))
               (nlist-filter pred? (cdr s)))]
          [else (cons (nlist-filter pred? (car s))
                      (nlist-filter pred? (cdr s)))])))

(define nlist-filter-cps
  (lambda (pred? s k)
    (if (null? s)
        (apply-k k '())
        (nlist-filter-cps pred? (cdr s) (make-k (lambda (nlist-cdr)
                                                  (cond [(number? (car s))
                                                         (if (pred? (car s))
                                                             (apply-k k (cons (car s) nlist-cdr))
                                                             (nlist-filter-cps pred? (cdr s) k)
                                                             )]
                                                        [else 
                                                         (nlist-filter-cps pred? (car s) (make-k (lambda (nlist-car)
                                                                                                   (apply-k k (cons nlist-car nlist-cdr)))))])))))))

                                                   
      
