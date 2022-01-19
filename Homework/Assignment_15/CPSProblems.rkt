#lang racket  
(define 1st car)
(define apply-k
  (lambda (k v)
    (k v)))

(define make-k    ; lambda is the "real" continuation 
  (lambda (v) v)) ; constructor here.
(define set-of   ; removes duplicates to make a set
  (lambda (s)
    (cond [(null? s) '()]
          [(member (car s) (cdr s))
           (set-of (cdr s))]
          [else (cons (car s)
                      (set-of (cdr s)))])))
(define set-of-cps
  (lambda (s k)
    (cond
      [(null? s) (apply-k k'())]
      [(member (car s) (cdr s))
       (set-of (cdr k) k)]
      [else
       (cons (car s)
             (set-of (cdr s)))]))
 )
(define domain
  (lambda (rel)
    (set-of (map 1st rel))))