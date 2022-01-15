#lang racket
(define (rotate ls)
  (cond
    [(null? ls) '()]
    [(= (length ls) 1) (list (first ls))]
    [else
     (append ( rest ls) (list (first ls)))
     ]
    )
  )

#| Ask Jordan about this|#
(define (compose-with-list list-of-fun)
  (cond
    [(null? list-of-fun) (lambda (x) x)]
    [else
     (lambda (x)
       ((first list-of-fun) (compose-with-list (rest list-of-fun) x)
                            ))]))
((compose-with-list (list car cdr)) '(1 2 3 4))
((compose-with-list (list car cdr cdr cddr)) '(1 2 3 4 5 6))
((compose-with-list (list reverse)) '(1 2 3 4))
((compose-with-list '()) '(1 2 3 4))
(define (compose)
  (case-lambda
    (() (lambda (x) (x)))
    ((first . rest ) (lambda (x) (first (apply compose rest) x))
                     )
    ))