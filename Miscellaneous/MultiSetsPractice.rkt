#lang racket
(define (ms-add-one ms sym)
  (cond
    [not (exists-in-multiset? ms sym)
         (append ms (list (list sym 1)))
         ]
    [else
     (cond
       [(null? ms) '()]
       [(equal? (caar ms) sym) (cons (list (caar ms) (add1 (cadr (car ms)))) (rest ms))]
       [else
        (cons (car ms) (ms-add-one (rest ms) sym))
        ]
       )
     ]      
      
    ))
(define (exists-in-multiset? ms sym)
  (ormap (lambda (ls) (equal? (car ls) sym)) ms)
  )

(define (ms-diff s1 s2)
  (cond
    [(null? s1) '()]
    [(not (null? (get-subset-sym s2 (caar s1))))
     (if (> (second (first s1)) (second (first (get-subset-sym s2 (caar s1)))))
         (cons (list (caar s1) (- (second (first s1)) (second (first (get-subset-sym s2 (caar s1)))) ))  (ms-diff (rest s1) s2))
         (ms-diff (rest s1) s2)
         )
     ]
    [else
     (cons (first s1) (ms-diff (rest s1) s2))
     ]
  
    )
  )

(define (get-subset-sym ls sym)
  (filter (lambda (ls) (equal? (car ls) sym)) ls)
  )

(define (ms-most-frequent ms)
    
  (ask jordan)
  )
