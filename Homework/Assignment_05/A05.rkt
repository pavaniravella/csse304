#lang racket

(provide minimize-interval-list exists? product replace remove-last)

;;need to figure out how to make this a better and more simpler way
(define minimize-interval-list
  (lambda (lst)
    (if (eq? (length lst) 1)
        lst
        (let ([sortedlst (sort lst (lambda (pr1 pr2) (< (car pr1) (car pr2))) )])
          (if (>= (cadar sortedlst) (caadr sortedlst))
              (minimize-interval-list [cons (list [caar sortedlst] (max [cadadr sortedlst] [cadar sortedlst])) (cddr sortedlst)])
              [cons (car sortedlst) (minimize-interval-list (cdr sortedlst))])))))

(define exists?
  (lambda (pred? ls)
    (cond
      [(null? ls) #f ]
      [else
       (or (pred? (first ls)) (exists? pred? (rest ls)))
       ]

      )))

(define product
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [(null? ls2) '()]
      [else
       (append (product-helper (first ls1) ls2) (product (rest ls1) ls2))
       ]
      )))

(define (product-helper x ls)
  (cond
    [(null? ls) '()]
    [(cons (list  x (first ls)) (product-helper x (rest ls)))]

    )
  )

(define replace
  (lambda (old new ls)
    (cond
      [(null? ls) '() ]
      [(equal? (car ls) old) (cons new (replace old new (rest ls)))]
      [else
       ( cons (first ls) (replace old new (rest ls)))
       ]
      )))

(define (remove-first element ls)
  (cond
    [(null? ls) '()]
    [(equal? element (first ls))
     (append '() (rest ls))
     ]
    [(cons (first ls) (remove-first element (rest ls)))]

    )

  )
(define remove-last
  (lambda (element ls)
    (reverse (remove-first element (reverse ls)))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
