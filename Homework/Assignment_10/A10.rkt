
#lang racket

(provide free-vars bound-vars occurs-free? occurs-bound? lexical-address un-lexical-address)

(define (lambda? lc-exp)
  (and (list? lc-exp)  ;;have a lambda expression 
       (<= 3 (length lc-exp ))
       (equal? (first lc-exp) 'lambda)
       (list? (second lc-exp))
       )
  )
(define free-vars
  (lambda (lc-exp)
    (cond
      [(symbol? lc-exp)
       (list lc-exp)
       ]
      [(number? lc-exp) '()]
      [(empty? lc-exp)
       '()]
      
      [(lambda? lc-exp)
       (remove* (second lc-exp) (free-vars (third lc-exp)))
       ]
      [else
       (remove-duplicates (append (free-vars (first lc-exp)) (free-vars (rest lc-exp))))
       ])))

(define (bound-vars exp)
  (cond
    [(symbol? exp) '()]
    [(equal? (first exp) 'lambda)
     (cond
       [(symbol? (third exp))
        (if (equal? (caadr exp) (caddr exp))
            (append (list (caddr exp)))
            (append '())
            )
        ]
       [(equal? (first (caddr exp)) 'lambda)
        (if (equal? (caadr exp) (caddr (caddr exp)))
            (append (list (caadr exp)))
            (append (bound-vars (caddr exp)))
            )
        ]
       [(pair? (caddr exp)) (append (bound-vars (car (caddr exp))) (bound-vars (cadr (caddr exp))))]	
       )
     ]
    [else
     (append (bound-vars (car exp)) (bound-vars (cadr exp)))
     ]
    )
  )

(define occurs-free?
  (lambda (var exp)
    (cond
      [(null? exp) #f]
      [(symbol? exp) (eqv? var exp)]
          [(eqv? (car exp) 'lambda) 
           (and (not (eqv? (caadr exp) var))
                (occurs-free? var (caddr exp)))]
          [else (or (occurs-free? var  (car exp))
                    (occurs-free? var (cadr exp)))])))

(define occurs-bound?
  (lambda (var exp)
    (cond [(symbol? exp) #f]
          [(eqv? (car exp) 'lambda)
           (or (occurs-bound? var (caddr exp))
               (and (eqv? (caadr exp) var)
                    (occurs-free? var (caddr exp))))]
          [else (or (occurs-bound? var  (car exp))
                    (occurs-bound? var (cadr exp)))])))

(define lexical-address
  (lambda (a)
    (nyi)))

(define un-lexical-address
  (lambda (a)
    (nyi)))


;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))