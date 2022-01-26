#lang racket

(require "chez-init.rkt")

;(define apply-k (lambda (k v) (k v)))
;(define make-k (lambda (v) v))

(define-datatype continuation continuation?
  [init-k] ;when teh continuation is applied to its
  [append-k
   (L1 list?)
   (k (lambda (x) (or (continuation? x) (procedure? x))))
   ]
  [flatten-cdr-k
   (L1 list?)
   (k (lambda (x) (or (continuation? x) (procedure? x))))
   ]
  [flatten-car-k
   (flattened-cdr list?)
   (k continuation?)]
  
  )
(define (apply-k k v)
  (if (procedure? k)
      (k v)
      (cases continuation k
        [init-k ()
                (pretty-print v)
                (read-flatten-print)]
        [flatten-cdr-k (L1 k)
                       (if (list? (car L1))
                                   (flatten-cps (car ls)
                                               (flatten-car-k v k)
                                               (apply-k k (cons ))
                       ]
        [flatten-car-k (L1 k)
                       ]
        [append-k (L1 k)
                  (apply-k k (cons (car L1) v))
                  ]
        
        )))
(define read-flatten-print
  (lambda ()
    (display "enter slist to flatten: ")
    (let ([slist (read)])
      (unless (eq? slist 'exit)
        (flatten-cps slist (init-k))))))

(define flatten-cps
  (lambda (ls k)
    (if (null? ls)
        (apply-k k ls)
        (flatten-cps (cdr ls) (flatten-cdr-k ls v)))))
(define append-cps 
  (lambda (L1 L2 k)
    (if (null? L1)
        (apply-k k L2)
        (append-cps (cdr L1)
                    L2
                    (append-k L1 k)))))

;(trace append-cps flatten-cps apply-k)
