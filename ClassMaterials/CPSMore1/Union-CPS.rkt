#lang racket
(require "chez-init.rkt")
(define-datatype continuation continuation?
  [init-k] 
  [union-memq-k 
   (loop procedure?)
   (s1 list?)
   (k (lambda (x) (or (procedure? x) (continuation? x))))
   ]
  [loop-cdr-k 
   (s1 list?)
   (k (lambda (x) (or (procedure? x) (continuation? x))))
   ]
  [remove-cdr-k
   (sym symbol?)
   (ls list?)
   (k (lambda (x) (or (procedure? x) (continuation? x))))
   ]
  [free-third-k
   
   (exp list?)
   (k (lambda (x) (or (procedure? x) (continuation? x))))
   ]
  [free-second-k
   (exp list?)
   (k (lambda (x) (or (procedure? x) (continuation? x))))
   ])
(define apply-k-ds
  (lambda (k v)
    (if (procedure? k)
        (k v)
        (cases continuation k
          [init-k () void]
          [union-memq-k (f s1 k)
                        (if v
                            (f (cdr s1) k)
                            (f (cdr s1) (loop-cdr-k s1 k)))
                        ]
          [loop-cdr-k (s1 k)
                      (apply-k-ds k (cons (car s1) v))
                      ]
          [remove-cdr-k (sym ls k)
                        (if (eq? sym (car ls))
                            (apply-k-ds k (cdr ls))
                            (apply-k-ds k (cons (car ls) v)))
                        ]
          [free-third-k (exp k)
                        (remove-cps-ds (2nd exp) v k)
                        ]
          [free-first-k (exp k)
                         (free-vars-cps (2nd exp) free-second-k (make-k
                                                                    (lambda (free-second)
                                                                      (union-cps free-first free-second k)))))))]))))

(define memq-cps
  (lambda (sym ls k)
    (cond [(null? ls) (apply-k-ds k #f)]
          [(eq? (car ls ) sym) (apply-k-ds k #t)]
          [else (memq-cps sym (cdr ls) k)])))
(define union-cps-ds
  (lambda (s1 s2 k)
    (let loop ([s1 s1] [k k])
      (if (null? s1)
          (apply-k-ds k s2)
          (memq-cps (car s1) s2 (union-memq-k loop s1 k))))))

(define remove-cps-ds
  (lambda (sym los k)
    (if (null? los)
        (apply-k-ds k '())
        (remove-cps-ds sym (cdr los) (remove-cdr-k sym los k)))))

(define free-vars-cps
  (lambda (exp k)
    (cond
      [(symbol? exp) (apply-k k (list exp))]
      [(eq? (1st exp) 'lambda)
       (free-vars-cps (3rd exp) (make-k (lambda (free-third)
                                          (remove-cps (car (2nd exp)) free-third k))))]
      [else
       (free-vars-cps (1st exp) (make-k (lambda (free-first)
                                          (free-vars-cps (2nd exp) (make-k
                                                                    (lambda (free-second)
                                                                      (union-cps free-first free-second k)))))))])))
      
(define free-vars-cps-ds
  (lambda (exp k)
    (cond
      [(symbol? exp) (apply-k-ds k (list exp))]
      [(eq? (1st exp) 'lambda)
       ])))