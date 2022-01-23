#lang racket  
(define 1st car)
(define apply-k
  (lambda (k v)
    (k v)))

(define make-k    ; lambda is the "real" continuation 
  (lambda (v) v)) ; constructor here.
(define member-cps
  (lambda (sym ls k)
    (cond [(null? ls)          
           (apply-k k #f)]; fill it in )
          [(equal? (car ls) sym)
           (apply-k k ls)
           ]
          [else (member-cps sym (cdr ls) k)])))
(define set-of   ; removes duplicates to make a set
  (lambda (s)
    (cond [(null? s) '()]
          [(member (car s) (cdr s))
           (set-of (cdr s))]
          [else (cons (car s)
                      (set-of (cdr s)))]))) ;here you call k explictly is because it is not in tail position
(define set-of-cps   ; removes duplicates to make a set
  (lambda (s k)
    (if (null? s)
        (apply-k k'())
        (member-cps (car s) (cdr s)
                    (make-k
                     (lambda (mem-result) ;member-cps result
                       (if mem-result
                           (set-of-cps (cdr s) k)
                           (set-of-cps (cdr s)
                                       (make-k
                                        (lambda (set-of-cdr) ;set-of-cdr result
                                          (apply-k k (cons (car s) set-of-cdr)
                                                   )))))))))))
                                                    
(set-of-cps '(1 2 3 2 3 5 6 7 8 'Jordan) (make-k list))                          
(define domain
  (lambda (rel)
    (set-of (map 1st rel))))


(define map1
  (lambda (p ls)
    (if (null? ls)
        '()
        (cons (p (car ls))
              (map1 p (cdr ls)))))) ;the other rule of cps is that the first call even if it not the original one is in always called in tail
;assume that p is in cps form 
(define (map-cps p ls k)
  (if (null? ls)
      (apply-k k '())
      (p (car ls) (make-k
                   (lambda (p-result)
                     (map-cps p (cdr ls)
                              (make-k
                               (lambda (map-result)
                                 (apply-k k (cons p-result map-result)))))))))
  )
(define fact-cps
  (lambda (n k)
    (if (zero? n)
        (apply-k k 1)
        (fact-cps (- n 1)
                  (make-k (lambda (v)
                            (apply-k k (* n v))))))))