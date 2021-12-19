#lang racket

(provide interval-contains? interval-intersects? interval-union my-first my-second my-third make-vec-from-points dot-product vector-magnitude distance)


(define interval-contains?
  (lambda (interval number)
    (and (<= number (cadr interval)) (>= number (car interval)))))



(define interval-intersects? 
  (lambda (i1 i2)
    (or (or (interval-contains? i1 (car i2)) (interval-contains? i1 (cadr i2))) 
        (or (interval-contains? i2 (car i1)) (interval-contains? i2 (cadr i1))))))


(define interval-union 
  (lambda (i1 i2)
    (if (interval-intersects? i1 i2)
        (list (list (min (car i1) (car i2)) (max (cadr i1) (cadr i2))))
        (list i1 i2))))


(define my-first
  (lambda (a)
    (car a)))

(define my-second
  (lambda (a)
    (cadr a)))

(define my-third
  (lambda (a)
    (caddr a)))


(define make-vec-from-points
  (lambda (l1 l2)
    (list (- (my-first l2) (my-first l1)) (- (my-second l2) (my-second l1)) (- (my-third l2) (my-third l1)))
    ))	


(define dot-product
  (lambda (l1 l2)
    (+ (* (my-first l2) (my-first l1)) (* (my-second l2) (my-second l1)) (* (my-third l2) (my-third l1)))))


(define vector-magnitude
  (lambda (ls)
    (sqrt (+ (square (car ls)) (square (cadr ls)) (square (my-third ls))))))


(define square
  (lambda (a)
    (* a a)
    )
  )
(define distance
  (lambda (l1 l2)
    (vector-magnitude(make-vec-from-points l1 l2))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
