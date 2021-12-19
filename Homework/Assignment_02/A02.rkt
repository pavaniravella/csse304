#lang racket

(provide choose sum-of-squares range my-set? union cross-product parallel? collinear?)

(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (sub1 n)))
      )
  )
(define choose
  (lambda (a b)
    (/ (fact a) (* (fact b)(fact (- a b)) ))))

(define sum-of-squares
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [else
       (+ (* (first ls) (first ls)) (sum-of-squares (rest ls)))
       ]
      )))

(define range
  (lambda (n m)
    (cond
      [(>= n m) '()]
      [(cons n (range (add1 n) m))]
      )))

(define (my-set? ls)
  (cond
    [(null? ls) #t]
    [(cons? ls)
     ;; check if that first element is unique 
     ;;   if so, produce #t iff the rest of the elements are each unique
     ;;   if not, produce #f 
     (and (unique-element? (first ls) (rest ls)) (my-set? (rest ls)))])) ;;check if the rest of the elements are unique amongst themselves 

;;checks if the given element is not contained in the ls 
;;
(define (unique-element? element ls)
  (cond
    [(null? ls) #t]
    [(cons? ls)
     (if (equal? element (first ls))
         #f
         (unique-element? element (rest ls))
         )]))

;;along for the ride template 
(define union
  (lambda (a b)
    (cond
      [(null? a) b]
      [(cons? a)
       (if (member (first a) b)
           (union (rest a) b)
           (cons (first a) (union (rest a) b))
           
           )
       ])))

(define cross-product
  (lambda (v1 v2)
    (list (- (* (second v1) (third v2)) (* (third v1) (second v2)))
          (- (* (third v1) (first v2)) (* (first v1) (third v2)))
          (- (* (first v1) (second v2)) (* (second v1) (first v2))))
    ))

(define parallel?
  (lambda (v1 v2)
    (and (and (zero? (first (cross-product v1 v2))) (zero? (second (cross-product v1 v2)))) (zero? (third (cross-product v1 v2))))))

(define collinear?
  (lambda (p1 p2 p3)
    (parallel? (list (- (first p1) (first p2)) (- (second p1) (second p2)) (- (third p1) (third p2)))
               (list (- (first p3) (first p2)) (- (second p3) (second p2)) (- (third p3) (third p2))))))
;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
