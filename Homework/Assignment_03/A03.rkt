#lang racket

(provide intersection subset? relation? domain reflexive? multi-set? ms-size last all-but-last)

(define intersection
  (lambda (a b)
    (cond
      [(null? a) '()]
      [(cons? a)
       (if (member (first a) b)
           (cons (first a)(intersection (rest a) b))
           (intersection (rest a) b)
           )
       ]
      )))

(define subset?
  (lambda (a b)
    (cond
      [(null? a) #t]
      [else
       (and (member (first a) b) (subset? (rest a) b))
       ]
      )))


(define relation?
  (lambda (x)
    (and (set? x) (andmap list? x) (andmap is-size-2? x))))

;;helper method to check if each element is the sie two 
(define (is-size-2? ls)
  (= (length ls) 2))

;;helper functions
(define (all-unique-elements? ls)
  (cond
    [(null? ls) #t]
    [ (cons? ls)
      (and (unique-element? (car ls) (cdr ls)) (all-unique-elements?(cdr ls)))]))

(define (unique-element? element ls)
  (cond
    [(null? ls) #t]
    [(cons? ls)
     (if (equal? element (first ls))
         #f
         (unique-element? element (cdr ls)))]))

(define (set? x)
  (and (list? x) (all-unique-elements? x)))


(define domain
  (lambda (a)
    (cond
      [(null? a) '()]
      [else
       (cons (first (first a)) (domain (rest a)))
       ]
      )))

(define reflexive?
  (lambda (a)
    (nyi)))

(define multi-set?
  (lambda (a)
    (cond
      [(null? a) #t]
      
      [else
       (and (list? a)(list? (first a)) (multi-set? (rest a)))
       ]
      )))

(define ms-size
  (lambda (a)
    (apply + (map second a))))

(define last
  (lambda (a)
    (cond
      [(null? (rest a)) (first a)]
      [else
       (last (rest a))
       ]
      )))

(define all-but-last
  (lambda (a)
    (cond
      [(null? (rest a)) '()]
      [(cons (first a) (all-but-last (rest a)))]

      )))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
