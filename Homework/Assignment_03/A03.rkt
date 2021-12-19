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

(define range
  (lambda (r)
    (cond
      [(null? r) '()]
      [(member (cadar r) (range (cdr r)))
       (range (cdr r))]
      [else (cons (cadar r) (range (cdr r)))])))



(define domain-range
  (lambda (r)
    (union (domain r) (range r))))


(define union
  (lambda (s1 s2)
    (cond
      [(null? s1) s2]
      [(member (car s1) s2) (union (cdr s1) s2)]
      [else (union (cdr s1) (cons (car s1) s2))])))
(define check
  (lambda (ls r)
    (cond
      [(null? ls) #t]
      [(not (member (list (car ls) (car ls)) r)) #f]
      [else (check (cdr ls) r)])))



(define reflexive?
  (lambda (r)
    (cond
      [(null? r) #t]
      [else (check (domain-range r) r)])))


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

(define multi-set?
  (lambda (obj)
    (if(relation? obj)
       (if(member #f (map format? obj))
          #f
          (set? (map car obj)))
       #f)))

(define format? 
  (lambda (ls)
    (and (symbol? (car ls)) (if(number? (cadr ls)) (positive? (cadr ls)) #f))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
