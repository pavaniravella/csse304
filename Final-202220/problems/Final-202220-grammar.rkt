#lang racket
(provide remove-double)

;; GRAMMAR QUESTION - 10 points
;;
;; So in this problem you will write code that works on a varation of the lambda calculus.
;; Recall that the lambda calculus has a 3 rules:

;; <LcExp> :- <identifier> | ( lambda (<identifier>) <LcExp> ) | ( <LcExp> <LcExp> )

;; This variation adds one more rule - a "double" application (i.e. an application with 2 parameters):
;;
;; ( <LcExp> <LcExp> <LcExp> )
;;
;; This is actually a shorthand for a "curried" function application
;;
;; ( <LcExp> <LcExp> <LcExp> ) is equivalent to ( ( <LcExp> <LcExp> ) <LcExp> )
;;
;; Write a function that takes an expression in the variant lambda calculus and returns the equivalent
;; structure in the original lambda calculus.
;;
;; For example:
;;
;; ((lambda (x) (lambda (y) (y x))) a (p q r)) becomes
;; (((lambda (x) (lambda (y) (y x))) a) ((p q) r))
;;
;; See the test cases for more examples.
;;
;; You can also assume that the reserved word "lambda" can never be an identifier (if you don't
;; do this, the variation grammar might seem ambigious).  If you didn't notice that - don't worry
;; about it.

(define (identifier? exp)
  (and (not (list? exp))(symbol? exp)))

(define (lam-identifier? exp)
  (and (list? exp) (= 3 (length exp)) (list? (second exp)) (lc-exp? (third exp)))) ;identifier? (second exp))

(define (three-lc-exp? exp)
  (and (list? exp) (= 3 (length exp)) (lc-exp? (first exp)) (lc-exp? (second exp)) (lc-exp? (third exp)))
  )
(define (lc-exp? exp)
  (or (identifier? exp) (three-lc-exp? exp) (lam-identifier? exp) (and (= 2 (length exp)) (lc-exp? (car exp)) (lc-exp? (second exp))))
  )
(define (two-lc-exp? exp)
  (and (list? exp) (= 2 (length exp)) (lc-exp? (first exp)) (lc-exp? (second exp)))
  )


(define remove-double
  (lambda (lc-exp)
    (cond
      [(three-lc-exp? lc-exp)
       (displayln "i am in three lc-exp")
       (list (list (remove-double (first lc-exp)) (remove-double (second lc-exp))) (remove-double (third lc-exp)))
       ]
      [(two-lc-exp? lc-exp)
       (list (remove-double (first lc-exp)) (remove-double (second lc-exp)))
       ]
      [(identifier? lc-exp) lc-exp] ;if symbol return expression
      [(lam-identifier? lc-exp)
       (displayln (first lc-exp))
       (displayln (second lc-exp))
       (list (first lc-exp) (second lc-exp) (remove-double (third lc-exp)))]
      
      )))
        

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))