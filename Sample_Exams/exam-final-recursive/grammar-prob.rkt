#lang racket
(require "chez-init.rkt")
;; Consider a language for boolean expressions defined in this way:
;;
;; <boolean_exp> = <and_exp> | <or_exp> | <not_exp> | <var_exp>
;; <and_exp> = ( <boolean_exp> and <boolean_exp> )
;; <or_exp> = ( <boolean_exp> or <boolean_exp> )
;; <not_exp> = ( not <boolean_exp> )
;; <var_exp> = <symbol>
;;(and boolean-exp 
;; Given this language, write a function remove-ands that takes a
;; boolean expression are returns a new boolean expression where all
;; the and_exps have been removed.  We'll use the usual DeMorgan's law
;; to removed an and expression:
;;
;; (exp1 and exp2) => (not ((not exp1) or (not exp2)))
;;
;; That's how a single and_exp should be removed, but remove-ands
;; should remove all ands in the expression, no matter how deeply they
;; appear in subexpressions.  For example:
;;
;; (remove-ands '(x and (y and z)))
;;
;; yields
;;
;; (not ( (not x) or (not (not ((not y) or (not z))))))
;;
;; non-and expressions should be unchanged by remove-ands (except if
;; they contain and_exps as a subexpression).  Look at the test
;; cases for some detailed examples.
(define (var-exp? ?exp)
  (symbol? ?exp))
(define (not-exp? ?exp)
  (and (list? ?exp) (= 2 (length ?exp)) (equal? 'not (car ?exp)) (bool-exp? (cadr ?exp))
  ))
(define (bool-exp? ?exp)
  (or (var-exp? ?exp) (not-exp? ?exp)(or-exp? ?exp)  (and-exp? ?exp)  ))
(define (and-exp? exp)
  (and (list? exp) (= 3 (length exp)) (equal? 'and (second exp)) (bool-exp? (first exp)) (bool-exp? (third exp))))
(define (or-exp? exp)
  (and (list? exp) (= 3 (length exp))(equal? 'or (cadr exp)) (bool-exp? (car exp)) (bool-exp? (third exp)))
  )

(define (remove-ands bool-exp)
  (cond
    [(var-exp? bool-exp) bool-exp] ;var-exp
    [(not-exp? bool-exp)
     (list 'not (remove-ands (second bool-exp)))]
    [(or-exp? bool-exp)
     (list (remove-ands (first bool-exp)) 'or (remove-ands (third bool-exp)))
     ]
    [(and-exp? bool-exp)
    ; (not ( (not x) or (not (not ((not y) or (not z))))))
     (let ([exp1 (first bool-exp)] [exp2 (third bool-exp)])
       (list 'not
             (list (list 'not (remove-ands exp1))
                   'or (list 'not (remove-ands exp2)))
       ))
     ]
            ))

(remove-ands 'q)
(remove-ands '(a or b))
(remove-ands '(not a))
(remove-ands '(a and b))
(remove-ands '(not (a and b)))
(remove-ands '(x or (y and z)))
(remove-ands '(x and (y and z)))
