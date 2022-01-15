#lang racket
;; QUESTION 4: Rename Free (15 points)

;; Rename free takes a free variable and renames it in a lambda
;; calculus expression.  You are given 3 values - an old variable
;; name, a new one and a lambda calculus expression.  Return a new
;; lambda calculus expression where all instances of the free variable
;; have been replaced with the new name.

;; For example:
;; (rename-free 'old 'new '(old old)) yields (new new)

;; Note that there might be places in the lambda calculus expression
;; where the old named variable is not free but bound - i.e. there is
;; an enclosing lambda with the name old.  In these cases, the
;; variable should not be renamed.

;; For example:
;; (rename-free 'old 'new '((lambda (old) old) old) yields ((lambda (old) old) new)

(define rename-free
  (lambda (old-name new-name lc-exp)
    (cond
      [(equal? old-name lc-exp) new-name]
      [(symbol? lc-exp) lc-exp]
      [(empty? lc-exp) '()]
      [(and (<= 3 (length lc-exp))
            (equal? 'lambda (first lc-exp))
            (list? (second lc-exp))
            (member old-name (second lc-exp)))
       lc-exp
       ]
      [else
       (cons (rename-free old-name new-name (first lc-exp))
             (rename-free old-name new-name (rest lc-exp)))
       ]
      )))

(rename-free equal? ; (run-test rename-free)
             [(rename-free 'old 'new 'old) 'new 1] ; (run-test rename-free 1)
             [(rename-free 'o 'n '(o o)) '(n n) 1] ; (run-test rename-free 2)
             [(rename-free 'o 'n '(o (x o))) '(n (x n)) 1] ; (run-test rename-free 3)
             [(rename-free 'o 'n '(o (lambda (x) (x o)))) '(n (lambda (x) (x n))) 1] ; (run-test rename-free 4)
             [(rename-free 'o 'n '(o (lambda (o) (x o)))) '(n (lambda (o) (x o))) 1] ; (run-test rename-free 5)
             [(rename-free 'o 'n '((lambda (o) o)  (lambda (y) (x o)))) '((lambda (o) o) (lambda (y) (x n))) 1] ; (run-test rename-free 6)
             [(rename-free 'o 'n '((lambda (o) (lambda (y) o)) o)) '((lambda (o) (lambda (y) o)) n) 7] ; (run-test rename-free 7)
             )