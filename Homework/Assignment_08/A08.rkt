#lang racket

(provide make-slist-leaf-iterator subst-leftmost)

(define make-stack
  (lambda ()
    (let ([stk '()])
      (lambda (msg . args)
        (case msg
          [(empty?) (null? stk)]
          [(push) (set! stk (cons (car args) stk))]
          [(pop) (let ([top (car stk)])
                   (set! stk (cdr stk))
                   top)]
          [else (error 'stack "illegal message to stack object: ~a" msg)])))))

(define make-slist-leaf-iterator
  (lambda (a)
    (nyi)))

(define subst-leftmost
  (lambda (a b c d)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
