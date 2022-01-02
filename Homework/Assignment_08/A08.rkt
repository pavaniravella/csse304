#lang racket
;Pavani Ravella

;Assignment 8

(provide make-slist-leaf-iterator subst-leftmost)

;(define make-slist-leaf-iterator
;  (lambda (a)
;    (nyi)))
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

(define stk-next

  (lambda (stk)

    (if (stk 'empty?)

        #f

        (let ([top (stk 'pop)])

          (cond[(symbol? top) top]

               [(not (null? top))

                (stk 'push (cdr top))

                (stk 'push (car top))

                (stk-next stk)]

               [else (stk-next stk)])))))

(define make-slist-leaf-iterator

  (lambda (lst)

    (let ([stk (make-stack)])

      (null? lst) #f

      (stk 'push (cdr lst))

      (stk 'push (car lst))

      (lambda (arg)

        (case arg

          [(next)

           (if (null? lst)

               #f

               (stk-next stk))])))))
;(define make-slist-leaf-iterator
;  (lambda (slist)
;    (let ([stack (make-stack)])
;      (stack 'push slist)
;      (letrec ([loop
;                (lambda ()
;                  (if (stack 'empty?)
;                      #f
;                      (let ([top (stack 'pop)])
;                        (cond
;                          [(null? top) (loop)]
;                          [(symbol? top) top]
;                          [(list? top) (begin (stack 'push (cdr top)) (stack 'push (car top)) (loop))]
;                          ))))])
;        loop))))
;
;
;(define make-stack
;  (lambda ()
;    (let ([stk '()])
;      (lambda (msg . args )
;        (case msg ; Scheme's case is a similar to switch in some other languages.
;          [(empty?) (null? stk)]
;          [(push) (set! stk (cons (car args) stk))]
;          [(pop) (let ([top (car stk)])
;                   (set! stk (cdr stk))
;                   top)]
;          [else (error 'stack "illegal message to stack object: ~a" msg)])))))

;(define subst-leftmost
;  (lambda (a b c d)
;    (nyi)))
;(define subst-leftmost
;  (lambda (new old lst pred)
;    (cond
;      ((pair? lst)
;       (let ((next (subst-leftmost new old (car lst) pred)))
;         (cons next
;               (if (eq? next (car lst))
;                   (subst-leftmost new old (cdr lst) pred)
;                   (cdr lst)))))
;      ((pred lst old) new)
;      (else lst))))
(define subst-help

  (lambda (new old lst comp?)

    (cond[(null? lst) (list '() #f)]

         [(symbol? lst)

          (if (comp? old lst)

              (list new #t)

              (list lst #f))]

         [else

          (let* ([top-car (subst-help new old (car lst) comp?)]

                 [lst-car (car top-car)]

                 [bool-car (cadr top-car)])

            (if bool-car

                (list (cons lst-car (cdr lst)) #t)

                (let* ([top-cdr (subst-help new old (cdr lst) comp?)]

                       [lst-cdr (car top-cdr)]

                       [bool-cdr (cadr top-cdr)])

                  (list (cons lst-car lst-cdr) bool-cdr))))])))

(define subst-leftmost

  (lambda (new old lst comp?)

    (car (subst-help new old lst comp?))))
;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))