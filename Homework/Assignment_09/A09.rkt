#lang racket

(provide sn-list-recur sn-list-sum sn-list-map sn-list-paren-count sn-list-reverse sn-list-occur sn-list-depth bt-recur bt-sum bt-inorder)

(define (my-fold proc base work)
  (cond
    [(null? work) base]
    [else
     (proc (car work) (my-fold proc base (cdr work)))
     ]
    ))
(map list '(1 2 3 4 5 6))
(define sn-list-recur
  (lambda (base-value sn-list-proc sn-proc)
    (lambda (sn-list)
      (cond
        [(null? sn-list) base-value]
        [(list? (car sn-list))  (sn-list-proc ((sn-list-recur base-value sn-list-proc sn-proc) (car sn-list)) ((sn-list-recur base-value sn-list-proc sn-proc) (cdr sn-list)))]
        [else
         (sn-proc (car sn-list) ((sn-list-recur base-value sn-list-proc sn-proc) (cdr sn-list)))]))))

(define sn-list-sum
  (lambda (a)
    ((sn-list-recur 0 + + ) a)))

(define (sn-list-map proc snlist)
  ((sn-list-recur '() (lambda (x y) (if (list? x)
                                        (cons x y)
                                        (cons (proc x) y)))) snlist
                                                             ))

(define sn-list-paren-count
  (sn-list-recur 2 (lambda (x y) (+ 0 y)) (lambda (x y) (+ x y))))


(define sn-list-reverse
  (lambda (a)
    (nyi)))

(define sn-list-occur
  (lambda (a b)
    (nyi)))

(define sn-list-depth
  (lambda (a)
    (nyi)))

(define bt-recur
  (lambda (a b)
    (nyi)))

(define bt-sum
  (lambda (a)
    (nyi)))

(define bt-inorder
  (lambda (a)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
