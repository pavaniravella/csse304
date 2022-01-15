#lang racket

(provide curry2 curried-compose compose make-list-c reverse-it map-by-position empty-BST empty-BST? BST-insert BST-inorder BST? BST-element BST-left BST-right BST-insert-nodes BST-contains? BST-height let->application let*->let qsort sort-list-of-symbols)

(define curry2
  (lambda (f)
    (lambda (a)
      (lambda (b)
        (f a b)
        )
      )))

(define curried-compose
  (lambda (f)
    (lambda (a)
      (lambda (b)
        (f (a b))))))

;;ask Jordan about 
(define (compose . list-of-functions)
  (lambda (x)
    (cond
      [(null? list-of-functions) x]
      [(cons? list-of-functions)
       ((car list-of-functions) ((apply compose (cdr list-of-functions)) x))])))

(define make-list-c
  (lambda (num)
    (lambda (obj)
      (make-list num obj)
      )
    ))

(define (reverse-it-helper ls acc)
  (cond
    [(null? ls) acc]
    [(reverse-it-helper (rest ls) (cons (first ls) acc))]
    ))

(define reverse-it
  (lambda (ls)
    (reverse-it-helper ls '())))

(define map-by-position
  (lambda (fn-list arg-list)
    (map (lambda (f a) (f a))fn-list arg-list)))

(define empty-BST
  (lambda () '()))
	
(define empty-BST?
  (lambda (obj)
    (null? obj)))


(define BST-insert
  (lambda (num bst)
    (cond
      [(null?)]
      
      )))

(define BST-inorder
  (lambda (a)
    (nyi)))

(define BST?
  (lambda (a)
    (nyi)))

(define BST-element
  (lambda (bst)
    (if (list? bst)
        (first bst)
        '())))

(define BST-left
  (lambda (bst)
    (if (list? bst)
        (second bst)
        '())))


(define (BST-right bst)
 (if (list? bst)
        (third bst)
        '()))


(define BST-insert-nodes
  (lambda (a b)
    (nyi)))

(define BST-contains?
  (lambda (a b)
    (nyi)))

(define BST-height
  (lambda (a)
    (nyi)))

(define let->application
  (lambda (a)
    (nyi)))

(define let*->let
  (lambda (a)
    (nyi)))

(define qsort
  (lambda (a b)
    (nyi)))

(define sort-list-of-symbols
  (lambda (a)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
