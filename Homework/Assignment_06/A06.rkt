#lang racket

(provide curry2 curried-compose compose make-list-c reverse-it map-by-position empty-BST empty-BST? BST-insert BST-inorder BST? BST-element BST-left BST-right BST-insert-nodes BST-contains? BST-height let->application let*->let qsort sort-list-of-symbols)

(define curry2
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (a b c)))))

(define curried-compose
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (a (b c))))))

(define (compose . list-of-functions)
  (lambda (x)
    (cond
      [(null? list-of-functions) x]
      [(cons? list-of-functions)
       ((car list-of-functions) ((apply compose (cdr list-of-functions)) x))])))


;(define make-list-c (curry2 make-list))
(define make-list-c
  (lambda (num)
    (lambda (obj)
      (make-list num obj))))

(define (reverse-it lst)
  (letrec ([helper
            (lambda (ls acc)
              (if (null? ls)
                  acc
                  (helper (cdr ls) (cons (car ls) acc))))])
    (helper lst '())))

(define map-by-position  ;mapping takes
  (lambda (fn-list arg-list)
    (map (lambda (func arg) (func arg)) fn-list arg-list)) )


(define empty-BST
  (lambda () '()))
	
(define empty-BST?
  (lambda (obj)
    (null? obj)))
		
(define BST-left
  (lambda (bst)
    (if (list? bst) (cadr bst) '())))
		
(define BST-right
  (lambda (bst)
    (if (list? bst) (caddr bst) '())))
		
(define BST-element
  (lambda (bst)
    (if (list? bst) (car bst) '())))
			
			
(define BST-insert
  (lambda (num bst)
    (cond 
      [(empty-BST? bst) (list num '() '())]
      [(member num bst) bst]
      [(< num (BST-element bst)) (list (BST-element bst)
                                       (BST-insert num (BST-left bst)) (BST-right bst))]
      [(< (BST-element bst) num) (list (BST-element bst)
                                       (BST-left bst) (BST-insert num (BST-right bst)))])))
		  		
(define BST-inorder
  (lambda (bst)
    (if (empty-BST? bst) '()
        (append (BST-inorder (BST-left bst)) (list (BST-element bst))
                (BST-inorder (BST-right bst))))))

(define sorted?
  (lambda (bst)
    (if (null? (cdr bst)) #t
        (if (< (BST-element bst) (BST-left bst))
            (sorted? (cdr bst)) #f))))

(define BST?
  (lambda (bst)
    (cond 
      [(null? bst) #t]
      [(not (list? bst)) #f]
      [(and (and 
             (= (length bst) 3) 
             (number? (BST-element bst))
             (list? (BST-left bst))
             (list? (BST-right bst))
             (and (BST? (BST-left bst))
                  (BST? (BST-right bst))))
            (sorted? (BST-inorder bst)))]
      [else #f])))
			
(define BST-insert-nodes
  (lambda (bst nums)
    (if (null? nums) bst
        (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))

(define BST-contains?
  (lambda (bst num)
    (cond [(empty-BST? bst) #f]
          [(= (BST-element bst) num) #t]
          [(< (BST-element bst) num) (BST-contains?  (BST-right bst) num)]
          [else (BST-contains? (BST-left bst) num)])))

(define BST-height 
  (lambda (bst)
    (if (empty-BST? bst) -1
        (+ 1 (max (BST-height (BST-left bst)) (BST-height (BST-right bst)))))))
;;;;;;;
(define let->application
  (lambda (exp)
    (let([front (map car (cadr exp))]
         [back (map cadr (cadr exp))]
         [body (caddr exp)])
      (cons (list 'lambda front body) back))))

(define let*-helper
  (lambda (bindings body)
    (if (null? bindings)
        body
        (list 'let (list (car bindings)) (let*-helper (cdr bindings) body)))))

(define let*->let
  (lambda (let*-exp)
    (let([bindings (cadr let*-exp)]
         [body (caddr let*-exp)])
      (let*-helper bindings body))))

(define qsort-helper
  (lambda (pred ls)
    (if (null? ls)
        '()
        (if (pred (car ls))
            (cons (car ls) (qsort-helper pred (cdr ls)))
            (qsort-helper pred (cdr ls))))))

(define qsort
  (lambda (pred ls)
    (if (null? ls)
        '()
        (append
         (qsort pred (qsort-helper (lambda (x) (pred x (car ls))) (cdr ls)))
         (list (car ls)) 
         (qsort pred (qsort-helper (lambda (x) (not (pred x (car ls)))) (cdr ls)))))))


(define sort-list-of-symbols
  (lambda (lst)
    (let([strings (map symbol->string lst)])
      (map string->symbol (sort strings string<?)))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))