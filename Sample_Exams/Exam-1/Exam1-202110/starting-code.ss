#lang scheme 
(define (alternating-reverse lol)
  (alternate-reverse-helper lol true)
  )
(define (alternate-reverse-helper lol should-rev)
  (cond
    [(null? lol) '()]
    [(equal? #t should-rev) (cons (reverse (car lol)) (alternate-reverse-helper (cdr lol) false))]
    [else
     (cons (car lol) (alternate-reverse-helper (cdr lol) true))
     ]
    )
  )
(alternating-reverse '())
(alternating-reverse '((a b c) (d e) (f g h) (i j)))
(alternating-reverse '((a b) () (c d) (e) (f g) (h) (i j)))

(define (opposites-attract m) (display "boo"))

(define (lower-triangle? m)
  ()
  )
(define (lower-triangle-helper m r c)

)
(define matrix-ref  ; helper from the homework
  (lambda (m row col)
    (list-ref (list-ref m row) col)))
(define (make-bst)
  (lambda ()

    )
  )
;--------- BST procedures fromm HW7, some slightly modified  -------------

(define BST-node list)

(define (BST-leafnode n) ; make a new leaf node
  (BST-node n (empty-BST) (empty-BST)))


; Accessors for part of a node
(define BST-element car)
(define BST-left cadr)
(define BST-right caddr)


; empty tree functions
(define (empty-BST) '())   ; make one
(define empty-BST? null?)  ; test one
(define (tree? obj) (list? obj))
(define (BST? obj)  ; Is this object a BST?
  (or (empty-BST? obj)
      (and (tree? obj) 	
           (let ([inorder-list (BST-inorder obj)])
             (and (set? inorder-list) 
                  (sorted? inorder-list))))))

(define (BST-insert num bst)
  (cond [(empty-BST? bst) (BST-leafnode num)]
        [(not (integer? num)) 
         (error 'BST-insert 
                "attempt to insert non-integer into BST")]
        [(= num (BST-element bst)) bst]
        [(< num (BST-element bst))
         (BST-node (BST-element bst) 
                   (BST-insert num 
                               (BST-left bst)) 
                   (BST-right bst))]
        [else 	 
         (BST-node (BST-element bst) 
                   (BST-left bst) 
                   (BST-insert num 
                               (BST-right bst)))]))



(define (BST-preorder bst)
  (if (empty-BST? bst)
      '()
      (append (list (BST-element bst)) 
              (BST-preorder (BST-left bst))
              (BST-preorder (BST-right bst)))))

(define (BST-inorder bst)
  (if (empty-BST? bst)
      '()
      (append (BST-inorder (BST-left bst)) 
              (list (BST-element bst))
              (BST-inorder (BST-right bst)))))

(define (BST-contains? bst num)
  (cond [(empty-BST? bst) #f]
        [(= (BST-element bst) num) #t]
        [(< (BST-element bst) num) (BST-contains? (BST-right bst) num)]
        [else (BST-contains? (BST-left bst) num)]))

(define (BST-height bst)
  (if (empty-BST? bst)
      -1
      (+ 1 (max (BST-height (BST-left bst))
                (BST-height (BST-right bst))))))


