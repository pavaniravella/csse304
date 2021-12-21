#lang racket

(provide matrix-ref matrix? matrix-transpose filter-in invert pascal-triangle)
(require racket/trace)
(define matrix-ref
  (lambda (m row col)
    (let ([r (list-ref m row)])
      (list-ref r col)
      )))


;; matrix? : Anything -> Bool
(define (matrix? x)
  ;; #t iff:
  ;;   x is a non-empty list
  (and (list? x)(cons? x)
       ;;every element is a cons
       (andmap cons? x)
       ;;   every element of x is a list
       (every-element-list? x)
       ;;   every element of x contains only numbers
       (andmap all-numbers? x)
       ;;   all elements of ls are of equal length
       (all-equal-length? x)
       ))

;;List[X]--> Bool
;;produces #t iff every element of ls is a list 
(define (every-element-list? ls)
  (andmap list? ls))

;; List[X]--> Bool
;;produces #t ##f every element of ls is a number
(define (all-numbers? ls)
  (or (null? ls)
      (and (number? (first ls)) (all-numbers? (cdr ls)))))

;; Listof[Listof[x]] -> Bool
;; produces #t iff every element of ls is of the same length
(define (all-equal-length? ls)
  (andmap (lambda (x) (= (length (first ls)) (length x))) ls)
  )


(define (matrix-transpose m)
  (apply map list m)
  )

(define filter-in
  (lambda (pred x)
    (cond
      [(equal? x '()) '()]
      [(pred (car x)) (cons (car x) (filter-in pred (cdr x)))]
      [else (filter-in pred (cdr x))])))


(define invert
  (lambda (ls)
    (map reverse ls)))

(define fact
  (lambda (n)
    (if (eq? n 0)
        1
        (* n (fact (- n 1))))))



(define choose
  (lambda (m n)
    (/ (fact m) (* (fact n) (fact (- m n))))))



(define create-row
  (lambda (rownum index)
    (if (= rownum index)
        '(1)
        (cons (choose rownum index) (create-row rownum (+ 1 index))))))

(define pascal-triangle
  (lambda (n)
    (if (negative? n)
        '()
        (if (= 0 n)
            '((1))
            (cons (create-row n 0) (pascal-triangle (- n 1)))))))
(trace matrix-transpose)
;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))