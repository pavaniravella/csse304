#lang racket
(define-syntax my-let
  (syntax-rules ()
    [(_ ([var val] ...) body1 body2 ...)
     ((lambda (var ...) body1 body2 ...) val ...)]))

(my-let ([a 3] [b 5])
        (my-let ([c (+ a b)])
                (+ c 4)))
		
; (my-if x then y else z)

(define-syntax my-if
  (syntax-rules (then else)
    [(_ e1 then e2 else e3)
     (if e1 e2 e3)
     ]
    [(_ e1 then e2)
     (if e1 e2)
     ]))

(my-if (< 3 4) then 5 else 6)
(my-if (< 3 2) then 5 else 6)
(my-if (< 3 2) then 5)
(list (my-if (< 3 2) then 5))

(define-syntax ++
  (syntax-rules ()
    [(_ x)
     (begin (set! x (+ 1 x)) x)
     ]))


(define a 5)
(++ a)
a
(define b (+ 3 (++ a)))
b
a

(define-syntax ++post
  (syntax-rules ()
    [(_ x)
     (let ([temp x])
       (++ x)
       temp
       )]))
 

(define b (+ 3 (++post a)))
b
a
(++ (* a 2))

(define-syntax my-and
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2) ...]
    (if e1 (my-and e2 ...) #f)))

(my-and)
(my-and 4)
(my-and 4 #f 6)
(my-and 4 5 6)
(expand '(my-and 4 5 6))

(define-syntax for
  (syntax-rules (:)
    [(_ ((init ...) : test : update ...) body ...)
     (begin
       init ...
       (let for-loop ()
         (if test
             (begin body ...
                    update ...
                    (for-loop)))))]))

(for (((define i 0) (define j 1)) :
                                  (< i 12) :
                                  (++ i) (set! j (* 2 j)))
  (display i)
  (display "  ")
  (display j)
  (newline))