#lang racket
(define-syntax my-let
  (syntax-rules ()
    [(_([var val] ...) e1 e2 ...)
     ((lambda (var ...) e1 e2 ...))
     val ...
     ]))

(my-let ([a 3] [b 5])
        (my-let ([c (+ a b)])
                (+ c 4)))
		
; (my-if x then y else z)

; (define-syntax my-if
;  (syntax-rules 

(my-if (< 3 4) then 5 else 6)
(my-if (< 3 2) then 5 else 6)
(my-if (< 3 2) then 5)
(list (my-if (< 3 2) then 5))

(define-syntax ++
  (syntax-rules (then else)
    [(_ e1 then e2 else e3)
     (if e1
         e2
         e3)]
    [(_e1 then e2)
     (when e1
       e2)]))


(define a 5)
(++ a)
a
(define b (+ 3 (++ a)))
b
a

(define-syntax ++post
  (syntax-rules ()
    [(_a)
     (begin
     (set! a (add1 a)) a)
     ]))
 

(define b (+ 3 (++post a)))
b
a
(++ (* a 2))

 (define-syntax my-and
  (syntax-rules ()
    [(_) #t]
    [(_ e1) e1]
    [(_ e2 e2 ...)
     (if e1 (my-and e2 ...) #f)]))

(my-and)
(my-and 4)
(my-and 4 #f 6)
(my-and 4 5 6)
(expand '(my-and 4 5 6))

(define-syntax for
  (syntax-rules (:)
    [(_ ((defines ...) : test : updates ...) body ...)
     (begin
       defines ...
       (let for-loop ()
         (if test
             (begin bodies ...
                    updates ...
                    (for-loop)))))]))

(for (((define i 0) (define j 1)) :
                                  (< i 12) :
                                  (++ i) (set! j (* 2 j)))
  (display i)
  (display "  ")
  (display j)
  (newline))
