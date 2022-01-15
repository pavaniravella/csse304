#lang racket
(define compose2 
  
  (lambda (f g)
    (displayln "bobb")
    ;(displayln "lambda f g")
    (lambda (x)
     (displayln "lambda x")
      (f (g x)))))
(define h
  (let ([g (lambda (x) (displayln "x")(+ 1 x))]
        [f (lambda (y) (displayln "y")(* 2 y))])
        (displayln "compose2")(compose2 g f)))
(displayln "making the call to h")
(h 4)