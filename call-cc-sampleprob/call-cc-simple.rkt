#lang racket
(+ 2 3)
(+ 2 (call/cc (lambda (k) 3)))
(+ 2 (call/cc (lambda (continuation) (continuation 3))))
(+ 2 (call/cc (lambda (continuation) (continuation 3) 6)))
(define xxx -)
(* 7 (call/cc (lambda (k)
                (set! xxx k)
                (+ 3 (k 4)))))
(+ 5 (xxx (xxx 2)))
(define *k* 'unused)
(+ (call/cc
    (lambda (k)
      (begin
        (set! *k* k)
        
      (k (* 3 4))))) 5)
(call/cc
 (lambda (k)
   (/ 5 (k 0))))
(call/cc
 (lambda (k)
   (error (k "foo"))))

(let ([x (call/cc (lambda (k) k))])
  (x (lambda (ignore) "hi")))

(let ([x (call/cc (lambda (k) k))])
  (x (lambda (ignore) "hi")))
(lambda (p)
  (let ([x p])
    (x (lambda (ignore) p))))

(require racket/trace)
(define abc #f)
(define fact
    (lambda (n)
      (cond
        [(= n 1)
         (call/cc (lambda (k)
                    (set! abc k)
                    (k 1)))]
        [else
         (* n (fact (- n 1)))])))
(trace fact)
(fact 4)
(displayln "abcdef")
(abc 2)
