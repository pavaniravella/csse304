#lang racket
(define-syntax my-and
  (syntax-rules ()
    [(_) #t]
    [(_ exp) exp]
    [(_ e1 e2 ...)
     (if e1
	 (my-and e2 ...)
	 #f)]))


(define-syntax my-do
    (syntax-rules ()
      ((_ (body ...) while test)
       (let loop ()
            body ...
            (if test
               (loop))))))