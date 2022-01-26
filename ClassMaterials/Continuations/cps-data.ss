#lang scheme
; ------------------------------------------------------
; CONTINUATIONS AS VARIANT RECORDS using define-datatype.
; ------------------------------------------------------

(load "chez-init.ss")
(define scheme-value? (lambda (x) #t))
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

; A helper procedure that will be useful:
(define exp?     ; Is obj a lambda-calculus expression? This uses
  (lambda (obj)  ; our original simple definition of lc-expressions.
    (or (symbol? obj)
	(and (list? obj)
	     (or 
	      (and (= (length obj) 3)
		   (eq? (1st obj) 'lambda)
		   (list? (2nd obj))
		   (= (length (2nd obj)) 1)
		   (symbol? (caadr obj))
		   (exp? (3rd obj)))
	      (and (= (length obj) 2)
		   (exp? (1st obj))
		   (exp? (2nd obj))))))))

(define-datatype continuation continuation?
  [init-k] ; These first continuation variants need no fields.
  [list-k]
  [not-k]
  [fact-k (n integer?)
	  (k continuation?)]
  [copy-k (car-L scheme-value?)
	  (k continuation?)]
  
  )

(define apply-k
  (lambda (k v)
    (cases continuation k
     [init-k () v]
     [list-k () (list v)]
     [not-k () (not v)]
     [fact-k (n k)
	(apply-k k (* n v))]
     [copy-k (car-L k)
	     (apply-k k (cons car-L v))]
     
     )))

(define fact-cps
  (lambda (n k)
    (if (zero? n)
	(apply-k k 1)
	(fact-cps (- n 1)
		  (fact-k n k)))))

(fact-cps 5 (init-k))
(fact-cps 6 (list-k))

(define list-copy-cps
  (lambda (L k)
    (if (null? L)
	(apply-k k '())
	(list-copy-cps (cdr L)
		       (copy-k (car L) k)))))

(list-copy-cps '(1 2 3) (list-k))

(define memq-cps
  (lambda (sym ls k)
    (cond [(null? ls)          
	   (apply-k k #f)]
	  [(eq? (car ls) sym)
	   (apply-k k #t)]
	  [else (memq-cps sym (cdr ls) k)])
	 ))

(memq-cps 'a '(b c a d) (list-k))
(memq-cps 'a '( b c d) (not-k))

; Copy the previous intersection-cps here, and rewrite
; it using data-structure continuations.




(intersection-cps
 '(a d e g h) '(s f c h b r a) (list-k))

(trace intersection-cps apply-k list-k int-memq-k cdr-intersection-k)

(intersection-cps
 '(a d e g h) '(s f c h b r a) (list-k))



; This is my solution to the free-vars problem from A10.
; It was for the original lambda-calculus expressions where lambdas
; have only one parameter and applications have only one operand.

(define free-vars ; convert to CPS.  We will first convert 
  (lambda (exp)   ; union and remove.
    (cond [(symbol? exp) (list exp)]
	  [(eq? (1st exp) 'lambda)       
	   (remove (car (2nd exp)) 
		   (free-vars (3rd exp)))]      
	  [else (union (free-vars (1st exp))
		       (free-vars (2nd exp)))])))


(define free-vars-cps ; convert to CPS
  (lambda (exp k)
    (cond [(symbol? exp) ;fill it in
	   ]
	  [(eq? (1st exp) 'lambda) ; fill it in
	   ]
	  [else ; fill it in
	   
	   ])))
					     
(free-vars-cps '(a (b ((lambda (x)
			 (c (d (lambda (y)
				 ((x y) e)))))
		       f)))
	       (init-k))

(trace free-vars-cps free-vars-lambda-k rator-k rand-k
       union-cps remove-cps cdr-union-k union-memq-k
       rem-cdr-k apply-k)

(free-vars-cps '(a (b ((lambda (x)
			 (c (d (lambda (y)
				 ((x y) e)))))
		       f)))
	       (init-k))


(define union-cps ; assumes that both arguments are sets of symbols
  (lambda (s1 s2 k)
    (if (null? s1) ; fill it in
	
)))

(union-cps '(3 1 11 6 8 4) '(5 1 8 9 2) (make-k list))

(define remove-cps ; removes the first occurrence of element in ls
  (lambda (element ls k)
    (if (null? ls) 
	; fill it in
	)))

(remove-cps 'a '(b c e a d a a ) (init-k))
(remove-cps 'b '(b c e a d a a ) (init-k))