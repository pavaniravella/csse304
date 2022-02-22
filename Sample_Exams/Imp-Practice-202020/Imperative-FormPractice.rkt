#lang racket
(require "chez-init.rkt")
(define-datatype kontinuation kontinuation? ; copied here so you don't have to scroll so much
  [init-k]
  [list-k]
  [cdr-union-k (car-s1 symbol?)
	       (s2 (list-of? symbol?)) ; and it's a set
	       (k kontinuation?)]
  [union-memq-k (car-s1 symbol?)
		(cdr-union list?)
		(k kontinuation?)]
  [all-syms-k  (car-rel (list-of? symbol?))
	       (k kontinuation?)]
  [all-union-k (caar symbol?)
	       (k kontinuation?)]
  [ref-pairs-k (car-rel (list-of? symbol?))
	       (k kontinuation?)]
  [ref-all-k  (rel (list-of? (list-of? symbol?)))
	       (k kontinuation?)]
  [ref-ref-pairs-k (all-syms (list-of? symbol?))
		   (k kontinuation?)])

; You may not need all of these variables, and you may need others. 
; These are here to help you quickly begin to think of what is needed.
(define k 'unused) 
(define v 'unused)
(define rel-ref 'unused)    ; relation variable for "argument" to reflexive?-imp
(define rel-all 'unused)    ; relation variable for "argument" to all-syms-imp
(define rel-pairs 'unused)  ; relation variable for "argument" to reflexive-pairs-imp
(define s1 'unused)         ; first "argument" to union-imp
(define s2 'unused)         ; second "argument" to union-imp
(define sym-memq 'unused)   ; sym "argument" to memq-imp
(define ls 'unused)         ; ls :arument" to memq-imp

; The calls to trace-it will do nothing unless you set the global variable *tracing* to #t.
; This makes it easy to turn tracing on and off.
; I suggest that you leave all of the calls to trace-it in place, just in case you need to use it.
; I improved (I hope you'll agree) trace-it so that instead of printing all of the global 
; variables each time, it prints only the ones that are relevant to the current call (I always print k).

(define apply-k-imp
  (lambda ()
    (trace-it "apply-k")
    (cases kontinuation k
	   [init-k () v] ; v is final answer
	   [list-k () (list v)]
	   [cdr-union-k (car-s1 s2  k1) ; I did this one for you.
	      (set! sym-memq car-s1)
	      (set! ls s2)
	      (set! k (union-memq-k car-s1 v k1))
	      (memq-imp)]
	   [union-memq-k (car-s1 cdr-union k1)  ; I did this one for you.
	      (set! v (if v
			  cdr-union
			  (cons car-s1 cdr-union)))
	      (set! k k1)
	      (apply-k-imp)]
	   [all-syms-k (car-rel k1)
	      (begin
                (set! k (all-union-k (car-rel) k1))
                (set! s1 (list (cadr car-rel)))
                (set! s2 v)
                (union-imp)
                )]
	   [all-union-k (caar k1)
	      (begin
                (set! s1 (list caar))
                (set! s2 v)
                (set! k k1)
                (union-imp)
                )]

	   [ref-pairs-k (car-rel k1)
	      (begin
                (set! k k1)
                (set! v (if (equal? (car car-rel) (cadr car-rel))
				       (cons car-rel v)
				       v))
                (apply-k-imp)
                )] 

	   [ref-all-k (rel k1)
	      (begin
                (set! rel-pairs rel)
                (set! k (ref-ref-pairs-k v k1))
                (reflexive-pairs-imp)
                )]

	   [ref-ref-pairs-k (all-syms k1)
	      (begin
                (set! k k1)
                (set! v (= (length all-syms) (length v)) )
                (apply-k-imp)
                )] )))

(define reflexive?-imp
  (lambda () ;rel-ref
    (trace-it "reflex? ")
    (begin
      (set! rel-all rel-reff)
      (set! k (ref-all-k rel-all k))
      (all-syms-imp)
      )
    ))

(define reflexive-pairs-imp
  (lambda ()
    (trace-it "pairs   ")
    
    (if (null? rel-pairs)
        (begin
          (set! v '())
	(apply-k-imp)
          )
        (begin
          (set! k (ref-pairs-k (car rel-pairs) k))
          (set! rel-pairs (cdr rel-pairs))
          (reflexive-pairs-imp)
          )
	    )))

(define all-syms-imp
  (lambda ()
    (trace-it "all-syms")
    (if (null? rel-all)
        (begin
          (set! v '())
          (apply-k-imp)
          )
        (begin
          (set! k (all-syms-k (car rel-all) k))
          (set! rel-all (cdr rel-all))
          (all-syms-imp)
          )
    )))

(define union-imp ; I did this one for you.
  (lambda ()
    (trace-it "union   ")
    (if (null? s1)
	(begin (set! v s2)
	       (apply-k-imp))
	(begin (set! k (cdr-union-k (car s1) s2 k))
	       (set! s1 (cdr s1))
	       (union-imp)))))

(define memq-imp ; I did this one for you.
  (lambda ()
    (trace-it "memq    ")
    (cond [(null? ls) (set! v #f) (apply-k-imp)]
	  [(eq? (car ls) sym-memq) (set! v #t) (apply-k-imp)]
	  [else (set! ls (cdr ls)) (memq-imp)])))

(define test
  (lambda (rel)
    (set! rel-ref rel)
    (set! k (init-k))
    (reflexive?-imp)))


; -----------------------------  tracing

(define *tracing* #f)

(define trace-it
  (lambda (sym)
    (when *tracing*
      (printf "~a " sym)
      (when (string=? sym "reflex? ") (printf "rel-ref=~a " rel-ref))
      (when (string=? sym "all-syms") (printf "rel-all=~a " rel-all))
      (when (string=? sym "pairs   ") (printf "rel-pairs=~a " rel-pairs))
      (when (string=? sym "union   ") (printf "s1=~s " s1))
      (when (string=? sym "union   ") (printf "s2=~s " s2))
      (when (string=? sym "memq    ") (printf "sym-memq=~s " sym-memq))
      (when (string=? sym "memq    ") (printf "ls=~s" ls))
      (unless (string=? sym "apply-k") (printf "~%         "))
      (when (string=? sym "apply-k")  (printf " v=~s " v))
      (printf "k=~s~%" k))))


; some tests:

'(test '())
'(test '((a a)))
'(test '((a b)))



  
