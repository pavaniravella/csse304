(load "chez-init.ss")


; original form.  Throughout this problem, snoc will not be counted as a substantial procedure.

(define snoc
  (lambda (ls obj)
    (append ls (list obj))))

(define slist-reverse
  (lambda (slist)
    (cond [(null? slist) '()]
	  [(symbol? (car slist))
	   (snoc (slist-reverse (cdr slist))
		 (car slist))]
	  [else (snoc (slist-reverse (cdr slist))
		      (slist-reverse (car slist)))])))

; (trace slist-reverse snoc)
; You probably don't need these, but they are here just in case.

;; (slist-reverse '())
;; (slist-reverse '(()))
;; (slist-reverse '(a b))
;; (slist-reverse '(a ()))
;; (slist-reverse '((a b) (c (d))))
;; (slist-reverse '(((a ((()))))))

; CPS with scheme procedure continuations. write slist-reverse-proc and the needed continuations.
; Must be in tail form to get points, even if the server gives you full credit.

(define make-k-proc (lambda (k) k))
(define apply-k-proc (lambda (k v) (k v)))

(define slist-reverse-proc
  (lambda (slist k)
    #f ; replace with real code
    ))

(define id (lambda (k) k))

; Test cases

;; > (slist-reverse-proc '() (make-k-proc id))
;; ()
;; > (slist-reverse-proc '(()) (make-k-proc id))
;; (())
;; > (slist-reverse-proc '(a b) (make-k-proc list))
;; ((b a))
;; > (slist-reverse-proc '(a ()) (make-k-proc list))
;; ((() a))
;; > (slist-reverse-proc '((a)) (make-k-proc id))
;; ((a))
;; > (slist-reverse-proc '((a b) (c (d))) (make-k-proc reverse))
;; ((b a) ((d) c))
;; > (slist-reverse-proc '(((a ((()))))) (make-k-proc id))
;; (((((())) a)))


					;
;data structures continuations.  You must add the other variants.

(define-datatype continuation continuation?
  [id-k]
  [list-k]
  [reverse-k]

  )


(define apply-k-ds
  (lambda (k v)
    (cases continuation k
	   [id-k () v]
	   [list-k () (list v)]
	   [reverse-k () (reverse v)]  ; add the needed cases
	   )))

(define slist-reverse-ds
  (lambda (slist k)
    #f ; replace with real code
    ))

'(trace slist-reverse-ds apply-k-ds snoc)

;; > (slist-reverse-ds '()  (id-k))
;; ()
;; > (slist-reverse-ds '(())  (id-k))
;; (())
;; > (slist-reverse-ds '(a b)  (list-k))
;; ((b a))
;; > (slist-reverse-ds '(a ())  (list-k))
;; ((() a))
;; > (slist-reverse-ds '((a))  (id-k))
;; ((a))
;; > (slist-reverse-ds '((a b) (c (d)))  (reverse-k))
;; ((b a) ((d) c))
;; > (slist-reverse-ds '(((a ((()))))) (id-k))
;; (((((())) a)))


; imperative form. If you need to debug, you may want to add trace-it code like 
; in the imperative code from the Live-in-class folder.

(define slist)
(define k)
; add any other needed global variables.

(define apply-k-imp
  (lambda ()
    (cases continuation k
	   [id-k () v]
	   [list-k () (list v)]
	   [reverse-k () (reverse v)] ; add the other needed cases
	  )))

(define slist-reverse-imp
  (lambda () 
    #f ; replace with real code
    ))

;; > (begin (set! slist '()) (set! k (id-k)) (slist-reverse-imp))
;; ()
;; > (begin (set! slist '(())) (set! k (id-k)) (slist-reverse-imp))
;; (())
;; > (begin (set! slist '(a b)) (set! k (list-k)) (slist-reverse-imp))
;; ((b a))
;; > (begin (set! slist '(a ())) (set! k (list-k)) (slist-reverse-imp))
;; ((() a))
;; > (begin (set! slist  '((a))) (set! k (id-k)) (slist-reverse-imp))
;; ((a))
;; > (begin (set! slist '((a b) (c (d)))) (set! k (reverse-k)) (slist-reverse-imp))
;; ((b a) ((d) c))
;; > (begin (set! slist '(((a ((())))))) (set! k(id-k)) (slist-reverse-imp))
;; (((((())) a)))
