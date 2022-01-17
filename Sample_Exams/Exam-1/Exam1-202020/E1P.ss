#lang scheme
;EXAM 202020
(define (prefix-helper lon prev)
  (cond
    [(null? lon) '()]
    ; [(null? (cdr lon)) (car lon)]
    [else
     (cons (+ (car lon) prev) (prefix-helper (cdr lon) (+ (car lon) prev)))
     ]
    )
  )
(define (prefix-sums lon)
  (prefix-helper lon 0)
  )

#|
 (4)
(1 4 9 11)
(1 3 6 10 15 21)
(3 5 5 0 6)
|#
;(prefix-sums '(4))
;(prefix-sums '(1 3 5 2))
;(prefix-sums '(1 2 3 4 5 6))
;(prefix-sums '(3 2 0 -5 6))

(define (suffix-sums lon)
  (cond
    [(null? (cdr lon)) lon]
    [else
     (cons (+ (car lon) (car (suffix-sums (cdr lon)))) (suffix-sums (cdr lon)))
     ]
    )
  )
#|
(4)
		     (11 10 7 2)
		     (21 20 18 15 11 6)
		     (6 3 1 1 6)
|#
(suffix-sums '(4))
(suffix-sums '(1 3 5 2))
(suffix-sums '(1 2 3 4 5 6))
;(suffix-sums '(3 2 0 -5 6))
(define (even-odds-helper ls even odd)
  (cond
    [(null? ls) (list even odd)]
    [(null? (cdr ls)) (list (cons (car ls) even) odd)]
    [else
     (even-odds-helper (cddr ls) (cons (car ls) even) (cons (cadr ls) odd) )
     ]
    )
  )
(define (evens-odds ls)
  (even-odds-helper ls '() '())
  )
(evens-odds '())
(evens-odds '(b))
(evens-odds '(c d))
(evens-odds '(a b c d e f g))
(evens-odds '(b c d e f g))
(define (notate-depth-and-flatten slist)
  (cond
    [(null? slist) '()]
    [(symbol? (car slist)) (list 1 2)]
    )
  )
(define (free-occurreence-count exp)
  exp
  )

