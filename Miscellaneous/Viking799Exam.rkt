#lang racket
;https://github.com/viking799/CSSE304/blob/master/E1-tests.ss
(define (max-diff ls)
  (- (apply max ls) (apply min ls)))

;expected : 0, 6, 0, 8
;(max-diff '(7))
;(max-diff '(8 4 5 2))
;(max-diff '(9 9 9 9 9))
;(max-diff '(3 2 1 4 7 8 2 9 1))

#|ASK JORDAN|#
(define (check-frequency ls sym)
  (cond
    [(null? ls) 0]
    [(equal? (first ls) sym) (+ 1 (check-frequency (rest ls) sym))]
    [#t
     (check-frequency (rest ls) sym)]
    )
  )
;; List[Sym], Symbol, Int --> Bool 
(define (member-n-times? ls sym freq)
  (= freq (check-frequency ls sym)))

(check-frequency '(a b c b d b d) 'b )
(check-frequency '(a b c d d b d) 'b )
(check-frequency '() 'b )
(check-frequency '(a) 'b)
(member-n-times? '(a b c b d b d) 'b 2)
(member-n-times? '(a b c b d b) 'b 3)
;(member-n-times? 'b '(a b c b d b) 4)
;(member-n-times? 'b '() 0)
;(member-n-times? 'b '() 1)

#| ASK JORDAN |#
(define (most-frequent ls)
  (cond
    [(null? ls) #f]
    [else
     (most-freq-helper ls)
     ]
    )
  )

;;none empty list
;;PURPOSE: return the element that occurs the most frequently in ls 
(define (most-freq-helper ls)
  (cond
    [(null? (rest ls)) (first ls)]
    [else
     (let ([result (most-freq-helper (rest ls))])
       (if (> (check-frequency ls (first ls)) (check-frequency (rest ls) result))
           (first ls)
           result
           ))]))

#| correct '(
		     #f
		     3
		     -2
		     4
		     4
		     1
		     -999
		     )|#
(most-frequent '())
(most-frequent '(3))
(most-frequent '(-2 3 -2))
(most-frequent '(4 3 2 1 1 2 1 4 5 4 2 4 5 4 6))
(most-frequent '(5 4 5 3 5 2 1 1 2 1 4 5 4 2 4 5 4 6))
(most-frequent '(2 1 3 2 1 3 2 1 3 2 1 3))
(most-frequent '(-70 63 49 -82 -999 55 -999 47 -82 -999 5))
(define (slist-same? slist1 slist2)
  (cond
    [(and (null? slist1) (null? slist2)) #t ]
    [(or (null? slist1) (null? slist2)) #f ]
    [else
     (and (slist-same?-sexp (first slist1) (first slist2)) (slist-same? (rest slist1) (rest slist2)))
     ]
    
    )
  )
(define (slist-same?-sexp sexp1 sexp2)
  (cond
    [(and (symbol? sexp1) (symbol? sexp2)) (equal? sexp1 sexp2)]
    [else
     (slist-same? sexp1 sexp2)
     ]
    )
  )
;(slist-same? '() '())
;(slist-same? '(a) '(a) )
;(slist-same? '(a) '(b) )
;(slist-same? '(a) '(b))
;(slist-same? '(a ((c))) '(a ((c ()))))
;(slist-same? '(() a ((c()))) '(() a ((b ()))))

