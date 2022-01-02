#lang scheme
(define (member-n? sym n ls)
  (let ([num (occurence-count sym ls)]
        )
    (cond
      [(= n num) #t]
      [(< n num) #t]
      [else
       #f
       ]
      )
    ))
(define (occurence-count sym ls)
  (cond
    [(null? ls) 0]
    [(equal? sym (car ls)) (+ 1 (occurence-count sym (cdr ls)))]
    [else
     (occurence-count sym (cdr ls))
     ]
    )
  )

(define (opposites-attract ls)

  (map list ls (reverse ls))
  )

#| ask JOrdan about this |#
(define (symmetric? ls)
  (andmap (lambda (x) (not (not (member (list (car ls) (cadr ls)) ls))) ls)
          ))
