#lang scheme
; (ins-sort lon) returns a sorted list of the nubers 
; that are in lon. Because it is easier to add to the front 
; of a Scheme list than to the end, the sorting starts at the 
; end of the list.
(define ins-sort
  (lambda (lon) ; fill in the details
    (if (or (null? lon) (null? (cdr lon)))
        lon
        (let ([sorted-cdr (ins-sort (cdr lon))])
          (insert (car lon) sorted-cdr)
          )
        )
    ))

(define insert
  (lambda (n sorted-list) ; fill in the details
    (cond
      [(null? sorted-list) (list n)]
      [(<= n (car sorted-list)) (cons n sorted-list)]
      [else (cons (car sorted-list) (insert n (cdr sorted-list)))]
      )
    ))


(ins-sort '())
(ins-sort '(3 7 -2 9 7 6 4))

