#lang racket
;Pavani Ravella HW 0
(provide fact)

; recursive factorial procedure
; parameter n: must be a non-zero integer

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))
