#lang racket
(define (max-contiguous-nonempty-subsequence-sum ls)
  (define (helper ls acc)
    (cond
      [(null? ls) 0]
      [(null? (rest ls)) (first ls)]
      [(= (length ls) 2)
       (max (first ls) (second ls) (+ (first ls) (second ls)))
       ]
      [else
       (max )
      ]
      [else
       (display "boo")
       ])))
(define (one-element-list numbers)
  (map list numbers)
)

(> (current-sum + (first element)) current-sum)
(max-contiguous-nonempty-subsequence-sum '())
(max-contiguous-nonempty-subsequence-sum '(-5))
(max-contiguous-nonempty-subsequence-sum '(1 2))
(max-contiguous-nonempty-subsequence-sum '(1 -2))
(max-contiguous-nonempty-subsequence-sum '(-5)) 
(max-contiguous-nonempty-subsequence-sum '(-3 4 -2)) 
(max-contiguous-nonempty-subsequence-sum '(-2 4 -2 6 -1 3 -18 5 3))
(max-contiguous-nonempty-subsequence-sum '(-7 -6 -4 -8)) 
(max-contiguous-nonempty-subsequence-sum '(-34980498509458045230982382802 -6349302123049834918120 -13498049850945804523098238280 ))
(max-contiguous-nonempty-subsequence-sum '(-2 11 -4 13 -5 2))
(max-contiguous-nonempty-subsequence-sum '(1 -3 4 -2 -1 6))
(max-contiguous-nonempty-subsequence-sum '(1 2 -1 3 -1 4 5 -1 6 7 -1 8 9 -1 10))

(define sampleExam-tests
  (test-suite
   "sample exam"
   (test-suite
    "max-contiguous-nonempty-subsequence-sum"
    (test-case "'(-5)"
      (check-equal?
       (max-contiguous-nonempty-subsequence-sum '(-5)) -5))
    (test-case "'(5)"
      (check-equal?
       (max-contiguous-nonempty-subsequence-sum '(5)) 5))
    (test-case "'(1 2)"
      (check-equal?
       (max-contiguous-nonempty-subsequence-sum '(1 2)) 3))
    (test-case "'(2 1)"
      (check-equal?
       (max-contiguous-nonempty-subsequence-sum '(2 1)) 3)))))

(require rackunit/text-ui)
(run-tests sampleExam-tests 'verbose)