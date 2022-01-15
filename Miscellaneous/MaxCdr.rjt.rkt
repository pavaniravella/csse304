#! /usr/bin/env racket
#lang racket

(require rackunit "MaxContigousSubSum.rkt")

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
