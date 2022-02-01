#lang racket
(require "chez-init.rkt")
(define-syntax my-do
  (syntax-rules (while)
    [(_ (loop-bodies ...) while test)
       (let while-loop ()
         loop-bodies ...
         (when test
             (while-loop)))]))
