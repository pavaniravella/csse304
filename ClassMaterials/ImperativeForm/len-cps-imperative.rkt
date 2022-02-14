#lang racket
;; Register definitions:
(require "chez-init.rkt")

(define *k* #f)		; the current-continuation register
(define *L* #f)	    ; the current list-argument register
(define *v* #f)		; the result register

(define-datatype continuation continuation?
  [add1-k
   (k (lambda (x) (or (procedure? x) (continuation? x)))
      )])
;; v3 name it like a registerized version
#;
(define (len/cps *L* *k*)
  (cond
    [(null? *L*) (apply-k *k* 0)]
    [else
     (len/cps (cdr *L*) (add1-k *k*))]))

;; Convert apply-k to a thunk:
(define apply-k 
  (lambda ()
    (if (procedure? *k*)
        (*k* *v*) 
        (cases continuation *k*
          [add1-k (cont)
                  (begin (set! *k* cont)
                         (set! *v* (add1 *v*)))
                  (apply-k)]
          ))))

;; v4 convert len/cps to a thunk
;; Preconditions for len/reg:
;;   1) *k* contains the current continuation
;;   2) *L* contains the argument to len/reg
;; Postcondition:
;;   - *v* contains the result before apply-k is called
(define (len/reg)
  (cond
    [(null? *L*)
     (begin (set! *v* 0)
            (apply-k))]
    [else
     (begin (set! *k* (add1-k *k*))
            (set! *L* (cdr *L*)))
     (len/reg)]))
     
(set! *L* '(1 2 3 4 5))
(set! *k* displayln)
(len/reg)