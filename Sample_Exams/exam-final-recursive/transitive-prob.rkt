#lang racket
(define (direct-relations relation element))
(define (tranisitive-closure rel)
  (cond
    [(null? rel) '()]
    
    )
  )
(transitive-closure '())
;()
(transitive-closure '((a a) (a b)))  ; an answer of ((a b) (a a)) is also correct for this case.
;((a a) (a b))
 (transitive-closure '((a a) (a b) (b c)))
;((a a) (b c) (a b) (a c))
 (transitive-closure '((a b) (b c) (d a)))
;((a b) (b c) (d a) (a c) (d c) (d b))
 (transitive-closure '((a b) (b c) (d a) (c a)))
;((a c) (b a) (a b) (d a) (d c) (c a) (c c) (b c) (b b) (a a) (c b) (d b))
 (transitive-closure '((a b) (b c) (a c)))
;((a b) (b c) (a c))
(transitive-closure '((a b) (b c) (d a) (c a) (e d)))
;((e d) (a c) (b a) (a b) (d a) (d c) (c a) (c c) (e c) (e b) (b c) (b b) (a a) (e a) (c b) (d b))
(transitive-closure '((a b) (c b)))
;((a b) (c b))
(transitive-closure '((a a) (a b) (b a)))
;((a b) (a a) (b b) (b a))
 (transitive-closure '((a b) (b a)))