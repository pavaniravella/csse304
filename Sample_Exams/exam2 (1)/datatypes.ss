
;; Parsed expression datatype.  You will probably want to replace this
;; with your expression datatype from A11b.

; (define-datatype expression expression?
;   [var-exp        ; variable references
;    (id symbol?)]
;   [lit-exp        ; "Normal" data.  Did I leave out any types?
;    (datum
;     (lambda (x)
;       (ormap 
;        (lambda (pred) (pred x))
;        (list number? vector? boolean? symbol? string? pair? null?))))]
;   [app-exp        ; applications
;    (rator expression?)
;    (rands (list-of expression?))]  
;   )
;from 11b
(define (lit-exp? e)
(or (null? e) (number? e) (string? e) (vector? e) (boolean? e))
)


; return the entire list except the last element
(define exceptLast
  (lambda (len n il)
    (cond
      [(equal? n len) '()]
      [#t (cons (car il) (exceptLast len (+ n 1) (cdr il)) )]
    )
  ))

(define-datatype expression expression?

  [namespace-record
    (syms (list-of symbol?))
    (vals (list-of expression?))
  ]

  [simplecase-exp
    (check expression?)
    (toCheck (list-of symbol?))
    (bodies (list-of expression?))
  ]

  [define-exp 
    (ids (list-of symbol?))
    (vals (list-of expression?))
    (bodies (list-of express?))
  ]

  [var-exp
   (id symbol?)]
  [lambda-exp
   (id symbol?)
   (bodies (list-of expression?))]

  [lambdaImproper-exp
    (ids (list-of symbol?))
    (arbitary symbol?)
    (bodies (list-of expression?))
  ]

  [app-exp
  (rator expression?)
  (rands rands?)]


  [let-exp
  ; (vars (list-of (list-of symbol?)))
  (vars (list-of symbol?))
  (varExp (list-of expression?))
  (bodies (list-of expression?))
  ]

  [let*-exp ;not sure
   (vars (list-of symbol?))
  (varExp (list-of expression?))
  (bodies (list-of expression?))
  ]

  [named-let
    (id symbol?)
    (param (list-of symbol?))
    (exprs (list-of expression?))
    (bodies (list-of expression?))
  ]
  
  [letrec-exp ;not sure
  (vars (list-of symbol?))
  ; (varExp (list-of expression?))
  ; (bodies (list-of expression?))
  (idss (list-of (list-of symbol?)))
  (bodiess (list-of (list-of expression?)))
  (bodies (list-of expression?))
  ]

  [if-exp
  (pred expression?)
  (then expression?)
  ]
  [if-else-exp
  (pred expression?)
  (then expression?)
  (else expression?)
    ]
  [set!-exp
  (id symbol?)
  (val-exp expression?)
  ]

  [lit-exp
  (id lit-exp?)
  ]

  [list-lambda-exp
  (ids (list-of symbol?))
  (bodies (list-of expression?))
  ]

  ; NEW EXPS for assignment 14
  [and-exp 
    (bodies (list-of expression?))
  ]
  [or-exp
    (bodies (list-of expression?))
  ]

  [cond-exp 
    (pred (list-of expression?))
    (exps (list-of (list-of expression?)))
  ]

  [begin-exp
    (bodies (list-of expression?))
  ]

  [case-exp
    (check symbol?)
    (procs (list-of expression?))
    (bodies (list-of (list-of expression?)))
  ]

  [while-exp 
    (pred expression?) 
    (bodies (list-of expression?))
  ]

  [for-to-do-exp
    (id symbol?)
    (start expression?)
    (end expression?)
    (toDo (list-of expression?))
  ]
  
  [for-downto-do-exp
    (id symbol?)
    (start expression?)
    (end expression?)
    (toDo (list-of expression?))
  ]

  )

 	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?))

  (extend-env-recursive
    (proc-names (list-of symbol?))
    (ids (list-of (list-of symbol?)))
    (bodies (list-of (list-of expression?)))
    (env environment?)
  )

  (namespace-record-env
  (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (old-env environment?)

  )


)


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
	[closure 
    (ids (list-of symbol?) )
    (bodies (list-of expression?))
    (env environment?)
  ]
  [closureImproper
    (ids (list-of symbol?))
    (arbitary symbol?)
    (bodies (list-of expression?))
    (env environment?)
  ]
  [closureAbitrary 
    (id symbol?)
    (bodies (list-of expression?))
    (old-env environment?)
  ]
  

  )
	 
	 
