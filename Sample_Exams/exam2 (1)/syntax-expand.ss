; When you write syntax-expand for A14, this could be a good place to put your code.
(define syntax-expand
        (lambda (datum)
        (cases expression datum
                [var-exp (id) datum] 
                [lit-exp (id) datum]
                [app-exp (rator rands) 
                    (app-exp (syntax-expand rator) (map syntax-expand rands))
                ]

                [simplecase-exp (check toCheck bodies) (expandSimpleCase check toCheck bodies)]
                [let-exp (vars varExp bodies) (app-exp (list-lambda-exp vars (map syntax-expand bodies)) (map syntax-expand varExp) )  ]
                [let*-exp (vars varExp bodies) (syntax-expand (let*helper vars varExp bodies))]
                [and-exp (bodies) (andhelper bodies)]
                [or-exp (bodies) (orhelper bodies)]
                [cond-exp (preds exps) (convertCons preds exps)]
                [begin-exp (bodies) 
                    (syntax-expand 
                        (let-exp '() '()
                            (map syntax-expand bodies)))]
                                        ; Bodies is also a list of list of bodies for each condition ( (...) (...) )
                [case-exp (check procs bodies) (expandCase  procs bodies) ]
                [lambda-exp (ids bodies) (lambda-exp ids (map syntax-expand bodies))]
                [list-lambda-exp (ids bodies) (list-lambda-exp ids (map syntax-expand bodies))]
                [lambdaImproper-exp (ids arbitary bodies) (lambdaImproper-exp ids arbitary (map syntax-expand bodies))]
                [if-else-exp (pred then else)
                    (if-else-exp (syntax-expand pred)(syntax-expand then)(syntax-expand else))
                ]
                [letrec-exp (vars idss bodiess bodies) (letrec-exp vars idss (map (lambda (bod) (map syntax-expand bod)) bodiess) (map syntax-expand bodies))]
                [named-let (id param exprs bodies)  (letrec-exp (list id) (list param) (list (map syntax-expand bodies) ) (list (app-exp (var-exp id) (map syntax-expand exprs))) )  ]
                [while-exp (pred bodies) (while-exp (syntax-expand pred) (map syntax-expand bodies))]
                [if-exp (pred then) (if-exp (syntax-expand pred)(syntax-expand then))]
                [namespace-record(syms vals) datum]
                [for-to-do-exp (id start end toDo) 
                
                (syntax-expand 
                (letrec-exp (list 'loop) (list (list id)) 
                (list  (list (if-exp  (app-exp (var-exp '>=) (list end (var-exp id) ))  (begin-exp 
                (append toDo (list  (app-exp (var-exp 'loop) (list  (app-exp (var-exp '+) (list (lit-exp 1) (var-exp id) ) ) ))
                                    
                             )
                )))))
                (list (app-exp (var-exp 'loop) (list 
                    start
                    ; (app-exp (var-exp '+) (list (lit-exp 1) (var-exp id) ) )
                 )))
                )
                
                )
                ]

                [for-downto-do-exp (id start end toDo) 
                 
                (syntax-expand 
                (letrec-exp (list 'loop) (list (list id)) 
                (list  (list (if-exp  (app-exp (var-exp '<=) (list start (var-exp id) ))  (begin-exp 
                (append toDo (list  (app-exp (var-exp 'loop) (list  (app-exp (var-exp '-) (list  (var-exp id) (lit-exp 1)) ) ))
                                    
                             )
                )))))
                (list (app-exp (var-exp 'loop) (list 
                    end
                    ; (app-exp (var-exp '+) (list (lit-exp 1) (var-exp id) ) )
                 )))
                )
                
                )
                ]


                [else (eopl:error 'syntax-expand "bad expression: ~s" datum)]
        )
    
))



(define expandSimpleCase
    (lambda (check toCheck bodies)
        (if (null? (cdr bodies))
            (if-exp (app-exp (var-exp 'eqv?) (list check (app-exp (var-exp 'quote) (list (var-exp (car toCheck))) )))
                (syntax-expand (let-exp '() '() (list (car bodies))  ))
            )
            
            (if-else-exp 
                (app-exp (var-exp 'eqv?) (list check (app-exp (var-exp 'quote) (list (var-exp (car toCheck))
                ))))

                (syntax-expand (let-exp '() '() (list (car bodies))))
                (expandSimpleCase check (cdr toCheck) (cdr bodies))
            )
        )
    ))


(define expandCase 
    (lambda (check proc-exp bodies)
    (if (null? (cdr bodies))
        (if-exp (app-exp (var-exp 'eqv?) (list check (car proc-exp)))
            (syntax-expand (let-exp '() '() (car bodies) )))

        (if-else-exp 
            (app-exp (var-exp 'eqv?) (list check (car proc-exp)))
            (syntax-expand (let-exp '() '() (car bodies) ))
            (expandCase check (cdr proc-exp) (cdr bodies))
        )
    )
    ))

(define convertCons
    (lambda (preds bodies)
        (if (null? (cdr bodies))
            (if-exp (syntax-expand (car preds)) (syntax-expand (let-exp '() '() (car bodies) )) )
            (if-else-exp (syntax-expand (car preds)) (syntax-expand (let-exp '() '() (car bodies) ))
                (convertCons (cdr preds) (cdr bodies))
            )

        )
    )
)

(define (andhelper bodies) 
(if (null? (cdr bodies)) (car bodies) 
            (cond
                ; [(null? bodies) (lit-exp #f)]
                [else (if-exp (syntax-expand (car bodies)) (andhelper (cdr bodies)))]
            )   
))


(define (let*helper vars exp bodies) 
(if (null? (cdr vars)) 
    (let-exp (list (car vars)) (list (car exp)) bodies)
    (let-exp (list (car vars)) (list (car exp)) (list (let*helper (cdr vars) (cdr exp) bodies)))
)
)

(define (orhelper bodies) 
; (map (lambda (val) (if-exp (synax-expand val) (synax-expand val)) ) bodies )
        (if (null? bodies)
            (lit-exp #f)
            (syntax-expand (let-exp 
                (list 'pred)
                (list (car bodies))
                (list (if-else-exp (var-exp 'pred) 
                (var-exp 'pred)
                (orhelper (cdr bodies)))
                
            ))
            )
        ))

