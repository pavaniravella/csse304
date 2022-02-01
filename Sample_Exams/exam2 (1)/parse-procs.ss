; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types,
; more options for these types, and error-checking.
; Travis Zheng (zhengy6), Zeyu Liao (liaoz1)
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
(define (rands? p)
(or ((list-of expression?) p) (expression? p))

)
(define (lit-exp? e)
(or (null? e) (number? e) (string? e) (vector? e) (boolean? e))
)
; (define parse-exp         
;   (lambda (datum)
;     (cond
;      [(symbol? datum) (var-exp datum)]
;      [(number? datum) (lit-exp datum)]
;      [(pair? datum)
;       (cond
       
;        [else (app-exp (parse-exp (1st datum))
; 		      (map parse-exp (cdr datum)))])]
;      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))
(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(null? datum) (lit-exp datum)]
     [(string? datum) (lit-exp datum)]
     [(vector? datum) (lit-exp datum)]
     [(boolean? datum) (lit-exp datum)]
     [(pair? datum)
      (cond
      ; [(eqv? (1st datum) 'quote) (lit-exp (2nd datum))]
       [(eqv? (1st datum) 'simplecase) (simplecase-exp   (parse-exp (cadr datum)) (map car (cddr datum)) (map (lambda (val) (parse-exp (cadr val) )) (cddr datum)))]
       [(eqv? (1st datum) 'for) 
        (if (eqv? (car (cddddr datum)) 'to)
          (for-to-do-exp (cadr datum) (parse-exp (cadddr datum)) (parse-exp (cadr (cddddr datum))) (map parse-exp (cdddr (cddddr datum))) )
          (for-downto-do-exp (cadr datum)  (parse-exp (cadr (cddddr datum))) (parse-exp (cadddr datum)) (map parse-exp (cdddr (cddddr datum))))
        )]

        [(eqv? (1st datum) 'make-namespace) (namespace-record (map car (cadr datum)) (map (lambda (val) (parse-exp (cadr val)) ) (cadr datum)) )]
      ;  [(eqv? (1st datum 'define)
       [(eqv? (car datum) 'lambda)
          (cond 
            [(<= (length datum) 2) (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
            [(symbol? (2nd datum))
                (lambda-exp (2nd datum) (map parse-exp (cddr datum)))
               ; (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" datum))
            ]

            [(and (pair? (2nd datum) ) (not (list? (2nd datum) ))) 
            (lambdaImproper-exp (exceptLast (2nd datum) ) 
                                (getL (2nd datum)) 
                               (map parse-exp (cddr datum)) )]

            [(if (andmap symbol? (2nd datum))
             (list-lambda-exp (2nd datum) (map parse-exp (cddr datum)))
              (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" datum))
            ]
            [else (lambda-exp (car (2nd  datum))
                  (parse-exp (cddr datum)))]

          )]
       [(eqv? (car datum) 'set!)
        (cond
        [(eqv? (length datum) 3)
          (if (expression? (3rd datum))
          (set!-exp (2nd datum) (parse-exp (3rd datum)))
          (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)
          )]
        [else (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)
        ])
      ]

      [(eqv? (car datum) 'if)
        (cond
        [(eqv? (length datum) 3) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
        [(eqv? (length datum) 4)(if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))]
        [else  (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" (list datum)) ]
        )
      ]

      [(or (eqv? (car datum) 'let) (eqv? (car datum) 'let*) (eqv? (car datum) 'letrec))
        (cond
         [(eqv? (length datum) 2) 
          (eopl:error 'parse-exp "~s-expression has incorrect length ~s" (car datum) 2 datum)]
         
          ;[(not (list? (cadr datum)))   (eopl:error 'parse-exp  "declarations in ~s-expression not a list ~s" (car datum) datum)] 
          [(and (eqv? 'let (car datum)) (symbol? (cadr datum) )) (named-let (cadr datum) (map car (caddr datum)) (map (lambda (body) (parse-exp (cadr body) )) (caddr datum)) (map (lambda (body) (parse-exp body)) (cdddr datum)))]
          [#t (let* ( [letType (car datum)]
                  [vars (map (lambda (val)
                          (cond

                            [(and (not (list? val)) (pair? val) )  (eopl:error 'parse-exp "declaration in ~s-exp is not a proper list ~s" letType datum)]
                            [(not (list? val))   (eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" letType datum)]
                            [(not (eqv? (length val) 2)) (eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s"  letType datum)]
                            [(integer? (car val)) (eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" letType datum)]
                            
                            [#t (car val)]  
                          )
                      ) 
                  (cadr datum))]
                [expr  (map (lambda (val) 
                            (parse-exp (cadr val)) 
                        ) 
                  (cadr datum))]
                
                [bodies 
                  (map (lambda (val) (parse-exp val) )  (cddr datum))
                ])

                (cond 
                  [(eqv? (car datum) 'letrec) 
                  ; (if (<= (length datum) 3) (eopl:error 'parse-exp "~s-expression has incorrect length <3" (cadr datum))
                  ;  (letrec-exp vars expr bodies))
                   (let ([idss (map (lambda (val) (cadr (car (cdr val))))  (cadr datum) )]
                          [bodiess (map (lambda (val)  (map parse-exp (cddr(cadr val))) ) (cadr datum) )]
                         )
                      (letrec-exp vars idss bodiess bodies)
                    )
                  ]
                  [#t (if (eqv? (car datum) 'let)
                      (let-exp vars expr bodies)
                      (let*-exp vars expr bodies)
                  )]
                )
            )]
          
        )]

      [(eqv? (car datum) 'and) (and-exp (map parse-exp (cdr datum)))]
      [(eqv? (car datum) 'or) (or-exp (map parse-exp (cdr datum)))]
      [(eqv? (car datum) 'cond) (cond-exp (map (lambda (pred) (if (or (eqv? (car pred) 'else) (eqv? pred #t )) (lit-exp #t) (parse-exp (car pred)) )) (cdr datum)) 

                                                                (map (lambda (val) 
                                                                    (map parse-exp (cdr val) )
                                                                         ) (cdr datum)))]
      [(eqv? (car datum) 'case) (case-exp (cadr datum) (map (lambda (val) 
                                                        (if (equal? (car val) 'else)
                                                            (lit-exp #t)
                                                            (parse-exp (caar val))
                                                        )) (cddr datum) )  
                                                        (map 
                                                        (lambda (val) 
                                                            (map (lambda (body) 
                                                               (parse-exp body) 
                                                            ) (cdr val))
                                                        )
                                                        (cddr datum)  ))]

      

      [(eqv? (car datum) 'while) (while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))]
      [(eqv? (car datum) 'begin) (begin-exp (map parse-exp (cdr datum)))]
      [else 
        (cond 
          [(and (pair? datum) (not (list? datum))) (eopl:error 'parse-exp "expression ~s have lenght < 4" datum)]
          [#t (app-exp (parse-exp (1st datum))
            (map  (lambda (var) (parse-exp var) ) (cdr datum)) )]
        )])
      ]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))
 	


; (define parse-body
;   (lambda (datum)
;     (if (eq? (car datum) 'define)
    
;     )

;   ))

; (define parse-this
;   (lambda (entire)
;     (cond
;       [(null? (cdr entire)) (parse-exp (car entire))] 
;       [(eqv? (car entire) 
;     )
;   )))

(define find-define 
  (lambda (datum pred)
    (cond
      [(null? datum) '()]
      [(eqv? (caar datum) 'define)
        (cons (pred (car datum)) (find-define (cdr datum) pred))
      ]
    )
  ))


(define unparse-exp
  (lambda (datum)
      (cases expression datum
        [var-exp (id) id]
        [lit-exp (id) id]
        [set!-exp (id val-exp) 
        (list 'set id (unparse-exp val-exp))]
        [if-exp (pred then)
        (list 'if (unparse-exp pred)
            (unparse-exp then)
        )
        ]
        [if-else-exp (pred then else)
        (list 'if (unparse-exp pred)
            (unparse-exp then)
            (unparse-exp else)
        )
        ]

        [lambda-exp (id bodies) 
         (append (list 'lambda id)
             (map unparse-exp bodies))  ]
        [list-lambda-exp (ids bodies) 
         (append (list 'lambda ids) 
             (map unparse-exp bodies)) ]
        [lambdaImproper-exp (ids arbitary bodies)
          (append (list 'lambda (cons ids arbitary))  
             (map unparse-exp bodies))
        ]

        [let-exp (var varExp bodies) 
         (unparse-lets 'let var varExp bodies)
        ]
        [let*-exp (var varExp bodies)
          (unparse-lets 'let* var varExp bodies)
        ]
        
        [letrec-exp (vars idss bodiess bodies)
           (unparse-lets 'letrec vars idss bodiess bodies)
        ]

         [app-exp (rator rands) 
            (cons (unparse-exp rator) (map unparse-exp rands ) )
         ]

         [and-exp (bodies) (append (list 'and) (map unparse-exp bodies))]
         [or-exp (bodies) (append (list 'or) (map unparse-exp bodies))]
         [begin-exp (bodies) (append (list 'begin) (map unparse-exp bodies))]
         [cond-exp (pred exps) (append (list 'cond) (map (lambda (p e) (cons (unparse-exp (car pred)) (map unparse-exp (car exps) ) ) ) pred exps )) ]
         [while-exp (pred bodies) (append (list 'while)  (map unparse-exp bodies)) ]
         [else 'not found]
      )
  )
)
(define unparse-lets
  (lambda (letType var varExp bodies)
     
     (cons letType 
              (append (list (map (lambda (val exp)
                  (list val (unparse-exp exp))
              )  
              var varExp))    
              ; map through bodie
           
            (map (lambda (body)
                 (unparse-exp body)
                )
               bodies
            )
      )
  )))
(define unparse-letrec
  (lambda (vars idss bodiess bodies)
    (cons 'letrec (append (list (map (lambda (var ids bodie) 
                                      (list var (list 'lambda ids (map unparse-exp bodie)))
    )
    vars idss bodiess
    )
    (map (lambda (body2s)
          (unparse-exp body2s)
    )
    bodies)
    )))
  )
)
(define exceptLast
  (lambda (il)
    (cond
      [(not (pair? il)) '()]
      [#t (cons (car il) (exceptLast (cdr il)) )]
    )
  ))
; get last from improper list
(define getL
  (lambda (il)
    (cond
      [(not (pair? il)) il ]
      [#t  (getL (cdr il)) ]
    )
  ))




   (define odd?
     (lambda (x)
       (letrec ( [odd? (lambda (n) (if (zero? n) #f (even? (- n 1))))]
        [even? (lambda (m) (if (zero? m) #t (odd? (- m 1))))]
        )
       (odd? x)
       )
       
       ))





   (let ((x 5))
  (letrec ([foo (lambda (y) (bar x y))]
           [bar (lambda (a b) (+ (* a b) a))])
    (foo (+ x 3))))

