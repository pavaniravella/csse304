; top-level-eval evaluates a form in the global environment
; Travis Zheng (zhengy6), Zeyu Liao (liaoz1)
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (checkIfInterpreterProc (eval-exp form (empty-env) ) )))


(define checkIfInterpreterProc  
  (lambda (val)    
    (cond       
    [(proc-val? val) '<interpreter-procedure>]      
    [else val]    
    )      
))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (if (equal? 'quote id)
          'quote
          (if (equal? id 'use-namespace)
            'use-namespace
            (apply-env env id)
          )
        )
	    ]

      [namespace-record (syms vals) (namespace-record-env syms (map (lambda (x) (eval-exp x env)) vals) env) ]

      [app-exp (rator rands)
        (if (equal? (eval-exp rator env) 'quote)
            (apply-proc (prim-proc 'quote) (quoteRand (1st rands) ) )
          (if (equal? (eval-exp rator env) 'use-namespace)
            (let ([namespace (eval-exp (car rands) env)])
                  (eval-exp (cadr rands) namespace )
            )

            (let ([proc-value (eval-exp rator env)]
                  [args (eval-rands rands env)])
              (apply-proc proc-value args))
          )
            
        )]
      [if-else-exp (pred then else) (if (eval-exp pred env) (eval-exp then env) (eval-exp else env))]
      [if-exp (pred then) (if (eval-exp pred env) (eval-exp then env))]
      [list-lambda-exp (ids bodies) (closure ids bodies env) ]
      [let-exp (vars exps bodies) 
        (eval-bodies bodies (extend-env vars (eval-rands exps env) env))]
      [lambda-exp (id bodies) (closureAbitrary id bodies env)]
      [lambdaImproper-exp (ids arbitary bodies) (closureImproper ids arbitary bodies env)  ]
      [while-exp (pred bodies) (let loop () (if (eval-exp pred env)
                                              (begin (eval-bodies bodies env) (loop))
      ))]

      
      [letrec-exp (vars idss bodiess bodies) (eval-bodies bodies (extend-env-rec vars idss bodiess env))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define quoteRand
  (lambda (rands)
    (unparse-exp rands)
  ))
  
(define eval-bodies 
  (lambda (bodies env)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env)
      (begin 
        (eval-exp (car bodies) env)
        (eval-bodies (cdr bodies) env))
      )
    )
  )
  
; (define eval-rands-namespace
;   (lambda (rands namespace env)
    
;   ))
; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (val) (eval-exp val env) )  rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda  (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      ; evaluate the parameter in the current environment, create new environment, evaulate bodies in the new environment. 

      [closure (ids bodies env) (eval-bodies bodies 
      (extend-env ids args env)) ]
      [closureAbitrary (id bodies env) (eval-bodies bodies (extend-env (list id) (list args) env))]
      [closureImproper (ids arbitary bodies env) (eval-bodies bodies (extend-env (append ids (list arbitary)) (convertArgToList args (length ids) 0) env))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))
  
  ; this procedure converts part of an argument into a list. Used for lambda with improper list
(define convertArgToList 
      (lambda (args n curr)
        (cond 
          [(null? args) (list '()) ]
          [(< curr n) (cons (car args) (convertArgToList (cdr args) n (+ 1 curr)))]
          [#t (list (cons (car args) (car (convertArgToList (cdr args) n (+ 1 curr))) ))]
        )
      )
)
(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? < <= > >= not car cdr list null? assq eq? eqv? 
 equal? atom? length list->vector list? pair?  procedure? vector->list vector vector-ref vector? number? symbol? set-car! set-cdr! display newline quote caar caaar cddr cdddr cadddr caadr cadr cdar cddar cdaar cadar 
  cdadr number? length quotient apply map vector-set! negative? positive? vector-set! append list-tail make-namespace
))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
; including  +, -, *, /, add1, sub1, zero?,  not, = and < 
; (and the other numeric comparison operators), and also cons, car, cdr, list, null?, assq, eq?,
;  equal?, atom?, length,  list->vector, list?, pair?,  procedure?, vector->list, vector,
;   make-vector, vector-ref, vector?, number?, symbol?, set-car! , set-cdr!, vector-set!, 
;  display , newline    
; Add the c**r and c***r procedures (where each "*" stands for an "a" or "d"). 
(define apply-prim-proc
  (lambda  (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (apply = args)]
      [(/)(apply / args)]
      [(zero?) (zero? (1st args))]
      [(<) (apply < args)]
      [(<=) (apply <= args)]
      [(>) (apply > args)]
      [(>=) (apply >= args)]
      [(not) (not (1st args))]
      [(car) (car (1st args)) ]
      [(cdr) (cdr (1st args))]
      [(list) args]
      [(null?) (apply null? args)]
      [(assq)(assq (1st args) (2nd args))]
      [(eq?)(eq? (1st args) (2nd args))]
      [(eqv?) (eqv? (1st args) (2nd args)) ]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (apply atom? args)]
      [(length) (apply length args)]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args) )] 
      [(pair?) (pair? (1st args))]  
      [(procedure?) (apply proc-val? args)]
      [(vector->list) (apply vector->list  args)]
      [(vector)(apply vector args)]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (vector-ref (1st args) (2nd args) )]
      [(vector?)(apply vector? args)]
      [(number?)(number? (1st args))]
      [(symbol?)(symbol? (1st args))]
      [(set-car!)(begin (set-car! (1st args) (2nd args)) (display (1st args) ))]
      [(set-cdr!)(set-cdr! (1st args) (2nd args))]
      [(vector-set!)(vector-set! (1st args) (2nd args) (3rd args))] 
      [(quote) args]
      [(display) (display (1st args))]
      [(newline) (newline)] 
      [(caar) (car (car (1st args)))]
      [(caaar) (car (car (car (1st args)))) ]   
      [(cddr) (cdr (cdr (1st args))) ] 
      [(cdddr) (car (cddr (cdr (1st args)))) ]
      [(cadddr) (cadddr (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadr) (cadr (1st args))]
      [(cdar) (cdr (car (1st args)))]
      [(cddar) (cdr (cdr (car (1st args))))] 
      [(cdaar) (cdr (car (car (1st args))))]
      [(cadar) (car (cdr (car (1st args))))]
      [(cdadr) (cdr (car (cdr (1st args))))]
      [(quotient) (quotient (1st args) (2nd args))]
      [(negative?)(negative? (1st args))]
      [(positive?)(positive? (1st args))]
      [(apply) (apply-proc (1st args) (2nd args) )]
      [(map) (customMap (1st args) (2nd args)) ]
      [(append) (apply append args)]
      [(list-tail) (list-tail (1st args) (2nd args))]
   

      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))


(define customMap
  (lambda (proc ls)
    (cond 
    [(null? ls) '()]
    [#t (cons (apply-proc proc (list (1st ls)) ) (customMap proc (cdr ls) ))]
    )
  )
)
(define global-env init-env)

(define primitiveProc? 
  (lambda (name)
    (ormap (lambda (val) (eq? name val) ) *prim-proc-names*)
  ))





(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x))))
  )








            
            



