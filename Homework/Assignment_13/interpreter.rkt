#lang racket

(require "../chez-init.rkt" racket/format)
(provide eval-one-exp)

;-------------------+
;                   |
;   sec:DATATYPES   |
;                   |
;-------------------+

; parsed expression.  You'll probably want to replace this 
; code with your expression datatype from A11b

;;CHECKING IF LEGIT LAMBDA
(define (lambda? exp)
  (and (list? exp) ;check if it is list
       (>= (length exp) 3) ;equal than 3 
       ; (or (symbol? (2nd exp))
       (and (list? (2nd exp)) (andmap symbol? (2nd exp)) 
            ) ; second element should either be a symbol or a list containing unique elements 
       ;(or (expression? 3rd) ((list-of? expression?) (3rd exp))) ;check if the third element is an expression or list of 
       (equal? 'lambda (1st exp))))
       

;;HELPER TO CHECK FOR UNIQUE ELEMENTS
(define (unique-elements? ls)
  (cond
    [(null? ls) #f]
    [(member (first ls) (rest ls)) #t]
    [else
     (unique-elements? (rest ls))
     ]
    )
  )
;;CHECK IF LEGIT LET
(define (let? exp)
  (and (list? exp) (<=  3(length exp))
       (equal? 'let (1st exp)) ((list-of? ))) ;check the binding
  )
;;CHECK IF-EXP
(define (if-exp? e)
  (and (equal? 'if (1st e))(= (length e) 4))
  )
;;CHECK IF-ELSE-EXP
(define (if-else? exp)
  (and (equal? 'if (1st exp)))
  )
;;CHECK SET! EXP
(define (set!-exp? exp)
  (and (= 3 (length exp)) (symbol? (2nd exp)))
  )

;;CHECK LET-EXP
(define (let-exp? exp) ;returns true IFF exp length is 3, 2nd is a list, 3rd is a list of expressions or just an expression
  (and (>=  (length exp) 3) ;check length 3
       (list? (2nd exp)) ;2nd part is alist
       (andmap (lambda (x) (and (list? x) (= 2 (length x)) (symbol? (car x)) )) (2nd exp)) ; check if it is proper structure of [symbol ...]
       ))

;;CHECK LET*-EXP
(define (let*-exp? exp)
  (and (> (length exp) 2)
       (list? (2nd exp)) ;2nd part is alist
       (andmap (lambda (x) (and (= 2 (length x)) (symbol? (car x)))) (2nd exp)) ; check if it is proper structure of [symbol ...]
       )
  )
      
;;CHECK LAMBDA-NO-PAREN
(define (lambda-no-paren-exp? exp)
  (and (> (length exp) 2) (symbol? (2nd exp)) ; simply check if the 2nd part is a symbol
       ))
;;CHECK LETREC
(define (letrec-exp? exp)
  (and (not (null? (2nd exp)))
       (not (null? (cddr exp)))
       (list? (cdr exp))
       (list? (2nd exp))
       (not (null? (caadr exp)))
       (andmap (lambda (ls) (equal? (length ls) 2)) (cadr exp))
       (andmap symbol? (map car (cadr exp)))
       )
  )
;;CHECK LITERAL
(define (literal? exp)
  (or (number? exp)
      (boolean? exp)
      (string? exp)
      
      )) ;removed the vector
;;CHECK QUOTED LITERAL
(define (quoted-exp? exp)
  (and (list? exp) (= (length exp) 2) (equal? 'quote (1st exp)))
  )

;;DATA TYPE DEFINITIOn
(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [quoted-exp ;maybe to add or delete 
   (data quoted-exp?)
   ]
  [lit-exp
   (data literal?) ;to be modified 
   ]
  [vector-exp ;vector expresion
   (vec vector?)
   ]
  [lambda-exp
   (ids (list-of? symbol?))
   (body (list-of? expression?))
   ]
  [lambda-no-paren-exp
   (id symbol?)
   (body (list-of? expression?))
   ]
  [let-exp
   (vars-vals (list-of? expression?))
   (body (list-of? expression?))]
  [let*-exp
   (vars-vals (list-of? expression?))
   (body (list-of? expression?))
   ]
  [name-let-exp ;figure out let and the list of stuff
   (id symbol?)
   (var-vals (list-of? expression?))  ;;value expression [(symbol expression) (symbol expression) ...] 
   (body (list-of? expression?))
   ]
  [letrec-exp
   (vars-vals (list-of? expression?))
   (body (list-of? expression?))
   ]
  [if-exp
   (pred expression?)
   (body expression?)
   ]
  [if-else-exp
   (pred expression?) 
   (consequent expression?)
   (alternative expression?)
   ]
  [set!-exp ;make sure that there is an exclamation point 
   (id symbol?)
   (body expression?)
   ]
  [app-exp
   (rator expression?)
   (rand (list-of? expression?))])

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))
  
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of? symbol?))
   (vals (list-of? scheme-value?))
   (env environment?)])


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.


(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)])

  
;-------------------+
;                   |
;    sec:PARSER     |
;                   |
;-------------------+

; This is a parser for simple Scheme expressions, such as those in EOPL 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Helper procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

; Again, you'll probably want to use your code from A11b

(define parse-exp         ;ask someone why you don't have cases 
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(vector? datum) (vector-exp datum)] ;adding a vector 
      [(quoted-exp? datum) (quoted-exp datum)]
      [(literal? datum) (lit-exp datum)]
      ;if it is a quoted datum return the literal as the second part 
     
      [(pair? datum)
       (cond
         ;LAMBDA
         [(equal? 'lambda (1st datum))
          (cond
            [(lambda? datum) ; (lambda x)
             (lambda-exp (2nd datum) (map parse-exp (drop datum 2)))]
            [(lambda-no-paren-exp? datum)
             (lambda-no-paren-exp (2nd datum) (map parse-exp (cddr datum)))
             ]
            [else
             (error 'parse-exp "parse-error")
             ]
            )
          ]
         ;IF
         [(equal? 'if (1st datum))
          (cond
            [(= 4 (length datum)) ;if pred consequent alternative (if else)
             (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))
             ]
            [(= 3 (length datum)) ; one armed if
             (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
             ]
            [else
             (error 'parse-exp "parse-error") ;length is not equal to three or four
             ]
            )
          ]
         ;SET
         [(equal? 'set! (1st datum))
          (cond
            [(and (= (length datum) 3) (symbol? (2nd datum))) ;set is legit is the datum length is 3
             (set!-exp (2nd datum) (parse-exp (3rd datum)))
             ]
            [else
             (error 'parse-exp "parse-error") ;length is not equal to three
             ]
            )
          ]
         ;LET
         [(equal? 'let (1st datum))
          (cond
            [(let-exp? datum)
             ;(let-exp (map parse-exp (map car (cadr datum))) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))
             (let-exp (map parse-exp (2nd datum))
                      (map parse-exp (cddr datum)))
             ]
            [else
             (error 'parse-exp "parse-error")
             ]
            )
          ]
         ;LET*
         [(equal? 'let* (1st datum))
          (cond
            [(let*-exp? datum)
             (let*-exp (map parse-exp (2nd datum))
                       (map parse-exp (cddr datum)))
             ]
            [else
             (error 'parse-exp "parse-error")
             ]
            )
          ]
         ;LETREC I AM DEAD
         [(equal? 'letrec (1st datum))
          (cond
            [(letrec-exp? datum)
             (letrec-exp (map parse-exp (2nd datum))
                         (map parse-exp (cddr datum)))
             ]
            [else
             (error 'parse-exp "parse-error")
             ]

            )
          ]
         [else
          (if (list? (cdr datum))
              (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))
              (error 'parse-exp "parse-error"))])]
    
      [else
       (error 'parse-exp "bad expression: ~s" datum)
       ;(~a "parse-exp bad expression: " datum)
       ])))


;-------------------+
;                   |
; sec:ENVIRONMENTS  |
;                   |
;-------------------+


; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
            [(eq? sym (car los)) pos]
            [else (loop (cdr los) (add1 pos))]))))
	    
(define apply-env
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
                        (error 'env "variable ~s not found." sym)]
      [extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))])))


;-----------------------+
;                       |
;  sec:SYNTAX EXPANSION |
;                       |
;-----------------------+

; To be added in assignment 14.

;---------------------------------------+
;                                       |
; sec:CONTINUATION DATATYPE and APPLY-K |
;                                       |
;---------------------------------------+

; To be added in assignment 18a.


;-------------------+
;                   |
;  sec:INTERPRETER  |
;                   |
;-------------------+

; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
               (apply-env init-env id)]
      ;adding quote experession
      [quoted-exp (data)
                 (2nd data)
                 ]
      [app-exp (rator rands)
               (let ([proc-value (eval-exp rator)]
                     [args (eval-rands rands)])
                 (apply-proc proc-value args))]
      [else (error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands)
    (map eval-exp rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      ; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                   proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons =))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
   (map prim-proc      
        *prim-proc-names*)
   (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (+ (1st args) (2nd args))]
      [(-) (- (1st args) (2nd args))]
      [(*) (* (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [else (error 'apply-prim-proc 
                   "Bad primitive procedure name: ~s" 
                   prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))
