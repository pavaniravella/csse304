#lang racket

(require "../chez-init.rkt" racket/format)
(provide eval-one-exp reset-global-env)
(require racket/trace)
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

;;CHECK FOR IMPROPER LAMBDA 
(define (lambda-improper-exp? exp)
  (and (list? exp) (pair? (2nd exp)))
  )

;;DATA TYPE DEFINITIOn
(define-datatype expression expression?
  [var-exp
   (id (lambda (x) 
         (or (symbol? x) 
             (lexical? x))))]
  ;(id symbol?)]
  [quoted-exp ;maybe to add or delete 
   (data quoted-exp?)
   ]
  [lit-exp
   (data literal?) ;to be modified 
   ]
  [vector-exp ;vector expresion
   (vec vector?)
   ]
  [lambda-improper-exp
   (ids pair?) ;might need to change
   (improper-id symbol?)
   (body (list-of? expression?))
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
   ;   (vars-vals (list-of? expression?))
   ;   (body (list-of? expression?))
   (vars (list-of? symbol?))
   (vals (list-of? expression?))
   (body (list-of? expression?))
   ]
  [let*-exp
   (vars (list-of? symbol?))
   (vals (list-of? expression?))
   (body (list-of? expression?))
   ]
  [name-let-exp ;figure out let and the list of stuff
   ;   (id symbol?)
   ;   (var-vals (list-of? expression?))  ;;value expression [(symbol expression) (symbol expression) ...] 
   ;   (body (list-of? expression?))
 
   (name symbol?)
   (syms (list-of? symbol?))
   (exps (list-of? expression?))
   (bodies (list-of? expression?))
   ]
  [letrec-exp
   (proc-names (list-of? symbol?))
   (ids (list-of? (list-of? symbol?)))
   (bodies (list-of? (list-of? expression?)))
   (letrec-bodies (list-of? expression?)) 
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
  [cond-exp ;two types of cond 
   (conditions (list-of? expression?))
   (body (list-of? expression?))]
  [and-exp
   (body (list-of? expression?))
   ]
  [or-exp
   (body (list-of? expression?))
   ]
  [case-exp
   (cond expression?)
   (body (list-of? expression?))
   ]
  [while-exp
   (cond expression?)
   (body (list-of? expression?))
   ]
  [begin-exp
    (body (list-of? expression?))
    ]
  [define-exp
    (var symbol?)
    (exp expression?)]
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
   (env environment?)]
  [recursively-extended-env-record
   (proc-names (list-of? symbol?))
   (idss (list-of? (list-of? symbol?)))
   (bodiess (list-of? (list-of? expression?)))
   (old-env environment?)] ;no mutation approach
  )


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.


(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (ids (list-of? symbol?))
   (bodies (list-of? expression?))
   (env environment?)]
  [closure-no-paren
   (id (list-of? symbol?))
   (bodies (list-of? expression?))
   (env environment?)]
  [closure-improper
   (vars (list-of? symbol?))
   (improper-var symbol?)
   (bodies (list-of? expression?))
   (env environment?)
   ])

  
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

(define all-but-last
  (lambda (exp)
    (if (symbol? exp)
        '()
        (cons (car exp) (all-but-last (cdr exp))))))

;last element
(define (get-last-element exp)
  (if (symbol? exp)
      exp
      (get-last-element (cdr exp)))
  )
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
         ;OR STATEMENT
         [(equal? 'or (1st datum))
          (or-exp (map parse-exp (cdr datum)))]
         ;AND EXP
         [(equal? 'and (1st datum))
          (and-exp (map parse-exp (cdr datum)))]
         ;CONDITIONAL STATMENT
         [(equal? 'cond (1st datum))
          ;(syntax-expand datum)]
          (cond-exp (map parse-exp (map car (cdr datum))) (map parse-exp (map cadr (cdr datum))))];syntax-expand cond datum
         ;LAMBDA
         [(equal? 'lambda (1st datum))
          (cond
            [(lambda? datum) ; (lambda x)
             (lambda-exp (2nd datum) (map parse-exp (drop datum 2)))]
            [(lambda-no-paren-exp? datum)
             (lambda-no-paren-exp (2nd datum) (map parse-exp (cddr datum)))
             ]
            [(lambda-improper-exp? datum)
             (lambda-improper-exp (all-but-last (2nd datum)) (get-last-element (2nd datum)) (map parse-exp (cddr datum)))
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
            [(and (>= 4 (length datum)) (symbol? (2nd datum))) ;this is a named let 
             (name-let-exp (2nd datum) (map car (3rd datum)) (map parse-exp (map cadr (3rd datum))) (map parse-exp (cdddr datum)) )
             ]
            [(let-exp? datum)
             (let-exp  (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))
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
             (let*-exp  (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))
             ]
            [else
             (error 'parse-exp "parse-error")
             ]
            )
          ]
         ;LETREC I AM DEAD
         [(equal? 'letrec (1st datum))
          (cond
            [(equal? 'letrec (1st datum))
             ;             (letrec-exp (map parse-exp (2nd datum))
             ;                         (map parse-exp (cddr datum)))
             (letrec-exp (map 1st (2nd datum))
                         (map cadadr (2nd datum))
                         (map (lambda (x) (map parse-exp x)) (map cddadr (2nd datum)))
                         (map parse-exp (cddr datum))
                         )

             ]
            [else
             (error 'parse-exp "parse-error")
             ]

            )
          ]
         ;BEGIN EXPRESSION
         [(equal? 'begin (1st datum))
          (begin-exp (map parse-exp (cdr datum)))]
         ;WHILE
         [(equal? 'while (1st datum))
          (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
         ;DEFINE
         [(equal? 'define (1st datum))
          (define-exp (2nd datum) (parse-exp (3rd datum)))]
         [else
          (if (list? (cdr datum))
              (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))
              (error 'parse-exp "parse-error"))])]
    
      [else
       (error 'parse-exp "bad expression: ~s" datum)
       ])))


;-------------------+
;                   |
; sec:ENVIRONMENTS  |
;                   |
;-------------------+

;also modify to include lexical for envirnonments 
; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

(define extend-env-recursively
  (lambda (proc-names idss bodiess old-env)
    (recursively-extended-env-record 
     proc-names idss bodiess old-env)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
            [(eq? sym (car los)) pos]
            [else (loop (cdr los) (add1 pos))]))))

(define deref unbox)
(define set-ref! set-box!)

(define apply-env;needs to be apply-env
  (lambda (env var)
    (deref (apply-env-ref env var))))
;makes it so that we don't have to refactor all apply-env procedure calls

(define apply-env-ref; needs to be apply-env-ref
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()   ;need to look up in the global environment HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
                        (apply-env-ref-global global-env sym)]
      ;(error 'env "variable ~s not found." sym)]
      [extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env-ref env sym)))]
      [recursively-extended-env-record
       (procnames idss bodiess old-env)
       (let ([pos (list-find-position sym procnames)]) ;change to lexical address , have i recurred downwards enough to reach so i know where to reach
         ;if not you have found it , return void 
         (if (number? pos)
             (box (closure (list-ref idss pos)
                           (list-ref bodiess pos)
                           env))
             (apply-env-ref old-env sym)))])))

(define apply-env-ref-global
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()     
                        (error 'env "variable ~s not found." sym)]
      [extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env-ref-global env sym)))]
      [recursively-extended-env-record
       (procnames idss bodiess old-env)
       (let ([pos (list-find-position sym procnames)])
         (if (number? pos)
             (box (closure (list-ref idss pos)
                           (list-ref bodiess pos)
                           env))
             (apply-env-ref-global old-env sym)))])))


(define-datatype lexical lexical?
  [free-lexical 
   (sym symbol?)]
  [bound-lexical  
   (dep number?)
   (pos number?)])

(define apply-lex-env 
  (lambda (env lex-var)
    (cases lexical lex-var
      [free-lexical (sym) 
                    (apply-env global-env sym)]
      [bound-lexical (dep pos) 
                     (get-val-by-address dep pos env)])))



(define get-val-by-address
  (lambda (depth pos env)
    (cases environment env
      [empty-env-record () (error 'get-val-by-address 
                                  "invalid lexical address ~s" 
                                  env)]
      [extended-env-record (syms vals environ)
                           (if (= depth 0)
                               (unbox (list-ref vals pos))
                               (get-val-by-address (- depth 1) pos environ))]
      [recursively-extended-env-record (proc-names idss bodiess old-env)
                                       (if (= depth 0)
                                           (my-unbox (list-ref proc-names pos))
                                           (get-val-by-address (- depth 1) pos old-env))])))
;)))
(define my-unbox
  (lambda (x)
    (if (box? x)
        (my-unbox (unbox x))
        x)))
;(proc-names (list-of? symbol?))
;  (idss (list-of? (list-of? symbol?)))
;  (bodiess (list-of? (list-of? expression?)))
;  (old-env environment?)]

;LEXICAL ADDRESS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(define lexical-address
  (lambda (exp)
    (let helper ([exp exp]
                 [bound '()]) ;ccontains a list of list of symbols that are bound , each representing a level of the scoping contour 
      (cases expression exp
        [var-exp (id) 
                 (var-exp (get-address id bound 0))]
        [quoted-exp (data)
                    exp]
        [vector-exp (vec) ;vector expresion
                    (error "stupid vector")]
        [lit-exp (datum) exp]
        [lambda-improper-exp (ids improper-var bodies)  
                             (lambda-improper-exp ids (map (lambda (x) 
                                                             (helper x (cons ids bound))) 
                                                           bodies))]
        [lambda-exp (id body)
                    (lambda-exp id (map (lambda (x) 
                                          (helper x (cons id bound))) 
                                        body))]
        [lambda-no-paren-exp (ids bodies)
                             (lambda-no-paren-exp ids (map (lambda (x) 
                                                             (helper x (cons ids bound))) 
                                                           bodies))]
        [let-exp (vars vals bodies)
                 (let-exp vars
                          (map (lambda (val) (helper val bound)) vals) 
                          (map (lambda (body) (helper body (cons vars bound))) bodies ) )]
        [if-exp (pred true)
                (if-exp (helper pred bound) 
                        (helper true bound))]
        [if-else-exp (pred true false)
                     (if-else-exp (helper pred bound) 
                                  (helper true bound)
                                  (helper false bound))]
        [while-exp (test-exp bodies)
                   (while-exp (helper test-exp bound) 
                              (map (lambda (x) 
                                     (helper x bound)) 
                                   bodies))]
        [letrec-exp (proc-names idss bodiess letrec-bodies)
                    ;                    (letrec-exp proc-names
                    ;                                idss
                    ;                                (apply map (lambda (x y) 
                    ;                                             (map (lambda (z) 
                    ;                                                    (helper z 
                    ;                                                            (cons y 
                    ;                                                                  (cons proc-names bound)))) 
                    ;                                                  x)) 
                    ;                                       (list bodiess idss))
                    ;                                (map (lambda (x) 
                    ;                                       (helper x 
                    ;                                               (cons proc-names bound))) 
                    ;                                     letrec-bodies))
                    (letrec-exp proc-names
                                idss
                                (map (lambda (ex) (helper ex (cons proc-names bound))) bodiess)
                                (map (lambda (ex) (helper ex (cons idss bound))) letrec-bodies))
                    ]
        [set!-exp (id exp)
                  (set!-exp id
                            (helper exp bound))]
        [define-exp (id val)
          (define-exp id (helper val bound))]
        [app-exp (rator rands)
                 (app-exp (helper rator bound) 
                          (map (lambda (x) 
                                 (helper x bound)) 
                               rands))]
        [else (error 'lexical-address "Bad abstract syntax: ~a" exp)]))))

(define get-address
  (lambda (sym bound depth)
    (cond [(null? bound) (free-lexical sym)]
          [(>= (get-pos sym (car bound)) 0) (bound-lexical depth 
                                                           (get-pos sym (car bound)))]
          [else (get-address sym (cdr bound) 
                             (+ 1 depth))])))

(define get-pos
  (lambda (ele lst)
    (cond [(null? lst) -1]
          [(eq? ele (car lst)) 0]
          [else (if (eq? -1 (get-pos ele (cdr lst)))
                    -1
                    (+ 1 (get-pos ele (cdr lst))))])))

;-----------------------+
;                       |
;  sec:SYNTAX EXPANSION |
;                       |
;-----------------------+



; To be added in assignment 14.

(define syntax-expand
  (lambda (exp)
    ;(display exp)
    (cases expression exp
      [lit-exp (datum) exp]
      [var-exp (id) exp]
      [lambda-exp (id bodies)
                  (lambda-exp id (map syntax-expand bodies))]
      [if-exp (test true-body)
              (if-exp (syntax-expand test) (syntax-expand true-body))]
      [if-else-exp (test true-body false-body)
                   (if-else-exp (syntax-expand test) (syntax-expand true-body) (syntax-expand false-body))]
      [let-exp (vars vals bodies)
               (app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand vals))]
      [quoted-exp (data) exp]
      [app-exp (rator rands)
               (app-exp (syntax-expand rator) (map syntax-expand rands))]
      
      ;added this section, similar to expand cond structure
      [and-exp (bodies)
               (expand-and bodies)]
      [begin-exp (bodies) (app-exp (lambda-exp '() (map syntax-expand bodies)) '())]
      ;while was at the bottom of the doc to add
      [while-exp (test bodies)
                 (while-exp (syntax-expand test) (map syntax-expand bodies))
                 ]
      
      ;already had this coded
      [or-exp (body)
              (expand-or-2 body)]
      [cond-exp (exps bodies) 
                (expand-cond exps bodies) 
                ]

      ;LET* expand
      [let*-exp (vars vals bodies)
                
                (if (null? vars)
                    (app-exp (lambda-exp '() (map syntax-expand bodies)) '())
                    (app-exp (lambda-exp (list (car vars)) (list (syntax-expand (let*-exp (cdr vars) (cdr vals) bodies))))
                             (list (car vals))))
                
                ]
      ;named let expand it equal to   (letrec ([proc-id (lambda (arg-id ...)  body ...+)]) (proc-id init-expr ...))
      [name-let-exp (name syms exps bodies)
                    (syntax-expand (letrec-exp (list name) (list syms) (list bodies) (list (app-exp (var-exp name) exps))))

                    ]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
                  (letrec-exp proc-names idss (map (lambda (x) (map syntax-expand x)) bodiess) (map syntax-expand letrec-bodies))]
      [define-exp (var exp)
        (define-exp var (syntax-expand exp))]
      [else
       exp]
      )))
(define (expand-or-2 exp)
  (if (null? exp)
      (lit-exp #f)
      ;(syntax-expand (list 'let-exp (list 'test) (list (if-else-exp (var-exp 'test) (var-exp 'test) (or-exp (cdr exp)))))))
      (syntax-expand (let-exp (list 'x) (list (1st exp)) (list (if-else-exp (var-exp 'x ) (var-exp 'x) (or-exp (cdr exp)))))))
  )
(define (expand-and exp)
  (if (null? exp)
      (lit-exp #t)
      (if-else-exp (syntax-expand (car exp)) 
                   (expand-or (cdr exp)) (lit-exp #f))
      ))
(define (expand-or exp)
  (if (null? exp)
      (lit-exp #f)
      (let ([current (syntax-expand (car exp))])
        (if-else-exp current current (expand-or (cdr exp))))))

(define (expand-cond exps bodies)
  (let loop ([exps exps] [bodies bodies])
    (cond
      [(null? (cdr exps)) (if (equal? (cdar exps) (list 'else)) 
                              (if-exp (lit-exp #t) (syntax-expand (car bodies)))
                              (if-exp (syntax-expand (car exps)) (syntax-expand (car bodies))))]
      [else ;(displayln "else")
       (if-else-exp (syntax-expand (car exps)) (syntax-expand (car bodies)) (loop (cdr exps) (cdr bodies)))])))

(define (expand-case body)
  body)

(define expand-let*
  (lambda (vars vals body)
    (if (null? (cdr vars)) 
        (let-exp (car vars) (car vals) (map syntax-expand body))
        (let-exp  (list (car vars)) (list (car vals)) (syntax-expand (let*-exp (cdr vars) (cdr vals) body))))))
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
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
               ;(displayln env)
               (apply-lex-env env id)]
      ;(apply-env env id)]
      ;adding quote experession
      [quoted-exp (data)
                  (2nd data)
                  ]
      ;begin expresssion
      [begin-exp (body)
                 (for-each (lambda (x) (eval-exp x env)) (map syntax-expand body))]
      ;LAMBDA PAREN
      [lambda-exp (ids body)
                  (closure ids body env)]
      ;LAMBDA IMPROPER 
      [lambda-improper-exp (ids improper-var body)
                           (closure-improper ids improper-var body env)]
      ;LAMBDA NO PAREN
      [lambda-no-paren-exp (id body)
                           (closure-no-paren (list id) body env)];
      ;IF-EXP
      [if-exp (pred body)
              (if(eval-exp pred env) ;might have to change
                 (eval-exp body env)
                 (void))]
      ;IF-ELSE EXP
      [if-else-exp (bool truecase falsecase)
                   (if (eval-exp bool env)
                       (eval-exp truecase env)
                       (eval-exp falsecase env))]
      ;REGULAR LET
      [let-exp (syms exps bodies)
               (let ([extended-env
                      (extend-env syms
                                  (map (lambda (x) (eval-exp x env)) exps) env)])
                 (eval-bodies bodies extended-env))]
      ;WHILE EXP
      [while-exp (test bodies)
                 (when(eval-exp test env)
                   (begin 
                     (map eval-exp bodies (list-of-items env (length bodies)))
                     (eval-exp (while-exp test bodies) env)))
                 ]
      ;LETREC EXP
      [letrec-exp (proc-names idss bodiess letrec-bodies)
                  (eval-bodies letrec-bodies
                               (extend-env-recursively 
                                proc-names idss bodiess env))]
      ;SET
      [set!-exp (id exp)
                (set-ref!
                 (apply-env-ref env id)
                 (eval-exp exp env))]
      ;DEFINE
      [define-exp (var exp)
        (set! global-env (extend-env (list var) (list (eval-exp exp env)) global-env))]
      ;(displayln "SHOULDN'T REACH THIS")];need to change this
      [app-exp (rator rands)
               (let ([proc-value (eval-exp rator env)]
                     [args (eval-rands rands env)])
                 (apply-proc proc-value args))]
      [else (error 'eval-exp "Bad abstract syntax: ~a" exp)])))
(define list-of-items 
  (lambda (x n)
    (if (= n 0)
        '()
        (append (list x) (list-of-items x (- n 1))))))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      ; You will add other cases
      [closure (ids bodies env)
               (last (eval-rands bodies (extend-env ids args env)))] ;(eval-bodies bodies (extend-env id args env))]
      [closure-no-paren (id bodies env);need to treat this differently
                        (last (eval-rands bodies (extend-env id (list args) env)))];also when we define datatype ;regular list (x y) --> proper (. z)
      [closure-improper (vars improper-var bodies env)
                        (last (eval-rands bodies (extend-env (append vars (list improper-var)) (format-args (length vars) args) env)))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                   proc-value)])))
;(define (args)) ; (x y z) : (1 2 3 4 5) -> (x = 1, y = 2, z = (3 4 5)
(define (format-args length args)
  (if (zero? length)
      (list args)
      (cons (1st args) (format-args (- length 1) (cdr args)))
      )
  )
(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env)
        (begin
          (eval-exp (1st bodies) env)
          (eval-bodies (cdr bodies) env)))))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / not zero? >= car cdr list null? eq? eqv? equal?
                              length list->vector list? pair? vector->list append list-tail assq
                              vector? number? symbol? caar cadr cadar procedure? vector vector-ref vector-set!
                              map apply < > quotient negative? positive?))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier. 
   (map prim-proc      
        *prim-proc-names*)
   (empty-env)))

(define global-env init-env)

(define reset-global-env
  (lambda ()
    (set! global-env init-env)))
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    ;(display args)
    (case prim-proc
      [(+) (apply + args)];]
      [(quotient) (apply quotient args)]
      [(negative?) (negative? (1st args))]
      [(positive?) (positive? (1st args))]
      [(-) (apply - args)] ;(- (1st args) (2nd args))]
      [(*) (apply * args)];(* (1st args) (2nd args))]
      [(add1) (add1 (1st args))]
      [(>) (> (1st args) (2nd args))]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(/) (apply / args)]
      [(not) (not (1st args))]
      [(zero?) (zero? (1st args))]
      [(>=) (>= (1st args) (2nd args))]
      [(car) (1st (1st args))]
      [(cdr) (cdr (1st args))]
      [(list) (apply list args)]
      [(list-tail) (apply list-tail args)]
      [(append) (append (1st args) (2nd args))]
      [(null?) (null? (1st args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(eqv?) (eqv? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(vector?) (vector? (1st args))]
      [(pair?) (pair? (1st args))]
      [(list?) (list? (1st args))]
      [(number?) (number? (1st args))]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(vector) (apply vector args)]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(<) (< (1st args) (2nd args))]
      ;[(vector) (vector (]
      [(procedure?) (proc-val? (1st args))]
      [(map) (map1 (1st args) (cadr args))]
      [(apply) (apply-proc (1st args) (2nd args))]
      [(assq) (assq (1st args) (2nd args))]
      [else (error 'apply-prim-proc 
                   "Bad primitive procedure name: ~s" 
                   prim-proc)])))
 
(define map1
  (lambda (p ls)
    (if (null? ls)
        '()
        (cons (apply-proc p (list (car ls)))
              (map1 p (cdr ls))))))


(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x)
    ;(top-level-eval (parse-exp x))
    (top-level-eval (lexical-address (syntax-expand (parse-exp x))))
    ))