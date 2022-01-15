#lang racket

(require "../chez-init.rkt")
(provide my-let my-or += return-first bintree? leaf-node interior-node bintree-to-list max-interior parse-exp unparse-exp)
(require racket/trace)
;Problem 1
;a
(define-syntax my-let
 (syntax-rules ()
   [(_ ((x v) ...) e1 e2 ...)
    ((lambda (x ...) e1 e2 ...) 
     v ...)]
   [(_ name ((x v) ...) e1 e2 ...)
    (letrec ((name (lambda (x ...) e1 e2 ...))) (name v ...))])) 

;b
(define-syntax my-or
  (syntax-rules ()
    [(_ ) #f]
    [(_ exp) exp]
    [(_ exp1 exp2 ...) (let ((val exp1))
                         (if val
                             val
                             (my-or exp2 ...)))]))
;c
(define-syntax +=
  (syntax-rules ()
    [(_ var num)
     ;(+ var num)]))
     (begin (set! var (+ var num)) var)]));need to return var, not just mutate
;d
(define-syntax return-first
    (syntax-rules ()
      [(_ e1 e2 ...)
       (let ((return e1)) (begin e2 ... return))]))

;Problem 2
(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      [leaf-node (datum) (list 'leaf-node datum)]
      [interior-node (key left-tree right-tree)
                     (list 'interior-node key (bintree-to-list left-tree) (bintree-to-list right-tree))])))

;Problem #3
(define max-interior
	(lambda (tree)
		(cadr (max-interior-helper tree));returns list of (sum leaf-node sum)
		))

(define max-interior-helper
  (lambda (tree)
    (cases bintree tree
      [leaf-node (datum) datum];copied in code from A7 and simplified
      [interior-node (key left-tree right-tree) (let ([max-left (max-interior-helper left-tree)] [max-right (max-interior-helper right-tree)])
                                                  (cond
                                                    [(and (number? max-left) (number? max-right)) (list (+ max-left max-right) key (+ max-left max-right))]
                                                    [(and (list? max-left) (number? max-right)) (let ((curr-sum (+ max-right (1st max-left))) (curr-max (3rd max-left))) (if (> curr-max curr-sum)
                                                                                                                                                                             (list curr-sum (2nd max-left) curr-max)
                                                                                                                                                                             (list curr-sum key curr-sum)))]
                                                    [(and (number? max-left) (list? max-right)) (let ((curr-sum (+ max-left (1st max-right))) (curr-max (3rd max-right))) (if (> curr-max curr-sum)
                                                                                                                                                                              (list curr-sum (2nd max-right) curr-max)
                                                                                                                                                                              (list curr-sum key curr-sum)))]
                                                    [else (let ((curr-sum (+ (1st max-left) (1st max-right))) (curr-max (if  (> (3rd max-right) (3rd max-left))
                                                                                                                             (cdr max-right)
                                                                                                                             (cdr max-left)))) 
                                                            (if (> (2nd curr-max) curr-sum)
                                                                (list curr-sum (1st curr-max) (2nd curr-max))
                                                                (list curr-sum key curr-sum)))]
                                                    ))])))

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

;Harrison Wight & Pavani Ravella
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
;;let, letrec, let*, if, set, lit-exp, namelet, 

(define-datatype binding-exp binding?
  [binding
   (id symbol?)
   (value expression?)
   ])

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
  (or (number? exp) (boolean? exp) (string? exp))) ;removed the vector 
;;DEFINING THE DATA TYPE
(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data number?)]
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
   ;(ids (list-of? expression?))
   ;(values (list-of? expression?))
   (body (list-of? expression?))]
  [let*-exp
   (vars-vals (list-of? expression?))
   ;(ids (list-of? expression?))
   ;(values (list-of? expression?))
   (body (list-of? expression?))
   ]
  [name-let-exp ;figure out let and the list of stuff
   (id symbol?)
   (var-vals (list-of? (list-of? binding-exp)))  ;;value expression [(symbol expression) (symbol expression) ...] 
   ;;TODO confirm that this is allowed to be a list of expressions 
   (body (list-of? expression?))
   ]

  [letrec-exp
   (vars-vals (list-of? expression?))
   ;(ids (list-of? expression?))
   ;(values (list-of? expression?))
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


(define parse-exp         ;ask someone why you don't have cases 
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(vector? datum) (vector-exp datum)] ;adding a vector 
      [(literal? datum) (lit-exp datum)]
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
    
      [else (error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (data) data]
      [lambda-exp (ids body) (cons 'lambda (cons ids (map unparse-exp body)))];works
      [lambda-no-paren-exp (id body) (cons 'lambda (cons id (map unparse-exp body)))];works
      [let-exp (var-vals body) (cons 'let (cons (map unparse-exp var-vals) (map unparse-exp body)))];(apply list 'let (map unparse-exp ids) (map unparse-exp values) (map unparse-exp body))];(cons 'let (cons (map unparse-exp values)]
      [let*-exp (var-vals body) (cons 'let* (cons (map unparse-exp var-vals) (map unparse-exp body)))]
      [name-let-exp (id var-vals body) 'letname]
      [vector-exp (vec) vec]
      [letrec-exp (var-vals body) (cons 'letrec (cons (map unparse-exp var-vals) (map unparse-exp body)))]
      [if-exp (pred body) (list 'if (unparse-exp pred) (unparse-exp body))]
      [if-else-exp (pred consequent alternative) (list 'if (unparse-exp pred) (unparse-exp consequent) (unparse-exp alternative))]
      [set!-exp (id body) (list 'set! (id) (unparse-exp body))]
      [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
      
      )))

; An auxiliary procedure that could be helpful.
(define var-exp?
  (lambda (x)
    (cases expression x
      [var-exp (id) #t]
      [else #f])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
