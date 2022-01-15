#lang racket
;Pavani Ravella
;Exam 1 
(provide largest-range combine-consec make-group rename-free listbox-func)
;(provide largest-range  listbox-func)

;; Note that mutation is not allowed on these problems, except where noted.

;; strictly optional, but you might find list-recur handy for largest-range
(define list-recur
  (lambda (init-val proc)
    (letrec ([helper
              (lambda (lst)
                (if (null? lst)
                    init-val
                    (proc (car lst) (helper (cdr lst)))))])
      helper)))


;; QUESTION 1: Largest Range (15 points)

;; largest-range takes a list of ranges (e.g. ((1 2) (4 7))) and returns
;; the longest range (e.g. (4 7)) - by longest we me the range encompassing
;; the most numbers.  There's a few additional rules:
;;
;; * (largest-range '()) is defined to be the special range (0 0).
;;   All other ranges can be assumed to have a positive length (i.e. not
;;   negative or zero).
;;
;; * If there is more than one range with the largest length,
;;   largest-range should return the range earliest in the list
;;   e.g. ((1 2) (3 4)) yields (1 2)
;;
;; For this and all exam problems, check the test cases to see examples.

(define largest-range
  (lambda(ls)
    (cond [(null? ls) '(0 0)]
          [else
           (list-ref  ls (get-index-of-occurance (ranges ls) (getmax (ranges ls)) 0)) 
           ]
      
          )))

(define (get-index-of-occurance ls num index)
  (cond
    [(null? ls) index]
    [(equal? (first ls) num) index]
    [else
     (get-index-of-occurance (rest ls) num (add1 index) )
     ]
    )
  )
(define (ranges ls)
  (map (lambda (ls) (- (second ls) (first ls))) ls)
  )
(define (getmax ls)
  (apply max ls)
  )

;(define largest-range
;  
;  (lambda (lst)
;    (cond
;      [(null? lst) '(0 0)]
;      []
;      )))


;; QUESTION 2: Combine Consecutive  (15 points)

;; combine-consec takes a *sorted* list of integers and combines them
;; into a series of ranges.  It compresses sequences of consecutive
;; numbers into ranges - so for example the list (1 2 3 4) becomes ((1
;; 4)) representing the single range 1-4.  However, when numbers are
;; missing then there must be multiple ranges - e.g. (1 2 3 6 7 8)
;; becomes ((1 3) (6 8)) representing 1-3,6-8.  If a number is by
;; itself (i.e. it is not consecutive with either its successor or
;; predecessor) it can be in a range by itself so (1 2 4 7) becomes
;; ((1 2) (4 4) (7 7)).

;;PURPOSE: takes a list of numbers and returns a list of lists the range 
(define (combine-consec ls)
  (cond
    [(null? ls) '()]
    [(= (length ls) 2)
     (list (list (first ls) (first ls)) (list (second ls) (second ls))) 
     ]
    [(not (null? cddr))
     (if (not (= (- (second ls) (first ls)) 1))
         (combine-consec (list (first ls) (combine-consec  (rest ls))) (combine-consec  (cdr ls)) )
         (cons (list (first ls) (second ls)) (combine-consec  (cddr ls))))
         
         
     ]
    [else
     (combine-consec  (rest ls))
     ]
   
     
    )

  )


(define (combine-helper lon acc)
  (cond
    [(null? lon) acc]
    
    )
  )



(define interval-contains?
  (lambda (interval number)
    (and (<= number (cadr interval)) (>= number (car interval)))))
;; QUESTION 3: Groups (15 points in 2 parts)
;;
;; Mutation is allowed on this question.
;;
;; So a group is going to be a scheme "object" that represents a group
;; of things.  You can add things to a group, check to see if
;; something is in a group, and create subgroups.  Here's the basics:

;; (define mygroup (make-group))
;; (mygroup 'add-member 'buffalo)
;; (mygroup 'has-member 'buffalo) ; returns a true value
;; (mygroup 'has-member 'robert)  ; returns #f

;; This first set of functionality is tested in make-group-basic testcase.
;; The basic functionality is worth 8 points.
;;
;; The last "method" of group is called 'make-subgroup.  Make subgroup
;; returns a new empty group, which has all the usual group
;; functionality (including, of course, make subgroup).  There is one
;; constraint through: unlike a "fresh" group, a subgroup can only
;; have members added to it that were in the original group at the
;; time the subgroup was created.
;;
;; When adding a group member fails, 'add-member can return an error
;; symbol or whatever you prefer (do not call the special scheme
;; procedure error though, that will abort the test).  However, the
;; only test requirement is that a later 'has-member call will not
;; return true.
;;
;; This subgroup functionality is worth 7 points.
;;
;; This continues the mygroup example above:
;;
;; (define mysubgroup (mygroup 'make-subgroup))
;; (mysubgroup 'add-member 'buffalo) ; works
;; (mysubgroup 'add-member 'robert) ; fails because robert is not in parent
;; (mysubgroup 'has-member 'buffalo) ; returns a true value
;; (mysubgroup 'has-member 'robert) ; returns #f


            

(define make-group
  (lambda ()
    (let ([contents '()])
     
      (lambda (command . args)
        (case command
          [(add-member) (set! contents (cons (car args) contents))]
          [(remove) (display "boo")]
          ;           (if (not (remove1 symbol contents))
          ;               (display "error")
          ;               (set! contents (remove1 symbol contents)))]
          [(has-member?) (ormap (lambda (x) (equal? x (car args))) contents) ]
          [(make-subgroup)
           ; (displayln "i am in subgroup")
           (let ([subgroup '()])
             (lambda ()
               ;               
               (lambda (cmd . argument)
                 (case cmd
                   [(add-member)
                    (if (ormap (lambda (x) (equal? x (car argument))) contents)
                        (set! subgroup (cons (car argument) subgroup))
                        #f
                        )
                    ]
                   [(has-member?) (ormap (lambda (x) (equal? x (car argument))) subgroup)]
                   )
                 )
              
               )
             )
           ])))))
(define (contains? ls sym)
  (cond
    [(null? list) #f]
    [(eq? (car ls) sym) #t]
    [else
     (contains? (rest ls) sym)
     ]
    )
    
  )

;(let ((g1 (make-group)))
;  (g1 'add-member 'buffalo)
;  (displayln "add is successful")
;  (let ((g2 (g1 'make-subgroup)))
;    (display "fail is here")
;    (g2 'add-member 'buffalo)
;    (g2 'has-member? 'buffalo)))
                                    


;(define make-bag
;  (lambda ()
;    (let ([contents '()])
;      (lambda (command symbol)
;        (case command
;          [(add) (set! contents (cons symbol contents))]
;          [(remove)
;           (if (not (remove1 symbol contents))
;               (display "error")
;               (set! contents (remove1 symbol contents)))]
;          [(count) (count symbol contents )]
;          [(dump) contents])))))


;; QUESTION 4: Rename Free (15 points)

;; Rename free takes a free variable and renames it in a lambda
;; calculus expression.  You are given 3 values - an old variable
;; name, a new one and a lambda calculus expression.  Return a new
;; lambda calculus expression where all instances of the free variable
;; have been replaced with the new name.

;; For example:
;; (rename-free 'old 'new '(old old)) yields (new new)

;; Note that there might be places in the lambda calculus expression
;; where the old named variable is not free but bound - i.e. there is
;; an enclosing lambda with the name old.  In these cases, the
;; variable should not be renamed.

;; For example:
;; (rename-free 'old 'new '((lambda (old) old) old) yields ((lambda (old) old) new)

;(define rename-free
;  (lambda (old-name new-name lc-exp)
;    (nyi)))

;;this is very similar to an slist follow the standard pattern think of it like
;; just like symbols 
(define rename-free
  (lambda (old-name new-name lc-exp)
    (cond
      [(equal? old-name lc-exp) new-name]
      [(symbol? lc-exp) lc-exp]
      [(empty? lc-exp) '()]

      [else
       (cons (rename-free old-name new-name (first lc-exp))
             (rename-free old-name new-name (rest lc-exp)))
       ]
      )))

;(define occurs-bound?
;  (lambda (var exp)
;    (cond
;      ((symbol? exp) #f)
;      ((eqv? (car exp) 'lambda)
;       (or (occurs-bound? var (caddr exp))
;            (and (eqv? (caadr exp) var)
;                   (occurs-free? var (caddr exp)))))
;      (else (or (occurs-bound? var  (car exp))
;                   (occurs-bound? var (cadr exp)))))))
;; QUESTION 5: Listbox funcs (15 points)

;; Imagine for some totally plausable reason we need to do a lot of
;; calcuations where the values are wrapped in single element lists (I
;; call these listboxes).  E.g. We wanna add (1) (2) and get (3).
;; Because we want to do this with a lot of operations, we don't want
;; to manually write new functions for each one.  Instead, we want to
;; write an function that takes an ordinary function and returns a new
;; version that works on listboxes - i.e. it expects all its
;; parameters be enclosed in listboxes, and the value it returns is
;; enclosed in a listbox.  It'll work like this:

;; (define L+ (listbox-func +))
;; (L+ (1) (2) (L+ (3) (4))) ; yields (10)


(define listbox-func
  (lambda (func)
    (lambda nums
      ;(display (map (lambda (x) (car x))nums) )
      (list (apply func (map (lambda (x) (car x))nums))))))
                    
(define compose
  (case-lambda
    [() (lambda (x) x)]
    [(first . rest)
     (let ([composed-fns 
            (apply compose rest)])
       (lambda (x)
         (first (composed-fns x))))]))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
