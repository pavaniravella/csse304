; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3
; Travis Zheng (zhengy6), Zeyu Liao (liaoz1)
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    
    (extended-env-record syms vals env)))



(define extend-env-rec
  (lambda (procname ids bodies old-env)
    (extend-env-recursive procname ids bodies old-env)
  )
)

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
        (lookupGlobal sym init-env)
        ; (eopl:error 'env "variable ~s not found." sym)
        ; (prim-proc '+)
      ]

      [extended-env-record (syms vals env)
        (cases environment env 
          [namespace-record-env (syms vals old-env) 
              (let ([pos (list-find-position sym syms)])
             (if (number? pos)
              ; (begin (display vals)
	            (list-ref vals pos)
             (apply-env env sym)
              ))
          ]
          [else (let ((pos (list-find-position sym syms)))
            (if (number? pos)
              (list-ref vals pos)
              (apply-env env sym)
            )
            )]
        )
	    ]
      ;get help from Day25's code
      [extend-env-recursive (procname ids bodies old-env)
        (let ([pos (list-find-position sym procname)])
          (if (number? pos)
            (closure (list-ref ids pos) (list-ref bodies pos) env)
            (apply-env old-env sym)
          )
        )
      ]

      [namespace-record-env (syms vals env) 
        (let ([pos (list-find-position sym syms)])
             (if (number? pos)
              ; (begin (display vals)
	            (list-ref vals pos)
             (apply-env env sym)
        ))
    ]
    
      
      )))


(define lookupGlobal
  (lambda (sym env) 
   (cases environment env 
    [empty-env-record ()   (eopl:error 'env "variable ~s not found." sym)]
    [extended-env-record (syms vals env)
        (let ([pos (list-find-position sym syms)])
             (if (number? pos)
              ; (begin (display vals)
	            (list-ref vals pos)
              
	            (eopl:error 'env "variable ~s not found." sym))
        )
    ]

    
    
    [else (eopl:error 'env "error found." sym)]
   )
    
  ))
