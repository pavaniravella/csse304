;;  Object-oriented approach to queue ADT.
;;  The internal data structure:
;;    Elements of the queue are in a list.
;;    We keep a pointer to the front and back of the queue.)



; Starting code for Final exam 202110
; Be sure to follow the rules that are in the problem statement.
; All of your changes should be inside the (define make-queue ...)
; None of your changes should be inside the (case (car args))
; The goal: fix the code to that the reference to self in dequeue-from-list! works correctly.

(define make-queue
  (lambda ()
    (let ([front '()] [rear '()])
      (lambda args
        (case (car args)
	  [(enqueue!) 
	   (let ([x (list (cadr args))])
	     (if (null? front)
		 (set! front x)
		 (set-cdr! rear x))
	     (set! rear x))]
	  [(enqueue-from-list!)
	   (let loop ([ls (cadr args)])
	     (if (not (null? ls))
		 (begin 
		   (self 'enqueue! (car ls))
		   (loop (cdr ls)))))]
	  [(empty?) (null? front)]
	  [(display) (printf "contents: ~s  ~s~%" front rear)]
	  [(dequeue!) (let ([obj (car front)])
			(set! front (cdr front))
			(if (null? front)
			    (set! rear '()))
			obj)])))))



; final exam self test-case:  answer should be #t.
(let  ([q1 (make-queue)] [q2 (make-queue)])
  (q1 'enqueue! 5)
  (q1 'enqueue! 6)
  (q1 'enqueue-from-list! '(1 2 3 4))
  (q2 'enqueue-from-list! '(5 6 1))
  (q2 'enqueue! 2)
  (q2 'enqueue! 3)
  (q2 'enqueue! 4)
  (let loop ()
    (cond [(q1 'empty?) #t]
	  [(= (q1 'dequeue!)(q2 'dequeue!)) (loop)]
	  [else #f])))
		     
