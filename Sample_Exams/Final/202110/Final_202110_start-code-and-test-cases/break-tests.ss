; uncomment each test when you are ready to try it.

> (eval-one-exp
  '(let ([sum 0] [max 6])
     (while (> max 0)
	    (begin (set! sum (+ sum max))
		   (set! max (- max 1))
		   (if (= max 3) (break))))
     sum))
15
> (eval-one-exp
 '(let ([i 0] [sum 0])
    (while (< i 6)
	   (begin (if (= i 3)
		      (break))
		  (set! i (+ i 1))
		  (set! sum (+ i sum))))
    (list i sum)))
(3 6)
> (eval-one-exp
  '(let ([sum 0]
	 [max 6]
	 [test (lambda (n) (if (= n 3) (break)))])
     (while (> max 0)
	    (begin (set! sum (+ sum max))
		   (set! max (- max 1))
		   (test max)))
     sum))
15
> (eval-one-exp
 '(let ([main-list (list)] [max 5])
    (while (> max 0)
	   (let ([counter max][inner-list (list)])
	     (while (> counter 0)
		    (begin
		      (set! inner-list (cons counter inner-list))
		      (if (= counter 3)
			  (break))
		      (set! counter (- counter 1))))
	     (set! main-list (cons inner-list main-list))
	     (set! max (- max 1))))
    main-list))
((1) (1 2) (3) (3 4) (3 4 5))
> (eval-one-exp
 '(let ([main-list '()] [max 1])
    (while (<= max 6)
	   (let ([counter 1][inner-list '()])
	     (while (<= counter max)
		    (begin 
		    (set! inner-list (cons counter inner-list))
		    (if (= counter 4)
			(break 1))
		    (set! counter (+ counter 1))))
	     (set! main-list (cons inner-list main-list))
	     (set! max (+ max 1))))
    main-list))
((3 2 1) (2 1) (1))
> (eval-one-exp
 '(let ([i 1] [j 1] [k 1] [m 1] [sum 0])
  (while (< i 3)
	 (begin
	   (set! i (+ i 1))
	   (set! j 1)
	   (while (<= j i)
		  (begin
		    (set! j (+ 1 j))
		    (set! k 1)
		    (while (<= k (* j j))
			   (begin
			     (set! k (+ k i))
			     (set! m 1)
			     (while (<= m k)
				    (begin
				      (set! sum (+ sum 1))
				      (if (> sum 20) (break))
				      (set! m (+ m 1)))
				  )))))))
  (list i j k m sum)))
(3 4 19 1 34)
> (eval-one-exp
 '(let ([i 1] [j 1] [k 1] [m 1] [sum 0])
  (while (< i 3)
	 (begin
	   (set! i (+ i 1))
	   (set! j 1)
	   (while (<= j i)
		  (begin
		    (set! j (+ 1 j))
		    (set! k 1)
		    (while (<= k (* j j))
			   (begin
			     (set! k (+ k i))
			     (set! m 1)
			     (while (<= m k)
				    (begin
				      (set! sum (+ sum 1))
				      (if (> sum 20) (break 0))
				      (set! m (+ m 1)))
				  )))))))
  (list i j k m sum)))
(3 4 19 1 34)
> (eval-one-exp
 '(let ([i 1] [j 1] [k 1] [m 1] [sum 0])
  (while (< i 3)
	 (begin
	   (set! i (+ i 1))
	   (set! j 1)
	   (while (<= j i)
		  (begin
		    (set! j (+ 1 j))
		    (set! k 1)
		    (while (<= k (* j j))
			   (begin
			     (set! k (+ k i))
			     (set! m 1)
			     (while (<= m k)
				    (begin
				      (set! sum (+ sum 1))
				      (if (> sum 20) (break 1))
				      (set! m (+ m 1)))
				  )))))))
  (list i j k m sum)))
(3 4 4 1 24)
> (eval-one-exp
 '(let ([i 1] [j 1] [k 1] [m 1] [sum 0])
  (while (< i 3)
	 (begin
	   (set! i (+ i 1))
	   (set! j 1)
	   (while (<= j i)
		  (begin
		    (set! j (+ 1 j))
		    (set! k 1)
		    (while (<= k (* j j))
			   (begin
			     (set! k (+ k i))
			     (set! m 1)
			     (while (<= m k)
				    (begin
				      (set! sum (+ sum 1))
				      (if (> sum 20) (break 2))
				      (set! m (+ m 1)))
				  )))))))
  (list i j k m sum)))
(3 2 4 1 22)
> (eval-one-exp
 '(let ([i 1] [j 1] [k 1] [m 1] [sum 0])
  (while (< i 3)
	 (begin
	   (set! i (+ i 1))
	   (set! j 1)
	   (while (<= j i)
		  (begin
		    (set! j (+ 1 j))
		    (set! k 1)
		    (while (<= k (* j j))
			   (begin
			     (set! k (+ k i))
			     (set! m 1)
			     (while (<= m k)
				    (begin
				      (set! sum (+ sum 1))
				      (if (> sum 20) (break 3))
				      (set! m (+ m 1)))
				  )))))))
  (list i j k m sum)))
(2 3 7 5 21)
> (eval-one-exp
 '(let* ([i 1]
       [k 1]
       [sum 0]
       [break-test (lambda (num) (break (- 1 num)))])
  (while (< i 6)
	 (begin 
	   (set! k 1)
	   (while (<= k i)
		  (begin
		    (set! sum (+ sum i))
		    (if (= sum 8) (break-test 0))
		    (set! k (+ 1 k))))
           (set! i (+ 1 i))))
  (list i k sum)))
(3 1 8)
> (eval-one-exp
 '(let* ([i 1]
       [k 1]
       [sum 0]
       [break-test (lambda (num) (break (- 1 num)))])
  (while (< i 6)
	 (begin 
	   (set! k 1)
	   (while (<= k i)
		  (begin
		    (set! sum (+ sum i))
		    (if (= sum 8) (break-test 1))
		    (set! k (+ 1 k))))
           (set! i (+ 1 i))))
  (list i k sum)))
(6 6 49)
> (eval-one-exp
   '(let ([i 1] [j 1] [k 1] [sum 0])
  (while (<= i 3)
	 (begin (set! j 1)
		(while (<= j i)
		       (begin
			 (if (= j 2) (break 0))
			 (set! sum (+ sum i j))
			 (set! j (+ 1 j))))
		(set! k 1)
		(while (<= k i)
		       (begin
			 (set! sum (+ sum k))
			 (set! k (+ 1 k))))
		(set! i (+ 1 i))))
  (list i j k sum)))
(4 2 4 19)
> (eval-one-exp
   '(let ([i 1] [j 1] [k 1] [sum 0])
  (while (<= i 3)
	 (begin (set! j 1)
		(while (<= j i)
		       (begin
			 (if (= j 2) (break 1))
			 (set! sum (+ sum i j))
			 (set! j (+ 1 j))))
		(set! k 1)
		(while (<= k i)
		       (begin
			 (set! sum (+ sum k))
			 (set! k (+ 1 k))))
		(set! i (+ 1 i))))
  (list i j k sum)))
(2 2 2 6)
>
