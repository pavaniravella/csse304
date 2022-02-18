; uncomment each test when you are ready to run it.

;; > (eval-one-exp
;;     '(let ([a 1] [b 2] [c 3])
;;        (mset! (a b c) ((+ b c) (+ 1 c) (+ a (+ b c))))
;;        (list a b c)))
;; (5 4 6)

;; >(eval-one-exp
;;   '(let ([a 2] [b 3])
;;      (mset! (a b) (b a))
;;      (list a b))
;; (3 2)
