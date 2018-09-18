(define (fast-fib-iter n)
	(fast-fib-iter-internal 1 0 0 1 n)
)

(define (fast-fib-iter-internal a b p q count)
	(cond
		((= count 0) b)
		((even? count)
			(fast-fib-iter-internal
				a
				b
                (+ (square p) (square q)) ; compute p'
                (+ (square q) (* 2 p q)) ; compute q'
				(/ count 2)
			)
		)
        (else
			(fast-fib-iter-internal
				(+ (* b q) (* a q) (* a p))
				(+ (* b p) (* a q))
				p
				q
				(- count 1)
			)
		)
	)
)

(define (square x)
	(* x x)
)

(define (fib n)
	(cond
		((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1)) (fib (- n 2))))
	)
)

(define (fib-iter n)
	(fib-iter-internal 1 0 n)
)

(define (fib-iter-internal a b count)
	(if
		(= count 0) b
		(fib-iter-internal (+ a b) a (- count 1))
	)
)
