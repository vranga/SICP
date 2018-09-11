(define (integer-mult a b)
	(if (= b 0) 0 (+ a (integer-mult a (- b 1))))
)

(define (double x)
	(* x 2)
)

(define (halve x)
	(/ x 2)
)

(define (fast-integer-mult a b)
	(cond
		((= b 0) 0)
		((even? b) (fast-integer-mult (double a) (halve b)))
		(else
			(+ a (fast-integer-mult a (- b 1)))
		)
	)
)

(define (even? n)
	(= (remainder n 2) 0)
)
