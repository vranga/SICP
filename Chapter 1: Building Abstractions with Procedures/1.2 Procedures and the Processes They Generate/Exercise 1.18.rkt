(define (integer-mult a b)
	(if (= b 0) 0 (+ a (integer-mult a (- b 1))))
)

(define (double x)
	(* x 2)
)

(define (halve x)
	(/ x 2)
)

(define (fast-integer-mult-iter a b)
	(fast-integer-mult-iter-internal a b 0)
)

(define (fast-integer-mult-iter-internal a b accumulator)
	(cond
		((= b 0) accumulator)
		((even? b) (fast-integer-mult-iter-internal (double a) (halve b) accumulator))
		(else
			(fast-integer-mult-iter-internal a (- b 1) (+ accumulator a))
		)
	)
)

(define (even? n)
	(= (remainder n 2) 0)
)
