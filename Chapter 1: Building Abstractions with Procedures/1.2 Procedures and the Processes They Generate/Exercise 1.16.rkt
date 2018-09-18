(define (fast-expt-iter base exponent)
	(fast-expt-iter-internal base exponent 1)
)

(define (fast-expt-iter-internal base exponent accumulator)
	(cond
		((= exponent 0) accumulator)
		((even? exponent)
			(fast-expt-iter-internal (square base) (/ exponent 2) accumulator)
		)
		(else
			(fast-expt-iter-internal base (- exponent 1) (* accumulator base))
		)
	)
)

(define (square x)
	(* x x)
)

(define (fast-expt b n)
	(cond
		((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
        (else
			 (* b (fast-expt b (- n 1)))
		)
	)
)

(define (even? n)
  (= (remainder n 2) 0)
)
