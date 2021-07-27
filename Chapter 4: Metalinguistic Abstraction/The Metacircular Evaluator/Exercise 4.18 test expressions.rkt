(define (delay expression)
	(lambda () expression)
)

(define (stream-ref s n)
	; (display "Entered stream-ref with n = ")
	; (display n)
	; (newline)
	(if (= n 0)
		(stream-first s)
		(stream-ref (stream-rest s) (- n 1))
	)
)

(define (integral delayed-integrand initial-value dt)
	(stream-cons
		initial-value
		(let ((integrand (force delayed-integrand)))
			(if (stream-empty? integrand)
				empty-stream
				(integral
					(stream-rest integrand)
					(+ (* dt (stream-first integrand))
					initial-value)
					dt
				)
			)
		)
	)
)

(define (solve f y0 dt)
	(define y (integral (delay dy) y0 dt))
	(define dy (stream-map f y))
	y
)

(define (solve f y0 dt)
	(let ((y '*unassigned*) (dy '*unassigned*))
		(let ((a (integral (delay dy) y0 dt)) (b (stream-map f y)))
			(set! y a)
			(set! dy b)
		)
		y
	)
)

(stream-ref (solve (lambda (y) y) 1 0.001) 2)

(define (transformed-solve f y0 dt)
	(let ((y '*unassigned*) (dy '*unassigned*))
		(let ((a (integral (delay dy) y0 dt)) (b (stream-map f y)))
			(set! y a)
			(set! dy b)
		)
		y
	)
)

(stream-ref (transformed-solve (lambda (y) y) 1 0.001) 1000)

(define (f x)
	(define (even? n)
		(if (= n 0)
			true
			(odd? (- n 1))
		)
	)
	(define (odd? n)
		(if (= n 0)
			false
			(even? (- n 1))
		)
	)

	(odd? x)
)
