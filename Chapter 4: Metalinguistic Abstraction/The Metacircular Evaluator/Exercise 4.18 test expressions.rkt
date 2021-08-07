#lang racket

(define (solve f y0 dt)
	(define y (integral (delay dy) y0 dt))
	(define dy (stream-map f y))
	y
)

(define (integral delayed-integrand initial-value dt)
	(define int
		(stream-cons initial-value
			(let ((integrand (force delayed-integrand)))
				(add-streams
					(scale-stream integrand dt)
					int
				)
			)
		)
	)
	int
)

(define (add-streams s1 s2)
	(if (stream-empty? s2)
		s1
		(if (stream-empty? s1)
			s2
			(stream-cons
				(+ (stream-first s1) (stream-first s2))
				(add-streams (stream-rest s1) (stream-rest s2))
			)
		)
	)	
)

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream)
)

(define (stream-map proc stream)
	(if (stream-empty? stream)
		empty-stream
		(stream-cons
			(apply proc (list (stream-first stream)))
			(stream-map proc (stream-rest stream))
		)
	)
)

(define (stream-ref s n)
	(if (= n 0)
		(stream-first s)
		(stream-ref (stream-rest s) (- n 1))
	)
)

(define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream
			low
			(stream-enumerate-interval (+ low 1) high)
		)
	)
)

(define stream-of-ten-elements (stream-enumerate-interval 5 14))
(stream-car stream-of-ten-elements)

; Tests

(stream-ref (solve (lambda (y) y) 1 0.001) 1)
(stream-ref (solve (lambda (y) y) 1 0.001) 10)
(stream-ref (solve (lambda (y) y) 1 0.001) 100)
(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

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
