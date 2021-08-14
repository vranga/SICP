#lang racket

(define the-empty-stream '())
(define (stream-empty? s)
	(if (eq? s the-empty-stream)
		true
		false
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

(define (add-streams s1 s2)
	(if (stream-empty? s2)
		s1
		(if (stream-empty? s1)
			s2
			(cons-stream
				(+ (stream-car s1) (stream-car s2))
				(add-streams (stream-cdr s1) (stream-cdr s2))
			)
		)
	)	
)

(define stream-of-five-elements (stream-enumerate-interval 5 9))
stream-of-five-elements
(stream-cdr stream-of-five-elements)
(stream-cdr (stream-cdr stream-of-five-elements))
(stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))
(stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements))))
(stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))))

(stream-car stream-of-five-elements)
(stream-car (stream-cdr stream-of-five-elements))
(stream-car (stream-cdr (stream-cdr stream-of-five-elements)))
(stream-car (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements))))
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))))

(define one-to-six (stream-enumerate-interval 1 6))
(define eleven-to-sixteen (stream-enumerate-interval 11 16))
(define sum-stream (add-streams one-to-six eleven-to-sixteen))
sum-stream
(stream-car sum-stream)
(stream-car (stream-cdr sum-stream))
(stream-car (stream-cdr (stream-cdr sum-stream)))
(stream-car (stream-cdr (stream-cdr (stream-cdr sum-stream))))
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr sum-stream)))))
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr sum-stream))))))

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

(define (solve f y0 dt)
	(define y (integral (delay dy) y0 dt))
	(define dy (stream-map f y))
	y
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
