#lang racket

(define (smooth f)
	(lambda (x)
		(/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)
	)
)

(define (repeated f n)
	(cond
		((> n 1) (compose f (repeated f (- n 1))))
		((= n 1) f)
		(else
			identity
		)
	)
)

(define (compose f g)
	(lambda (w) (f (g w)))
)

(define (identity x)
	x
)

(define dx 0.00001)

(define (n-fold-smooth n)
	(repeated smooth n)
)

; Tests
