#lang racket

(define (cons a b)
	(define (dispatch m)
		(let
			((store (* (expt 2 a) (expt 3 b))))
			(cond
				((= m 0) (number-of-times-y-divides-x store 2))
				((= m 1) (number-of-times-y-divides-x store 3))
				(else (error "Argument not 0 or 1 -- CONS" m))
			)
		)
	)
	dispatch
)

(define (car z)
	(z 0)
)

(define (cdr z)
	(z 1)
)

(define (number-of-times-y-divides-x x y)
	(define rem (remainder x y))
	(cond
		((= rem 0) (+ 1 (number-of-times-y-divides-x (/ x y) y)))
		(else
			0
		)
	)
)

; Tests

> (car (cons 15 17))
15
> (cdr (cons 15 17))
17
> (cdr (cons 24 2))
2
> (car (cons 24 2))
24
> (car (cons 2 24))
2
> (cdr (cons 2 24))
24
> 
