#lang racket

(define (make-rat numer denom)

	(let
		(
			(n
				(cond
					(
						(or (and (positive? numer) (negative? denom)) (and (negative? numer) (negative? denom)))
						(* numer -1)
					)
					(else
						numer
					)
				)
			)
			(d
				(cond
					(
						(or (and (positive? numer) (negative? denom)) (and (negative? numer) (negative? denom)))
						(* denom -1)
					)
					(else
						denom
					)
				)
			)
		)
		(let
			((divisor (gcd n d)))
			(cons (/ n divisor) (/ d divisor))
		)
	)

)

(define (print-rat x)
	(display (numer x))
	(display "/")
	(display (denom x))
)

(define (numer x) (car x))

(define (denom x) (cdr x))

; Tests

> (print-rat (make-rat 2 3))
2/3
> (print-rat (make-rat -2 3))
-2/3
> (print-rat (make-rat 2 -3))
-2/3
> (print-rat (make-rat -2 -3))
2/3
> (print-rat (make-rat -25 -15))
5/3
> (print-rat (make-rat 25 15))
5/3
> (print-rat (make-rat 25 -15))
-5/3
> (print-rat (make-rat -25 15))
-5/3
> 
