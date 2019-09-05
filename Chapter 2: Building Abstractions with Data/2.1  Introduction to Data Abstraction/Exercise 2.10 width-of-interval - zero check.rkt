#lang racket

; Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder
; and comments that it is not clear what it means to divide by an interval that spans zero.
; Modify Alyssa's code to check for this condition and to signal an error if it occurs.

; SOLUTION

(define (make-interval a b)
	(cons a b)
)

(define (upper-bound interval)
	(cdr interval)
)

(define (lower-bound interval)
	(car interval)
)

(define (width-of-interval interval)
	(/ (- (upper-bound interval) (lower-bound interval)) 2)
)

(define (add-interval x y)
	(make-interval
		(+ (lower-bound x) (lower-bound y))
		(+ (upper-bound x) (upper-bound y))
	)
)

(define (sub-interval x y)
	(make-interval
		(- (lower-bound x) (upper-bound y))
		(- (upper-bound x) (lower-bound y))
	)
)

(define (mul-interval x y)
	(let
		((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
		(make-interval
			(min p1 p2 p3 p4)
			(max p1 p2 p3 p4)
		)
	)
)

(define (div-interval x y)
	(cond
		((and (>= (upper-bound y) 0) (<= (lower-bound y) 0)) (error "Cannot divide by interval that spans zero"))
	)
	(mul-interval
		x
		(make-interval
			(/ 1.0 (upper-bound y))
			(/ 1.0 (lower-bound y))
		)
	)
)

; Tests

> (define A (make-interval 60 100))
> (define B (make-interval -10 50))
> (define C (make-interval 10 30))
> (div-interval A C)
'(2.0 . 10.0)
> (div-interval A B)
. . Cannot divide by interval that spans zero
