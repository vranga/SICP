#lang racket

; Exercise 2.9.  The width of an interval is half of the difference between its upper and lower
; bounds. The width is a measure of the uncertainty of the number specified by the interval.
; For some arithmetic operations the width of the result of combining two intervals is a
; function only of the widths of the argument intervals, whereas for others the width of the
; combination is not a function of the widths of the argument intervals. Show that the width
; of the sum (or difference) of two intervals is a function only of the widths of the intervals
; being added (or subtracted). Give examples to show that this is not true for multiplication
; or division.

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
	(mul-interval
		x
		(make-interval
			(/ 1.0 (upper-bound y))
			(/ 1.0 (lower-bound y))
		)
	)
)

; Tests

> (width-of-interval (make-interval -10 50))
30
> (width-of-interval (make-interval 60 100))
20
> (define A (make-interval 60 100))
> (define B (make-interval -10 50))
> (width-of-interval A)
20
> (width-of-interval B)
30
> (define C (add-interval A B))
> C
'(50 . 150)
> (width-of-interval C)
50
> (define D (sub-interval A B))
> (width-of-interval D)
50
> (define E (mul-interval A B))
> E
'(-1000 . 5000)
> (width-of-interval E)
3000
> (define F (div-interval A B))
> F
'(-10.0 . 2.0)
> (width-of-interval F)
6.0
> 
