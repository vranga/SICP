#lang racket

; Perimeter of Rectangle
(define (perimeter-rect r)
	(* 2 (+ (dim1-rect r) (dim2-rect r)))
)

; Area of Rectangle
(define (area-rect r)
	(* (dim1-rect r) (dim2-rect r))
)

; Print Rectangle
(define (print-rect r)
	(display "[")
	(print-point (start-segment (diag1-rect r)))
	(display " ")
	(print-point (end-segment (diag2-rect r)))
	(display " ")
	(print-point (end-segment (diag1-rect r)))
	(display " ")
	(print-point (start-segment (diag2-rect r)))
	(display "]")
)

; Rectangle Construction from two line segments which are any two sides with a common end point
; The segments have to be at 90 degrees to each other otherwise it will be an error
(define (make-rect side1 side2)
	; Check for validity first
	(cond
		; the two segments should have a common point
		(
			(and
				(and
					(not (points-equal? (start-segment side1) (start-segment side2)))
					(not (points-equal? (start-segment side1) (end-segment side2)))
				)
				(and
					(not (points-equal? (end-segment side1) (start-segment side2)))
					(not (points-equal? (end-segment side1) (end-segment side2)))
				)
			)
			(error "Invalid Input: Segments do not share a common point")
		)
		; the two segments should be at right angles to each other (dot product should be zero)
		(
			(not (orthogonal? side1 side2))
			(error "Invalid Input: Segments are not orthogonal to each other")
		)
		(else
			; construct the rectangle
			(cons side1 side2)
		)
	)
)

; Rectangle Selectors (return the two sides of the rectangle)
(define (dim1-rect r)
	(length-of-segment (car r))
)

(define (dim2-rect r)
	(length-of-segment (cdr r))
)

(define (diag1-rect r)
	; Find common point between the two given sides
	(define cp (common-point (car r) (cdr r)))
	; Find 3rd side that completes the triangle (this will be one diagonal)
	(make-segment
		(cond
			((points-equal? (start-segment (car r)) cp) (end-segment (car r)))
			((points-equal? (end-segment (car r)) cp) (start-segment (car r)))
		)
		(cond
			((points-equal? (start-segment (cdr r)) cp) (end-segment (cdr r)))
			((points-equal? (end-segment (cdr r)) cp) (start-segment (cdr r)))
		)
	)
)

(define (diag2-rect r)
	(define rect-center (midpoint-segment (diag1-rect r)))
	; Find common point between the two given sides
	(define cp (common-point (car r) (cdr r)))
	; Find 2nd diagonal
	(make-segment
		cp
		(make-point
			(+ (x-point cp) (* 2 (- (x-point rect-center) (x-point cp))))
			(+ (y-point cp) (* 2 (- (y-point rect-center) (y-point cp))))
		)
	)
)

; Mid-point of a segment
(define (midpoint-segment line-segment)
	(make-point 
		(/ (+ (x-point (start-segment line-segment)) (x-point (end-segment line-segment))) 2)
		(/ (+ (y-point (start-segment line-segment)) (y-point (end-segment line-segment))) 2)
	)
)

; Line Segment Constructor
(define (make-segment start-segment end-segment)
	(cons start-segment end-segment)
)

; Line Segment Selectors
(define (start-segment segment)
	(car segment)
)

(define (end-segment segment)
	(cdr segment)
)

; Line Segment length
(define (length-of-segment segment)
	(sqrt
		(+
			(square (- (x-point (end-segment segment)) (x-point (start-segment segment))))
			(square (- (y-point (end-segment segment)) (y-point (start-segment segment))))
		)
	)
)

; Are two segments at right angles to each other?
(define (orthogonal? segment1 segment2)
	; compute the dot product to determine orthogonality
	(= 
		0
		(+
			(* (x-comp segment1) (x-comp segment2))
			(* (y-comp segment1) (y-comp segment2))
		)
	)
)

; Comparing two segments (are they the same?)
(define (segments-same? segment1 segment2)
	(or
		(and (points-equal? (start-segment segment1) (start-segment segment2)) (points-equal? (end-segment segment1) (end-segment segment2)))
		(and (points-equal? (start-segment segment1) (end-segment segment2)) (points-equal? (end-segment segment1) (start-segment segment2)))
	)
)

; Find a common end-point between two segments
(define (common-point segment1 segment2)
	(cond
		((points-equal? (start-segment segment1) (start-segment segment2)) (start-segment segment1))
		((points-equal? (start-segment segment1) (end-segment segment2)) (start-segment segment1))
		((points-equal? (end-segment segment1) (start-segment segment2)) (end-segment segment1))
		((points-equal? (end-segment segment1) (end-segment segment2)) (end-segment segment1))
		(else
			(error "No common point")
		)
	)
)

; Components of a segment
(define (x-comp segment)
	(- (x-point (end-segment segment)) (x-point (start-segment segment)))
)

(define (y-comp segment)
	(- (y-point (end-segment segment)) (y-point (start-segment segment)))
)

; Point Constructor
(define (make-point x-coord y-coord)
	(cons x-coord y-coord)
)

; Point Selectors
(define (x-point point)
	(car point)
)

(define (y-point point)
	(cdr point)
)

; Equality of two points (i.e. are two points the same?)
(define (points-equal? point1 point2)
	(and (= (x-point point1) (x-point point2)) (= (y-point point1) (y-point point2)))
)

(define (print-point p)
	; (newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")")
)

(define (square x)
	(* x x)
)

; Tests

> (define a (make-point 1 1))
(define b (make-point 11 1))
(define c (make-point 11 7))
(define d (make-point 1 7))

(define side1 (make-segment a b))
(define side2 (make-segment a d))
(define r (make-rect side1 side2))
(perimeter-rect r)
(area-rect r)
32
60
> (define m (make-point 5 0))
(define n (make-point 22 17))
(define o (make-point 17 22))
(define p (make-point 0 5))

(define side1 (make-segment m n))
(define side2 (make-segment m p))
(define r (make-rect side1 side2))
(perimeter-rect r)
(area-rect r)
(dim1-rect r)
(dim2-rect r)
(* (dim1-rect r) (dim2-rect r))
62.22539674441618
170.0
24.041630560342615
7.0710678118654755
170.0

> (define w (make-point 0 0))
(define x (make-point 4 4))
(define y (make-point 10 3))
(define z (make-point -10 7))
(make-rect (make-segment w y) (make-segment z x))
. . Invalid Input: Segments do not share a common point

> (define a (make-point 4 4))
(define b (make-point 4 10))
(define c (make-point 4 10))
(define d (make-point 4 4))
(define r (make-rect (make-segment a c) (make-segment d b)))
. . Invalid Input: Segments are not orthogonal to each other
> 


(define a (make-point 5 0))
(define b (make-point 0 5))
(define c (make-point -5 0))
(define d (make-point 0 -5))
(define r (make-rect (make-segment a b) (make-segment c a)))
(perimeter-rect r)
28.284271247461902
> (area-rect r)
50.00000000000001
> (define d (make-point 0 -6))
> (define r (make-rect (make-segment a c) (make-segment b d)))
. . Invalid Input: Segments have unequal lengths
> (define a (make-point 6 0))
> (define r (make-rect (make-segment a c) (make-segment b d)))
. . Invalid Input: Mid-points of segments don't coincide

> (define a (make-point 5 0))
(define b (make-point 0 5))
(define c (make-point -5 0))
(define d (make-point 0 -5))
(define r (make-rect (make-segment a b) (make-segment c d)))
. . Invalid Input: Segments do not share a common point

> (define a (make-point 5 0))
(define b (make-point 0 5))
(define c (make-point -5 0))
(define d (make-point 0 -5))
(define r (make-rect (make-segment a b) (make-segment c a)))
. . Invalid Input: Segments are not orthogonal to each other

> (define a (make-point 5 0))
(define b (make-point 0 5))
(define c (make-point -5 0))
(define d (make-point 0 -5))
(define r (make-rect (make-segment a b) (make-segment b c)))
(define r (make-rect (make-segment a b) (make-segment a c)))
. . Invalid Input: Segments are not orthogonal to each other

> (define r (make-rect (make-segment a b) (make-segment c a)))
. . Invalid Input: Segments are not orthogonal to each other

> (define r (make-rect (make-segment a b) (make-segment b c)))
> (define r (make-rect (make-segment a b) (make-segment c b)))
> (define r (make-rect (make-segment b a) (make-segment b c)))
> (define r (make-rect (make-segment b a) (make-segment c b)))
> (define r (make-rect (make-segment b a) (make-segment a c)))
. . Invalid Input: Segments are not orthogonal to each other

> (define a (make-point 2 0))
> (define b (make-point 0 2))
> (define c (make-point -2 0))
> (define d (make-point 0 -2))
> (define r (make-rect (make-segment a b) (make-segment a d)))
> (define r (make-rect (make-segment a b) (make-segment d a)))
> (define r (make-rect (make-segment b a) (make-segment a d)))
> (define r (make-rect (make-segment b a) (make-segment d a)))
> (perimeter-rect r)
11.313708498984761
> (area-rect r)
8.000000000000002
> 
