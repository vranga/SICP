#lang racket

; Exercise 2.2.  Consider the problem of representing line segments in a plane. Each segment
; is represented as a pair of points: a starting point and an ending point. Define a
; constructor make-segment and selectors start-segment and end-segment that define the
; representation of segments in terms of points. Furthermore, a point can be represented as
; a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify a
; constructor make-point and selectors x-point and y-point that define this representation.
; Finally, using your selectors and constructors, define a procedure midpoint-segment that
; takes a line segment as argument and returns its midpoint (the point whose coordinates are
; the average of the coordinates of the endpoints). To try your procedures, you'll need a
; way to print points:

; (define (print-point p)
;   (newline)
;   (display "(")
;   (display (x-point p))
;   (display ",")
;   (display (y-point p))
;   (display ")"))

; SOLUTION

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

(define (print-point p)
	; (newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")")
)

; Tests

> (print-point (make-point 4 5))
(4,5)
> (print-point (midpoint-segment (make-segment (make-point 4 5) (make-point 10 13))))
(7,9)
> (print-point (midpoint-segment (make-segment (make-point 5 5) (make-point -5 -5))))
(0,0)
> (print-point (midpoint-segment (make-segment (make-point 5 5) (make-point -5 5))))
(0,5)
> (print-point (midpoint-segment (make-segment (make-point 5 5) (make-point 5 -5))))
(5,0)
> (print-point (midpoint-segment (make-segment (make-point 6 12) (make-point 5 -5))))
(11/2,7/2)
> 
