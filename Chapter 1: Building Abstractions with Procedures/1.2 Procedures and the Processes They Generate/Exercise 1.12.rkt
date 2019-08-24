; Exercise 1.12.  The following pattern of numbers is called Pascal's triangle.

;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1

; The numbers at the edge of the triangle are all 1, and each number inside the triangle
; is the sum of the two numbers above it.35 Write a procedure that computes elements of
; Pascal's triangle by means of a recursive process.

(define error "error")

(define (pascal-internal depth position)
	(cond
		((or (< depth 1) (< position 1)) error)
		((> position depth) error)
		((= depth 1) 1)
		((= position 1) 1)
		((= position depth) 1)
		(else
			(+
				(pascal-internal (- depth 1) (- position 1))
				(pascal-internal (- depth 1) position)
			)
		)
	)
)
