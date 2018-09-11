#lang racket 

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; Note: The implementation of the procedure segments->painter in the textbook
; is different from the similarly named procedure in the sicp plt package.
;
; The plt package procedure takes a list of segments and draws them. Moreover,
; this procedure draws segments only inside the unit square where 0 <= x,y <= 1. Any
; points ouside the unit square are simply ignored (at least, there is no visual
; output)
;
; The textbook implementation also takes a list of segments but instead of drawing them
; directly, evaluates to a procedure that takes a frame as its input.
; This procedure when supplied with a frame, maps all the segments from the originally
; supplied list to this frame and then draws them. Now, the 'draw-line' procedure
; in the textbook implementation has no equivalent in the plt package. So in order
; to use the plt package implementation of segments->painter, I have modified the
; textbook implementation of segments->painter
;
; Finally, to prevent a name-clash, I have renamed the textbook procedure by
; appending a '-local' suffix

(define (below painter1 painter2)
	(let ((split-point (make-vect 0.0 0.5)))
		(let
			(
				(paint-up
					(transform-painter
						painter2
						split-point
						(make-vect 1.0 0.5)
						(make-vect 0.0 1.0)
					)
				)
				(paint-down
					(transform-painter
						painter1
						(make-vect 0.0 0.0)
						(make-vect 1.0 0.0)
						split-point
					)
				)
			)
			(lambda (frame)
				(paint-up frame)
				(paint-down frame)
			)
		)
	)
)

(define (flip-horiz painter)
	(transform-painter
		painter
		(make-vect 1 0)
		(make-vect 0 0)
		(make-vect 1 1)
	)
)

(define (flip-180 painter)
	(transform-painter
		painter
		(make-vect 1 1)
		(make-vect 0 1)
		(make-vect 1 0)
	)
)

(define (flip-270-counter-clockwise painter)
	; same as a 90 degree clock-wise turn
	(transform-painter
		painter
		(make-vect 0 1)
		(make-vect 0 0)
		(make-vect 1 1)
	)
)

(define (shrink-to-middle painter)
	(transform-painter
		painter
		(make-vect .3 .3)
		(make-vect .7 .3)
		(make-vect .3 .7)
	)
)

(define (transform-painter painter origin corner1 corner2)
	(painter
		(make-frame origin
			(sub-vect corner1 origin)
			(sub-vect corner2 origin)
		)
	)
)

(define (frame-X-painter)
	(lambda (f)
		((segments->painter-local
			; The diagonals of the unit square need to be the segments in this list
			(list
				; diagonal 1
				(make-segment
					(make-vect 0 0)
					(make-vect 1 1)
				)
				; diagonal 2
				(make-segment
					(make-vect 0 1)
					(make-vect 1 0)
				)
			)
		) f)
	)
)

(define (wave-painter)
	(lambda (f)
		((segments->painter-local
			; The list of segments that together form the human-like figure
			(list
				(make-segment (make-vect 0 .5) (make-vect .1875 .3125))
				(make-segment (make-vect .1875 .3125) (make-vect .3125 .5625))
				(make-segment (make-vect .3125 .5625) (make-vect .375 .5))
				(make-segment (make-vect .375 .5) (make-vect .25 0))
				(make-segment (make-vect .375 0) (make-vect .5 .3125))
				(make-segment (make-vect .5 .3125) (make-vect .625 0))
				(make-segment (make-vect .75 0) (make-vect .625 .375))
				(make-segment (make-vect .625 .375) (make-vect 1 .1875))
				(make-segment (make-vect 1 .375) (make-vect .75 .625))
				(make-segment (make-vect .75 .625) (make-vect .5625 .625))
				(make-segment (make-vect .5625 .625) (make-vect .6875 .8125))
				(make-segment (make-vect .6875 .8125) (make-vect .5625 1))
				(make-segment (make-vect .4375 1) (make-vect .3125 .8125))
				(make-segment (make-vect .3125 .8125) (make-vect .4375 .625))
				(make-segment (make-vect .4375 .625) (make-vect .25 .625))
				(make-segment (make-vect .25 .625) (make-vect .125 .5))
				(make-segment (make-vect .125 .5) (make-vect 0 .625))
			)
		) f)
	)
)

(define (segments->painter-local segment-list)
	(lambda (frame)
		; The following procedure implementation exists in the plt package
		(segments->painter
			(map
				(lambda (segment)
					(make-segment
						((frame-coord-map frame) (start-segment segment))
						((frame-coord-map frame) (end-segment segment))
					)
				)
				segment-list
			)
		)
	)
)

(define (frame-coord-map frame)
	(lambda (v)
		(add-vect
			(origin-frame frame)
			(add-vect
				(scale-vect (edge1-frame frame) (xcor-vect v))
				(scale-vect (edge2-frame frame) (ycor-vect v))
			)
		)
	)
)

; Frame Definitions

(define (make-frame origin edge1 edge2)
	(list origin edge1 edge2)
)

(define (origin-frame f)
	(car f)
)

(define (edge1-frame f)
	(car (cdr f))
)

(define (edge2-frame f)
	(car (cdr (cdr f)))
)

; Segment Definitions

(define (make-segment v1 v2)
	(cons v1 v2)
)

(define (start-segment s)
	(car s)
)

(define (end-segment s)
	(cdr s)
)

; Vector Definitions

(define (make-vect x y)
	(cons x y)
)

(define (xcor-vect v)
	(car v)
)

(define (ycor-vect v)
	(cdr v)
)

(define (add-vect v1 v2)
	; evaluates v1 + v2
	(make-vect
		(+ (xcor-vect v1) (xcor-vect v2))
		(+ (ycor-vect v1) (ycor-vect v2))
	)
)

(define (sub-vect v1 v2)
	; evaluates v1 - v2
	(make-vect
		(- (xcor-vect v1) (xcor-vect v2))
		(- (ycor-vect v1) (ycor-vect v2))
	)
)

(define (scale-vect v k)
	; Scales the vector v using the constant k
	(make-vect (* (xcor-vect v) k) (* (ycor-vect v) k))
)

; Tests

; Define some frames
; Note: These frames can be defined anywhere on the coordinate plane but just for
; the plt segments->painter to work properly, I am creating frames inside the
; unit square

(define unit-square (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))
(define w (wave-painter))
(define X (frame-X-painter))
(paint (w unit-square))
(paint (shrink-to-middle w))
(paint (below w w))
(paint (below X w))
(paint (below w X))
