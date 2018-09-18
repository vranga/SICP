#lang racket 

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; In this exercise, I have used the procedures in the package above. I have used
; segments->painter directly from the package. I am also using transform-painter
; directly from the package. The same is true for vector, segment and frame.
; transform-painter is implemented slightly differently (see below)
; from what is shown in the text book. Accordingly, all other procedures like
; wave-painter and frame-X-painter have been modified to work with the procedures
; in the package

(define (below painter1 painter2)
	(flip-90-counter-clockwise
		(beside (flip-90-clockwise painter1) (flip-90-clockwise painter2))
	)
)

(define (beside painter1 painter2)
	(let ((split-point (make-vect 0.5 0.0)))
		(let
			(
				(paint-left
					((transform-painter
						(make-vect 0.0 0.0)
						split-point
						(make-vect 0.0 1.0)
					) painter1)
				)
				(paint-right
					((transform-painter
						split-point
						(make-vect 1.0 0.0)
						(make-vect 0.5 1.0)
					) painter2)
				)
			)
			(lambda (f)
				(paint-left f)
				(paint-right f)
			)
		)
	)
)

(define (flip-horiz painter)
	((transform-painter
		(make-vect 1 0)
		(make-vect 0 0)
		(make-vect 1 1)
	) painter)
)

(define (flip-180 painter)
	((transform-painter
		(make-vect 1 1)
		(make-vect 0 1)
		(make-vect 1 0)
	) painter)
)

(define (flip-90-clockwise painter)
	(flip-270-counter-clockwise painter)
)

(define (flip-270-counter-clockwise painter)
	; same as a 90 degree clock-wise turn
	((transform-painter
		(make-vect 0 1)
		(make-vect 0 0)
		(make-vect 1 1)
	) painter)
)

(define (flip-90-counter-clockwise painter)
	(flip-270-clockwise painter)
)

(define (flip-270-clockwise painter)
	; same as a 90 degree counter-clock-wise turn
	((transform-painter
		(make-vect 1 0)
		(make-vect 1 1)
		(make-vect 0 0)
	) painter)
)

(define (shrink-to-middle painter)
	((transform-painter
		(make-vect .3 .3)
		(make-vect .7 .3)
		(make-vect .3 .7)
	) painter)
)

(define (frame-X-painter)
	(segments->painter
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
	)
)

(define (wave-painter)
	(lambda (f)
		((segments->painter
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

(define (frame-coord-map f)
	(lambda (v)
		(vector-add
			(frame-origin f)
			(vector-add
				(vector-scale (frame-edge1 f) (vector-xcor v))
				(vector-scale (frame-edge2 f) (vector-ycor v))
			)
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define dude (wave-painter))
> (define X (frame-X-painter))
> (define e einstein)
> (below dude dude)
> (paint (below dude dude))
> (paint (below X X))
> (paint (below e e))
> (paint (beside dude dude))
> (paint (beside X X))
> (paint (beside e e))
> (paint (beside e dude))
> (paint (beside dude X))
> (paint (beside X e))
> (paint (below X e))
> (paint (below e (flip-180 e)))
> (paint (below (flip-180 e) e))
> (paint (below (flip-180 dude) dude))
