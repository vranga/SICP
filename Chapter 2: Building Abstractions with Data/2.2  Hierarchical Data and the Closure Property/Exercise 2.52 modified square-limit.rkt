#lang racket 

; Exercise 2.52.  Make changes to the square limit of wave shown in figure 2.9 by working at
; each of the levels described above. In particular:

; a.  Add some segments to the primitive wave painter of exercise  2.49 (to add a smile, for
; example).

; b.  Change the pattern constructed by corner-split (for example, by using only one copy of
; the up-split and right-split images instead of two).

; c.  Modify the version of square-limit that uses square-of-four so as to assemble the
; corners in a different pattern. (For example, you might make the big Mr. Rogers look
; outward from each corner of the square.)

; SOLUTION

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; In this exercise, I have used the procedures in the package above. I have used
; segments->painter directly from the package. I am also using transform-painter
; directly from the package. The same is true for vector, segment and frame.
; transform-painter is implemented slightly differently (see below)
; from what is shown in the text book. Accordingly, all other procedures like
; wave-painter and frame-X-painter have been modified to work with the procedures
; in the package

(define (square-limit painter n)
	(let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
		(combine4 (corner-split painter n))
	)
)

(define (square-limit1 painter n)
	(let ((combine4 (square-of-four identity flip-horiz flip-vert rotate180)))
		(combine4 (corner-split painter n))
	)
)

(define (square-of-four tl tr bl br)
	(lambda (painter)
		(let
			(
				(top (beside (tl painter) (tr painter)))
				(bottom (beside (bl painter) (br painter)))
			)
			(below bottom top)
		)
	)
)

(define (corner-split painter n)
	(if (= n 0)
		painter
		(let ((up (up-split painter (- n 1))) (right (right-split painter (- n 1))))
			(let
				(
					(top-left (beside up up))
					(bottom-right (below right right))
					(corner (corner-split painter (- n 1)))
				)
				(beside (below painter top-left) (below bottom-right corner))
			)
		)
	)
)

(define (corner-split1 painter n)
	(if (= n 0)
		painter
		(let ((up (up-split painter (- n 1))) (right (right-split painter (- n 1))))
			(let
				(
					(top-left up)
					(bottom-right right)
					(corner (corner-split painter (- n 1)))
				)
				(beside (below painter top-left) (below bottom-right corner))
			)
		)
	)
)

(define (up-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (up-split painter (- n 1))))
        	(below painter (beside smaller smaller))
		)
	)
)

(define (right-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (right-split painter (- n 1))))
        	(beside painter (below smaller smaller))
		)
	)
)

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

				; eye
				(make-segment (make-vect .365 .8125) (make-vect .4275 .8125))
				; eye
				(make-segment (make-vect .5625 .8125) (make-vect .625 .8125))
				; smile
				(make-segment (make-vect .4275 .6875) (make-vect .5625 .6875))


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
> (paint (wave-painter))
> (paint (corner-split einstein 0))
> (paint (corner-split einstein 1))
> (paint (corner-split einstein 2))
> (paint (corner-split1 einstein 0))
> (paint (corner-split1 einstein 1))
> (paint (corner-split1 einstein 2))
> (paint (corner-split (wave-painter) 5))
> (paint (corner-split (wave-painter) 4))
> (paint (corner-split (wave-painter) 3))
> (paint (corner-split (wave-painter) 2))
> (paint (corner-split (wave-painter) 1))
> (paint (corner-split (wave-painter) 0))
> (paint einstein)
> (paint-hires (square-limit einstein 0))
> (paint-hires (square-limit1 einstein 0))
> (paint-hires (square-limit einstein 1))
> (paint-hires (square-limit1 einstein 1))
> (paint-hires (square-limit einstein 3))
> (paint-hires (square-limit1 einstein 3))
