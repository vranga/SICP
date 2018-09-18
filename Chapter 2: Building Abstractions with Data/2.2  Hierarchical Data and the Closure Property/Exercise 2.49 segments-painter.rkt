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

; Part a
(define (frame-outline-painter f)
	(paint
		((segments->painter-local
			; The sides of the unit square need to be the segments in this list
			(list
				; edge2
				(make-segment
					(make-vect 0 0)
					(make-vect 0 1)
				)
				; Opposite edge from edge1
				(make-segment
					(make-vect 0 1)
					(make-vect 1 1)
				)
				; Opposite edge from edge2
				(make-segment
					(make-vect 1 1)
					(make-vect 1 0)
				)
				; edge1
				(make-segment
					(make-vect 1 0)
					(make-vect 0 0)
				)
			)
		) f)
	)
)

; Part b
(define (frame-X-painter f)
	(paint
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

; Part c
(define (frame-diamond-painter f)
	(paint
		((segments->painter-local
			; The connectors of the mid-points of the unit square need
			; to be the segments in this list
			(list
				(make-segment
					(make-vect .5 0)
					(make-vect 0 .5)
				)
				(make-segment
					(make-vect 0 .5)
					(make-vect .5 1)
				)
				(make-segment
					(make-vect .5 1)
					(make-vect 1 .5)
				)
				(make-segment
					(make-vect 1 .5)
					(make-vect .5 0)
				)
			)
		) f)
	)
)

; Part d
(define (wave-painter f)
	(paint
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

(define (scale-vect v k)
	; Scales the vector v using the constant k
	(make-vect (* (xcor-vect v) k) (* (ycor-vect v) k))
)

; Tests

; Define some frames
; Note: These frames can be defined anywhere on the coordinate plane but just for
; the plt segments->painter to work properly, I am creating frames inside the
; unit square

(define f1 (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))
(define f2 (make-frame (make-vect .1 .1) (make-vect .6 .2) (make-vect .2 .6)))
(define f3 (make-frame (make-vect .9 .9) (make-vect -.3 0) (make-vect -.4 -.6)))
(define f4 (make-frame (make-vect .5 0) (make-vect -.4 .5) (make-vect .4 .4)))
(define f5 (make-frame (make-vect .9 .1) (make-vect -.4 .3) (make-vect -.3 .4)))
(define f6 (make-frame (make-vect .4 .4) (make-vect .2 0) (make-vect 0 .4)))

(frame-outline-painter f1)
(frame-X-painter f1)
(frame-diamond-painter f1)
(wave-painter f1)
(frame-outline-painter f2)
(frame-X-painter f2)
(frame-diamond-painter f2)
(wave-painter f2)
(frame-outline-painter f3)
(frame-X-painter f3)
(frame-diamond-painter f3)
(wave-painter f3)
(frame-outline-painter f4)
(frame-X-painter f4)
(frame-diamond-painter f4)
(wave-painter f4)
(frame-outline-painter f5)
(frame-X-painter f5)
(frame-diamond-painter f5)
(wave-painter f5)
(frame-outline-painter f6)
(frame-X-painter f6)
(frame-diamond-painter f6)
(wave-painter f6)
