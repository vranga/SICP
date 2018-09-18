#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split f1 f2)
	(lambda (painter n)
		(if (= n 0)
			painter
			(let ((smaller ((split f1 f2) painter (- n 1))))
				(f1 painter (f2 smaller smaller))
			)
		)
	)
)

(define right-split (split beside below))

(define up-split (split below beside))

; Test Results in the image(s) below
