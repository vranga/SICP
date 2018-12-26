#lang racket

; Exercise 3.16.  Ben Bitdiddle decides to write a procedure to count the number of pairs
; in any list structure. "It's easy," he reasons. "The number of pairs in any structure
; is the number in the car plus the number in the cdr plus one more to count the current
; pair." So Ben writes the following procedure:

; (define (count-pairs x)
;   (if (not (pair? x))
;       0
;       (+ (count-pairs (car x))
;          (count-pairs (cdr x))
;          1)))

; Show that this procedure is not correct. In particular, draw box-and-pointer diagrams
; representing list structures made up of exactly three pairs for which Ben's procedure
; would return 3; return 4; return 7; never return at all.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (count-mpairs x)
	(if (not (mpair? x))
		0
		(+
			(count-mpairs (mcar x))
			(count-mpairs (mcdr x))
			1
		)
	)
)

; Test Driver

(define (run-test proc . args)

	(define (print-item-list items first-time?)
		(cond
			((not (pair? items)) (void))
			(else
				(if (not first-time?)
					(display ", ")
					(void)
				)
				(print (car items))
				(print-item-list (cdr items) false)
			)
		)
	)

	; (display "Running Test: ") (display (cons proc args)) (display " ")
	; (newline)
	(display "Applying ")
	(display proc)
	(display " on: ")
	(print-item-list args true)
	(newline)
	(let ((result (apply proc args)))
		(display "Result: ")
		(display result)
		(newline)
		(print result)
		(newline)
	)
	(newline)
)

; Tests

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define A (mcons 'u 'v))
> (define B (mcons 'w 'x))
> (define C (mcons 'y 'z))
> (set-mcdr! A B)
> (set-mcdr! B C)
> (count-mpairs A)
3
> (set-mcar! B C)
> (count-mpairs A)
4
> (set-mcar! A B)
> (count-mpairs A)
7
> (set-mcdr! A B)
> (set-mcdr! B C)
> (set-mcdr! C A)
> (set-mcar! A 'u)
> (set-mcar! B 'w)
> (set-mcar! C 'y)
> (count-mpairs A)

Interactions disabled

