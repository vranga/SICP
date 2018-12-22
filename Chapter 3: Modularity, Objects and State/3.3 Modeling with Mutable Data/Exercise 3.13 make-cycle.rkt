#lang racket

; Exercise 3.13.  Consider the following make-cycle procedure, which uses the last-pair procedure defined in exercise 3.12:

; (define (make-cycle x)
;   (set-cdr! (last-pair x) x)
; x)

; Draw a box-and-pointer diagram that shows the structure z created by

; (define z (make-cycle (list 'a 'b 'c)))

; What happens if we try to compute (last-pair z)?

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (make-cycle x)
	(set-mcdr! (last-pair x) x)
	x
)

(define (last-pair x)
	(if (null? (mcdr x))
		x
		(last-pair (mcdr x))
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
> (define z (make-cycle (mlist 'a 'b 'c)))
> (display z)
#0={a b c . #0#}
> (last-pair z)
. . user break
> 

OBSERVATION: z is cyclic. (last-pair z) results in an infinite loop because a pair with a null second
element, which is the condition for ending the recursive process, is never encountered.
