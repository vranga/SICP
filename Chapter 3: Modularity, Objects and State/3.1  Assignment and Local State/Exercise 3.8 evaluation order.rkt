#lang racket

; Exercise 3.8.  When we defined the evaluation model in section 1.1.3, we said that the first step in
; evaluating an expression is to evaluate its subexpressions. But we never specified the order in which
; the subexpressions should be evaluated (e.g., left to right or right to left). When we introduce assignment,
; the order in which the arguments to a procedure are evaluated can make a difference to the result.
; Define a simple procedure f such that evaluating (+ (f 0) (f 1)) will return 0 if the arguments to +
; are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.

; S O L U T I O N

(define called-with-zero 0)
(define called-with-one 0)

(define (f x)
	(display "Procedure f called with argument ")
	(display x)
	(newline)
	(if (and (eq? called-with-zero 1) (eq? called-with-one 1))
		; if the test (+ (f 0) (f 1)) has been run once, then reset the state so it can be run again
		(begin
			(set! called-with-zero 0)
			(set! called-with-one 0)
		)
		(void)
	)
	(cond
		((eq? x 0)
			(begin
				(set! called-with-zero 1)
				0
			)
		)

		((eq? x 1)
			(begin
				(set! called-with-one 1)
				(if (eq? called-with-zero 1)
					0
					1
				)
			)
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
> (+ (f 0) (f 1))
Procedure f called with argument 0
Procedure f called with argument 1
0
> (+ (f 1) (f 0))
Procedure f called with argument 1
Procedure f called with argument 0
1
> (+ (f 0) (f 1))
Procedure f called with argument 0
Procedure f called with argument 1
0
> (+ (f 1) (f 0))
Procedure f called with argument 1
Procedure f called with argument 0
1
> 
