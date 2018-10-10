#lang racket

; Exercise 3.1.  An accumulator is a procedure that is called repeatedly with a single numeric
; argument and accumulates its arguments into a sum. Each time it is called, it returns the
; currently accumulated sum. Write a procedure make-accumulator that generates accumulators,
; each maintaining an independent sum. The input to make-accumulator should specify the
; initial value of the sum; for example

; (define A (make-accumulator 5))
; (A 10)
; 15
; (A 10)
; 25

; S O L U T I O N

(define (make-accumulator sum)
	(lambda (value)
		(set! sum (+ sum value))
		sum
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

	(display "Running Test: ") (display (cons proc args)) (display " ")
	(newline)
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

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define A (make-accumulator 5))
> (A 10)
15
> (A 10)
25
> (A -20)
5
> (A -20)
-15
> (A 10)
-5
> (A 10)
5
> (A 10)
15
> (A 10)
25
> 
