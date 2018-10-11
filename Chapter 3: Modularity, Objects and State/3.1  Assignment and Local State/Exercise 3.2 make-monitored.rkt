#lang racket

; Exercise 3.2.  In software-testing applications, it is useful to be able to count the number
; of times a given procedure is called during the course of a computation. Write a procedure
; make-monitored that takes as input a procedure, f, that itself takes one input. The result
; returned by make-monitored is a third procedure, say mf, that keeps track of the number of
; times it has been called by maintaining an internal counter. If the input to mf is the
; special symbol how-many-calls?, then mf returns the value of the counter. If the input is
; the special symbol reset-count, then mf resets the counter to zero. For any other input,
; mf returns the result of calling f on that input and increments the counter.
; For instance, we could make a monitored version of the sqrt procedure:

; (define s (make-monitored sqrt))

; (s 100)
; 10

; (s 'how-many-calls?)
; 1

; S O L U T I O N

(define (make-monitored f)
	(let ((counter 0))
		(lambda (message)
			(cond
				((eq? message 'how-many-calls?) counter)
				((eq? message 'reset-count) (set! counter 0))
				(else
					(set! counter (+ counter 1))
					(f message)
				)
			)
		)
	)
)

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
> (define s (make-monitored sqrt))
> (s 'how-many-calls?)
0
> (s 'how-many-calls?)
0
> (s 100)
10
> (s 'how-many-calls?)
1
> (s 'how-many-calls?)
1
> (s 1000)
31.622776601683793
> (s 10000)
100
> (s 49)
7
> (s 16900)
130
> (s 'how-many-calls?)
5
> (s 'reset-count)
> (s 'how-many-calls?)
0
> (s (* 81 81))
81
> (s 'how-many-calls?)
1
> 
