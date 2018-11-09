#lang racket

; Exercise 3.6.  It is useful to be able to reset a random-number generator to produce a sequence starting
; from a given value. Design a new rand procedure that is called with an argument that is either the symbol
; generate or the symbol reset and behaves as follows: (rand 'generate) produces a new random number;
; ((rand 'reset) <new-value>) resets the internal state variable to the designated <new-value>.
; Thus, by resetting the state, one can generate repeatable sequences. These are very handy to have when
; testing and debugging programs that use random numbers.

; S O L U T I O N

(define (new-rand message)
	(cond
		((eq? message 'generate) (random))
		((eq? message 'reset)
			(lambda (new-seed)
				(display "Resetting random seed to ")
				(display new-seed)
				(newline)
				(random-seed new-seed)
			)
		)
		(else
			(error "Unknown message to procedure new-rand -- " message)
		)
	)
)

(define (random-in-range low high)
	(let ((range (- high low)))
		; (+ low (random range))
		; returns decimal numbers in the range
		(+ low (* (random) range))
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

(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
((new-rand 'reset) 43)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
((new-rand 'reset) 43)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)

((new-rand 'reset) 101)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)

((new-rand 'reset) 101)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)
(random-in-range 1 1000)

((new-rand 'reset) 29)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)

((new-rand 'reset) 29)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)
(random 1 1000)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
0.7536610790438728
0.9969781884857136
0.5982221079594918
0.22999980506486248
0.33550189965972566
Resetting random seed to 43
0.7588627764125023
0.753801144843604
0.5228586529275868
0.17311339918700677
0.551975487221708
Resetting random seed to 43
0.7588627764125023
0.753801144843604
0.5228586529275868
0.17311339918700677
0.551975487221708
Resetting random seed to 101
803.6324760049012
441.1369687373493
842.6805944891097
893.2221923084502
987.0488221918129
94.6527153271634
969.4338482036816
695.3907457551163
931.327842783926
12.937681401855716
330.76325543638256
Resetting random seed to 101
803.6324760049012
441.1369687373493
842.6805944891097
893.2221923084502
987.0488221918129
94.6527153271634
969.4338482036816
695.3907457551163
931.327842783926
12.937681401855716
330.76325543638256
Resetting random seed to 29
200
307
55
521
250
590
104
393
864
443
957
Resetting random seed to 29
200
307
55
521
250
590
104
393
864
443
957
> 
