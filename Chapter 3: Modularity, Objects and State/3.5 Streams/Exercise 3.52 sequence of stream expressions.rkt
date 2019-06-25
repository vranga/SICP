#lang racket

; Exercise 3.52.  Consider the sequence of expressions

; (define sum 0)
; (define (accum x)
;   (set! sum (+ x sum))
;   sum)
; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; (define y (stream-filter even? seq))
; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                          seq))
; (stream-ref y 7)
; (display-stream z)

; What is the value of sum after each of the above expressions is evaluated? What is the printed
; response to evaluating the stream-ref and display-stream expressions? Would these responses differ
; if we had implemented (delay <exp>) simply as (lambda () <exp>) without using the optimization
; provided by memo-proc ? Explain.

; S O L U T I O N

; Note: stream-enumerate-interval produces the stream:
;	1, 2, 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,  14,  15,  16,  17,  18,  19,  20
; Note: seq produces the stream:
;	1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210
; Note: y produces:
;	6, 10, 28, 36, 66, 78, 120, 136, 190, 210
; Note: z produces:
;	10, 15, 45, 55, 105, 120, 190, 210

; In the Racket streams implementation, when a stream is constructed the evaluations of both the car and
; the cdr are delayed until ‘stream-car’ and ‘stream-cdr’ are executed.

(define (stream-map proc . argstreams)
	; (displayln "Entered stream-map")
	(if (stream-empty? (car argstreams))
		empty-stream
		(stream-cons
			(apply proc (map stream-first argstreams))
			(apply stream-map (cons proc (map stream-rest argstreams)))
		)
	)
)

(define (stream-filter pred stream)
	(cond
		((stream-empty? stream) empty-stream)
		((pred (stream-first stream))
			(stream-cons
				(stream-first stream)
				(stream-filter pred (stream-rest stream))
			)
		)
		(else
			(stream-filter pred (stream-rest stream))
		)
	)
)

(define (stream-ref s n)
	; (display "Entered stream-ref with n = ")
	(display n)
	(newline)
	(if (= n 0)
		(stream-first s)
		(stream-ref (stream-rest s) (- n 1))
	)
)

(define (stream-for-each proc s)
	(if (stream-empty? s)
		'done
		(begin
			(proc (stream-first s))
			(stream-for-each proc (stream-rest s))
		)
	)
)

(define (stream-enumerate-interval low high)
	; (display "Entered stream-enumerate-interval with low = ")
	; (display low)
	; (display " and high = ")
	; (display high)
	; (newline)
	(if (> low high)
		empty-stream
		(stream-cons
			low
			(stream-enumerate-interval (+ low 1) high)
		)
	)
)

(define (display-stream s)
	(stream-for-each display-line s)
)

(define (display-line x)
	(newline)
	(display x)
)

(define (show x)
	(display "Showing ")
	(displayln x)
	x
)

; Test Driver

(define (run-test return-type proc . args)

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

	(display "Applying ")
	(display proc)
	(if (not (null? args))
		(begin
			(display " on: ")
			(print-item-list args true)
		)
		(void)
	)
	(newline)
	(let ((result (apply proc args)))
		(if (not (eq? return-type 'none))
			(display "Result: ")
			(void)
		)
		(cond
			((procedure? result) ((result 'print)))
			; ((eq? return-type 'deque) (print-deque result))
			((eq? return-type 'none) (void))
			(else
				(print result)
				(newline)
			)
		)
	)
	(newline)
)

(define (execution-time proc . args)
	(define start-time (current-milliseconds))
	; (display start-time)
	; (display " ")
	(apply proc args)
	(define end-time (current-milliseconds))
	; (display end-time) 
	(display "Execution time of ")
	(display proc)
	(display ": ")
	(- end-time start-time)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define sum 0)
> (display sum)
0
> (define (accum x) (set! sum (+ x sum)) sum)
> (display sum)
0
> (define seq (stream-map accum (stream-enumerate-interval 1 20)))
> (display sum)
0
> (define y (stream-filter even? seq))
> (display sum)
6
> (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
> (display sum)
10
> (stream-ref y 7)
Entered stream-ref with n = 7
Entered stream-ref with n = 6
Entered stream-ref with n = 5
Entered stream-ref with n = 4
Entered stream-ref with n = 3
Entered stream-ref with n = 2
Entered stream-ref with n = 1
Entered stream-ref with n = 0
136
> (display-stream z)

10
15
45
55
105
120
190
210'done
> 

; To the last question on whether the responses will differ if 'delay' were implemented without memoization,
; the answer is that the value of 'sum' will be higher since 'accum' will be applied multiple more times.
; Specifically, after y is defined, 'sum' is 6 but when z is defined, due to the absence of memoization
; 'accum' will be applied again on the elements 1, 2 and 3 from the enumerated interval. So z will be a 
; different stream from when memoization is used. Additionally 'seq' changes everytime it is accessed. 
