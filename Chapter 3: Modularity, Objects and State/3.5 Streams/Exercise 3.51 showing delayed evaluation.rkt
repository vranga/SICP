#lang racket

; Exercise 3.51.  In order to take a closer look at delayed evaluation, we will use the following procedure,
; which simply returns its argument after printing it:

; (define (show x)
;   (display-line x)
;   x)

; What does the interpreter print in response to evaluating each expression in the following sequence?59

; (define x (stream-map show (stream-enumerate-interval 0 10)))
; (stream-ref x 5)
; (stream-ref x 7)

; S O L U T I O N

(define (stream-map proc . argstreams)
	(displayln "Entered stream-map")
	(if (stream-empty? (car argstreams))
		empty-stream
		(let ((first-set-of-args (map stream-first argstreams)))
			; (display "Streams passed to stream-map: ")
			; (display argstreams)
			; (newline)
			(display "Arguments to be passed to ")
			(display proc)
			(display ": ")
			(display first-set-of-args)
			(newline)
			(stream-cons
				(apply proc first-set-of-args)
				(apply stream-map (cons proc (map stream-rest argstreams)))
			)
		)
	)
)

(define (stream-ref s n)
	(display "Entered stream-ref with n = ")
	(display n)
	(newline)
	(if (= n 0)
		(stream-first s)
		(stream-ref (stream-rest s) (- n 1))
	)
)

(define (stream-enumerate-interval low high)
	(display "Entered stream-enumerate-interval with low = ")
	(display low)
	(display " and high = ")
	(display high)
	(newline)
	(if (> low high)
		empty-stream
		(stream-cons
			low
			(stream-enumerate-interval (+ low 1) high)
		)
	)
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

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Entered stream-enumerate-interval with low = 0 and high = 10
Entered stream-map
Arguments to be passed to #<procedure:show>: (0)
Entered stream-ref with n = 5
Entered stream-ref with n = 4
Entered stream-map
Entered stream-enumerate-interval with low = 1 and high = 10
Arguments to be passed to #<procedure:show>: (1)
Entered stream-ref with n = 3
Entered stream-map
Entered stream-enumerate-interval with low = 2 and high = 10
Arguments to be passed to #<procedure:show>: (2)
Entered stream-ref with n = 2
Entered stream-map
Entered stream-enumerate-interval with low = 3 and high = 10
Arguments to be passed to #<procedure:show>: (3)
Entered stream-ref with n = 1
Entered stream-map
Entered stream-enumerate-interval with low = 4 and high = 10
Arguments to be passed to #<procedure:show>: (4)
Entered stream-ref with n = 0
Entered stream-map
Entered stream-enumerate-interval with low = 5 and high = 10
Arguments to be passed to #<procedure:show>: (5)
Showing 5
5
Entered stream-ref with n = 7
Entered stream-ref with n = 6
Entered stream-ref with n = 5
Entered stream-ref with n = 4
Entered stream-ref with n = 3
Entered stream-ref with n = 2
Entered stream-ref with n = 1
Entered stream-map
Entered stream-enumerate-interval with low = 6 and high = 10
Arguments to be passed to #<procedure:show>: (6)
Entered stream-ref with n = 0
Entered stream-map
Entered stream-enumerate-interval with low = 7 and high = 10
Arguments to be passed to #<procedure:show>: (7)
Showing 7
7
> (stream-ref x 5)
Entered stream-ref with n = 5
Entered stream-ref with n = 4
Entered stream-ref with n = 3
Entered stream-ref with n = 2
Entered stream-ref with n = 1
Entered stream-ref with n = 0
5
> (stream-ref x 5)
Entered stream-ref with n = 5
Entered stream-ref with n = 4
Entered stream-ref with n = 3
Entered stream-ref with n = 2
Entered stream-ref with n = 1
Entered stream-ref with n = 0
5
> (stream-ref x 7)
Entered stream-ref with n = 7
Entered stream-ref with n = 6
Entered stream-ref with n = 5
Entered stream-ref with n = 4
Entered stream-ref with n = 3
Entered stream-ref with n = 2
Entered stream-ref with n = 1
Entered stream-ref with n = 0
7
> (stream-ref x 3)
Entered stream-ref with n = 3
Entered stream-ref with n = 2
Entered stream-ref with n = 1
Entered stream-ref with n = 0
Showing 3
3
> 
