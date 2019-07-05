#lang racket

; Exercise 3.55.  Define a procedure partial-sums that takes as argument a stream S and returns
; the stream whose elements are S0, S0 + S1, S0 + S1 + S2, .... For example, (
; partial-sums integers) should be the stream 1, 3, 6, 10, 15, ....

; S O L U T I O N

(define (partial-sums S)
	(stream-cons (stream-first S) (add-streams (partial-sums S) (stream-rest S)))
)

(define factorials (stream-cons 1 (mul-streams factorials (stream-rest integers))))

(define integers (stream-cons 1 (add-streams ones integers)))

(define ones (stream-cons 1 ones))

(define (mul-streams s1 s2)
	(stream-map * s1 s2)
)

(define (add-streams s1 s2)
	(stream-map + s1 s2)
)

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

(define (stream-ref s n)
	; (display "Entered stream-ref with n = ")
	; (display n)
	; (newline)
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

(define (display-stream s)
	(stream-for-each display-line s)
)

; This procedure displays a finite number of elements from the supplied stream
; as specified by 'count'
(define (display-stream-elements count s)
	(if (= 0 count)
		(begin
			(newline)
			'done
		)
		(begin
			(newline)
			(display (stream-first s))
			(display-stream-elements (- count 1) (stream-rest s))
		)
	)
)

(define (display-line x)
	(newline)
	(display x)
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

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define S (partial-sums integers))
> (stream-ref S 0)
1
> (stream-ref S 1)
3
> (stream-ref S 2)
6
> (stream-ref S 3)
10
> (stream-ref S 4)
15
> (stream-ref S 5)
21
> (stream-ref S 6)
28
> (stream-ref S 7)
36
> (stream-ref S 8)
45
> (stream-ref S 9)
55
> (stream-ref S 10)
66
> 
