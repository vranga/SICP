#lang racket

; Exercise 3.58.  Give an interpretation of the stream computed by the following procedure:

; (define (expand num den radix)
;   (cons-stream
;    (quotient (* num radix) den)
;    (expand (remainder (* num radix) den) den radix)))

; (Quotient is a primitive that returns the integer quotient of two integers.) What are the
; successive elements produced by (expand 1 7 10) ? What is produced by (expand 3 8 10) ?

; S O L U T I O N

; (expand 1 7 10) produces:

; 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, 1, 4 ...

; (expand 3 8 10) produces

; 3, 7, 5, 0, 0, 0, 0 ...

(define (expand num den radix)
	(stream-cons
		(quotient (* num radix) den)
		(expand (remainder (* num radix) den) den radix)
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
> (define S1 (expand 1 7 10))
> (display-stream-elements 25 S1)

1
4
2
8
5
7
1
4
2
8
5
7
1
4
2
8
5
7
1
4
2
8
5
7
1
'done
> (define S2 (expand 3 8 10))
> (display-stream-elements 20 S2)

3
7
5
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
'done
> 
