#lang racket

; Exercise 3.50.  Complete the following definition, which generalizes stream-map to allow procedures
; that take multiple arguments, analogous to map in section 2.2.3, footnote 12.

; (define (stream-map proc . argstreams)
;   (if (<??> (car argstreams))
;       the-empty-stream
;       (<??>
;        (apply proc (map <??> argstreams))
;        (apply stream-map
;               (cons proc (map <??> argstreams))))))

; S O L U T I O N

(define (stream-map proc . argstreams)
	(if (stream-empty? (car argstreams))
		empty-stream
		(let ((first-set-of-args (map stream-first argstreams)))
			; (display "Streams passed to stream-map: ")
			; (display argstreams)
			; (newline)
			(display "Arguments about to be passed to ")
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

; (define (stream-map proc s)
; 	(if (stream-empty? s)
; 		empty-stream
; 		(stream-cons
; 			(proc (stream-first s))
; 			(stream-map proc (stream-rest s))
; 		)
; 	)
; )

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

(define S1 (stream 1 2 3))
(define S2 (stream 7 8 9))
(define S3 (stream 13 14 15))

(define (add x y z) (+ x y z))
(define R (stream-map add S1 S2 S3))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Arguments about to be passed to #<procedure:add>: (1 7 13)
> (stream-first R)
21
> (stream-first (stream-rest R))
Arguments about to be passed to #<procedure:add>: (2 8 14)
24
> (stream-first (stream-rest (stream-rest R)))
Arguments about to be passed to #<procedure:add>: (3 9 15)
27
> 
