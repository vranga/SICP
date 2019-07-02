#lang racket

; Exercise 3.54.  Define a procedure mul-streams, analogous to add-streams, that produces the
; elementwise product of its two input streams. Use this together with the stream of integers to
; complete the following definition of the stream whose nth element (counting from 0) is
; n + 1 factorial:

; (define factorials (cons-stream 1 (mul-streams <??> <??>)))

; S O L U T I O N

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
> (stream-first factorials)
1
> (stream-first (stream-rest factorials))
2
> (stream-first (stream-rest (stream-rest factorials)))
6
> (stream-first (stream-rest (stream-rest (stream-rest factorials))))
24
> (stream-first (stream-rest (stream-rest (stream-rest (stream-rest factorials)))))
120
> (stream-first (stream-rest (stream-rest (stream-rest (stream-rest (stream-rest factorials))))))
720
> (stream-ref factorials 5)
720
> (stream-ref factorials 4)
120
> (stream-ref factorials 3)
24
> (stream-ref factorials 2)
6
> (stream-ref factorials 1)
2
> (stream-ref factorials 0)
1
> 
