#lang racket

; Exercise 3.56.  A famous problem, first raised by R. Hamming, is to enumerate, in ascending
; order with no repetitions, all positive integers with no prime factors other than 2, 3, or 5.
; One obvious way to do this is to simply test each integer in turn to see whether it has any
; factors other than 2, 3, and 5. But this is very inefficient, since, as the integers get larger,
; fewer and fewer of them fit the requirement. As an alternative, let us call the required stream
; of numbers S and notice the following facts about it.

; S begins with 1.
; The elements of (scale-stream S 2) are also elements of S.
; The same is true for (scale-stream S 3) and (scale-stream S 5).
; These are all the elements of S.
; Now all we have to do is combine elements from these sources. For this we define a procedure
; merge that combines two ordered streams into one ordered result stream, eliminating repetitions:

; (define (merge s1 s2)
; 	(cond
; 		((stream-null? s1) s2)
; 		((stream-null? s2) s1)
; 		(else
; 			(let ((s1car (stream-car s1)) (s2car (stream-car s2)))
; 				(cond
; 					((< s1car s2car)
; 						(cons-stream s1car (merge (stream-cdr s1) s2))
; 					)
; 					((> s1car s2car)
; 						(cons-stream s2car (merge s1 (stream-cdr s2)))
; 					)
; 					(else
; 						(cons-stream s1car
; 							(merge (stream-cdr s1) (stream-cdr s2))
; 						)
; 					)
; 				)
; 			)
; 		)
; 	)
; )

; Then the required stream may be constructed with merge, as follows:

; (define S (cons-stream 1 (merge <??> <??>)))

; Fill in the missing expressions in the places marked <??> above.

; S O L U T I O N

(define S
	(stream-cons 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5))))
)

(define (merge s1 s2)
	(cond
		((stream-empty? s1) s2)
		((stream-empty? s2) s1)
		(else
			(let ((s1car (stream-first s1)) (s2car (stream-first s2)))
				(cond
					((< s1car s2car)
						(stream-cons s1car (merge (stream-rest s1) s2))
					)
					((> s1car s2car)
						(stream-cons s2car (merge s1 (stream-rest s2)))
					)
					(else
						(stream-cons s1car
							(merge (stream-rest s1) (stream-rest s2))
						)
					)
				)
			)
		)
	)
)

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

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream)
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
> (stream-ref S 0)
1
> (stream-ref S 1)
2
> (stream-ref S 2)
3
> (stream-ref S 3)
4
> (stream-ref S 4)
5
> (stream-ref S 5)
6
> (stream-ref S 6)
8
> (stream-ref S 7)
9
> (stream-ref S 8)
10
> (stream-ref S 9)
12
> (stream-ref S 10)
15
> (stream-ref S 11)
16
> (stream-ref S 12)
18
> (stream-ref S 13)
20
> (stream-ref S 14)
24
> (stream-ref S 15)
25
> (stream-ref S 16)
27
> (stream-ref S 17)
30
> (stream-ref S 18)
32
> (stream-ref S 19)
36
> (stream-ref S 20)
40
> 
