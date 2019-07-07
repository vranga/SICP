#lang racket

; Exercise 3.57.  How many additions are performed when we compute the nth Fibonacci number
; using the definition of fibs based on the add-streams procedure? Show that the number of
; additions would be exponentially greater if we had implemented (delay <exp>) simply as
; (lambda () <exp>), without using the optimization provided by the memo-proc procedure
; described in section 3.5.1.64

; S O L U T I O N

; (define fibs
; 	(stream-cons
; 		0
; 		(stream-cons
; 			1
; 			(add-streams (stream-rest fibs) fibs)
; 		)
; 	)
; )

; Number of additions performed:
;
; 0th Fibonacci number (0): 0 additions
; 1st Fibonacci number (1): 0 additions
; 2nd Fibonacci number (1): 1 addition
; 3rd Fibonacci number (2): 2 additions
; 4th Fibonacci number (3): 3 additions
;
; As we can see above, for every higher Fibonacci number, one additional addition is performed.
; So when we compute the nth Fibonacci number, n - 1 additions are performed in total
;
; Now, if we had implemented (delay <exp>) simply as (lambda () <exp>) without memoization,
; then, upon accessing the nth Fibonacci number using (stream-ref fibs n) where n > 1, add-streams
; is executed which accesses the fibs stream twice. Since accessing fibs results in a call to
; add-streams, the result is that every add-streams call results in two calls to add-streams.
; This pattern results in 2^n additions. 

; Therefore the number of additions will be exponentially greater compared to when we use
; memoization in the 'delay' implementation.

(define (add-streams s1 s2)
	(stream-map + s1 s2)
)

(define fibs
	(stream-cons
		0
		(stream-cons
			1
			(add-streams (stream-rest fibs) fibs)
		)
	)
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
