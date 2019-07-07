; Exercise 3.57.  How many additions are performed when we compute the nth Fibonacci number
; using the definition of fibs based on the add-streams procedure? Show that the number of
; additions would be exponentially greater if we had implemented (delay <exp>) simply as
; (lambda () <exp>), without using the optimization provided by the memo-proc procedure
; described in section 3.5.1.64

; S O L U T I O N

; (define fibs
; 	(cons-stream
; 		0
; 		(cons-stream
; 			1
; 			(add-streams (stream-cdr fibs) fibs)
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

(define addition-count 0)

(define (add-numbers x y)
	(set! addition-count (+ addition-count 1))
	(+ x y)
)

(define (add-streams s1 s2)
	(stream-map add-numbers s1 s2)
)

(define fibs
	(cons-stream
		0
		(cons-stream
			1
			(add-streams (stream-cdr fibs) fibs)
		)
	)
)

(define (merge s1 s2)
	(cond
		((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
			(let ((s1car (stream-car s1)) (s2car (stream-car s2)))
				(cond
					((< s1car s2car)
						(cons-stream s1car (merge (stream-cdr s1) s2))
					)
					((> s1car s2car)
						(cons-stream s2car (merge s1 (stream-cdr s2)))
					)
					(else
						(cons-stream s1car
							(merge (stream-cdr s1) (stream-cdr s2))
						)
					)
				)
			)
		)
	)
)

(define (partial-sums S)
	(cons-stream (stream-car S) (add-streams (partial-sums S) (stream-cdr S)))
)

(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(define integers (cons-stream 1 (add-streams ones integers)))

(define ones (cons-stream 1 ones))

(define (mul-streams s1 s2)
	(stream-map * s1 s2)
)

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream)
)

(define (stream-map proc . argstreams)
	; (displayln "Entered stream-map")
	(if (stream-null? (car argstreams))
		empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map (cons proc (map stream-cdr argstreams)))
		)
	)
)

(define (stream-ref s n)
	; (display "Entered stream-ref with n = ")
	; (display n)
	; (newline)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))
	)
)

(define (stream-for-each proc s)
	(if (stream-null? s)
		'done
		(begin
			(proc (stream-car s))
			(stream-for-each proc (stream-cdr s))
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
			(display (stream-car s))
			(display-stream-elements (- count 1) (stream-cdr s))
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

BANL154931268:3.5 Streams vranganath$ scheme
MIT/GNU Scheme running under OS X
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2014 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even
for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Saturday May 17, 2014 at 2:39:25 AM
  Release 9.2 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118 || Edwin 3.116

1 ]=> (load "Exercise 3.57 fibs using add-streams.scm")

;Loading "Exercise 3.57 fibs using add-streams.scm"... done
;Value: execution-time

1 ]=> (stream-ref fibs 0)

;Value: 0

1 ]=> (stream-ref fibs 1)

;Value: 1

1 ]=> (stream-ref fibs 2)

;Value: 1

1 ]=> (stream-ref fibs 3)

;Value: 2

1 ]=> (stream-ref fibs 4)

;Value: 3

1 ]=> (stream-ref fibs 5)

;Value: 5

1 ]=> (stream-ref fibs 6)

;Value: 8

1 ]=> (stream-ref fibs 7)

;Value: 13

1 ]=> (stream-ref fibs 8)

;Value: 21

1 ]=> (stream-ref fibs 9)

;Value: 34

1 ]=> (stream-ref fibs 10)

;Value: 55

1 ]=> (stream-ref fibs 50)

;Value: 12586269025

1 ]=> (stream-ref fibs 100)

;Value: 354224848179261915075

1 ]=> addition-count

;Value: 99

1 ]=> (stream-ref fibs 200)

;Value: 280571172992510140037611932413038677189525

1 ]=> addition-count

;Value: 199

1 ]=> 

