#lang racket
	
; Exercise 3.63.  Louis Reasoner asks why the sqrt-stream procedure was not written in the
; following more straightforward way, without the local variable guesses:

; (define (sqrt-stream x)
;   (cons-stream 1.0
;                (stream-map (lambda (guess)
;                              (sqrt-improve guess x))
;                            (sqrt-stream x))))

; Alyssa P. Hacker replies that this version of the procedure is considerably less efficient
; because it performs redundant computation. Explain Alyssa's answer. Would the two versions
; still differ in efficiency if our implementation of delay used only (lambda () <exp>)
; without using the optimization provided by memo-proc (section 3.5.1)?

; E X P L A N A T I O N

; If we do not use the local variable 'guesses', then every call to procedure sqrt-stream 
; creates a brand-new stream. In Louis Reasoner's implementation of 'sqrt-stream', a call to:
;
; (display-stream (sqrt-stream 100))
;
; will result in generating the stream elements from the first element till the 101st element.
; If after this we call:
;
; (display-stream (sqrt-stream 101))
;
; a new stream is created so the evaluations to generate all elements from the first
; till the 102nd element will be performed again. 
;
; Coming to the second part of the problem, if our implementation of delay used only (lambda () <exp>)
; without taking advantage of memo-proc, then the two versions will not differ in efficiency at all.
; They will essentially be the same.

(define (louis-reasoner-sqrt-stream x)
	(stream-cons
		1.0
		(stream-map
			(lambda (guess) (sqrt-improve guess x))
			(louis-reasoner-sqrt-stream x)
		)
	)
)

(define (sqrt-stream x)
	(define guesses
		(stream-cons
			1.0
			(stream-map
				(lambda (guess) (sqrt-improve guess x))
				guesses
			)
		)
	)
	guesses
)

(define (sqrt-improve guess x)
	(average guess (/ x guess))
)

(define (average x y) (/ (+ x y) 2))

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

(define (display-stream s)
	(stream-for-each display-line s)
)

(define (display-line x)
	(newline)
	(display x)
)

(define (div-series dividend-series divisor-series)
	(cond
		((= 0 (stream-first divisor-series))
			(error "Denominator should not have a zero constant: " (stream-first divisor-series))
		)
		(else
			(mul-series
				dividend-series
				(invert-unit-series (scale-stream divisor-series (/ 1 (stream-first divisor-series))))
			)
		)
	)
)

(define (invert-unit-series s)
	(stream-cons
		1
		(mul-series
			(scale-stream (stream-rest s) -1)
			(invert-unit-series s)
		)
	)
)

(define (mul-series s1 s2)
	(stream-cons
		; (a0 * b0) (The following is the constant term of the series resulting from
		; the multiplication)
		(* (stream-first s1) (stream-first s2))
		; The following is the rest of the series starting with the x^1 term
		(add-streams
			; {a0 * (b1x + b2x^2 + b3x^3 + ...)} +
			; {b0 * (a1x + a2x^2 + a3x^3 + ...)} +
			(add-streams
				(scale-stream (stream-rest s2) (stream-first s1))
				(scale-stream (stream-rest s1) (stream-first s2))
			)
			; {(a1x + a2x^2 + a3x^3 + ...) * (b1x + b2x^2 + b3x^3 + ...)}
			(stream-cons
				; 0 needs to be prepended to this stream so that the first term of the stream
				; is the x^1 term. Only then the outer add-streams will add like terms in the
				; two series supplied to it
				0
				(mul-series (stream-rest s1) (stream-rest s2))
			)
		)
	)
)

(define exp-series
	(stream-cons 1 (integrate-series exp-series))
)

; the integral of negative sine is cosine
(define cosine-series
	(stream-cons 1 (integrate-series (scale-stream sine-series -1)))
)

; the integral of cosine is sine
(define sine-series
	(stream-cons 0 (integrate-series cosine-series))
)

(define tan-series 
	(div-series sine-series cosine-series)
)

(define (integrate-series s)
	(div-streams s integers)
)

(define (add-streams s1 s2)
	(stream-map + s1 s2)
)

(define (div-streams s1 s2)
	(stream-map / s1 s2)
)

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams ones integers)))

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
