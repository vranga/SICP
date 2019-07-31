#lang racket

; Exercise 3.60.  With power series represented as streams of coefficients as in exercise 3.59,
; adding series is implemented by add-streams. Complete the definition of the following procedure
; for multiplying series:

; (define (mul-series s1 s2)
;   (cons-stream <??> (add-streams <??> <??>)))

; You can test your procedure by verifying that sin^2 x + cos^2 x = 1, using the series from
; exercise 3.59.

; S O L U T I O N

; Logic used in mul-series:
; (a0 + a1x + a2x^2 + a3x^3 + ...) * (b0 + b1x + b2x^2 + b3x^3 + ...) is:
;
; (a0 * b0) +
; {a0 * (b1x + b2x^2 + b3x^3 + ...)} +
; {b0 * (a1x + a2x^2 + a3x^3 + ...)} +
; {(a1x + a2x^2 + a3x^3 + ...) * (b1x + b2x^2 + b3x^3 + ...)}
;
; The coeffcients after multiplyng the two series above will be:
; Variable	|		Coefficient
;-------------------------------
; x^0		|		a0*b0
; x^1		|		a0*b1 + a1*b0
; x^2		|		a0*b2 + a1*b1 + a2*b0
; x^3		|		a0*b3 + a1*b2 + a2*b1 + a3*b0
; x^4		|		a0*b4 + a1*b3 + a2*b2 + a3*b1 + a4*b0
; and so on

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
				; 0 needst to be prepended to this stream so that the first term of the series
				; is the x^1 term. Only then the outer add-streams will add like terms in the
				; two series supplied to it
				0
				(mul-series (stream-rest s1) (stream-rest s2))
			)
		)
	)
)

; Ignore the commented out code below. It is not half as elegant as the one above
; (define (mul-series s1 s2)
; 	(define (compute-coeff position)
; 		(define (add-part offset)
; 		)
; 	)
; 
; 	(define (mul-series-internal stream1 stream2 n)
; 		(stream-cons 
; 			(compute-coeff n)
; 			(mul-series-internal stream1 stream2 (+ n 1))
; 		)
; 	)
; 
; 	(mul-series-internal stream1 stream2 0)
; )

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

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream)
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

(define S
	(add-streams
		(mul-series sine-series sine-series)
		(mul-series cosine-series cosine-series)
	)
)

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (stream-ref S 0)
1
> (display-stream-elements 20 S)

1
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
0
0
'done
> 
