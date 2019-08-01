#lang racket

; Exercise 3.62.  Use the results of exercises 3.60 and 3.61 to define a procedure div-series
; that divides two power series. Div-series should work for any two series, provided that the
; denominator series begins with a nonzero constant term. (If the denominator has a zero constant
; term, then div-series should signal an error.) Show how to use div-series together with the result
; of exercise 3.59 to generate the power series for tangent.

; S O L U T I O N

; Note that the power series for tan(x) is:
;
; tan(x) = 1 * (x/1!) + 2 * ((x^3)/3!) + 16 * ((x^5)/5!) + 272 * ((x^7)/7!) + 7936 * ((x^9)/9!) + ...
;

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

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (stream-ref tan-series 0)
0
> (stream-ref tan-series 1)
1
> (stream-ref tan-series 2)
0
> (stream-ref tan-series 3)
1/3
> (stream-ref tan-series 4)
0
> (stream-ref tan-series 5)
2/15
> (stream-ref tan-series 6)
0
> (stream-ref tan-series 7)
17/315
> (stream-ref tan-series 8)
0
> (stream-ref tan-series 9)
62/2835
> (display-stream-elements 20 tan-series)

0
1
0
1/3
0
2/15
0
17/315
0
62/2835
0
1382/155925
0
21844/6081075
0
929569/638512875
0
6404582/10854718875
0
443861162/1856156927625
'done
> 
