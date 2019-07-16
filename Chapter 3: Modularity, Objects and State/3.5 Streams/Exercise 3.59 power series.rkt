#lang racket

; Exercise 3.59. In section 2.5.3 we saw how to implement a polynomial arithmetic system representing polynomials
; as lists of terms. In a similar way, we can work with power series, such as

; e^x = 1 + x + (x^2)/2! + (x^3)/(3!) + (x^4)/4! + ...

; cos x = 1 - (x^2)/2! + (x^4)/4! - ...

; sin x = x - (x^3)/(3!) + (x^5)/5! - ...

; represented as infinite streams. We will represent the series a0 + a1 x + a2 x2 + a3 x3 + ··· as the stream whose
; elements are the coefficients a0, a1, a2, a3, ....

; a. The integral of the series a0 + a1 x + a2 x2 + a3 x3 + ··· is the series

; c + a0x + (1/2)a1(x^2) + (1/3)a2(x^3) + (1/4)a3(x^4) + ...

; where c is any constant. Define a procedure integrate-series that takes as input a stream a0, a1, a2, ...
; representing a power series and returns the stream a0, (1/2)a1, (1/3)a2, ... of coefficients of the non-constant
; terms of the integral of the series. (Since the result has no constant term, it doesn't represent a power series;
; when we use integrate-series, we will cons on the appropriate constant.)

; b. The function x -> e^x is its own derivative. This implies that e^x and the integral of e^x are the same series,
; except for the constant term, which is e^0 = 1. Accordingly, we can generate the series for e^x as

; (define exp-series
;   (cons-stream 1 (integrate-series exp-series)))

; Show how to generate the series for sine and cosine, starting from the facts that the derivative of sine is
; cosine and the derivative of cosine is the negative of sine:

; (define cosine-series
;   (cons-stream 1 <??>))

; (define sine-series
;   (cons-stream 0 <??>))

; S O L U T I O N

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

(define (stream-ref s n)
	; (display "Entered stream-ref with n = ")
	; (display n)
	; (newline)
	(if (= n 0)
		(stream-first s)
		(stream-ref (stream-rest s) (- n 1))
	)
)

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream)
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

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (stream-ref (integrate-series ones) 0)
1
> (stream-ref (integrate-series ones) 1)
1/2
> (stream-ref (integrate-series ones) 2)
1/3
> (stream-ref (integrate-series ones) 3)
1/4
> (stream-ref (integrate-series ones) 4)
1/5
> (define coeffs (stream-cons 2 (stream-cons 7 (stream-cons 4 (stream-cons 9 (stream-cons 1 (stream-cons 15 empty-stream)))))))
> (display-stream (integrate-series coeffs))

2
7/2
4/3
9/4
1/5
5/2
> (stream-ref exp-series 0)
1
> (stream-ref exp-series 1)
1
> (stream-ref exp-series 2)
1/2
> (stream-ref exp-series 3)
1/6
> (display-stream-elements 10 exp-series)

1
1
1/2
1/6
1/24
1/120
1/720
1/5040
1/40320
1/362880
'done
> (display-stream-elements 10 sine-series)

0
1
0
-1/6
0
1/120
0
-1/5040
0
1/362880
'done
> (display-stream-elements 10 cosine-series)

1
0
-1/2
0
1/24
0
-1/720
0
1/40320
0
'done
> 
