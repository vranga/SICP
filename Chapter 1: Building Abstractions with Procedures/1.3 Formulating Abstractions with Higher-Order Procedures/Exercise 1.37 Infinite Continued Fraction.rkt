#lang racket

; (a) Recursive

(define (cont-frac-recur n d k)
	(define (cont-frac-recur-internal n d k i)
		(if (< i k)
			(/ (n i) (+ (d i) (cont-frac-recur-internal n d k (+ i 1))))
			(/ (n i) (d i))
		)
	)

	(cont-frac-recur-internal n d k 1)
)

; (b) Iterative

(define (cont-frac-iter n d k)

	; No deferred calcualtions so we need to work in the reverse direction to the 
	; recursive process

	(define (basic-op a b c)
		(+ a (/ b c))
	)

	(define (cont-frac-iter-internal n d iters accumulator)
		(cond
			((= iters k) (cont-frac-iter-internal n d (- iters 1) (basic-op (d (- k 1))  (n k) (d k))))
			((and (< iters k) (> iters 1)) (cont-frac-iter-internal n d (- iters 1) (basic-op (d (- iters 1)) (n iters) accumulator)))
			((= iters 1) (/ (n 1) accumulator))
		)
	)

	(cont-frac-iter-internal n d k 0)
)

; Tests

> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 10)
0.6179775280898876
> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 20)
0.6180339850173579
> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 40)
0.6180339887498948
> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 80)
0.6180339887498948
> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 160)
0.6180339887498948
> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 200)
0.6180339887498948
> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 400)
0.6180339887498948
> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 800)
0.6180339887498948
> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 8000)
0.6180339887498948
> 

; Observation: Does not change after k = 40. From calculator, the reciprocal of the golden ratio
; is 0.61803398874985468379271708299583. So our value is correct to the 13th place after the
; decimal. To get get accuracy upto 4 decimal places, we need k to be at least 11. See below:

> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 11)
0.6180555555555556
> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 10)
0.6179775280898876

; Comparing Recur and Iter

> (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 10)
(cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 100)
(cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 1000)
(cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 10000)
(cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 100000)
(cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 1000000)
(cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 10000000)
0.6179775280898876
0.6180339887498948
0.6180339887498948
0.6180339887498948
0.6180339887498948
0.6180339887498948

Interactions disabled (Out of Memory)

> (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 1000)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10000)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100000)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 1000000)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10000000)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100000000)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 1000000000)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10000000000)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100000000000)
0.6179775280898876
0.6180339887498948
0.6180339887498948
0.6180339887498948
0.6180339887498948
0.6180339887498948
0.6180339887498948
0.6180339887498948
0.6180339887498948
0.6180339887498948
0.6180339887498948