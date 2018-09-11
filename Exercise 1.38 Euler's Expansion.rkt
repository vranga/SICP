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

(define (euler-d-term n)
	(cond
		((= 0 (remainder n 3)) 1)
		((= 1 (remainder n 3)) 1)
		((= 2 (remainder n 3)) (* 2 (+ (/ (- n 2) 3) 1)))
		((= n 2) 2)
		((= n 1) 1)
	)
)

; Tests

(cont-frac-recur (lambda (i) 1.0) euler-d-term 1)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 2)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 3)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 4)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 5)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 6)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 7)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 8)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 0)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 10)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 15)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 20)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 50)
(cont-frac-recur (lambda (i) 1.0) euler-d-term 100)
1.0
0.6666666666666666
0.75
0.7142857142857143
0.71875
0.717948717948718
0.7183098591549296
0.7182795698924731
1.0
0.7182817182817183
0.7182818284705836
0.7182818284590452
0.7182818284590453
0.7182818284590453
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 1)
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 2)
0.6666666666666666
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 3)
0.75
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 4)
0.7142857142857143
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 5)
0.71875
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 6)
0.717948717948718
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 7)
0.7183098591549296
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 8)
0.7182795698924731
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 9)
0.7182835820895522
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 10)
0.7182817182817183
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 20)
0.7182818284590452
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 50)
0.7182818284590453
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 100)
0.7182818284590453
> (cont-frac-iter (lambda (i) 1.0) euler-d-term 10000)
0.7182818284590453
> 
