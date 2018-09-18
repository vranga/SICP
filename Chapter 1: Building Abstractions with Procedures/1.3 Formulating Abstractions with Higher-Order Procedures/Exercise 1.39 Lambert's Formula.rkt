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

(define (tan-cf x k)
	(cont-frac-recur
		(lambda (i)
			(if (= i 1)
				x
				(- 0.0 (square x)) ; return the negative of the square
			)
		)
		(lambda (i) (- (* 2 i) 1))
		k
	)
)

(define (square x)
	(* x x)
)

; Tests

> (tan-cf 0 10)
0
> (tan-cf pi 10)
-1.893214149359168e-009
> (tan-cf (/ pi 4) 10)
1.0
> (tan-cf (/ pi 2) 10)
744656605476677.9
> (tan-cf (/ pi 3) 10)
1.732050807568877
> (tan-cf (/ pi 3) 100)
1.732050807568877
> (tan-cf (/ pi 3) 5)
1.7320501979592633
> (tan-cf (/ (* 2 pi) 3) 5)
-1.7329572186349194
> (tan-cf (/ pi 6) 20)
0.5773502691896257
> (tan-cf (/ (* 5 pi) 6) 20)
-0.5773502691896255
> (tan-cf (/ (* 5 pi) 6) 200)
-0.5773502691896255
> (tan-cf (/ (* 5 pi) 6) 2000)
-0.5773502691896255
> (tan-cf (/ (* 3 pi) 4) 10)
-1.0000000000111287
>  
