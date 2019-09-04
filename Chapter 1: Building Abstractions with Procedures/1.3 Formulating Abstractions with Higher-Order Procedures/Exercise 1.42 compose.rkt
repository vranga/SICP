#lang racket

; Exercise 1.42.  Let f and g be two one-argument functions. The composition f after g is
; defined to be the function x -> f(g(x)). Define a procedure compose that implements
; composition. For example, if inc is a procedure that adds 1 to its argument,

; ((compose square inc) 6)
; 49

; SOLUTION

(define (compose f g)
	(lambda (w) (f (g w)))
)

(define (cube x)
	(* x x x)
)

(define (square x)
	(* x x)
)

(define (double f)
	(lambda (w) (f (f w)))
)

(define (inc x)
	(+ x 1)
)

; Tests

> ((compose square inc) 6)
49
> ((compose inc square) 6)
37
> ((compose inc (double square)) 6)
1297
> ((compose square cube) 5)
15625
> ((compose cube square) 5)
15625
> ((compose cube cube) 5)
1953125
> 
