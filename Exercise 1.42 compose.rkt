#lang racket

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
