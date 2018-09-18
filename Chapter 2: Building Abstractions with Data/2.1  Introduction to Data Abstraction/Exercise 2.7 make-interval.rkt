#lang racket

(define (make-interval a b)
	(cons a b)
)

(define (upper-bound interval)
	(cdr interval)
)

(define (lower-bound interval)
	(car interval)
)

; Tests

> (upper-bound (make-interval 10 20))
20
> (lower-bound (make-interval 10 20))
10
> 
