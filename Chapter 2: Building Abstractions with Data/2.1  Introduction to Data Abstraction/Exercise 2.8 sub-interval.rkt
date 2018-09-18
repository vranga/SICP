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

(define (add-interval x y)
	(make-interval
		(+ (lower-bound x) (lower-bound y))
		(+ (upper-bound x) (upper-bound y))
	)
)

(define (sub-interval x y)
	(make-interval
		(- (lower-bound x) (upper-bound y))
		(- (upper-bound x) (lower-bound y))
	)
)

; Tests

> (sub-interval (make-interval 40 50) (make-interval 10 20))
'(20 . 40)
> (sub-interval (make-interval 40 50) (make-interval 10 40))
'(0 . 40)
> (sub-interval (make-interval 40 50) (make-interval 10 45))
'(-5 . 40)
> (sub-interval (make-interval 40 50) (make-interval 10 55))
'(-15 . 40)
> (sub-interval (make-interval 40 50) (make-interval 40 55))
'(-15 . 10)
> (sub-interval (make-interval 40 50) (make-interval 45 55))
'(-15 . 5)
> (sub-interval (make-interval 40 50) (make-interval 50 55))
'(-15 . 0)
> (sub-interval (make-interval 40 50) (make-interval 55 60))
'(-20 . -5)
> 
