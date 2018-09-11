#lang racket

(define (make-vect x y)
	(cons x y)
)

(define (xcor-vect v)
	(car v)
)

(define (ycor-vect v)
	(cdr v)
)

(define (add-vect v1 v2)
	; evaluates v1 + v2
	(make-vect
		(+ (xcor-vect v1) (xcor-vect v2))
		(+ (ycor-vect v1) (ycor-vect v2))
	)
)

(define (sub-vect v1 v2)
	; evaluates v1 - v2
	(make-vect
		(- (xcor-vect v1) (xcor-vect v2))
		(- (ycor-vect v1) (ycor-vect v2))
	)
)

(define (scale-vect v k)
	; Scales the vector v using the constant k
	(make-vect (* (xcor-vect v) k) (* (ycor-vect v) k))
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define a (make-vect 3 4))
> (define b (make-vect -2 9))
> (xcor-vect a)
3
> (xcor-vect b)
-2
> (ycor-vect a)
4
> (ycor-vect b)
9
> (add-vect a b)
'(1 . 13)
> (sub-vect a b)
'(5 . -5)
> (sub-vect b a)
'(-5 . 5)
> (scale-vect a 50)
'(150 . 200)
> (scale-vect b 40)
'(-80 . 360)
> (scale-vect (add-vect a b) 15)
'(15 . 195)
> 
