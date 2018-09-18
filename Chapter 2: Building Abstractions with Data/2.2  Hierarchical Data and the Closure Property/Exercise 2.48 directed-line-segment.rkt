#lang racket

(define (make-directed-line-segment v1 v2)
	(cons v1 v2)
)

(define (start-directed-line-segment s)
	(car s)
)

(define (end-directed-line-segment s)
	(cdr s)
)

(define (make-vect x y)
	(cons x y)
)

(define (xcor-vect v)
	(car v)
)

(define (ycor-vect v)
	(cdr v)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define v1 (make-vect 2 5))
> (define v2 (make-vect -2 -5))
> (define s (make-directed-line-segment v1 v2))
> (start-directed-line-segment s)
'(2 . 5)
> (end-directed-line-segment s)
'(-2 . -5)
