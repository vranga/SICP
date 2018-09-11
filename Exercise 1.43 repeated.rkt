#lang racket

(define (repeated f n)
	(cond
		((> n 1) (compose f (repeated f (- n 1))))
		((= n 1) f)
		(else
			identity
		)
	)
)

(define (identity x)
	x
)

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

> ((repeated square 0) 5)
5
> ((repeated square 1) 5)
25
> ((repeated square 2) 5)
625
> ((repeated square 3) 5)
390625
> ((repeated square 4) 5)
152587890625
> ((repeated inc 0) 5)
5
> ((repeated inc 1) 5)
6
> ((repeated inc 2) 5)
7
> ((repeated inc 3) 5)
8
> ((repeated inc 10) 5)
15
> ((repeated inc 100) 5)
105
> ((repeated inc -1) 5)
5
> ((repeated sin 2) 5)
-0.8185741444617193
> ((repeated cube 0) 5)
5
> ((repeated cube 1) 5)
125
> ((repeated cube 0) 7)
7
> ((repeated cube 1) 7)
343
> ((repeated cube 2) 7)
40353607
> ((repeated cube 4) 7)
283753509180010707824461062763116716606126555757084586223347181136007
> ((repeated cube 3) 7)
65712362363534280139543
>  
