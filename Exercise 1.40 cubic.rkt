#lang racket

(define (cubic a b c)
	(lambda (w) (+ (cube w) (* a (square w)) (* b w) c))
)

(define (cube x)
	(* x x x)
)

(define (square x)
	(* x x)
)

(define (newtons-method g guess)
	(fixed-point (newton-transform g) guess)
)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance)
	)

  (define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next)
			)
		)
	)

  (try first-guess)
)

(define tolerance 0.00001)

(define (newton-transform g)
	(lambda (x) (- x (/ (g x) ((deriv g) x))))
)

(define (deriv g)
	(lambda (x)
		(/ (- (g (+ x dx)) (g x)) dx)
	)
)

(define dx 0.00001)

; Tests

> (newtons-method (cubic 0 0 0) 1000)
2.5555229328341865e-006
> (newtons-method (cubic 0 0 0) 10000)
2.617517159109334e-006
> (newtons-method (cubic 0 0 0) 10000000)
2.62097439104511e-006
> (newtons-method (cubic 2 3 8) 10000000)
-2.2482978222845467
> ((cubic 2 3 8) (newtons-method (cubic 2 3 8) 10000000))
1.7763568394002505e-015
> (newtons-method (cubic 1 1 1) 1)
-0.9999999999997796
> ((cubic 1 1 1) (newtons-method (cubic 1 1 1) 10000000))
2.191580250610059e-013
> 
