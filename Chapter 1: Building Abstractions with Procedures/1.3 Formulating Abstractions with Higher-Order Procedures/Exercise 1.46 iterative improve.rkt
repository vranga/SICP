#lang racket

; Exercise 1.46.  Several of the numerical methods described in this chapter are instances of
; an extremely general computational strategy known as iterative improvement. Iterative
; improvement says that, to compute something, we start with an initial guess for the answer,
; test if the guess is good enough, and otherwise improve the guess and continue the process
; using the improved guess as the new guess. Write a procedure iterative-improve that takes
; two procedures as arguments: a method for telling whether a guess is good enough and a
; method for improving a guess. Iterative-improve should return as its value a procedure that
; takes a guess as argument and keeps improving the guess until it is good enough. Rewrite
; the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms
; of iterative-improve.

; SOLUTION

(define (iterative-improve good-enough? improve)
	(lambda (first-guess)
		(define (try guess)
			(let ((next (improve guess)))
				(cond
					((good-enough? guess next) next)
					(else
						(try next)
					)
				)
			)
		)
		(try first-guess)
	)
)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance)
	)
	((iterative-improve close-enough? f) first-guess)
)

(define (sqrt x)
	(fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)

(define (cube-root x)
	(fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0)
)

(define (fourth-root-one-damp x)
	(fixed-point (average-damp (lambda (y) (/ x (cube y)))) 1.0)
)

(define (fourth-root-two-damp x)
	(fixed-point (average-damp (average-damp (lambda (y) (/ x (cube y))))) 1.0)
)

(define (nth-root x n m)
	; x is the number for which the nth root is to be computed
	; m is the number of times average damping should be done
	(fixed-point ((repeated average-damp m) (lambda (y) (/ x (expt y (- n 1))))) 1.0)
)

(define tolerance 0.00000001)

(define (inc x)
	(+ x 1)
)

(define (average-damp f)
	(lambda (x) (average x (f x)))
)

(define (average x y)
	(/ (+ x y) 2)
)

(define (repeated f n)
	(cond
		((> n 1) (compose f (repeated f (- n 1))))
		((= n 1) f)
		(else
			identity
		)
	)
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

(define (identity x)
	x
)

; Tests

(cube-root 1)
(cube-root 2)
(cube-root 3)
(cube-root 4)
(cube-root 5)
(cube-root 6)
(cube-root 7)
(cube-root 8)
(sqrt 1)
(sqrt 4)
(sqrt 9)
(sqrt 10)
(sqrt 25)
(sqrt 15)
(sqrt 50)
(cube-root 50)
(fourth-root-two-damp 17)
(fourth-root-two-damp 16)
(fourth-root-two-damp 81)
(fourth-root-two-damp 90)
(fourth-root-two-damp 50)
(fourth-root-two-damp 256)
(fourth-root-two-damp 250)
(nth-root 9 2 1)
(sqrt 9)
(nth-root 100 2 1)
(sqrt 100)
(nth-root 100 3 1)
(cube-root 100)
(nth-root 100 4 2)
(nth-root 100 4 3)
(nth-root 99999999 8 4)
(nth-root 99999999 8 5)
(nth-root 7648593 7 4)
(nth-root 7648593 6 4)
(nth-root 7648593 5 4)
(nth-root 7648593 8 4)
(nth-root 7648593 9 4)
(nth-root 7648593 10 4)
(nth-root 7648593 11 4)
(nth-root 7648593 12 4)
(nth-root 7648593 13 4)
(nth-root 7648593 13 3)

Welcome to DrRacket, version 6.9 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (cube-root 1)
1.0
> (cube-root 6)
1.81712059492772
> (cube-root 1)
(cube-root 2)
(cube-root 3)
(cube-root 4)
(cube-root 5)
(cube-root 6)
(cube-root 7)
(cube-root 8)
(sqrt 1)
(sqrt 4)
(sqrt 9)
(sqrt 10)
(sqrt 25)
(sqrt 15)
(sqrt 50)
(cube-root 50)
(fourth-root-two-damp 17)
(fourth-root-two-damp 16)
(fourth-root-two-damp 81)
(fourth-root-two-damp 90)
(fourth-root-two-damp 50)
(fourth-root-two-damp 256)
(fourth-root-two-damp 250)
(nth-root 9 2 1)
(sqrt 9)
(nth-root 100 2 1)
(sqrt 100)
(nth-root 100 3 1)
(cube-root 100)
(nth-root 100 4 2)
(nth-root 100 4 3)
(nth-root 99999999 8 4)
(nth-root 99999999 8 5)
(nth-root 7648593 7 4)
(nth-root 7648593 6 4)
(nth-root 7648593 5 4)
(nth-root 7648593 8 4)
(nth-root 7648593 9 4)
(nth-root 7648593 10 4)
(nth-root 7648593 11 4)
(nth-root 7648593 12 4)
(nth-root 7648593 13 4)
(nth-root 7648593 13 3)
1.0
1.2599210520301471
1.4422495724833264
1.5874010540873353
1.7099759448228977
1.81712059492772
1.9129311857760343
1.9999999982250738
1.0
2.0
3.0
3.162277660168379
5.0
3.872983346207417
7.0710678118654755
3.684031500961882
2.0305431848689306
2.0
3.0
3.080070288241023
2.6591479484724942
4.0
3.976353643835253
3.0
3.0
10.0
10.0
4.641588830678613
4.641588830678613
3.1622776601683795
3.1622776656035585
9.9999999961324
10.000000013823481
9.624292058144878
14.036653922334388
23.807638017946097
7.251831331163606
5.818920115008623
4.879307127651792
4.224546951830381
3.7465522715410797
3.384591607884878
3.3845916101906384
> 
