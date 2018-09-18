#lang racket

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

(define (fixed-point f first-guess)
	(define iters 0)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance)
	)

	(define (try guess)
		(set! iters (inc iters))
		; (display guess)
		; (newline)
		(let ((next (f guess)))
			


			; (if (close-enough? guess next)
			; 	next
			; 	(try next)
			; )

			(cond
				((close-enough? guess next) (display "Number of iterations: ") (display iters) (newline) next)
				(else
					(try next)
				)
			)


		)
	)

	(try first-guess)
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

; Observations from the test below:
; 1. Fourth and Fifth roots need two average dampings. Three dampings also works but
; interestingly, it takes more iterations to converge to the same tolerance
; 2. I tried the 8th root of a number and found that even 3 average dampings were not enough
; but 4 dampings worked. 5 worked too but again, took more iterations than 4 dampings

> (cube-root 1)
Number of iterations: 1
1.0
> (cube-root 2)
Number of iterations: 27
1.2599210520301471
> (cube-root 3)
Number of iterations: 27
1.4422495724833264
> (cube-root 4)
Number of iterations: 25
1.5874010540873353
> (cube-root 5)
Number of iterations: 27
1.7099759448228977
> (cube-root 6)
Number of iterations: 28
1.81712059492772
> (cube-root 7)
Number of iterations: 28
1.9129311857760343
> (cube-root 8)
Number of iterations: 29
1.9999999982250738
> (sqrt 1)
Number of iterations: 1
1.0
> (sqrt 4)
Number of iterations: 6
2.0
> (sqrt 9)
Number of iterations: 6
3.0
> (sqrt 10)
Number of iterations: 6
3.162277660168379
> (sqrt 25)
Number of iterations: 7
5.0
> (sqrt 15)
Number of iterations: 7
3.872983346207417
> (sqrt 50)
Number of iterations: 8
7.0710678118654755
> (cube-root 50)
Number of iterations: 30
3.684031500961882
> (fourth-root-two-damp 17)
Number of iterations: 9
2.0305431848689306
> (fourth-root-two-damp 16)
Number of iterations: 9
2.0
> (fourth-root-two-damp 81)
Number of iterations: 13
3.0
> (fourth-root-two-damp 90)
Number of iterations: 13
3.080070288241023
> (fourth-root-two-damp 50)
Number of iterations: 12
2.6591479484724942
> (fourth-root-two-damp 256)
Number of iterations: 16
4.0
> (fourth-root-two-damp 250)
Number of iterations: 16
3.976353643835253
> (nth-root 9 2 1)
Number of iterations: 6
3.0
> (sqrt 9)
Number of iterations: 6
3.0
> (nth-root 100 2 1)
Number of iterations: 8
10.0
> (sqrt 100)
Number of iterations: 8
10.0
> (nth-root 100 3 1)
Number of iterations: 31
4.641588830678613
> (cube-root 100)
Number of iterations: 31
4.641588830678613
> (nth-root 100 4 2)
Number of iterations: 13
3.1622776601683795
> (nth-root 100 4 3)
Number of iterations: 39
3.1622776656035585
> (nth-root 99999999 8 4)
Number of iterations: 235
9.9999999961324
> (nth-root 99999999 8 5)
Number of iterations: 461
10.000000013823481
> (nth-root 7648593 7 4)
Number of iterations: 201
9.624292058144878
> (nth-root 7648593 6 4)
Number of iterations: 203
14.036653922334388
> (nth-root 7648593 5 4)
Number of iterations: 206
23.807638017946097
> (nth-root 7648593 8 4)
Number of iterations: 200
7.251831331163606
> (nth-root 7648593 9 4)
Number of iterations: 199
5.818920115008623
> (nth-root 7648593 10 4)
Number of iterations: 198
4.879307127651792
> (nth-root 7648593 11 4)
Number of iterations: 198
4.224546951830381
> (nth-root 7648593 12 4)
Number of iterations: 197
3.7465522715410797
> (nth-root 7648593 13 4)
Number of iterations: 197
3.384591607884878
> (nth-root 7648593 13 3)
Number of iterations: 131
3.3845916101906384
> (nth-root 7648593 13 2)
(DOES NOT CONVERGE!!)
