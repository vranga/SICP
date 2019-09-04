; Exercise 1.29.  Simpson's Rule is a more accurate method of numerical integration than the
; method illustrated above. Using Simpson's Rule, the integral of a function f between a and
; b is approximated as

; (h/3)*[y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + yn]

; where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing n increases
; the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b,
; and n and returns the value of the integral, computed using Simpson's Rule. Use your
; procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000), and compare the
; results to those of the integral procedure shown above.

; SOLUTION

; n needs to be even

(define (simpsons-numerical-integ func a b n)
	(define (interval a b n)
		(/ (- b a) n)
	)
	(define h (interval a b n))
	(define (add-interval x)
		(+ x h)
	)
	(define (simpsons-term k)
		(+
			(func (+ a (* k h)))
			(* 2 (func (+ a (* (+ k 1) h))))
		)
	)
	(define (next-simpsons-term k)
		(+ k 2)
	)

	; separating out the first and last y terms for convenience

	(+
		(* (/ h 3) (+ (func a) (func b)))
		(/ (* 4 h (func (add-interval a))) 3)
		(* (/ (* 2 h) 3) (sum simpsons-term 2 next-simpsons-term (- n 2)))
	)
)

(define (sum term a next b)
	(if
		(> a b) 0
		(+ (term a) (sum term (next a) next b))
	)
)

(define (cube x)
	(* x x x)
)

; Tests

> (simpsons-numerical-integ cube 0 1 8)
1/4
> (simpsons-numerical-integ cube 0.0 1.0 8)
0.25
> (simpsons-numerical-integ cube 0.0 1.0 4)
0.24999999999999997
> (simpsons-numerical-integ cube 0.0 1.0 2)
0.25
> (simpsons-numerical-integ cube 0.0 1.0 2)
0.25
> (simpsons-numerical-integ cube 0.0 1.0 4)
0.24999999999999997
> (simpsons-numerical-integ cube 0.0 1.0 8)
0.25
> (simpsons-numerical-integ cube 0.0 1.0 10)
0.25
> (simpsons-numerical-integ cube 0.0 1.0 12)
0.24999999999999994
> (simpsons-numerical-integ cube 0.0 1.0 14)
0.24999999999999992
> (simpsons-numerical-integ cube 0.0 1.0 16)
0.24999999999999997
> (simpsons-numerical-integ cube 0.0 1.0 18)
0.24999999999999994
> (simpsons-numerical-integ cube 0.0 1.0 20)
0.25
> (simpsons-numerical-integ cube 0.0 1.0 40)
0.25000000000000006
> (simpsons-numerical-integ cube 0.0 1.0 80)
0.25000000000000006
> 
