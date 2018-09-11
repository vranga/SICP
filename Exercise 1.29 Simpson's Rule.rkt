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
