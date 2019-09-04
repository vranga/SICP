; Exercise 1.32.  a. Show that sum and product (exercise 1.31) are both special cases of a
; still more general notion called accumulate that combines a collection of terms, using
; some general accumulation function:

; (accumulate combiner null-value term a next b)

; Accumulate takes as arguments the same term and range specifications as sum and product,
; together with a combiner procedure (of two arguments) that specifies how the current term
; is to be combined with the accumulation of the preceding terms and a null-value that
; specifies what base value to use when the terms run out. Write accumulate and show how sum
; and product can both be defined as simple calls to accumulate.

; b. If your accumulate procedure generates a recursive process, write one that generates
; an iterative process. If it generates an iterative process, write one that generates a
; recursive process.

; SOLUTION

(define (accumulate-recur combiner null-value term a next b)
	(if
		(> a b) null-value
		(combiner (term a) (accumulate-recur combiner null-value term (next a) next b))
	)
)

(define (accumulate-iter combiner null-value term a next b)
	(define (iter x result)
		(if (> x b)
			result
			(iter (next x) (combiner (term x) result))
		)
	)
  (iter a null-value)
)

(define (sum-recur term a next b)
	(accumulate-recur + 0 term a next b)
)

(define (sum-iter term a next b)
	(accumulate-iter + 0 term a next b)
)

(define (product-recur term a next b)
	(accumulate-recur * 1 term a next b)
)

(define (product-iter term a next b)
	(accumulate-iter * 1 term a next b)
)

(define (inc x)
	(+ x 1)
)

(define (identity x)
	x
)

(define (factorial-recur n)
	(product-recur identity 1 inc n)
)

(define (factorial-iter n)
	(product-iter identity 1 inc n)
)

; Higher values of n will give better approximations of pi
(define (pi-approx-recur n)
	(* (product-recur pi-approx-term 1 inc n) 4)
)

(define (pi-approx-iter n)
	(* (product-iter pi-approx-term 1 inc n) 4)
)

(define (pi-approx-term x)
	(/ (* (* 2.0 x) (* 2 (+ x 1))) (square (+ (* 2 x) 1)))
)

(define (square x)
	(* x x)
)

; Tests
> (sum-recur identity 1 inc 20)
210
> (sum-recur identity 1 inc 35)
630
> (sum-iter identity 1 inc 35)
630
> (product-recur identity 1 inc 35)
10333147966386144929666651337523200000000
> (product-iter identity 1 inc 35)
10333147966386144929666651337523200000000
> (factorial-recur 24)
620448401733239439360000
> (factorial-iter 24)
620448401733239439360000
> (factorial-iter 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
> (factorial-recur 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
> (pi-approx-recur 1)
(pi-approx-recur 5)
(pi-approx-recur 10)
(pi-approx-recur 100)
(pi-approx-recur 1000)
(pi-approx-recur 10000)
(pi-approx-recur 100000)
(pi-approx-recur 1000000)
3.5555555555555554
3.275101041334807
3.213784940293188
3.1493784731686008
3.142377365093882
3.1416711865344946
3.141600507501579
3.141593438981073
> (pi-approx-iter 1)
(pi-approx-iter 5)
(pi-approx-iter 10)
(pi-approx-iter 100)
(pi-approx-iter 1000)
(pi-approx-iter 10000)
(pi-approx-iter 100000)
(pi-approx-iter 1000000)
(pi-approx-iter 10000000)
(pi-approx-iter 100000000)
(pi-approx-iter 1000000000)
(pi-approx-iter 10000000000)
(pi-approx-iter 100000000000)
3.5555555555555554
3.2751010413348074
3.213784940293189
3.149378473168601
3.1423773650938855
3.1416711865345
3.1416005075026887
3.1415934389872975
3.1415927321451758
3.1415926690744156
3.1415926440043203
3.141592640508495
