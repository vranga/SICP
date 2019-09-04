; Exercise 1.33.  You can obtain an even more general version of accumulate (exercise 1.32) by
; introducing the notion of a filter on the terms to be combined. That is, combine only those
; terms derived from values in the range that satisfy a specified condition. The resulting
; filtered-accumulate abstraction takes the same arguments as accumulate, together with an
; additional predicate of one argument that specifies the filter. Write filtered-accumulate as
; a procedure. Show how to express the following using filtered-accumulate:

; a. the sum of the squares of the prime numbers in the interval a to b (assuming that you
; have a prime? predicate already written)

; b. the product of all the positive integers less than n that are relatively prime to n
; (i.e., all positive integers i < n such that GCD(i,n) = 1).

; SOLUTION

#lang racket

;Part b

(define (prod-integer-rel-prime-recur n)
	(define (rel-prime? x)
		(cond
			((= 1 (gcd x n)) true)
			(else
				false
			)
		)
	)
	(filtered-accumulate-recur * 1 identity 1 inc n rel-prime?)
)

(define (prod-integer-rel-prime-iter n)
	(define (rel-prime? x)
		(cond
			((= 1 (gcd x n)) true)
			(else
				false
			)
		)
	)
	(filtered-accumulate-iter * 1 identity 1 inc n rel-prime?)
)

;Part a

(define (sum-squares-primes-recur a b)
	(filtered-accumulate-recur + 0 square a inc b prime?)
)

(define (sum-squares-primes-iter a b)
	(filtered-accumulate-iter + 0 square a inc b prime?)
)

(define (filtered-accumulate-recur combiner null-value term a next b filter?)
	(cond
		((> a b) null-value)
		(else
			(define current-term (term a))
			(define next-term (next a))
			(cond
				((filter? a)
					(combiner current-term (filtered-accumulate-recur combiner
					null-value term next-term next b filter?))
				)
				(else
					; don't combine with this term
					(filtered-accumulate-recur combiner null-value term next-term next b filter?)
				)
			)
		)
	)
)

(define (filtered-accumulate-iter combiner null-value term a next b filter?)
	(define (iter x result)
		(cond
			((> x b) result)
			(else
				(define current-term (term x))
				(define next-term (next x))
				(cond
					((filter? x) (iter next-term (combiner current-term result)))
					(else
						; don't combine with this term
						(iter next-term result)
					)
				)
			)
		)
	)
  (iter a null-value)
)

(define (always-true x)
	true
)

(define (sum-recur term a next b)
	(filtered-accumulate-recur + 0 term a next b always-true)
)

(define (sum-iter term a next b)
	(filtered-accumulate-iter + 0 term a next b always-true)
)

(define (product-recur term a next b)
	(filtered-accumulate-recur * 1 term a next b always-true)
)

(define (product-iter term a next b)
	(filtered-accumulate-iter * 1 term a next b always-true)
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

(define (prime? n)
	(if (= n 1) false (= n (smallest-divisor n)))
)

(define (smallest-divisor n)
	(find-divisor n 2)
)

(define (find-divisor n test-divisor)
	(cond
		((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))
	)
)

(define (next divisor)
	(cond
		((= divisor 2) 3)
		(else (+ divisor 2))
	)
)

(define (square x)
	(* x x)
)

(define (divides? a b)
  (= (remainder b a) 0)
)

; Tests
(sum-recur identity 1 inc 20)
(sum-recur identity 1 inc 35)
(sum-iter identity 1 inc 35)
(product-recur identity 1 inc 35)
(product-iter identity 1 inc 35)
(factorial-recur 24)
(factorial-iter 24)
(factorial-iter 100)
(factorial-recur 100)
210
630
630
10333147966386144929666651337523200000000
10333147966386144929666651337523200000000
620448401733239439360000
620448401733239439360000
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
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

> (sum-squares-primes-recur 1 9)
(sum-squares-primes-recur 1 10)
(sum-squares-primes-recur 5 6)
(sum-squares-primes-recur 15 16)
87
87
25
0
> (sum-squares-primes-recur 10 16) 
290
> (sum-squares-primes-iter 1 9)
(sum-squares-primes-iter 1 10)
(sum-squares-primes-iter 5 6)
(sum-squares-primes-iter 15 16)
(sum-squares-primes-iter 10 16)
87
87
25
0
290
> (sum-squares-primes-iter 50 60)
6290
> (sum-squares-primes-recur 50 60)
6290
> (sum-squares-primes-recur 1 20)
1027
> (sum-squares-primes-recur 1 21)
1027
> (sum-squares-primes-recur 1 22)
1027
> (sum-squares-primes-recur 1 23)
1556
> (sum-squares-primes-recur 1 100)
65796
> (sum-squares-primes-recur 100 200)
499269
> (sum-squares-primes-iter 1 100)
65796
> (sum-squares-primes-iter 100 200)
499269 

>(prod-integer-rel-prime-iter 3)
(prod-integer-rel-prime-iter 10)
(prod-integer-rel-prime-iter 30)
(prod-integer-rel-prime-iter 50)
(prod-integer-rel-prime-iter 100)
(prod-integer-rel-prime-iter 120)
(prod-integer-rel-prime-iter 300)
2
189
215656441
19787798161590610642439949
426252881942771063138176712755660145456313428952105524817872601
43050594261939447419527053602904653655332696392323361
26618141629127862004526395653751644432464276579742831763091833513351225844500365841229477366277762467766027685077083068563524919759050310026185275144882070561256401
> (prod-integer-rel-prime-recur 3)
(prod-integer-rel-prime-recur 10)
(prod-integer-rel-prime-recur 30)
(prod-integer-rel-prime-recur 50)
(prod-integer-rel-prime-recur 100)
(prod-integer-rel-prime-recur 120)
(prod-integer-rel-prime-recur 300)
2
189
215656441
19787798161590610642439949
426252881942771063138176712755660145456313428952105524817872601
43050594261939447419527053602904653655332696392323361
26618141629127862004526395653751644432464276579742831763091833513351225844500365841229477366277762467766027685077083068563524919759050310026185275144882070561256401
> 
