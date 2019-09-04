; Exercise 1.31.   
; a.  The sum procedure is only the simplest of a vast number of similar abstractions that
; can be captured as higher-order procedures. Write an analogous procedure called product
; that returns the product of the values of a function at points over a given range. Show
; how to define factorial in terms of product. Also use product to compute approximations
; to pi using the formula

; pi   2 * 4 * 4 * 6 * 6 * 8 ...
; -- = --------------------------
; 4    3 * 3 * 5 * 5 * 7 * 7 ...

; b.  If your product procedure generates a recursive process, write one that generates an
; iterative process. If it generates an iterative process, write one that generates a
; recursive process.

; SOLUTION

(define (product-recur term a next b)
	(if
		(> a b) 1
		(* (term a) (product-recur term (next a) next b))
	)
)

(define (product-iter term a next b)
	(define (iter x result)
		(if (> x b)
			result
			(iter (next x) (* (term x) result))
		)
	)
  (iter a 1)
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

> (product-recur identity 1 inc 20)
2432902008176640000
> (product-iter identity 1 inc 20)
2432902008176640000
> (product-recur identity 1 inc 19)
121645100408832000
> (product-iter identity 1 inc 19)
121645100408832000
> (product-recur identity 1 inc 15)
1307674368000
> (product-iter identity 1 inc 15)
1307674368000
> (product-recur identity 1 inc 10)
3628800
> (product-iter identity 1 inc 10)
3628800
> (product-recur identity 1 inc 5)
120
> (product-iter identity 1 inc 5)
120
> (product-recur identity 1 inc 1)
1
> (product-iter identity 1 inc 1)
1
> (factorial-recur 10)
3628800
> (factorial-iter 10)
3628800
> (factorial-recur 5)
120
> (factorial-iter 5)
120
> (factorial-recur 1)
1
> (factorial-iter 1)
1
> (factorial-recur 15)
1307674368000
> (factorial-iter 15)
1307674368000
> (factorial-recur 14)
87178291200
> (factorial-iter 14)
87178291200
> (factorial-recur 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
> (factorial-iter 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
> (factorial-recur 99)
933262154439441526816992388562667004907159682643816214685929638952175999932299156089414639761565182862536979208272237582511852109168640000000000000000000000
> (factorial-iter 99)
933262154439441526816992388562667004907159682643816214685929638952175999932299156089414639761565182862536979208272237582511852109168640000000000000000000000

(pi-approx-recur 1)
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

(pi-approx-iter 1)
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
