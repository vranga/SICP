; Exercise 1.30.  The sum procedure above generates a linear recursion. The procedure can be
; rewritten so that the sum is performed iteratively. Show how to do this by filling in the
; missing expressions in the following definition:

; (define (sum term a next b)
;   (define (iter a result)
;     (if <??>
;         <??>
;         (iter <??> <??>)))
;   (iter <??> <??>))

; SOLUTION

(define (sum-recur term a next b)
	(if
		(> a b) 0
		(+ (term a) (sum-recur term (next a) next b))
	)
)

(define (sum-iter term a next b)
	(define (iter x result)
		(if (> x b)
			result
			(iter (next x) (+ (term x) result))
		)
	)
  (iter a 0)
)

(define (inc x)
	(+ x 1)
)

(define (identity x)
	x
)

; Tests

> (sum-recur identity 1 inc 1)
1
> (sum-recur identity 1 inc 10)
55
> (sum-recur identity 1 inc 100)
5050
> (sum-recur identity 1 inc 1000)
500500
> (sum-recur identity 1 inc 10000)
50005000
> (sum-recur identity 1 inc 100000)
5000050000
> (sum-recur identity 1 inc 1000000)
500000500000
> (sum-iter identity 1 inc 1000000)
500000500000
> (sum-iter identity 1 inc 10000000)
50000005000000
> (sum-iter identity 1 inc 100000000)
5000000050000000
> (sum-iter identity 1 inc 1000000000)
500000000500000000
> (sum-iter identity 1 inc 10000000000)
50000000005000000000
> (sum-iter identity 1 inc 1000000)
500000500000
> (sum-iter identity 1 inc 100000)
5000050000
> (sum-iter identity 1 inc 10000)
50005000
> (sum-iter identity 1 inc 1000)
500500
> (sum-iter identity 1 inc 100)
5050
> (sum-iter identity 1 inc 10)
55
> (sum-iter identity 1 inc 1)
1
> 
