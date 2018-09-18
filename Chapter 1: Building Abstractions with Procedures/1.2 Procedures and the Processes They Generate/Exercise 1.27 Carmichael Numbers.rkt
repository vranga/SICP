(define (carmichael-test n)
	(cond
		((prime? n) (display n) (display " is prime") (newline))
	)
	(cond
		((fermat-test-all? n) (display n) (display " passed the Fermat prime test for all numbers less than it"))
		(else (display n) (display " failed the Fermat prime test"))
	)
)

(define (prime? n)
	(define sd (smallest-divisor n))
	(cond
		((< sd n) (display n) (display " has smallest divisor ") (display sd) (newline))
	)
	(= n sd)
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

(define (fermat-test-all? n)
	(fermat-test-all-internal? n 1)
)

(define (fermat-test-all-internal? n counter)
	(cond
		((= 1 (- n counter)) true)
		((fermat-test n counter) (fermat-test-all-internal? n (+ counter 1)))
		(else false)
	)
)

(define (fermat-test n a)
	(= (expmod a n n) a)
)

(define (expmod base exp m)
	(cond
		((= exp 0) 1)
		((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
		(else
			(remainder (* base (expmod base (- exp 1) m)) m)
		)
	)
)

(define (square x)
	(* x x)
)

(define (divides? a b)
  (= (remainder b a) 0)
)

(define (even? n)
	(= (remainder n 2) 0)
)

;Tests
;=====

> (carmichael-test 11)
11 is prime
11 passed the Fermat prime test for all numbers less than it
> (carmichael-test 10)
10 has smallest divisor 2
10 failed the Fermat prime test
> (carmichael-test 12)
12 has smallest divisor 2
12 failed the Fermat prime test
> (carmichael-test 13)
13 is prime
13 passed the Fermat prime test for all numbers less than it
> (carmichael-test 561)
561 has smallest divisor 3
561 passed the Fermat prime test for all numbers less than it
> (carmichael-test 560)
560 has smallest divisor 2
560 failed the Fermat prime test
> (carmichael-test 561)
561 has smallest divisor 3
561 passed the Fermat prime test for all numbers less than it
> (carmichael-test 1105)
1105 has smallest divisor 5
1105 passed the Fermat prime test for all numbers less than it
> (carmichael-test 1729)
1729 has smallest divisor 7
1729 passed the Fermat prime test for all numbers less than it
> (carmichael-test 2465)
2465 has smallest divisor 5
2465 passed the Fermat prime test for all numbers less than it
> (carmichael-test 2821)
2821 has smallest divisor 7
2821 passed the Fermat prime test for all numbers less than it
> (carmichael-test 6601)
6601 has smallest divisor 7
6601 passed the Fermat prime test for all numbers less than it

;Observations
;============

The above program demonstrates that the Carmichael numbers 561, 1105....to 6601 fool the Fermat
test. These are all composite numbers that pass the Fermat test.
