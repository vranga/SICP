; Exercise 1.28.  One variant of the Fermat test that cannot be fooled is called the
; Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat's
; Little Theorem, which states that if n is a prime number and a is any positive integer less
; than n, then a raised to the (n - 1)st power is congruent to 1 modulo n. To test the
; primality of a number n by the Miller-Rabin test, we pick a random number a < n and raise a
; to the (n - 1)st power modulo n using the expmod procedure. However, whenever we perform
; the squaring step in expmod, we check to see if we have discovered a "nontrivial square
; root of 1 modulo n," that is, a number not equal to 1 or n - 1 whose square is equal to 1
; modulo n. It is possible to prove that if such a nontrivial square root of 1 exists, then n
; is not prime. It is also possible to prove that if n is an odd number that is not prime,
; then, for at least half the numbers a < n, computing a^(n-1) in this way will reveal a
; nontrivial square root of 1 modulo n. (This is why the Miller-Rabin test cannot be fooled.)
; Modify the expmod procedure to signal if it discovers a nontrivial square root of 1, and
; use this to implement the Miller-Rabin test with a procedure analogous to fermat-test.
; Check your procedure by testing various known primes and non-primes. Hint: One convenient
; way to make expmod signal is to have it return 0.

; SOLUTION

(define (carmichael-fermat-millerrabin-compare-test n)
	(cond
		((prime? n) (display n) (display " is prime") (newline))
	)
	(cond
		((fermat-test-all? n) (display n) (display " passed the Fermat prime test for all numbers less than it") (newline))
		(else (display n) (display " failed the Fermat prime test") (newline))
	)
	(cond
		((miller-rabin-test-all? n) (display n) (display " passed the Miller-Rabin prime test for all numbers less than it") (newline))
		(else (display n) (display " failed the Miller-Rabin prime test") (newline))
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

(define (miller-rabin-test-all? n)
	(miller-rabin-test-all-internal? n 1)
)

(define (fermat-test-all-internal? n counter)
	(cond
		((= 1 (- n counter)) true)
		((fermat-test n counter) (fermat-test-all-internal? n (+ counter 1)))
		(else false)
	)
)

(define (miller-rabin-test-all-internal? n counter)
	;(display "Entered miller-rabin-test-all-internal? ") (display n) (display " ") (display counter)
	;(newline)
	(cond
		((= 1 (- n counter)) true)
		((miller-rabin-test n counter) (miller-rabin-test-all-internal? n (+ counter 1)))
		(else false)
	)
)

(define (fermat-test n a)
	(= (expmod a n n) a)
)

(define (miller-rabin-test n a)
	(define mod (expmodmillerrabin a (- n 1) n))
	;(display "expmodmillerrabin " ) (display a) (display " ") (display (- n 1)) (display " ") (display n) (display " returned with ") (display mod) (newline)
	(= mod 1)
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

(define (expmodmillerrabin base exp m)
	;(display "Entered expmodmillerrabin ") (display base) (display " ") (display exp) (display " ") (display m)
	;(newline)
	(cond
		((= exp 0) 1)
		((even? exp)
			(define val (square (expmodmillerrabin base (/ exp 2) m)))
			(cond
				((non-triv-sqrt-of-unity-mod-n? val m) (display "Found non-trivial square root of 1 modulo ") (display m) (newline) 0)
				(else
					(remainder val m)
				)
			)
		)
		(else
			(remainder (* base (expmodmillerrabin base (- exp 1) m)) m)
		)
	)
)

(define (non-triv-sqrt-of-unity-mod-n? number n)
	;(display "Entered non-triv-sqrt-of-unity-mod-n? ") (display number) (display " ") (display n)
	;(newline)
	(and
		(> number 1)
		(< number (- n 1))
		;(not (= number 1))
		;(not (= number (- n 1)))
		;(= (remainder (square number) n) 1)
		(= (remainder number n) 1)
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

> (carmichael-fermat-millerrabin-compare-test 1009)
1009 is prime
1009 passed the Fermat prime test for all numbers less than it
1009 passed the Miller-Rabin prime test for all numbers less than it
> (carmichael-fermat-millerrabin-compare-test 10007)
10007 is prime
10007 passed the Fermat prime test for all numbers less than it
10007 passed the Miller-Rabin prime test for all numbers less than it
> (carmichael-fermat-millerrabin-compare-test 10003)
10003 has smallest divisor 7
10003 failed the Fermat prime test
10003 failed the Miller-Rabin prime test
> (carmichael-fermat-millerrabin-compare-test 10037)
10037 is prime
10037 passed the Fermat prime test for all numbers less than it
10037 passed the Miller-Rabin prime test for all numbers less than it
> (carmichael-fermat-millerrabin-compare-test 561)
561 has smallest divisor 3
561 passed the Fermat prime test for all numbers less than it
561 failed the Miller-Rabin prime test
> (carmichael-fermat-millerrabin-compare-test 1105)
1105 has smallest divisor 5
1105 passed the Fermat prime test for all numbers less than it
1105 failed the Miller-Rabin prime test
> (carmichael-fermat-millerrabin-compare-test 1729)
1729 has smallest divisor 7
1729 passed the Fermat prime test for all numbers less than it
1729 failed the Miller-Rabin prime test
> (carmichael-fermat-millerrabin-compare-test 1728)
1728 has smallest divisor 2
1728 failed the Fermat prime test
1728 failed the Miller-Rabin prime test
> (carmichael-fermat-millerrabin-compare-test 1731)
1731 has smallest divisor 3
1731 failed the Fermat prime test
1731 failed the Miller-Rabin prime test
> (carmichael-fermat-millerrabin-compare-test 2465)
2465 has smallest divisor 5
2465 passed the Fermat prime test for all numbers less than it
2465 failed the Miller-Rabin prime test
> (carmichael-fermat-millerrabin-compare-test 2821)
2821 has smallest divisor 7
2821 passed the Fermat prime test for all numbers less than it
2821 failed the Miller-Rabin prime test
> (carmichael-fermat-millerrabin-compare-test 6601)
6601 has smallest divisor 7
6601 passed the Fermat prime test for all numbers less than it
6601 failed the Miller-Rabin prime test
> (carmichael-fermat-millerrabin-compare-test 10000019)
10000019 is prime
10000019 passed the Fermat prime test for all numbers less than it
10000019 passed the Miller-Rabin prime test for all numbers less than it
> (carmichael-fermat-millerrabin-compare-test 8)
8 has smallest divisor 2
8 failed the Fermat prime test
8 failed the Miller-Rabin prime test
> 
