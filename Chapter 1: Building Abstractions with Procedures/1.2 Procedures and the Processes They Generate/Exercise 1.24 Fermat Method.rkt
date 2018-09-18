(define (search-for-primes start end)
	(cond
		((> start end) (newline) (display "Done."))
		((even? start) (search-for-primes (+ start 1) end))
		(else (timed-prime-test start) (search-for-primes (+ start 2) end))
	)
)

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (current-milliseconds))
)

(define limit-for-repeat-tests 100000000)

(define (start-prime-test n start-time)
	(cond
		((fast-prime? n 100)
			(cond
				((< n limit-for-repeat-tests)
					(run-prime-test-in-loop n (quotient limit-for-repeat-tests n))
					(report-prime
						(exact->inexact
							(/
								(- (current-milliseconds) start-time)
								(+ (quotient limit-for-repeat-tests n) 1)
							)
						)
					)
				)
				(else
					(run-prime-test-in-loop n 10)
					(report-prime
						(exact->inexact
							(/
								(- (current-milliseconds) start-time) 11
							)
						)
					)
				)
			)
		)
	)
)

(define (run-prime-test-in-loop n times)
	(cond
		((> times 0) (fast-prime? n 100) (run-prime-test-in-loop n (- times 1)))
	)
)

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time)
)

(define (fast-prime? n times)
	(cond
		((= times 0) true) ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)
	)
)

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a)
	)
	(try-it (+ 1 (random (- n 1))))
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

(search-for-primes 101 107)
(search-for-primes 1009 1019)
(search-for-primes 10007 10037)
(search-for-primes 100003 100043)
(search-for-primes 1000003 1000037)
(search-for-primes 10000019 10000103)
;(search-for-primes 100000007 100000039)
;(search-for-primes 1000000007 1000000021)
;(search-for-primes 10000000019 10000000061)
;(search-for-primes 100000000003 100000000057)
;(search-for-primes 1000000000039 1000000000063)
;(search-for-primes 10000000000037 10000000000099)
;(search-for-primes 100000000000031 100000000000097)
;(search-for-primes 1000000000000037 1000000000000159)
;(search-for-primes 10000000000000061 10000000000000079)
;(search-for-primes 100000000000000003 100000000000000019)
;(search-for-primes 1000000000000000003 1000000000000000031)

Results (reformatted & non-prime numbers removed to shorten the amount of text)
=======

(5 random numbers per prime)
============================

101 *** 0.02506428119749357
103 *** 0.025600969205276415
107 *** 0.02505941067995247
Done.
1009 *** 0.033889282230713295
1013 *** 0.03777800896687278
1019 *** 0.040177436426375265
Done.
10007 *** 0.04562489179936776
10009 *** 0.04323186544703689
10037 *** 0.04394096640215835
Done.
100003 *** 0.051311026220524414
100019 *** 0.06821227821007782
100043 *** 0.07277056363672743
Done.
1000003 *** 0.0702
1000033 *** 0.0874
1000037 *** 0.0749
Done.
10000019 *** 0.078
10000079 *** 0.11
10000103 *** 0.078

(100 random numbers per prime)
==============================

101 *** 0.6265559034440965
103 *** 0.6662347534283543
107 *** 0.6647831111301333
1009 *** 0.9167381367988781
1013 *** 0.971879210267735
1019 *** 0.9927651422515692
10007 *** 1.2317390434260556
10009 *** 1.167834267413931
10037 *** 1.1961059815335207
100003 *** 1.435
100019 *** 1.482
100043 *** 1.451
1000003 *** 1.56
1000033 *** 1.56
1000037 *** 1.72
10000019 *** 6.2
10000079 *** 1.6
10000103 *** 3.1

Observations
============

the O(log n) growth can be seen. For every 10 fold increase of the number, the
time taken is increasing by the same roughly constant amount.

There are some discrepancies (see Excel sheet). These are probably due to
other stuff happening on the computer
