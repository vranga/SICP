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

(define limit-for-repeat-tests 10000000000)

(define (start-prime-test n start-time)
	(cond
		((prime? n)
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
		((> times 0) (prime? n) (run-prime-test-in-loop n (- times 1)))
	)
)

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time)
)

(define (prime? n)
	(= n (smallest-divisor n))
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
(search-for-primes 100000007 100000039)
(search-for-primes 1000000007 1000000021)
(search-for-primes 10000000019 10000000061)
(search-for-primes 100000000003 100000000057)
(search-for-primes 1000000000039 1000000000063)
(search-for-primes 10000000000037 10000000000099)
(search-for-primes 100000000000031 100000000000097)
(search-for-primes 1000000000000037 1000000000000159)
(search-for-primes 10000000000000061 10000000000000079)
(search-for-primes 100000000000000003 100000000000000019)
(search-for-primes 1000000000000000003 1000000000000000031)

Results (non-prime numbers removed to shorten the amount of text)
=======

101 *** 0.002883741899711626
103 *** 0.0028690031893846882
107 *** 0.0029100040976719967
1009 *** 0.007977254718916318
1013 *** 0.008045245439246392
1019 *** 0.007952989047890248
10007 *** 0.02346340091724115
10009 *** 0.02495143133677176
10037 *** 0.023736492712136938
100003 *** 0.07379147582951659
100019 *** 0.0739533115960873
100043 *** 0.07788271073851018
1000003 *** 0.2325
1000033 *** 0.2355
1000037 *** 0.2324
10000019 *** 0.749
10000079 *** 0.795
10000103 *** 0.765
100000007 *** 2.34
100000037 *** 2.18
100000039 *** 2.49
1000000007 *** 12.5
1000000009 *** 7.8
1000000021 *** 7.8
10000000019 *** 25.545454545454547
10000000033 *** 22.727272727272727
10000000061 *** 21.272727272727273
100000000003 *** 79.36363636363636
100000000019 *** 76.54545454545455
100000000057 *** 75.0909090909091
1000000000039 *** 239.63636363636363
1000000000061 *** 232.54545454545453
1000000000063 *** 235.45454545454547
10000000000037 *** 734.5454545454545
10000000000051 *** 726.0909090909091
10000000000099 *** 730.3636363636364
100000000000031 *** 2365.5454545454545
100000000000067 *** 2296.0
100000000000097 *** 2341.3636363636365
1000000000000037 *** 7291.727272727273
1000000000000091 *** 7323.727272727273
1000000000000159 *** 7377.363636363636
10000000000000061 *** 23164.727272727272
10000000000000069 *** 23112.909090909092
10000000000000079 *** 23136.272727272728
100000000000000003 *** 73618.72727272728
100000000000000013 *** 73326.54545454546
100000000000000019 *** 73884.27272727272
1000000000000000003 *** 234368.54545454544
1000000000000000009 *** 259096.45454545456
1000000000000000031 *** 288978.7272727273

Observations
============

Generally takes lesser time than when we don't skip even numbers while testing
divisors. But, the number is not near half of the previous number (as we might expect)

Observed Ratios
===============

Observed ratios keep dropping as the numbers get larger and stabilize near
1.5. (For large numbers, the fluctuations caused by other processes in the
computer become less significant.) The reason it is not 2 is that there is an
extra conditional being checked inside the 'next' procedure.
