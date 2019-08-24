; Exercise 1.22.  Most Lisp implementations include a primitive called runtime that returns
; an integer that specifies the amount of time the system has been running
; (measured, for example, in microseconds). The following timed-prime-test procedure, when
; called with an integer n, prints n and checks to see if n is prime. If n is prime, the
; procedure prints three asterisks followed by the amount of time used in performing the test.

; (define (timed-prime-test n)
;   (newline)
;   (display n)
;   (start-prime-test n (runtime)))
; (define (start-prime-test n start-time)
;   (if (prime? n)
;       (report-prime (- (runtime) start-time))))
; (define (report-prime elapsed-time)
;   (display " *** ")
;   (display elapsed-time))

; Using this procedure, write a procedure search-for-primes that checks the primality of
; consecutive odd integers in a specified range. Use your procedure to find the three
; smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than
; 1,000,000. Note the time needed to test each prime. Since the testing algorithm has
; order of growth of (n), you should expect that testing for primes around 10,000 should
; take about 10 times as long as testing for primes around 1000. Do your timing data bear
; this out? How well do the data for 100,000 and 1,000,000 support the n prediction? Is
; your result compatible with the notion that programs on your machine run in time
; proportional to the number of steps required for the computation?

; SOLUTION

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
        (else (find-divisor n (+ test-divisor 1)))
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
;(search-for-primes 1000000000000000003 1000000000000000031)

Results (reformatted & non-prime numbers removed to shorten the amount of text)
=======

101		 0.00637079719936292
103		 0.004361864583861101
107 	 0.0039275205968579835
1009	 0.011792687232306
1013 	 0.0119264533687262
1019 	 0.01724698205327067
10007 	 0.08252368405515455
10009 	 0.07239508317977862
10037 	 0.05888906509393625
100003 	 0.18470369407388149
100019 	 0.1783421015782841
100043 	 0.19039996798655434
1000003 	 0.4493
1000033 	 0.5835
1000037 	 0.7005
10000019 	 2.169
10000079 	 2.184
10000103 	 2.262
100000007 	 7.8
100000037 	 8.42
100000039 	 6.56
1000000007 	 23.4
1000000009 	 24.9
1000000021 	 23.4
10000000019 	 73.72727272727273
10000000033 	 76.54545454545455
10000000061 	 89.36363636363636
100000000003 	 231.0909090909091
100000000019 	 224.0909090909091
100000000057 	 235.45454545454547
1000000000039 	 755.9090909090909
1000000000061 	 727.4545454545455
1000000000063 	 730.3636363636364
10000000000037 	 2252.090909090909
10000000000051 	 2279.090909090909
10000000000099 		 1404.090909090909
100000000000031 	 4570.818181818182
100000000000067 	 4647.363636363636
100000000000097 	 3892.909090909091
1000000000000037 	 12405.09090909091
1000000000000091 	 12450.454545454546
1000000000000159 	 12078.363636363636
10000000000000061 	 37540.545454545456
10000000000000069 	 37391.27272727273
10000000000000079 	 37386.181818181816
100000000000000003 	 118307.72727272728
100000000000000013 	 118536.27272727272
100000000000000019 	 117955.90909090909

Observations:

Yes, the timing data bears out the expectation that testing for
primes around 10^n 	should take about square-root (10) times as long as
testing for primes around 10^(n-1)

Q: Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?

A: More or less

see plots in related spreadsheet. The square root plot is a straight line!
