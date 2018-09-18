#lang racket

(define us-coins (list 50 25 10 5 1))
(define us-coins-reverse (list 1 5 10 25 50))
(define us-coins-random1 (list 5 25 50 1 10))
(define us-coins-random2 (list 25 5 1 50 10))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define uk-coins-reverse (list 0.5 1 2 5 10 20 50 100))
(define uk-coins-random1 (list 1 100 0.5 10 20 5 50 2))
(define uk-coins-random2 (list 100 1 0.5 50 2 5 10 20))
(define uk-coins-random3 (list 1 5 20 0.5 10 2 50 100))

(define (cc amount coin-values)
	(cond
		((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else
			(+
				(cc amount (except-first-denomination coin-values))
				(cc (- amount (first-denomination coin-values)) coin-values)
			)
		)
	)
)

(define (first-denomination coin-values)
	(car coin-values)
)

(define (except-first-denomination coin-values)
	(cdr coin-values)
)

(define (no-more? coin-values)
	(null? coin-values)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (cc 100 us-coins)
292
> (cc 150 us-coins)
972
> (cc 100 uk-coins)
104561
> (cc 150 uk-coins)
684717
> (define us-coins-reverse (list 1 5 10 25 50))
> (cc 100 us-coins-reverse)
292
> (define us-coins-random1 (list 5 25 50 1 10))
> (cc 100 us-coins-random1)
292
> (define us-coins-random2 (list 25 5 1 50 10))
> (cc 100 us-coins-random2)
292
> (define uk-coins-random2 (list 100 1 0.5 50 2 5 10 20))
> (define uk-coins-reverse (list 0.5 1 2 5 10 20 50 100))
> (define uk-coins-random1 (list 1 100 0.5 10 20 5 50 2))
> (cc 100 uk-coins)
104561
> (cc 100 uk-coins-reverse)
104561
> (cc 100 uk-coins-random1)
104561
> (cc 100 uk-coins-random2)
104561
> (cc 150 us-coins)
972
> (cc 150 us-coins-reverse)
972
> (cc 150 us-coins-random1)
972
> (cc 150 us-coins-random2)
972
> (cc 150 uk-coins)
684717
> (cc 150 uk-coins-reverse)
684717
> (cc 150 uk-coins-random1)
684717
> (cc 150 uk-coins-random2)
684717
> (define uk-coins-random3 (list 1 5 20 0.5 10 2 50 100))
> (cc 150 uk-coins-random3)
684717
> (cc 200 us-coins)
2435
> (cc 400 us-coins)
26517
> (cc 800 us-coins)
343145
> (cc 1600 us-coins)
4908497
> 

Observation: The order of denominations in the coin-list does not affect the result produced by cc. The algorithm makes no assumptions about the order so the order does not matter. However, performance of the function (execution time) differs significantly. The fastest execution happens when the coins are arranged in descending order starting with the highest denomination. The slowest execution happens when the coins are arranged in ascending order starting with the smallest denomination.

In the case where the list is in ascending order, the subtractions are small so the depth of tree recursion is much larger. So the call stack is much deeper which results in larger memory usage and running time. The reverse is true when the coins are arrange in decreasing order.

Also, the higher the amount to be changed, the more execution time it takes.
