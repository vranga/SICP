#lang racket

; Exercise 3.5.  Monte Carlo integration is a method of estimating definite integrals by means
; of Monte Carlo simulation. Consider computing the area of a region of space described by a
; predicate P(x, y) that is true for points (x, y) in the region and false for points not in
; the region. For example, the region contained within a circle of radius 3 centered at (5, 7)
; is described by the predicate that tests whether (x - 5)2 + (y - 7)2< 32. To estimate the
; area of the region described by such a predicate, begin by choosing a rectangle that contains
; the region. For example, a rectangle with diagonally opposite corners at (2, 4) and (8, 10)
; contains the circle above. The desired integral is the area of that portion of the rectangle
; that lies in the region. We can estimate the integral by picking, at random, points (x,y)
; that lie in the rectangle, and testing P(x, y) for each point to determine whether the point
; lies in the region. If we try this with many points, then the fraction of points that fall
; in the region should give an estimate of the proportion of the rectangle that lies in the
; region. Hence, multiplying this fraction by the area of the entire rectangle should produce
; an estimate of the integral.

; Implement Monte Carlo integration as a procedure estimate-integral that takes as arguments
; a predicate P, upper and lower bounds x1, x2, y1, and y2 for the rectangle, and the number
; of trials to perform in order to produce the estimate. Your procedure should use the same
; monte-carlo procedure that was used above to estimate . Use your estimate-integral to produce
; an estimate of  by measuring the area of a unit circle.

; You will find it useful to have a procedure that returns a number chosen at random from a
; given range. The following random-in-range procedure implements this in terms of the random
; procedure used in section 1.2.6, which returns a nonnegative number less than its input.8

; (define (random-in-range low high)
;   (let ((range (- high low)))
;     (+ low (random range))))

; S O L U T I O N

(define (estimate-integral P x1 x2 y1 y2 n-trials)
	
	(define (in-region-test)
		; test-point is a pair (x,y) where x and y are the coordinates of a randomly chosen
		; point in the rectangular region
		(let ((test-point-x (random-in-range x1 x2)) (test-point-y (random-in-range y1 y2)))
			(P test-point-x test-point-y)
		)
	)

	; Multiply the fraction returned by the monte-carlo procedure by the area of the entire
	; rectangle to produce an estimate of the integral
	(* (monte-carlo n-trials in-region-test) (abs (* (- x2 x1) (- y2 y1))) 1.0)
)

(define (monte-carlo trials experiment)

	(define (iter trials-remaining trials-passed)
		(cond
			((= trials-remaining 0) (/ trials-passed trials))
			((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
			(else
				(iter (- trials-remaining 1) trials-passed)
			)
		)
	)

	(iter trials 0)
)

(define (random-in-range low high)
	(let ((range (- high low)))
		; (+ low (random range))
		; returns decimal numbers in the range
		(+ low (* (random) range))
	)
)

(define (make-account balance password)

	(define (withdraw amount)
		(if (>= balance amount)
			(begin
				(set! balance (- balance amount))
				balance
			)
			"Insufficient funds"
		)
	)

	(define (deposit amount)
		(set! balance (+ balance amount))
		balance
	)

	(define (proc-incorrect-password x)
		"Incorrect password"
	)

	(define (call-the-cops)
		(display "*** Called the cops ***")
		(newline)
	)


	(let ((number-of-access-attempts 0) (account-password password))
		(lambda (p m)
			(if (eq? p account-password)
				(cond
					((eq? m 'withdraw) withdraw)
					((eq? m 'deposit) deposit)
					(else
						(error "Unknown request -- MAKE-ACCOUNT" m)
					)
				)
				(begin
					(set! number-of-access-attempts (+ number-of-access-attempts 1))
					(if (> number-of-access-attempts 7)
						(call-the-cops)
						(void)
					)
					proc-incorrect-password
				)
			)
		)
	)
)

(define (make-monitored f)
	(let ((counter 0))
		(lambda (message)
			(cond
				((eq? message 'how-many-calls?) counter)
				((eq? message 'reset-count) (set! counter 0))
				(else
					(set! counter (+ counter 1))
					(f message)
				)
			)
		)
	)
)

(define (make-accumulator sum)
	(lambda (value)
		(set! sum (+ sum value))
		sum
	)
)

; Test Driver

(define (run-test proc . args)

	(define (print-item-list items first-time?)
		(cond
			((not (pair? items)) (void))
			(else
				(if (not first-time?)
					(display ", ")
					(void)
				)
				(print (car items))
				(print-item-list (cdr items) false)
			)
		)
	)

	; (display "Running Test: ") (display (cons proc args)) (display " ")
	; (newline)
	(display "Applying ")
	(display proc)
	(display " on: ")
	(print-item-list args true)
	(newline)
	(let ((result (apply proc args)))
		(display "Result: ")
		(display result)
		(newline)
		(print result)
		(newline)
	)
	(newline)
)

; Tests

(define (P1 x y)
	; circle with radius of 1 and centered at (0,0)
	(<= (+ (* x x) (* y y)) 1.0)
)

(run-test estimate-integral P1 -1.0 1.0 -1.0 1.0 10)
(run-test estimate-integral P1 -1.0 1.0 -1.0 1.0 100)
(run-test estimate-integral P1 -1.0 1.0 -1.0 1.0 1000)
(run-test estimate-integral P1 -1.0 1.0 -1.0 1.0 10000)
(run-test estimate-integral P1 -1.0 1.0 -1.0 1.0 100000)
(run-test estimate-integral P1 -1.0 1.0 -1.0 1.0 1000000)
(run-test estimate-integral P1 -1.0 1.0 -1.0 1.0 10000000)
(run-test estimate-integral P1 -1.0 1 -1 1 100000000)
; (run-test estimate-integral P1 -1 1 -1 1 1000000000)
; (run-test estimate-integral P1 -1 1 -1 1 10000000000)
; (run-test estimate-integral P1 -1 1 -1 1 100000000000)
; (run-test estimate-integral P1 -1 1 -1 1 1000000000000)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Applying #<procedure:estimate-integral> on: #<procedure:P1>, -1.0, 1.0, -1.0, 1.0, 10
Result: 2.8
2.8

Applying #<procedure:estimate-integral> on: #<procedure:P1>, -1.0, 1.0, -1.0, 1.0, 100
Result: 2.96
2.96

Applying #<procedure:estimate-integral> on: #<procedure:P1>, -1.0, 1.0, -1.0, 1.0, 1000
Result: 3.132
3.132

Applying #<procedure:estimate-integral> on: #<procedure:P1>, -1.0, 1.0, -1.0, 1.0, 10000
Result: 3.1588
3.1588

Applying #<procedure:estimate-integral> on: #<procedure:P1>, -1.0, 1.0, -1.0, 1.0, 100000
Result: 3.13804
3.13804

Applying #<procedure:estimate-integral> on: #<procedure:P1>, -1.0, 1.0, -1.0, 1.0, 1000000
Result: 3.141652
3.141652

Applying #<procedure:estimate-integral> on: #<procedure:P1>, -1.0, 1.0, -1.0, 1.0, 10000000
Result: 3.142262
3.142262

Applying #<procedure:estimate-integral> on: #<procedure:P1>, -1.0, 1, -1, 1, 100000000
Result: 3.14171168
3.14171168

> pi
3.141592653589793
> 
