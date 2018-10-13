#lang racket

; Exercise 3.4.  Modify the make-account procedure of exercise 3.3 by adding another local state
; variable so that, if an account is accessed more than seven consecutive times with an incorrect
; password, it invokes the procedure call-the-cops.

; S O L U T I O N

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

	(display "Running Test: ") (display (cons proc args)) (display " ")
	(newline)
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

(define acc (make-account 100 'secret-password))
(run-test (acc 'secret-password 'withdraw) 30)
(run-test (acc 'secret-password 'withdraw) 30)
(run-test (acc 'secret-password 'withdraw) 30)
(run-test (acc 'secret-password 'withdraw) 30)
(run-test (acc 'some-other-password 'deposit) 50)
(run-test (acc 'secret-password 'deposit) 50)
(run-test (acc 'secret-password 'deposit) 50)
(run-test (acc 'secret-password 'deposit) 50)
(run-test (acc 'secret-password 'deposit) 50)
(run-test (acc 'abc 'withdraw) 80)
(run-test (acc 'secret-password 'deposit) 90)
(run-test (acc 'secret-password 'withdraw) 60)
(run-test (acc 'secret-password 'withdraw) 60)
(run-test (acc 'secret-password 'withdraw) 10)
(run-test (acc 'some-other-password 'withdraw) 10)
(run-test (acc 'some-other-password 'withdraw) 10)
(run-test (acc 'some-other-password 'withdraw) 10)
(run-test (acc 'some-other-password 'withdraw) 10)
(run-test (acc 'some-other-password 'withdraw) 10)
(run-test (acc 'some-other-password 'withdraw) 10)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Running Test: (#<procedure:withdraw> 30) 
Applying #<procedure:withdraw> on: 30
Result: 70
70

Running Test: (#<procedure:withdraw> 30) 
Applying #<procedure:withdraw> on: 30
Result: 40
40

Running Test: (#<procedure:withdraw> 30) 
Applying #<procedure:withdraw> on: 30
Result: 10
10

Running Test: (#<procedure:withdraw> 30) 
Applying #<procedure:withdraw> on: 30
Result: Insufficient funds
"Insufficient funds"

Running Test: (#<procedure:proc-incorrect-password> 50) 
Applying #<procedure:proc-incorrect-password> on: 50
Result: Incorrect password
"Incorrect password"

Running Test: (#<procedure:deposit> 50) 
Applying #<procedure:deposit> on: 50
Result: 60
60

Running Test: (#<procedure:deposit> 50) 
Applying #<procedure:deposit> on: 50
Result: 110
110

Running Test: (#<procedure:deposit> 50) 
Applying #<procedure:deposit> on: 50
Result: 160
160

Running Test: (#<procedure:deposit> 50) 
Applying #<procedure:deposit> on: 50
Result: 210
210

Running Test: (#<procedure:proc-incorrect-password> 80) 
Applying #<procedure:proc-incorrect-password> on: 80
Result: Incorrect password
"Incorrect password"

Running Test: (#<procedure:deposit> 90) 
Applying #<procedure:deposit> on: 90
Result: 300
300

Running Test: (#<procedure:withdraw> 60) 
Applying #<procedure:withdraw> on: 60
Result: 240
240

Running Test: (#<procedure:withdraw> 60) 
Applying #<procedure:withdraw> on: 60
Result: 180
180

Running Test: (#<procedure:withdraw> 10) 
Applying #<procedure:withdraw> on: 10
Result: 170
170

Running Test: (#<procedure:proc-incorrect-password> 10) 
Applying #<procedure:proc-incorrect-password> on: 10
Result: Incorrect password
"Incorrect password"

Running Test: (#<procedure:proc-incorrect-password> 10) 
Applying #<procedure:proc-incorrect-password> on: 10
Result: Incorrect password
"Incorrect password"

Running Test: (#<procedure:proc-incorrect-password> 10) 
Applying #<procedure:proc-incorrect-password> on: 10
Result: Incorrect password
"Incorrect password"

Running Test: (#<procedure:proc-incorrect-password> 10) 
Applying #<procedure:proc-incorrect-password> on: 10
Result: Incorrect password
"Incorrect password"

Running Test: (#<procedure:proc-incorrect-password> 10) 
Applying #<procedure:proc-incorrect-password> on: 10
Result: Incorrect password
"Incorrect password"

*** Called the cops ***
Running Test: (#<procedure:proc-incorrect-password> 10) 
Applying #<procedure:proc-incorrect-password> on: 10
Result: Incorrect password
"Incorrect password"

> 
