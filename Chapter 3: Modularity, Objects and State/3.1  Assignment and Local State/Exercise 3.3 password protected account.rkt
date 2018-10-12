#lang racket

; Exercise 3.3.  Modify the make-account procedure so that it creates password-protected accounts.
; That is, make-account should take a symbol as an additional argument, as in

; (define acc (make-account 100 'secret-password))

; The resulting account object should process a request only if it is accompanied by the
; password with which the account was created, and should otherwise return a complaint:

; ((acc 'secret-password 'withdraw) 40)
; 60

; ((acc 'some-other-password 'deposit) 50)
; "Incorrect password"

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

	(define (dispatch p m)
		(let ((account-password password))
			(if (eq? p account-password)
				(cond
					((eq? m 'withdraw) withdraw)
					((eq? m 'deposit) deposit)
					(else
						(error "Unknown request -- MAKE-ACCOUNT" m)
					)
				)
				proc-incorrect-password
			)
		)
	)

	dispatch
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

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define acc (make-account 100 'secret-password))
> ((acc 'secret-password 'withdraw) 30)
70
> ((acc 'secret-password 'withdraw) 30)
40
> ((acc 'secret-password 'withdraw) 30)
10
> ((acc 'secret-password 'withdraw) 30)
"Insufficient funds"
> ((acc 'some-other-password 'deposit) 50)
"Incorrect password"
> ((acc 'secret-password 'deposit) 50)
60
> ((acc 'secret-password 'deposit) 50)
110
> ((acc 'secret-password 'deposit) 50)
160
> ((acc 'secret-password 'deposit) 50)
210
> ((acc 'abc 'withdraw) 80)
"Incorrect password"
> ((acc 'secret-password 'deposit) 90)
300
> ((acc 'secret-password 'withdraw) 60)
240
> 
