#lang racket

; Exercise 3.7.  Consider the bank account objects created by make-account, with the password modification
; described in exercise 3.3. Suppose that our banking system requires the ability to make joint accounts.
; Define a procedure make-joint that accomplishes this. Make-joint should take three arguments.
; The first is a password-protected account. The second argument must match the password with which the
; account was defined in order for the make-joint operation to proceed. The third argument is a new password.
; Make-joint is to create an additional access to the original account using the new password.
; For example, if peter-acc is a bank account with password open-sesame, then

; (define paul-acc
;  (make-joint peter-acc 'open-sesame 'rosebud))

; will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud.
; You may wish to modify your solution to exercise 3.3 to accommodate this new feature.

; S O L U T I O N

(define (make-joint account account-password new-password)
	(if (account account-password 'check-password)
		(lambda (p m)
			(if (eq? p new-password)
				; since the supplied password matches the joint account password, pass on 
				; the request to the base account
				(account account-password m)
				; since the supplied password does not match the joint account password, pass on 
				; the request to the base account (but ensure that you supply the wrong password)
				; This way, all the safety measures like call-the-cops etc. continue to apply and
				; are not bypassed. I create a wrong password by appending 'a to the correct
				; password
				(account (apply string-append (map symbol->string (list account-password 'a))) m)
			)
		)
		(error "Cannot make joint account -- Wrong Password")
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
			(if (eq? m 'check-password)
				; the request is to check if the password is correct
				(eq? p account-password)
				; the request is for some other action
				(if (eq? p account-password)
					(cond
						((eq? m 'withdraw) withdraw)
						((eq? m 'deposit) deposit)
						((eq? m 'balance) balance)
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

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.

> (define acc (make-account 1000 'tamasha))
> ((acc 'tamasha 'withdraw) 30)
970
> ((acc 'tamasha 'withdraw) 30)
940
> ((acc 'tamasha 'deposit) 10)
950
> ((acc 'tamasha 'deposit) 5)
955
> (define joint-acc (make-joint acc 'tamasha 'natasha))
> (acc 'tamasha 'balance)
955
> (acc 'galata 'balance)
#<procedure:proc-incorrect-password>
> (acc 'tamasha 'balance)
955
> (acc 'tamasha 'balance)
955
> ((joint-acc 'natasha 'withdraw) 30)
925
> ((acc 'tamasha 'withdraw) 30)
895
> ((joint-acc 'natasha 'withdraw) 30)
865
> ((joint-acc 'natasha 'deposit) 300)
1165
> ((acc 'tamasha 'withdraw) 65)
1100
> (acc 'tamasha 'balance)
1100
> (joint-acc 'natasha 'balance)
1100
> ((acc 'tamasha 'withdraw) 65)
1035
> ((joint-acc 'natasha 'withdraw) 65)
970
> ((acc 'tamasha 'withdraw) 65)
905
> ((joint-acc 'natasha 'withdraw) 65)
840
> ((joint-acc 'abc 'withdraw) 65)
"Incorrect password"
> ((joint-acc 'abc 'withdraw) 65)
"Incorrect password"
> ((acc 'abc 'withdraw) 65)
"Incorrect password"
> ((joint-acc 'abc 'withdraw) 65)
"Incorrect password"
> ((acc 'abc 'withdraw) 65)
"Incorrect password"
> ((joint-acc 'abc 'withdraw) 65)
"Incorrect password"
> ((acc 'abc 'withdraw) 65)
*** Called the cops ***
"Incorrect password"
> 
