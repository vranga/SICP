#lang racket

; Exercise 4.15.  Given a one-argument procedure p and an object a, p is said to "halt" on a
; if evaluating the expression (p a) returns a value (as opposed to terminating with an error
; message or running forever). Show that it is impossible to write a procedure halts? that
; correctly determines whether p halts on a for any procedure p and object a. Use the following
; reasoning: If you had such a procedure halts?, you could implement the following program:

; (define (run-forever) (run-forever))

; (define (try p)
;   (if (halts? p p)
;       (run-forever)
;       'halted))

; Now consider evaluating the expression (try try) and show that any possible outcome (either
; halting or running forever) violates the intended behavior of halts?

; R E A S O N I N G

The following is a simple zero-argument procedure that runs forever

(define (run-forever) (run-forever))

The following is a hypothetical procedure that checks if procedure p halts on input a
(define (halts? p a)
	(if <p halts on a>
		true
		false
	)
)

Assuming that the "halts?" procedure above exists and works, we should be able to write 
another procedure:

(define (try p)
	(if (halts? p p)
		(run-forever)
		'halted
	)
)

The "try" procedure takes a procedure as its argument and checks if that procedure halts
on itself. If true, then try runs forever. If false, then try returns.

If p halts on itself, then try runs forever.
If p runs forever, then try halts on p.

So "try" behaves oppositely to the procedure supplied. 

Now if we evaluate (try try), then the logic would work like this:

If try halts on itself, try runs forever.
If try runs forever, try halts on itself.

In both cases, the behaviour of "try" violates the intended behavior of "halts?"
