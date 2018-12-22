#lang racket
	
; Exercise 3.12.  The following procedure for appending lists was introduced in section 2.2.1:

; (define (append x y)
;  (if (null? x)
;      y
;      (cons (car x) (append (cdr x) y))))

; Append forms a new list by successively consing the elements of x onto y. The procedure append!
; is similar to append, but it is a mutator rather than a constructor. It appends the lists by
; splicing them together, modifying the final pair of x so that its cdr is now y.
; (It is an error to call append! with an empty x.)

; (define (append! x y)
;   (set-cdr! (last-pair x) y)
;   x)

; Here last-pair is a procedure that returns the last pair in its argument:

; (define (last-pair x)
;   (if (null? (cdr x))
;       x
;       (last-pair (cdr x))))

; Consider the interaction

; (define x (list 'a 'b))
; (define y (list 'c 'd))
; (define z (append x y))
; z
; (a b c d)
; (cdr x)
; <response>
; (define w (append! x y))
; w
; (a b c d)
; (cdr x)
; <response>

; What are the missing <response>s? Draw box-and-pointer diagrams to explain your answer.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (append x y)
	(if (null? x)
		y
		(mcons (mcar x) (append (mcdr x) y))
	)
)

(define (append! x y)
	(set-mcdr! (last-pair x) y)
	x
)

(define (last-pair x)
	(if (null? (mcdr x))
		x
		(last-pair (mcdr x))
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
> (define x (mlist 'a 'b))
> (define y (mlist 'c 'd))
> (define z (append x y))
> (display z)
{a b c d}
> (display (mcdr x))
{b}
> (define w (append! x y))
> (display w)
{a b c d}
> (display (mcdr x))
{b c d}
> 
