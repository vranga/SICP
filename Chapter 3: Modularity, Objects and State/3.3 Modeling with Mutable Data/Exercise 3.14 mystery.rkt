#lang racket

; Exercise 3.14.  The following procedure is quite useful, although obscure:

; (define (mystery x)
;   (define (loop x y)
;     (if (null? x)
;         y
;         (let ((temp (cdr x)))
;           (set-cdr! x y)
;           (loop temp x))))
;   (loop x '()))

; Loop uses the ``temporary'' variable temp to hold the old value of the cdr of x,
; since the set-cdr! on the next line destroys the cdr. Explain what mystery does in general.
; Suppose v is defined by (define v (list 'a 'b 'c 'd)). Draw the box-and-pointer diagram that
; represents the list to which v is bound. Suppose that we now evaluate (define w (mystery v)).
; Draw box-and-pointer diagrams that show the structures v and w after evaluating this
; expression. What would be printed as the values of v and w ?

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (mystery x)
	(define (loop x y)
		(if (null? x)
			y
			(let ((temp (mcdr x)))
				(set-mcdr! x y)
				(loop temp x)
			)
		)
	)

	(loop x '())
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

(define v (mlist 'a 'b 'c 'd))
(define w (mystery v))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (display v)
{a}
> (display w)
{d c b a}
> 
