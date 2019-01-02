#lang racket

; Exercise 3.17.  Devise a correct version of the count-pairs procedure of exercise 3.16
; that returns the number of distinct pairs in any structure. (Hint: Traverse the structure,
; maintaining an auxiliary data structure that is used to keep track of which pairs have
; already been counted.)

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (count-mpairs structure)

	; List data structure to keep track of which pairs have already been visited
	; during the counting process
	(define visited-pairs '())

	(define (count-mpairs-internal x)
		(if (not (mpair? x))
			0
			(begin
				(if (visited? x)
					; do not double-count
					0
					(begin
						(record-mpair x)
						(+
							(count-mpairs-internal (mcar x))
							(count-mpairs-internal (mcdr x))
							1
						)
					)
				)
			)
		)
	)

	(define (record-mpair x)
		; add x to the visited-pair list
		(if (null? visited-pairs)
			(set! visited-pairs (mcons x '()))
			; insert the new pair at front of the list
			; so the insertion is fast
			(set! visited-pairs (mcons x visited-pairs))
		)
	)

	; This procedure looks for the supplied pair in the visited-pairs list
	; and if it finds x, it returns true. Otherwise it returns false.
	(define (visited? x)
		(define (present-in-list? l i)
			(if (null? l)
				false
				(if (eq? (mcar l) i)
					true
					(present-in-list? (mcdr l) i)
				)
			)
		)

		(present-in-list? visited-pairs x)
	)

	(count-mpairs-internal structure)
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
(define A (mcons 'u 'v))
(define B (mcons 'w 'x))
(define C (mcons 'y 'z))

(define M (mcons 'u 'b))
(define N (mcons 'v 'c))
(define O (mcons 'w 'd))
(define P (mcons 'd 'e))
(define Q (mcons 'x 'f))
(define R (mcons 'y 'g))
(define S (mcons 'z 'h))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (set-mcdr! A B)
> (set-mcdr! B C)
> (count-mpairs A)
3
> (set-mcar! B C)
> (count-mpairs A)
3
> (set-mcar! A B)
> (count-mpairs A)
3
> (set-mcdr! A B)
> (set-mcdr! B C)
> (set-mcdr! C A)
> (set-mcar! A 'u)
> (set-mcar! B 'w)
> (set-mcar! C 'y)
> (count-mpairs A)
3
> (set-mcdr! M N)
(set-mcdr! N O)
(set-mcdr! O P)
(set-mcdr! P M)
(set-mcar! P Q)
(set-mcdr! Q R)
(set-mcdr! R S)
(set-mcdr! S M)
(count-mpairs M)
7
>
