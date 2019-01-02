#lang racket

; Exercise 3.18.  Write a procedure that examines a list and determines whether it contains
; a cycle, that is, whether a program that tried to find the end of the list by taking
; successive cdrs would go into an infinite loop. Exercise 3.13 constructed such lists.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; Note: This program expects an ordinary linked list as in exercise 3.13. If given a
; more complex structure with multiple cycles, it will miss detecting cycles that don't
; lie on the 'main' path. The 'main' path is the path traversed by successive cdrs from the
; root pair. I deliberately kept it simple.
; I reasoned that if we want to write a more generic cycle detector that can find all
; cycles in a complex structure, then this program can be used as a component there. This
; will allow us to keep modularity in our design.

; Returns true if there is a cycle in the list, false if there is no cycle
(define (contains-cycle? list)
	
	; Note: A 'cycle' means only one thing - successive cdrs would go into an
	; infinite loop

	; List data structure to keep track of which pairs have already been visited
	; during the counting process
	(define visited-pairs '())

	(define (contains-cycle?-internal p)
		(if (not (mpair? p))
			false
			(if (visited? p)
				true
				(begin
					(record-mpair p)
					(contains-cycle?-internal (mcdr p))
				)
			)
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

	(define (record-mpair x)
		; add x to the visited-pair list
		(if (null? visited-pairs)
			(set! visited-pairs (mcons x '()))
			; insert the new pair at front of the list
			; so the insertion is fast
			(set! visited-pairs (mcons x visited-pairs))
		)
	)

	(contains-cycle?-internal list)
)

(define (make-cycle x)
	(set-mcdr! (last-pair x) x)
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

(define non-cyclic-list (mlist 'a 'b 'c 'd 'e 'f 'g))
(define cyclic-list (make-cycle (mlist 'a 'b 'c 'd 'e 'f 'g 'h)))

(define M (mcons 'u 'b))
(define N (mcons 'v 'c))
(define O (mcons 'w 'd))
(define P (mcons 'd 'e))
(define Q (mcons 'x 'f))
(define R (mcons 'y 'g))
(define S (mcons 'z 'h))

(set-mcdr! M N)
(set-mcdr! N O)
(set-mcdr! O P)
(set-mcdr! P M)
(set-mcar! P Q)
(set-mcdr! Q R)
(set-mcdr! R S)
(set-mcdr! S P)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (contains-cycle? non-cyclic-list)
#f
> (contains-cycle? cyclic-list)
#t
> (contains-cycle? M)
#t
> (set-mcdr! S '())
> (contains-cycle? M)
#t
> (set-mcdr! P Q)
> (contains-cycle? M)
#f
> 
