#lang racket

; Exercise 3.19. Redo exercise 3.18 using an algorithm that takes only a constant amount
; of space. (This requires a very clever idea.)

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; I use the definition of an interative process from section 1.2.1. here:
; Scheme will execute an iterative process in constant space, even if the iterative process is
; described by a recursive procedure. An implementation with this property is called
; tail-recursive. With a tail-recursive implementation, iteration can be expressed using the
; ordinary procedure call mechanism.
; In general, an iterative process is one whose state can be summarized by a fixed number of
; state variables, together with a fixed rule that describes how the state variables should be
; updated as the process moves from state to state and an (optional) end test that specifies
; conditions under which the process should terminate. The program variables provide a complete
; description of the state of the process at any point. If we stopped the computation between
; steps, all we would need to do to resume the computation is to supply the interpreter with
; the values of the program variables. An interpreter need keep track of only these variables
; in order to execute the process.
; See below that there are no deferred evaluations that are typical of a recursive process.
; Deferred evaluations result in continuously increasing space usage since the interpreter
; needs to keep track of operations to be performed later on.
;
; Additionally, I am doing away with the 'visited-pairs' list altogether and using the 
; hare-and-tortoise algorithm in which one pointer advances one step at a time and the other
; pointer advances two steps at a time.

; Returns true if there is a cycle in the list, false if there is no cycle
(define (contains-cycle? list)
	
	; Note: A 'cycle' means only one thing - successive cdrs would go into an
	; infinite loop

	; t and h are two pointers (tortoise and hare)
	(define (contains-cycle?-internal t h)

		; (display "Tortoise is at: ")
		; (display t)
		; (newline)
		; (display "Hare is at: ")
		; (display h)
		; (newline)

		; The idea here is that if there is a cycle, then the hare will catch up with the
		; tortoise in the cycle. If there is no cycle, the hare will reach the end of the
		; list before the tortoise

		(cond
			; tortoise can be null only if the original list itself is null
			((null? t) false)
			; hare reached the end of the list, so there is no cycle
			((null? h) false)
			; hare caught up with the tortoise, so there is a cycle
			((eq? h t) true)
			(else
				; h and t are non-null and unequal, so advance
				(contains-cycle?-internal (safe-cdr t) (safe-cdr (safe-cdr h)))
			)
		)
	)

	(define (safe-cdr x)
		(if (mpair? x)
			(mcdr x)
			null
		)
	)

	(contains-cycle?-internal list (safe-cdr list))
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
> (display M)
#0={u v w . #1={{x y z . #1#} . #0#}}
> (contains-cycle? M)
#t
> (set-mcdr! S '())
> (display M)
#0={u v w {x y z} . #0#}
> (contains-cycle? M)
#t
> (set-mcdr! P Q)
> (display M)
{u v w {x y z} x y z}
> (contains-cycle? M)
#f
> 
