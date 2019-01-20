#lang racket

; Exercise 3.23.  A deque ("double-ended queue") is a sequence in which items can be inserted and
; deleted at either the front or the rear. Operations on deques are the constructor make-deque,
; the predicate empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!,
; rear-insert-deque!, front-delete-deque!, and rear-delete-deque!. Show how to represent deques
; using pairs, and give implementations of the operations. All operations should be accomplished
; in Theta(1) steps.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (make-deque)
	(mcons null null)
)

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
	(if (empty-deque? deque)
		(error "FRONT-DEQUE called with an empty deque" deque)
		(mcar (mcdr (mcdr (front-ptr deque))))
	)
)

(define (rear-deque deque)
	(if (empty-deque? deque)
		(error "REAR-DEQUE called with an empty deque" deque)
		(mcar (mcdr (mcdr (rear-ptr deque))))
	)
)

(define (front-insert-deque! deque item)
	(let ((new-deque-unit (make-deque-unit null null item)))
		(cond
			((empty-deque? deque)
				(set-front-ptr! deque new-deque-unit)
				(set-rear-ptr! deque new-deque-unit)
			)
			(else
				; Since the new element is inserted in the front of the deque, its prev-ptr
				; should be set to null
				(set-prev-ptr! new-deque-unit null)
				; The next-ptr of the new element should be the first element in the deque. With this,
				; the first element becomes the second element in the deque
				(set-next-ptr! new-deque-unit (front-ptr deque))
				; The prev-ptr of the 2nd element should point to the newly inserted element
				(set-prev-ptr! (front-ptr deque) new-deque-unit)
				; The front-ptr of the deque shifts left one step
				(set-front-ptr! deque new-deque-unit)
			)
		)
	)
)

(define (rear-insert-deque! deque item)
	(let ((new-deque-unit (make-deque-unit null null item)))
		(cond
			((empty-deque? deque)
				(set-front-ptr! deque new-deque-unit)
				(set-rear-ptr! deque new-deque-unit)
			)
			(else
				; Since the new element is inserted at the rear of the deque, its next-ptr
				; should be set to null
				(set-next-ptr! new-deque-unit null)
				; The prev-ptr of the new element should be the last element in the deque. With this,
				; the last element becomes the second last element in the deque
				(set-prev-ptr! new-deque-unit (rear-ptr deque))
				; The next-ptr of the 2nd last element should point to the newly inserted element
				(set-next-ptr! (rear-ptr deque) new-deque-unit)
				; The rear-ptr of the deque shifts right one step
				(set-rear-ptr! deque new-deque-unit)
			)
		)
	)
)

(define (front-delete-deque! deque)
	(cond
		((empty-deque? deque) (error "FRONT-DELETE! called with an empty deque" deque))
		(else
			; Since we are removing an item from the front, the 2nd element's prev-ptr should be
			; set to null
			(set-prev-ptr! (next-ptr (front-ptr deque)) null)
			; The front-ptr of the deque shifts right one step
			(set-front-ptr! deque (next-ptr (front-ptr deque)))
			deque
		)
	)
)

(define (rear-delete-deque! deque)
	(cond
		((empty-deque? deque) (error "REAR-DELETE! called with an empty deque" deque))
		(else
			; Since we are removing an item from the rear, the 2nd last element's next-ptr should be
			; set to null
			(set-next-ptr! (prev-ptr (rear-ptr deque)) null)
			; The rear-ptr of the deque shifts left one step
			(set-rear-ptr! deque (prev-ptr (rear-ptr deque)))
			deque
		)
	)
)

(define (front-ptr deque) (mcar deque))
(define (rear-ptr deque) (mcdr deque))

(define (set-front-ptr! deque deque-unit)
	(set-mcar! deque deque-unit)
)

(define (set-rear-ptr! deque deque-unit)
	(set-mcdr! deque deque-unit)
)

(define (set-prev-ptr! deque-unit x)
	(set-mcar! deque-unit x)
)

(define (set-next-ptr! deque-unit x)
	(set-mcar! (mcdr deque-unit) x)
)

(define (next-ptr deque-unit)
	(mcar (mcdr deque-unit))
)

(define (prev-ptr deque-unit)
	(mcar deque-unit)
)

(define (make-deque-unit prev-ptr next-ptr data)
	(mlist prev-ptr next-ptr data)
)

(define (print-deque deque)

	(define (print-deque-internal deque-unit)
		(if (null? deque-unit)
			(void)
			(begin
				(display (mcar (mcdr (mcdr deque-unit))))
				(display " ")
				(print-deque-internal (next-ptr deque-unit))
			)
		)
	)

	(if (contains-cycle? deque)
		(error "Deque contains cycle. Not printing: " deque)
		(begin
			(display "{ ")
			(print-deque-internal (mcar deque))
			(display "}")
			(newline)
		)
	)
)

(define (contains-cycle? deque)
	
	; Note: A 'cycle' means only one thing - stepping through the deque would go into an
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
				(contains-cycle?-internal (safe-next t) (safe-next (safe-next h)))
			)
		)
	)

	(define (safe-next deque-unit)
		(if (null? deque-unit)
			null
			(next-ptr deque-unit)
		)
	)

	(if (not (null? deque))
		(contains-cycle?-internal (front-ptr deque) (safe-next (front-ptr deque)))
		(error "Null value passed to contains-cycle: " deque)
	)
)

; Test Driver

(define (run-test return-type proc . args)

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

	(display "Applying ")
	(display proc)
	(display " on: ")
	(print-item-list args true)
	(newline)
	(let ((result (apply proc args)))
		(if (not (eq? return-type 'none))
			(display "Result: ")
			(void)
		)
		(cond
			((procedure? result) ((result 'print)))
			((eq? return-type 'deque) (print-deque result))
			((eq? return-type 'none) (void))
			(else
				(print result)
				(newline)
			)
		)
	)
	(newline)
)

(define (growth-test deque limit)
	(define (growth-test-internal deque down-counter)
		(if (eq? down-counter 0)
			(void)
			; Insert 2 elements in the front, 2 elements in the back
			(begin
				(front-insert-deque! deque (random))
				(rear-insert-deque! deque (random))
				; Then remove 1 element from the front and 1 element from the back
				; Call the selectors 
				; continue the process
				(growth-test-internal deque (- down-counter 1))
			)
		)
	)

	(growth-test-internal deque limit)
)

(define (execution-time proc . args)
	(define start-time (current-milliseconds))
	; (display start-time)
	; (display " ")
	(apply proc args)
	(define end-time (current-milliseconds))
	; (display end-time) 
	(display "Execution time of ")
	(display proc)
	(display ": ")
	(- end-time start-time)
)

; Tests

(define D (make-deque))
(run-test 'none print-deque D)
(run-test 'boolean empty-deque? D)
(run-test 'none front-insert-deque! D 'Mercury)
(run-test 'none front-insert-deque! D 'Venus)
(run-test 'none print-deque D)
(run-test 'unknown front-deque D)
(run-test 'unknown rear-deque D)
(run-test 'none rear-insert-deque! D 'Earth)
(run-test 'none print-deque D)
(run-test 'unknown front-deque D)
(run-test 'unknown rear-deque D)
(run-test 'none rear-insert-deque! D 'Mars)
(run-test 'none rear-insert-deque! D 'Jupiter)
(run-test 'none rear-insert-deque! D 'Saturn)
(run-test 'none print-deque D)
(run-test 'deque front-delete-deque! D)
(run-test 'none print-deque D)
(run-test 'deque front-delete-deque! D)
(run-test 'none print-deque D)
(run-test 'none rear-insert-deque! D 'Uranus)
(run-test 'none rear-insert-deque! D 'Neptune)
(run-test 'none print-deque D)
(run-test 'none front-insert-deque! D 'Venus)
(run-test 'none front-insert-deque! D 'Mercury)
(run-test 'none print-deque D)
(run-test 'none rear-insert-deque! D 'Pluto)
(run-test 'none print-deque D)
(run-test 'deque rear-delete-deque! D)
(run-test 'none print-deque D)

(execution-time print-deque D)
(execution-time front-insert-deque! D 'Sirius)
(execution-time rear-insert-deque! D 'Sirius)
(execution-time front-delete-deque! D)
(execution-time rear-delete-deque! D)
(execution-time empty-deque? D)
(execution-time front-deque D)
(execution-time rear-deque D)

(growth-test D 1000)

(execution-time print-deque D)
(execution-time front-deque D)
(execution-time rear-deque D)
(execution-time empty-deque? D)
(execution-time rear-insert-deque! D 'Sirius)
(execution-time front-insert-deque! D 'Sirius)
(execution-time front-delete-deque! D)
(execution-time rear-delete-deque! D)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Applying #<procedure:print-deque> on: (mcons '() '())
{ }

Applying #<procedure:empty-deque?> on: (mcons '() '())
Result: #t

Applying #<procedure:front-insert-deque!> on: (mcons '() '()), 'Mercury

Applying #<procedure:front-insert-deque!> on: (mcons (mcons '() (mcons '() (mcons 'Mercury '()))) (mcons '() (mcons '() (mcons 'Mercury '())))), 'Venus

Applying #<procedure:print-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons '() (mcons 'Mercury '()))) (mcons 'Venus '()))) #1#)
{ Venus Mercury }

Applying #<procedure:front-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons '() (mcons 'Mercury '()))) (mcons 'Venus '()))) #1#)
Result: 'Venus

Applying #<procedure:rear-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons '() (mcons 'Mercury '()))) (mcons 'Venus '()))) #1#)
Result: 'Mercury

Applying #<procedure:rear-insert-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons '() (mcons 'Mercury '()))) (mcons 'Venus '()))) #1#), 'Earth

Applying #<procedure:print-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons '() (mcons 'Earth '()))) (mcons 'Mercury '()))) (mcons 'Venus '()))) #2#)
{ Venus Mercury Earth }

Applying #<procedure:front-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons '() (mcons 'Earth '()))) (mcons 'Mercury '()))) (mcons 'Venus '()))) #2#)
Result: 'Venus

Applying #<procedure:rear-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons '() (mcons 'Earth '()))) (mcons 'Mercury '()))) (mcons 'Venus '()))) #2#)
Result: 'Earth

Applying #<procedure:rear-insert-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons '() (mcons 'Earth '()))) (mcons 'Mercury '()))) (mcons 'Venus '()))) #2#), 'Mars

Applying #<procedure:rear-insert-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons '() (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Mercury '()))) (mcons 'Venus '()))) #3#), 'Jupiter

Applying #<procedure:rear-insert-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons '() (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Mercury '()))) (mcons 'Venus '()))) #4#), 'Saturn

Applying #<procedure:print-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons '() (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Mercury '()))) (mcons 'Venus '()))) #5#)
{ Venus Mercury Earth Mars Jupiter Saturn }

Applying #<procedure:front-delete-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons '() (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Mercury '()))) (mcons 'Venus '()))) #5#)
Result: { Mercury Earth Mars Jupiter Saturn }

Applying #<procedure:print-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons '() (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Mercury '()))) #4#)
{ Mercury Earth Mars Jupiter Saturn }

Applying #<procedure:front-delete-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons '() (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Mercury '()))) #4#)
Result: { Earth Mars Jupiter Saturn }

Applying #<procedure:print-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons '() (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) #3#)
{ Earth Mars Jupiter Saturn }

Applying #<procedure:rear-insert-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons '() (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) #3#), 'Uranus

Applying #<procedure:rear-insert-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons '() (mcons 'Uranus '()))) (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) #4#), 'Neptune

Applying #<procedure:print-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons '() (mcons 'Neptune '()))) (mcons 'Uranus '()))) (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) #5#)
{ Earth Mars Jupiter Saturn Uranus Neptune }

Applying #<procedure:front-insert-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons '() (mcons 'Neptune '()))) (mcons 'Uranus '()))) (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) #5#), 'Venus

Applying #<procedure:front-insert-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons #6=(mcons #5# (mcons '() (mcons 'Neptune '()))) (mcons 'Uranus '()))) (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Venus '()))) #6#), 'Mercury

Applying #<procedure:print-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons #6=(mcons #5# (mcons #7=(mcons #6# (mcons '() (mcons 'Neptune '()))) (mcons 'Uranus '()))) (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Venus '()))) (mcons 'Mercury '()))) #7#)
{ Mercury Venus Earth Mars Jupiter Saturn Uranus Neptune }

Applying #<procedure:rear-insert-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons #6=(mcons #5# (mcons #7=(mcons #6# (mcons '() (mcons 'Neptune '()))) (mcons 'Uranus '()))) (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Venus '()))) (mcons 'Mercury '()))) #7#), 'Pluto

Applying #<procedure:print-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons #6=(mcons #5# (mcons #7=(mcons #6# (mcons #8=(mcons #7# (mcons '() (mcons 'Pluto '()))) (mcons 'Neptune '()))) (mcons 'Uranus '()))) (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Venus '()))) (mcons 'Mercury '()))) #8#)
{ Mercury Venus Earth Mars Jupiter Saturn Uranus Neptune Pluto }

Applying #<procedure:rear-delete-deque!> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons #6=(mcons #5# (mcons #7=(mcons #6# (mcons #8=(mcons #7# (mcons '() (mcons 'Pluto '()))) (mcons 'Neptune '()))) (mcons 'Uranus '()))) (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Venus '()))) (mcons 'Mercury '()))) #8#)
Result: { Mercury Venus Earth Mars Jupiter Saturn Uranus Neptune }

Applying #<procedure:print-deque> on: (mcons #0=(mcons '() (mcons #1=(mcons #0# (mcons #2=(mcons #1# (mcons #3=(mcons #2# (mcons #4=(mcons #3# (mcons #5=(mcons #4# (mcons #6=(mcons #5# (mcons #7=(mcons #6# (mcons '() (mcons 'Neptune '()))) (mcons 'Uranus '()))) (mcons 'Saturn '()))) (mcons 'Jupiter '()))) (mcons 'Mars '()))) (mcons 'Earth '()))) (mcons 'Venus '()))) (mcons 'Mercury '()))) #7#)
{ Mercury Venus Earth Mars Jupiter Saturn Uranus Neptune }

{ Mercury Venus Earth Mars Jupiter Saturn Uranus Neptune }
Execution time of #<procedure:print-deque>: 0
Execution time of #<procedure:front-insert-deque!>: 0
Execution time of #<procedure:rear-insert-deque!>: 0
Execution time of #<procedure:front-delete-deque!>: 0
Execution time of #<procedure:rear-delete-deque!>: 0
Execution time of #<procedure:empty-deque?>: 0
Execution time of #<procedure:front-deque>: 0
Execution time of #<procedure:rear-deque>: 0

<Since the deque is large, I am not showing the print output removed>

Execution time of #<procedure:print-deque>: 201
Execution time of #<procedure:front-deque>: 0
Execution time of #<procedure:rear-deque>: 0
Execution time of #<procedure:empty-deque?>: 0
Execution time of #<procedure:rear-insert-deque!>: 0
Execution time of #<procedure:front-insert-deque!>: 0
Execution time of #<procedure:front-delete-deque!>: 0
Execution time of #<procedure:rear-delete-deque!>: 0
> 

CONCLUSION: The tests above show that all operations (except print-deque) are accomplished in theta(1)
steps. The first set of execution times is when the deque is small with just a handful of elements.
The second set is after running the 'growth-test' procedure and adding 2000 elements. We can see that
while the print-deque procedure takes a significantly longer time, the rest of the operations take
almost no time (just like when the deque was small).
