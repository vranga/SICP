#lang racket

; Exercise 3.22.  Instead of representing a queue as a pair of pointers, we can build a queue as
; a procedure with local state. The local state will consist of pointers to the beginning and the end
; of an ordinary list. Thus, the make-queue procedure will have the form

; (define (make-queue)
;   (let ((front-ptr ...)
;         (rear-ptr ...))
;     <definitions of internal procedures>
;     (define (dispatch m) ...)
;     dispatch))

; Complete the definition of make-queue and provide implementations of the queue operations
; using this representation.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (make-queue)
	(let ((front-ptr '()) (rear-ptr '()))

		; Definitions of internal procedures

		(define (empty-queue?)
			(null? front-ptr)
		)

		(define (insert-queue! item)
			(let ((new-pair (mcons item '())))
				(cond
					((empty-queue?)
						(set-front-ptr! new-pair)
						(set-rear-ptr! new-pair)
						dispatch
					)
					(else
						(set-mcdr! rear-ptr new-pair)
						(set-rear-ptr! new-pair)
						dispatch
					)
				)
			)
		)

		(define (delete-queue!)
			(cond
				((empty-queue?) (error "DELETE! called with an empty queue"))
				(else
					(set-front-ptr! (mcdr front-ptr))
					dispatch
				)
			)
		)

		(define (front-queue)
			(if (empty-queue?)
				(error "FRONT called with an empty queue")
				(mcar front-ptr)
			)
		)

		(define (set-front-ptr! item)
			(set! front-ptr item)
		)

		(define (set-rear-ptr! item)
			(set! rear-ptr item)
		)

		(define (print-queue)
			(display front-ptr)
		)

		(define (dispatch m)
			(cond
				((eq? m 'empty-queue?) empty-queue?)
				((eq? m 'insert-queue!) insert-queue!)
				((eq? m 'delete-queue!) delete-queue!)
				((eq? m 'front-queue) front-queue)
				((eq? m 'front-ptr) front-ptr)
				((eq? m 'rear-ptr) rear-ptr)
				((eq? m 'set-front-ptr!) set-front-ptr!)
				((eq? m 'set-rear-ptr!) set-rear-ptr!)
				((eq? m 'print) print-queue)
				(else
					(error "Undefined queue operation" m)
				)
			)
		)

		dispatch
	)
)

(define (empty-queue? queue) ((queue 'empty-queue?)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) ((queue 'delete-queue!)))
(define (front-queue queue) ((queue 'front-queue))) 
(define (front-ptr queue) (queue 'front-ptr)) 
(define (rear-ptr queue) (queue 'rear-ptr)) 
(define (set-front-ptr! queue item) ((queue 'set-front-ptr!) item)) 
(define (set-rear-ptr! queue item) ((queue 'set-rear-ptr!) item)) 
(define (print-queue queue) ((queue 'print))) 

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
		; (display result)
		; (newline)
		(cond
			((procedure? result) ((result 'print)))
			(else
				(print result)
			)
		)
		(newline)
	)
	(newline)
)

; Tests

(define q1 (make-queue))
q1
(define q2 (make-queue))
q2
(run-test empty-queue? q1)
(run-test empty-queue? q2)
(run-test insert-queue! q1 'a)
(run-test insert-queue! q2 'm)
(run-test front-queue q1)
(run-test insert-queue! q1 'b)
(run-test insert-queue! q2 'n)
(run-test insert-queue! q1 'c)
(run-test insert-queue! q2 'p)
(run-test insert-queue! q1 'd)
(run-test insert-queue! q2 'q)
(run-test insert-queue! q1 'e)
(run-test insert-queue! q1 'f)
(run-test front-queue q1)

(run-test empty-queue? q1)
(run-test empty-queue? q2)

(display "q1 is: ")
(print-queue q1)
(newline)
(display "q2 is: ")
(print-queue q2)
(newline)

(run-test delete-queue! q1)
(run-test front-queue q1)
(run-test delete-queue! q1)
(run-test front-queue q1)
(run-test delete-queue! q1)
(run-test front-queue q1)

(display "q1 is: ")
(print-queue q1)
(newline)
(display "q2 is: ")
(print-queue q2)
(newline)

(run-test delete-queue! q2)
(run-test front-queue q2)
(run-test delete-queue! q2)
(run-test front-queue q2)
(run-test delete-queue! q2)
(run-test front-queue q2)

(display "q1 is: ")
(print-queue q1)
(newline)
(display "q2 is: ")
(print-queue q2)
(newline)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
#<procedure:dispatch>
#<procedure:dispatch>
Applying #<procedure:empty-queue?> on: #<procedure:dispatch>
Result: #t

Applying #<procedure:empty-queue?> on: #<procedure:dispatch>
Result: #t

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'a
Result: {a}

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'm
Result: {m}

Applying #<procedure:front-queue> on: #<procedure:dispatch>
Result: 'a

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'b
Result: {a b}

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'n
Result: {m n}

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'c
Result: {a b c}

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'p
Result: {m n p}

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'd
Result: {a b c d}

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'q
Result: {m n p q}

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'e
Result: {a b c d e}

Applying #<procedure:insert-queue!> on: #<procedure:dispatch>, 'f
Result: {a b c d e f}

Applying #<procedure:front-queue> on: #<procedure:dispatch>
Result: 'a

Applying #<procedure:empty-queue?> on: #<procedure:dispatch>
Result: #f

Applying #<procedure:empty-queue?> on: #<procedure:dispatch>
Result: #f

q1 is: {a b c d e f}
q2 is: {m n p q}
Applying #<procedure:delete-queue!> on: #<procedure:dispatch>
Result: {b c d e f}

Applying #<procedure:front-queue> on: #<procedure:dispatch>
Result: 'b

Applying #<procedure:delete-queue!> on: #<procedure:dispatch>
Result: {c d e f}

Applying #<procedure:front-queue> on: #<procedure:dispatch>
Result: 'c

Applying #<procedure:delete-queue!> on: #<procedure:dispatch>
Result: {d e f}

Applying #<procedure:front-queue> on: #<procedure:dispatch>
Result: 'd

q1 is: {d e f}
q2 is: {m n p q}
Applying #<procedure:delete-queue!> on: #<procedure:dispatch>
Result: {n p q}

Applying #<procedure:front-queue> on: #<procedure:dispatch>
Result: 'n

Applying #<procedure:delete-queue!> on: #<procedure:dispatch>
Result: {p q}

Applying #<procedure:front-queue> on: #<procedure:dispatch>
Result: 'p

Applying #<procedure:delete-queue!> on: #<procedure:dispatch>
Result: {q}

Applying #<procedure:front-queue> on: #<procedure:dispatch>
Result: 'q

q1 is: {d e f}
q2 is: {q}
> 
