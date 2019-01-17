#lang racket

; Exercise 3.21.  Ben Bitdiddle decides to test the queue implementation described above.
; He types in the procedures to the Lisp interpreter and proceeds to try them out:

; (define q1 (make-queue))
; (insert-queue! q1 'a)
; ((a) a)
; (insert-queue! q1 'b)
; ((a b) b)
; (delete-queue! q1)
; ((b) b)
; (delete-queue! q1)
; (() b)
 
; "It's all wrong!" he complains. "The interpreter's response shows that the last item is inserted into
; the queue twice. And when I delete both items, the second b is still there, so the queue isn't empty,
; even though it's supposed to be." Eva Lu Ator suggests that Ben has misunderstood what is happening.
; "It's not that the items are going into the queue twice," she explains. "It's just that the standard
; Lisp printer doesn't know how to make sense of the queue representation. If you want to see the queue
; printed correctly, you'll have to define your own print procedure for queues." Explain what Eva Lu
; is talking about. In particular, show why Ben's examples produce the printed results that they do.
; Define a procedure print-queue that takes a queue as input and prints the sequence of items in the queue.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (make-queue) (mcons '() '()))
(define (empty-queue? queue) (null? (front-ptr queue)))

(define (insert-queue! queue item)
	(let ((new-pair (mcons item '())))
		(cond
			((empty-queue? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair)
				queue
			)
			(else
				(set-mcdr! (rear-ptr queue) new-pair)
				(set-rear-ptr! queue new-pair)
				queue
			)
		)
	)
)

(define (delete-queue! queue)
	(cond
		((empty-queue? queue) (error "DELETE! called with an empty queue" queue))
		(else
			(set-front-ptr! queue (mcdr (front-ptr queue)))
			queue
		)
	)
)

(define (front-queue queue)
	(if (empty-queue? queue)
		(error "FRONT called with an empty queue" queue)
		(mcar (front-ptr queue))
	)
)

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (print-queue queue)
	(display (mcar queue))
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
				(print (mcar items))
				(print-item-list (mcdr items) false)
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

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define q1 (make-queue))
> (display (insert-queue! q1 'a))
{{a} a}
> (display (insert-queue! q1 'b))
{{a b} b}
> (display (insert-queue! q1 'c))
{{a b c} c}
> (display (insert-queue! q1 'd))
{{a b c d} d}
> (display (delete-queue! q1))
{{b c d} d}
> (display (delete-queue! q1))
{{c d} d}
> (display (delete-queue! q1))
{{d} d}
> (display (delete-queue! q1))
{() d}
> (define q2 (make-queue))
> (print-queue (insert-queue! q2 'a))
{a}
> (print-queue (insert-queue! q2 'b))
{a b}
> (print-queue (insert-queue! q2 'c))
{a b c}
> (print-queue (insert-queue! q2 'd))
{a b c d}
> (print-queue (insert-queue! q2 'e))
{a b c d e}
> (print-queue (delete-queue! q2))
{b c d e}
> (print-queue (delete-queue! q2))
{c d e}
> (print-queue (delete-queue! q2))
{d e}
> (print-queue (delete-queue! q2))
{e}
> (print-queue (delete-queue! q2))
()
> 

Explanation: 

Eva Lu is right. The printer doesn't know how to make sense of the queue representation. In our queue
representation, we have a standard list which is a chain of pairs with a pointer to the beginning of the
list and an additional pointer that indicates the final pair in the list. The pointer to the beginning of
the list and the pointer to the final pair are combined with a 'cons' and the resulting pair is the queue
object. When the interpreter tries to print the queue object, it follows the standard way of first printing
the 'car' of the pair and then the 'cdr' of the pair. The 'car' points to the list that contains the items
of the queue, so the entire queue is printed in the standard list format. After that the cdr of the queue
object is printed. This being the last pair in the queue, the last item is printed again. That is why every
time the queue is printed, the last item appears twice: once as the last item in the 'car' of the queue and
once as the 'cdr' of the queue.

Note that delete-queue! merely moves the front pointer to the second item in the queue. So even when
the last item is "deleted", the rear pointer continues to point to it. That is why Ben sees the output:
(() b)

I have written the procedure print-queue to print the items of the queue without any repetition.
See the test results above.
