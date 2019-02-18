#lang racket

; Exercise 3.32.  The procedures to be run during each time segment of the agenda are kept in a queue.
; Thus, the procedures for each segment are called in the order in which they were added to the agenda
; (first in, first out). Explain why this order must be used. In particular, trace the behavior of an
; and-gate whose inputs change from 0,1 to 1,0 in the same segment and say how the behavior would
; differ if we stored a segment's procedures in an ordinary list, adding and removing procedures only
; at the front (last in, first out).

; EXPLANATION

; Let us look more closely at the logic of how action-procedures run. The call sequence is:
; action-proc --> after-delay --> add-to-agenda
; The action-proc first computes the new value *to be set* on the output wire, and this new value
; becomes available to the lambda function that is constructed and passed to after-delay.
; after-delay adds the delay to the current agenda time and passes this computed time and the
; lambda function to add-to-agenda. add-to-agenda inserts this pair into a queue that contains other
; pairs with the same time. All the actions within a time segment are executed one after the other
; in the order in which they were inserted into the agenda.
;
; The important point to note here is that the simulation is really a record-and-play-later
; mechanism so the order in which we play should be the order in which we record. Each action
; affects the state of the circuit wires and the subsequent actions depend on the state created
; by the preceding actions. The gate output decisions are made whenever the input wires change state
; but these decisions are applied on the output wires only when propagate executes the lambdas stored
; in the agenda queues.
;
; Let's look at the behavior of an AND-gate whose inputs change from 0,1 to 1,0 in the same segment.
;
; Let the input wires to the AND-gate be 'inputA' and 'inputB'. Let the output wire be called 'output'
; Further, assume that inputA is set to 0 and inputB is set to 1.
; Upon wiring up the AND-gate the action procedures will be as follows:
;
; inputA will have one element in its action-proc-list: the procedure 'and-action-procedure'
; inputB will also have one element in its action-proc-list: the procedure 'and-action-procedure'
;
; Both the procedures above (actually the same procedure) will be executed as soon as they are added
; to the action-proc-list of the two input wires. As shown in the call sequence above, 'new-value' is
; computed to be 0 in both cases since the logical-and of 0 and 1 is 0. Then the lambda function and
; time are inserted into the agenda. At the end of this process, the agenda will look as follows:
;
; (list 0 (list (3.(queue (lambda 1) (lambda 2)))))
;
; In the queue above, the first lambda function was produced when add-action! was called on inputA
; and the second lambda function was produced when add-action! was called on inputB. In both cases, 
; new-value is 0.
;
; When 'propagate' is run, it executes lambda 1 first which sets the output to 0 and then it 
; executes lambda 2 which again sets the output to 0. At the end of this, we have an empty agenda.
;
; Now let's assume that inputA is set to 1. This will trigger the and-action-procedure which will
; compute new-value to be 1 (since both inputs are 1). Again, after-delay and add-to-agenda are 
; called. The agenda will become:
;
; (list 0 (list (3.(queue (lambda 1))))) <--- Here lambda 1 uses new-value of 1
;
; Now let's assume that inputB is set to 0. This will trigger the and-action-procedure which will
; compute new-value to be 0 (since inputB is 0). Again, after-delay and add-to-agenda are 
; called. The agenda will become:
;
; (list 0 (list (3.(queue (lambda 1) (lambda 2))))) <--- Here lambda 1 uses new-value of 1 and
; lambda 2 uses new-value of 0
;
; When 'propagate' is run, it executes lambda 1 first which sets the output to 1 and then it 
; executes lambda 2 which sets the output to 0. At the end of this, we have an empty agenda.
; And output is 0. This is the right behavior. The output value changes from 0 to 1 and back to 0
; faithfully following the the input combinations of (0, 1), (1, 1) and (1, 0).
;
; If we had used an ordinary list with LIFO behavior, the agenda would have looked like this:
;
; (list 0 (list (3.(list (lambda 2) (lambda 1))))) <--- Here lambda 1 uses new-value of 1 and
; lambda 2 uses new-value of 0.
;
; When 'propagate' is run, it executes lambda 2 first which sets the output to 0 and then it 
; executes lambda 1 which sets the output to 1. At the end of this, we have an empty agenda.
; And output is 1. So for inputs (1, 0) the output is 1 which is wrong.
;
; See test results in the code below

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (ripple-carry-adder inputA inputB outputSum outputCarry)

	; Addition logic
	;1 1 1 0 0 1 1 0 0 0 0 <-- carry bits
	;  1 0 1 1 0 1 1 0 1 1 <-- inputA
	;  1 1 1 0 0 1 1 1 0 0 <-- inputB
	;  1 0 0 1 1 1 0 1 1 1 <-- outputSum
	;
	; Note: Addition needs to start from the least significant bit. So if the inputs are in the
	; form of lists, they need to be combined starting with the last items in the lists. So we
	; need to reverse the lists first. When we pass these bits through the ripple carry adder,
	; the sum bits are also produced in order from the least significant bit to the most
	; significant bit. So the final result is obtained by simply constructing a list of sum bits
	; where each new bit is inserted in the front of the list

	(define (ripple-carry-adder-internal aWires bWires c-in sumWires finalCarryWire)
		; If there are no wires, we have finished the ripple carry addition
		(if (null? aWires)
			; (set-signal! finalCarryWire (get-signal c-in))
			'ok
			(let ((intermediateCarryWire (make-wire)))
				(if (null? (cdr aWires))
					; We are about to create the last full adder
					(full-adder (car aWires) (car bWires) c-in (car sumWires) finalCarryWire)
					; This is not the last full-adder
					(full-adder (car aWires) (car bWires) c-in (car sumWires) intermediateCarryWire)
				)
				(ripple-carry-adder-internal (cdr aWires) (cdr bWires) intermediateCarryWire (cdr sumWires) finalCarryWire)
			)
		)
	)

	; ensure that the procedure arguments contain an equal number of wires
	(if (and (= (length inputA) (length inputB)) (= (length inputB) (length outputSum)))
		(let ((aWires (reverse-list inputA))
			  (bWires (reverse-list inputB))
			  (sumWires (reverse-list outputSum)))
			(ripple-carry-adder-internal aWires bWires (make-wire) sumWires outputCarry)
		)
		(error "RIPPLE-CARRY-ADDER: Invalid inputs")
	)
)

(define (reverse-list l)

	(define (reverse-list-internal l new-list)
		(cond
			((null? l) new-list)
			(else
				(reverse-list-internal (cdr l) (cons (car l) new-list))
			)
		)
	)

	(reverse-list-internal l (list))
)

(define (half-adder a b s c)
	(let ((d (make-wire)) (e (make-wire)))
		(or-gate a b d)
		(and-gate a b c)
		(inverter c e)
		(and-gate d e s)
		'ok
	)
)

(define (full-adder a b c-in sum c-out)
	(let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
		(half-adder b c-in s c1)
		(half-adder a s sum c2)
		(or-gate c1 c2 c-out)
		'ok
	)
)

(define (compound-or-gate a1 a2 output)
	(let ((inverter-output-1 (make-wire)) (inverter-output-2 (make-wire)) (and-gate-output (make-wire)))
		(inverter a1 inverter-output-1)
		(inverter a2 inverter-output-2)
		(and-gate inverter-output-1 inverter-output-2 and-gate-output)
		(inverter and-gate-output output)
	)
)

(define (inverter input output)
	(define inverter-gate-delay 2) ; inverter delay is in seconds
	(define (invert-action-procedure)
		; (displayln "INVERT action proc invoked")
		(let ((new-value (logical-not (get-signal input))))
			(after-delay
				inverter-gate-delay
				(lambda ()
					; set signal on output only if the new value is different
					; from the existing signal on the output
					(if (not (= (get-signal output) new-value))
						(begin
							(set-signal! output new-value)
							; (display "Set signal on INVERTER output wire to ")
							; (output 'print)
							; (newline)
						)
						(void)
						; (displayln "INVERTER output signal unchanged")
					)
				)
			)
		)
	)
	(add-action! input invert-action-procedure)
	'ok
)

(define (or-gate a1 a2 output)
	(define or-gate-delay 5) ; or-gate delay is in seconds
	(define (or-action-procedure)
		; (displayln "OR action proc invoked")
		(let ((new-value (logical-or (get-signal a1) (get-signal a2))))
			(after-delay
				or-gate-delay
				(lambda ()
					; set signal on output only if the new value is different
					; from the existing signal on the output
					(if (not (= (get-signal output) new-value))
						(begin
							(set-signal! output new-value)
							; (display "Set signal on OR output wire to ")
							; (output 'print)
							; (newline)
						)
						(void)
						; (displayln "OR-GATE output signal unchanged")
					)
				)
			)
		)
	)
	(add-action! a1 or-action-procedure)
	(add-action! a2 or-action-procedure)
	'ok
)

(define (and-gate a1 a2 output)
	(define and-gate-delay 3) ; and-gate delay is in seconds
	(define (and-action-procedure)
		; (displayln "AND action proc invoked")
		(let ((new-value (logical-and (get-signal a1) (get-signal a2))))
			(after-delay
				and-gate-delay
				(lambda ()
					; set signal on output only if the new value is different
					; from the existing signal on the output
					(if (not (= (get-signal output) new-value))
						(begin
							(set-signal! output new-value)
							; (display "Set signal on AND output wire to ")
							; (output 'print)
							; (newline)
						)
						(void)
						; (displayln "AND-GATE output signal unchanged")
					)
				)
			)
		)
	)
	(add-action! a1 and-action-procedure)
	(add-action! a2 and-action-procedure)
	'ok
)

; Logical Operations on signals
(define (logical-not s)
	(cond
		((= s 0)  1)
		((= s 1)  0)
		(else
			(error "LOGICAL-NOT: Invalid signal")
		)
	)
)

(define (logical-and s1 s2)
	(cond
		((and (= s1 0) (= s2 0)) 0)
		((and (= s1 0) (= s2 1)) 0)
		((and (= s1 1) (= s2 0)) 0)
		((and (= s1 1) (= s2 1)) 1)
		(else
			(error "LOGICAL-AND: Invalid signal(s)")
		)
	)
)

(define (logical-or s1 s2)
	(cond
		((and (= s1 0) (= s2 0)) 0)
		((and (= s1 0) (= s2 1)) 1)
		((and (= s1 1) (= s2 0)) 1)
		((and (= s1 1) (= s2 1)) 1)
		(else
			(error "LOGICAL-OR: Invalid signal(s)")
		)
	)
)

; Wire Operations

(define (make-wire)

	(define (call-each procedures)
		(if (null? procedures)
			(void)
			(begin
				((car procedures))
				(call-each (cdr procedures))
			)
		)
	)

	(let ((signal-on-wire 0) (action-procedures '()))

		(define (set-signal! new-value)
			(if (not (= signal-on-wire new-value))
				(begin
					(set! signal-on-wire new-value)
					(call-each action-procedures)
				)
				(void)
			)
		)
	
		(define (accept-action-procedure! no-arg-proc)
			(set! action-procedures (cons no-arg-proc action-procedures))
			(no-arg-proc)
		)
	
		(define (dispatch m)
			(cond
				((eq? m 'get-signal) signal-on-wire)
				((eq? m 'set-signal!) set-signal!)
				((eq? m 'add-action!) accept-action-procedure!)
				((eq? m 'print)
					(begin
						; (display "Signal on wire is ")
						(display signal-on-wire)
					)
				)
				(else (error "Unknown request -- MAKE-WIRE" m))
			)
		)
		dispatch
	)
)

(define (get-signal wire)
	(wire 'get-signal)
)

(define (flip-signal! wire)
	(let ((existing-signal (get-signal wire)))
		(if (= existing-signal 1)
			(set-signal! wire 0)
			(set-signal! wire 1)
		)
	)
)

(define (set-signal! wire new-value)
	((wire 'set-signal!) new-value)
)

(define (add-action! wire no-arg-proc)
	((wire 'add-action!) no-arg-proc)
)

; Delay and Propagate Operations

(define after-delay-call-count 0)

(define (after-delay delay action)
	(add-to-agenda!
		(+ delay (current-time the-agenda))
		action
		the-agenda
	)
)

(define (propagate)
	(if (empty-agenda? the-agenda)
		'done
		(let ((first-item (first-agenda-item the-agenda)))
			(first-item)
			(remove-first-agenda-item! the-agenda)
			(propagate)
		)
	)
)

; Agenda Operations

(define (make-time-segment time queue) (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))

(define (make-agenda) (mlist 0))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time) (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments) (set-mcdr! agenda segments))
(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
	(define (belongs-before? segments)
		(or
			(null? segments)
			(< time (segment-time (mcar segments)))
		)
	)

	(define (make-new-time-segment time action)
		(let ((q (make-queue)))
		; (let ((q (make-list)))
			(insert-queue! q action)
			; (insert-list! q action)
			(make-time-segment time q)
		)
	)

	(define (add-to-segments! segments)
		(if (= (segment-time (mcar segments)) time)
			(insert-queue! (segment-queue (mcar segments)) action)
			; (insert-list! (segment-queue (mcar segments)) action)
			(let ((rest (mcdr segments)))
				(if (belongs-before? rest)
					(set-mcdr!
						segments
						(mcons (make-new-time-segment time action) (mcdr segments))
					)
					(add-to-segments! rest)
				)
			)
		)
	)

	(let ((segments (segments agenda)))
		(if (belongs-before? segments)
			(set-segments!
				agenda
				(mcons (make-new-time-segment time action) segments)
			)
			(add-to-segments! segments)
		)
	)
)

(define (remove-first-agenda-item! agenda)
	(let ((q (segment-queue (first-segment agenda))))
		(delete-queue! q)
		; (delete-list! q)
		(if (empty-queue? q)
		; (if (empty-list? q)
			(set-segments! agenda (rest-segments agenda))
			(void)
		)
	)
)

(define (first-agenda-item agenda)
	(if (empty-agenda? agenda)
		(error "Agenda is empty -- FIRST-AGENDA-ITEM")
		(let ((first-seg (first-segment agenda)))
			(set-current-time! agenda (segment-time first-seg))
			(front-queue (segment-queue first-seg))
			; (front-list (segment-queue first-seg))
		)
	)
)

(define the-agenda (make-agenda))

; Queue Operations

; (define (make-list)
; 	(let ((list-ptr '()))
; 
; 		(define (insert-list! item)
; 			(set! list-ptr (mcons item list-ptr))
; 			dispatch
; 		)
; 
; 		(define (delete-list!)
; 			(set! list-ptr (mcdr list-ptr))
; 			dispatch
; 		)
; 
; 		(define (front-list)
; 			(if (null? list-ptr)
; 				(error "FRONT called with an empty list")
; 				(mcar list-ptr)
; 			)
; 		)
; 
; 		(define (empty-list?)
; 			(null? list-ptr)
; 		)
; 
; 		(define (dispatch m)
; 			(cond
; 				((eq? m 'insert-list!) insert-list!)
; 				((eq? m 'delete-list!) delete-list!)
; 				((eq? m 'front-list) front-list)
; 				((eq? m 'empty-list?) empty-list?)
; 				(else
; 					(error "Undefined list operation" m)
; 				)
; 			)
; 		)
; 		dispatch
; 	)
; )

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
; (define (empty-list? l) ((l 'empty-list?)))

(define (insert-queue! queue item) ((queue 'insert-queue!) item))
; (define (insert-list! l item) ((l 'insert-list!) item))

(define (delete-queue! queue) ((queue 'delete-queue!)))
; (define (delete-list! l) ((l 'delete-list!)))

(define (front-queue queue) ((queue 'front-queue))) 
; (define (front-list l) ((l 'front-list)))

(define (front-ptr queue) (queue 'front-ptr)) 
(define (rear-ptr queue) (queue 'rear-ptr)) 
(define (set-front-ptr! queue item) ((queue 'set-front-ptr!) item)) 
(define (set-rear-ptr! queue item) ((queue 'set-rear-ptr!) item)) 
(define (print-queue queue) ((queue 'print))) 

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
	(if (not (null? args))
		(begin
			(display " on: ")
			(print-item-list args true)
		)
		(void)
	)
	(newline)
	(let ((result (apply proc args)))
		(if (not (eq? return-type 'none))
			(display "Result: ")
			(void)
		)
		(cond
			((procedure? result) ((result 'print)))
			; ((eq? return-type 'deque) (print-deque result))
			((eq? return-type 'none) (void))
			(else
				(print result)
				(newline)
			)
		)
	)
	(newline)
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

; Given a list of 1s and 0s, this procedure will setup a set of wires that
; contain these values and return the wires in a list
(define (make-wire-set bits)
	(if (null? bits)
		null
		(let ((wire (make-wire)))
			(set-signal! wire (car bits))
			(cons wire (make-wire-set (cdr bits)))
		)
	)
)

; Print the signals in a list of wires
(define (print-wire-set wires)
	(if (not (null? wires))
		(begin
			(display (get-signal (car wires)))
			(print-wire-set (cdr wires))
		)
		(void)
	)
)

(define (print-adder-result operandAWires operandBWires sumWires outputCarryWire)
	(display "InputA: ")
	(print-wire-set operandAWires)
	(display " and InputB: ")
	(print-wire-set operandBWires)
	(newline)

	(displayln "--------------------------------- ")
	(display "Computed Sum: ")
	(print-wire-set sumWires)
	(display " and Carry Bit: ")
	(outputCarryWire 'print)

	(display ", Delay proc called ")
	(display after-delay-call-count)
	(display " time(s)")
	(newline)
	(displayln "--------------------------------- ")
)

(define (test-single-ripple-carry-adder)
	; This is to test a single adder multiple times by changing the inputs
	; multiple times

	(define (make-zero-bit-list size)
		(if (> size 0)
			(cons 0 (make-zero-bit-list (- size 1)))
			null
		)
	)

	(define inputABits (make-zero-bit-list 5))
	(define inputBBits (make-zero-bit-list 5))

	(define operandAWires (make-wire-set inputABits))
	(define operandBWires (make-wire-set inputBBits))

	(define sumWires (make-wire-set (make-zero-bit-list (length inputABits))))
	(define outputCarryWire (make-wire))
	
	(define (run-single-ripple-carry-adder-test numberOfTimes)
		(if (= numberOfTimes 0)
			"Finished running single full adder tests"
			(begin
				(set! after-delay-call-count 0)
				(change-bit operandAWires)
				(print-adder-result operandAWires operandBWires sumWires outputCarryWire)
				(set! after-delay-call-count 0)
				(change-bit operandBWires)
				(print-adder-result operandAWires operandBWires sumWires outputCarryWire)
				(run-single-ripple-carry-adder-test (- numberOfTimes 1))
			)
		)
	)

	(define (change-bit wire-set)
		(let ((chosen-wire (get-random-item wire-set)))
			(flip-signal! chosen-wire)
		)
	)

	(define (get-random-item items)

		(define (get-item item-list position)
			(if (or (= position 0) (= position 1))
				(car item-list)
				(get-item (cdr item-list) (- position 1))
			)
		)

		(get-item items (round (* (random) (length items))))
	)

	(display "Setting up a new ripple-carry-adder...")
	(ripple-carry-adder operandAWires operandBWires sumWires outputCarryWire)
	(displayln "done.")

	(run-single-ripple-carry-adder-test 20)
)

(define (test-ripple-carry-adder inputABits inputBBits)
	
	(set! after-delay-call-count 0)

	(define (make-zero-bit-list size)
		(if (> size 0)
			(cons 0 (make-zero-bit-list (- size 1)))
			null
		)
	)

	(define operandAWires (make-wire-set inputABits))
	(define operandBWires (make-wire-set inputBBits))

	(display "Adding ")
	(print-wire-set operandAWires)
	
	(display " and ")
	(print-wire-set operandBWires)
	(newline)
	
	(define sumWires (make-wire-set (make-zero-bit-list (length inputABits))))
	(define outputCarryWire (make-wire))
	
	(ripple-carry-adder operandAWires operandBWires sumWires outputCarryWire)
	
	(displayln "--------------------------------- ")
	(display "Computed Sum: ")
	(print-wire-set sumWires)
	(display " and Carry Bit: ")
	(outputCarryWire 'print)
	(display ", Delay proc called ")
	(display after-delay-call-count)
	(display " time(s)")
	(newline)
	(displayln "--------------------------------- ")
)

(define (probe name wire)
	(add-action! wire
		(lambda ()        
			(display name)
			(display " ")
			(display (current-time the-agenda))
			(display "  New-value = ")
			(display (get-signal wire))
			(newline)
		)
	)
)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))

; Test Results

; Correct behaviour when the lambdas are kept in a queue with FIFO behavior
Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (probe 'output output)
output 0  New-value = 0
> (set-signal! input-2 1)
> (and-gate input-1 input-2 output)
'ok
> (propagate)
'done
> (set-signal! input-1 1)
> (set-signal! input-2 0)
> (propagate)
output 6  New-value = 1
output 6  New-value = 0
'done
> 

; Wrong behaviour when the lambdas are kept in a list with LIFO behavior
Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (probe 'output output)
output 0  New-value = 0
> (set-signal! input-2 1)
> (and-gate input-1 input-2 output)
'ok
> (propagate)
'done
> (set-signal! input-1 1)
> (set-signal! input-2 0)
> (propagate)
output 6  New-value = 1
'done
> 
