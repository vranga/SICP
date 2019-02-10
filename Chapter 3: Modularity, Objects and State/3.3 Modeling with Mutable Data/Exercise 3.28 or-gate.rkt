#lang racket

; Exercise 3.28.  Define an or-gate as a primitive function box. Your or-gate constructor should
; be similar to and-gate.

; S O L U T I O N

(define (or-gate a1 a2 output)
	(define or-gate-delay 2) ; or-gate delay is in seconds
	(define (or-action-procedure)
		(displayln "OR action proc invoked")
		(let ((new-value (logical-or (get-signal a1) (get-signal a2))))
			(after-delay
				or-gate-delay
				(lambda ()
					; set signal on output only if the new value is different
					; from the existing signal on the output
					(if (not (= (get-signal output) new-value))
						(begin
							(set-signal! output new-value)
							(displayln "Set signal on OR output wire to ")
							(displayln new-value)
							(output 'print)
						)
						(void)
					)
				)
			)
		)
	)
	(add-action! a1 or-action-procedure)
	(add-action! a2 or-action-procedure)
	'ok
)

; Logical Operations on signals
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

	(define signal-on-wire 0)
	(define action-proc null)

	(define (set-signal! new-value)
		(if (not (= signal-on-wire new-value))
			(begin
				(set! signal-on-wire new-value)
				(if (not (null? action-proc))
					(action-proc)
					(void)
				)
			)
			(void)
		)
	)

	(define (add-action! no-arg-proc)
		(set! action-proc no-arg-proc)
	)

	(define (dispatch m)
		(cond
			((eq? m 'get-signal) signal-on-wire)
			((eq? m 'set-signal!) set-signal!)
			((eq? m 'add-action!) add-action!)
			((eq? m 'print)
				(begin
					(display "Signal on wire is ")
					(displayln signal-on-wire)
				)
			)
			(else (error "Unknown request -- MAKE-WIRE" m))
		)
	)

	dispatch
)

(define (get-signal wire)
	(wire 'get-signal)
)

(define (set-signal! wire new-value)
	((wire 'set-signal!) new-value)
)

(define (add-action! wire no-arg-proc)
	((wire 'add-action!) no-arg-proc)
)

; Delay Operation

(define (after-delay delay proc)
	(display "Delaying for ")
	(display delay)
	(displayln " second(s)")
	(sleep delay)
	(proc)
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

(define inputA (make-wire))
(define inputB (make-wire))
(define output (make-wire))
(define og (or-gate inputA inputB output))
(run-test 'unknown get-signal inputA)
(run-test 'unknown get-signal inputB)
(run-test 'unknown get-signal output)
(run-test 'none set-signal! inputA 1)
(run-test 'none set-signal! inputB 1)
(run-test 'none set-signal! inputA 0)
(run-test 'none set-signal! inputB 0)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Applying #<procedure:get-signal> on: #<procedure:dispatch>
Result: 0

Applying #<procedure:get-signal> on: #<procedure:dispatch>
Result: 0

Applying #<procedure:get-signal> on: #<procedure:dispatch>
Result: 0

Applying #<procedure:set-signal!> on: #<procedure:dispatch>, 1
OR action proc invoked
Delaying for 2 second(s)
Set signal on OR output wire to 
1
Signal on wire is 1

Applying #<procedure:set-signal!> on: #<procedure:dispatch>, 1
OR action proc invoked
Delaying for 2 second(s)

Applying #<procedure:set-signal!> on: #<procedure:dispatch>, 0
OR action proc invoked
Delaying for 2 second(s)

Applying #<procedure:set-signal!> on: #<procedure:dispatch>, 0
OR action proc invoked
Delaying for 2 second(s)
Set signal on OR output wire to 
0
Signal on wire is 0

> 
