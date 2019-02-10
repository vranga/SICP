#lang racket

; Exercise 3.29.  Another way to construct an or-gate is as a compound digital logic device,
; built from and-gates and inverters. Define a procedure or-gate that accomplishes this.
; What is the delay time of the or-gate in terms of and-gate-delay and inverter-delay?

; S O L U T I O N

; Note: I made an enhancement to all the function procedures such as and-gate, or-gate etc. to
; call the action procedure once as soon as the gate is constructed. This way we ensure that
; the outut of the gate is correct right from the beginning.

; Delay time of the compound-or-gate = (3 * inverter-delay) + (and-gate-delay)

; (OR A B) = (NOT (AND (NOT A) (NOT B)))

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
		(displayln "INVERT action proc invoked")
		(let ((new-value (logical-not (get-signal input))))
			(after-delay
				inverter-gate-delay
				(lambda ()
					; set signal on output only if the new value is different
					; from the existing signal on the output
					(if (not (= (get-signal output) new-value))
						(begin
							(set-signal! output new-value)
							(display "Set signal on INVERTER output wire to ")
							(output 'print)
							(newline)
						)
						(displayln "INVERTER output signal unchanged")
					)
				)
			)
		)
	)
	(add-action! input invert-action-procedure)
	(invert-action-procedure)
	'ok
)

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
							(display "Set signal on OR output wire to ")
							(output 'print)
							(newline)
						)
						(displayln "OR-GATE output signal unchanged")
					)
				)
			)
		)
	)
	(add-action! a1 or-action-procedure)
	(add-action! a2 or-action-procedure)
	(or-action-procedure)
	'ok
)

(define (and-gate a1 a2 output)
	(define and-gate-delay 2) ; and-gate delay is in seconds
	(define (and-action-procedure)
		(displayln "AND action proc invoked")
		(let ((new-value (logical-and (get-signal a1) (get-signal a2))))
			(after-delay
				and-gate-delay
				(lambda ()
					; set signal on output only if the new value is different
					; from the existing signal on the output
					(if (not (= (get-signal output) new-value))
						(begin
							(set-signal! output new-value)
							(display "Set signal on AND output wire to ")
							(output 'print)
							(newline)
						)
						(displayln "AND-GATE output signal unchanged")
					)
				)
			)
		)
	)
	(add-action! a1 and-action-procedure)
	(add-action! a2 and-action-procedure)
	(and-action-procedure)
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
					; (display "Signal on wire is ")
					(display signal-on-wire)
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
; (define og (or-gate inputA inputB output))
(define cog (compound-or-gate inputA inputB output))

(run-test 'unknown get-signal inputA)
(run-test 'unknown get-signal inputB)
(run-test 'unknown get-signal output)

(run-test 'none set-signal! inputA 1)
(displayln "Compound Or Gate")
(displayln "----------------")
(display "Input A: ")
(inputA 'print)
(display ", Input B: ")
(inputB 'print)
(display ", Output: ")
(output 'print)
(newline)
(displayln "----------------")

(run-test 'none set-signal! inputB 1)
(displayln "Compound Or Gate")
(displayln "----------------")
(display "Input A: ")
(inputA 'print)
(display ", Input B: ")
(inputB 'print)
(display ", Output: ")
(output 'print)
(newline)
(displayln "----------------")

(run-test 'none set-signal! inputA 0)
(displayln "Compound Or Gate")
(displayln "----------------")
(display "Input A: ")
(inputA 'print)
(display ", Input B: ")
(inputB 'print)
(display ", Output: ")
(output 'print)
(newline)
(displayln "----------------")

(run-test 'none set-signal! inputB 0)
(displayln "Compound Or Gate")
(displayln "----------------")
(display "Input A: ")
(inputA 'print)
(display ", Input B: ")
(inputB 'print)
(display ", Output: ")
(output 'print)
(newline)
(displayln "----------------")

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
INVERT action proc invoked
Delaying for 2 second(s)
Set signal on INVERTER output wire to 1
INVERT action proc invoked
Delaying for 2 second(s)
Set signal on INVERTER output wire to 1
AND action proc invoked
Delaying for 2 second(s)
Set signal on AND output wire to 1
INVERT action proc invoked
Delaying for 2 second(s)
INVERTER output signal unchanged
Applying #<procedure:get-signal> on: #<procedure:dispatch>
Result: 0

Applying #<procedure:get-signal> on: #<procedure:dispatch>
Result: 0

Applying #<procedure:get-signal> on: #<procedure:dispatch>
Result: 0

Applying #<procedure:set-signal!> on: #<procedure:dispatch>, 1
INVERT action proc invoked
Delaying for 2 second(s)
AND action proc invoked
Delaying for 2 second(s)
INVERT action proc invoked
Delaying for 2 second(s)
Set signal on INVERTER output wire to 1
Set signal on AND output wire to 0
Set signal on INVERTER output wire to 0

Compound Or Gate
----------------
Input A: 1, Input B: 0, Output: 1
----------------
Applying #<procedure:set-signal!> on: #<procedure:dispatch>, 1
INVERT action proc invoked
Delaying for 2 second(s)
AND action proc invoked
Delaying for 2 second(s)
AND-GATE output signal unchanged
Set signal on INVERTER output wire to 0

Compound Or Gate
----------------
Input A: 1, Input B: 1, Output: 1
----------------
Applying #<procedure:set-signal!> on: #<procedure:dispatch>, 0
INVERT action proc invoked
Delaying for 2 second(s)
AND action proc invoked
Delaying for 2 second(s)
AND-GATE output signal unchanged
Set signal on INVERTER output wire to 1

Compound Or Gate
----------------
Input A: 0, Input B: 1, Output: 1
----------------
Applying #<procedure:set-signal!> on: #<procedure:dispatch>, 0
INVERT action proc invoked
Delaying for 2 second(s)
AND action proc invoked
Delaying for 2 second(s)
INVERT action proc invoked
Delaying for 2 second(s)
Set signal on INVERTER output wire to 0
Set signal on AND output wire to 1
Set signal on INVERTER output wire to 1

Compound Or Gate
----------------
Input A: 0, Input B: 0, Output: 0
----------------
> 
