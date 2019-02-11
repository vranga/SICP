#lang racket

; Exercise 3.30.  Figure 3.27 shows a ripple-carry adder formed by stringing together n full-adders.
; This is the simplest form of parallel adder for adding two n-bit binary numbers. The inputs A1,
; A2, A3, ..., An and B1, B2, B3, ..., Bn are the two binary numbers to be added (each Ak and Bk is
; a 0 or a 1). The circuit generates S1, S2, S3, ..., Sn, the n bits of the sum, and C, the carry
; from the addition. Write a procedure ripple-carry-adder that generates this circuit.
; The procedure should take as arguments three lists of n wires each -- the Ak, the Bk, and the Sk
; -- and also another wire C. The major drawback of the ripple-carry adder is the need to wait for
; the carry signals to propagate. What is the delay needed to obtain the complete output from an
; n-bit ripple-carry adder, expressed in terms of the delays for and-gates, or-gates, and inverters?

; S O L U T I O N

; Look at implementation and test results below. I am also printing the number of times the
; delay procedure is called for each addition. When we add two 1-bit numbers, the delay proc is
; called 9 times. When we add two 2-bit numbers, the delay proc is called 18 times.

; A full-adder contains two half adders and an or-gate.
; A half-adder contains 2 and-gates, 1 or-gate and 1 inverter
; So the total number of components in a full-adder is:
; {2 * (2 + 1 + 1)} + 1 = 9
; So the delay in one full-adder is
; Full-Adder Delay = {2 * (2 * and-gate-delay + or-gate-delay + inverter-delay)} + or-gate-delay
; Therefore, for an n-bit ripple carry adder, the delay will be:
; 
; n * [{2 * (2 * and-gate-delay + or-gate-delay + inverter-delay)} + or-gate-delay]

; Of course, in real circuits, signals flow in parallel and therefore gates may also function in
; parallel so the actual delay will be lesser than what is computed above.

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
			(set-signal! finalCarryWire (get-signal c-in))
			(let ((intermediateCarryWire (make-wire)))
				(full-adder (car aWires) (car bWires) c-in (car sumWires) intermediateCarryWire)
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
	(define inverter-gate-delay 0.1) ; inverter delay is in seconds
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
	(invert-action-procedure)
	'ok
)

(define (or-gate a1 a2 output)
	(define or-gate-delay 0.1) ; or-gate delay is in seconds
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
	(or-action-procedure)
	'ok
)

(define (and-gate a1 a2 output)
	(define and-gate-delay 0.1) ; and-gate delay is in seconds
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

(define after-delay-call-count 0)
(define (after-delay delay proc)
	(set! after-delay-call-count (+ after-delay-call-count 1))
	; (display "Delaying for ")
	; (display delay)
	; (displayln " second(s)")
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

(test-ripple-carry-adder (list 0) (list 0))
(test-ripple-carry-adder (list 0) (list 1))
(test-ripple-carry-adder (list 1) (list 0))
(test-ripple-carry-adder (list 1) (list 1))

(test-ripple-carry-adder (list 0 0) (list 0 0))
(test-ripple-carry-adder (list 0 1) (list 0 0))
(test-ripple-carry-adder (list 1 0) (list 0 0))
(test-ripple-carry-adder (list 1 1) (list 0 0))

(test-ripple-carry-adder (list 0 0) (list 0 1))
(test-ripple-carry-adder (list 0 1) (list 0 1))
(test-ripple-carry-adder (list 1 0) (list 0 1))
(test-ripple-carry-adder (list 1 1) (list 0 1))

(test-ripple-carry-adder (list 0 0) (list 1 0))
(test-ripple-carry-adder (list 0 1) (list 1 0))
(test-ripple-carry-adder (list 1 0) (list 1 0))
(test-ripple-carry-adder (list 1 1) (list 1 0))

(test-ripple-carry-adder (list 0 0) (list 1 1))
(test-ripple-carry-adder (list 0 1) (list 1 1))
(test-ripple-carry-adder (list 1 0) (list 1 1))
(test-ripple-carry-adder (list 1 1) (list 1 1))

;  1 0 1 1 0 1 1 0 1 1 <-- inputA
;  1 1 1 0 0 1 1 1 0 0 <-- inputB

(test-ripple-carry-adder (list 1 0 1 1 0 1 1 0 1 1) (list 1 1 1 0 0 1 1 1 0 0))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Adding 0 and 0
--------------------------------- 
Computed Sum: 0 and Carry Bit: 0, Delay proc called 9 time(s)
--------------------------------- 
Adding 0 and 1
--------------------------------- 
Computed Sum: 1 and Carry Bit: 0, Delay proc called 9 time(s)
--------------------------------- 
Adding 1 and 0
--------------------------------- 
Computed Sum: 1 and Carry Bit: 0, Delay proc called 9 time(s)
--------------------------------- 
Adding 1 and 1
--------------------------------- 
Computed Sum: 0 and Carry Bit: 1, Delay proc called 9 time(s)
--------------------------------- 
Adding 00 and 00
--------------------------------- 
Computed Sum: 00 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 01 and 00
--------------------------------- 
Computed Sum: 01 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 10 and 00
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 11 and 00
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 00 and 01
--------------------------------- 
Computed Sum: 01 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 01 and 01
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 10 and 01
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 11 and 01
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 18 time(s)
--------------------------------- 
Adding 00 and 10
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 01 and 10
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 10 and 10
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 18 time(s)
--------------------------------- 
Adding 11 and 10
--------------------------------- 
Computed Sum: 01 and Carry Bit: 1, Delay proc called 18 time(s)
--------------------------------- 
Adding 00 and 11
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 18 time(s)
--------------------------------- 
Adding 01 and 11
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 18 time(s)
--------------------------------- 
Adding 10 and 11
--------------------------------- 
Computed Sum: 01 and Carry Bit: 1, Delay proc called 18 time(s)
--------------------------------- 
Adding 11 and 11
--------------------------------- 
Computed Sum: 10 and Carry Bit: 1, Delay proc called 18 time(s)
--------------------------------- 
Adding 1011011011 and 1110011100
--------------------------------- 
Computed Sum: 1001110111 and Carry Bit: 1, Delay proc called 90 time(s)
--------------------------------- 
> 
