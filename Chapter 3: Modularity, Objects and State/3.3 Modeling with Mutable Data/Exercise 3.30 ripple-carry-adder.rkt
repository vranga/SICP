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
; called 16 times. When we add two 2-bit numbers, the delay proc is called 32 times.

; A full-adder contains two half adders and an or-gate.
; A half-adder contains 2 and-gates, 1 or-gate and 1 inverter
; So the total number of components in a full-adder is:
; {2 * (2 + 1 + 1)} + 1 = 9

; Note that the first computation/processing in any gate happens during its construction.
; The numbers 16 and 32 above are the number of times the delay proc is called when the
; ripple-carry-adder is constructed. Each-time an or-gate is constructed, its action procedure
; is called twice (since add-action! is called twice). The same is true for the and-gate. Each
; time an inverter is constructed, its action procedure is called once.
; So when a half-adder is constructed, the delay proc is called {(2 * 2) + (1 * 2) + 1} = 7 times
; Since a full-adder contains two half-adders and an or-gate, when a full-adder is constructed,
; the delay proc is called {(7 * 2) + 2} = 16 times.

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
	(define inverter-gate-delay 0.01) ; inverter delay is in seconds
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
	(define or-gate-delay 0.01) ; or-gate delay is in seconds
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
	(define and-gate-delay 0.01) ; and-gate delay is in seconds
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
	
		(define (add-action! no-arg-proc)
			(set! action-procedures (cons no-arg-proc action-procedures))
			(no-arg-proc)
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

; Test all combinations of inputs into a single adder (for 1 bit numbers)

(define a1Wires (make-wire-set (list 1)))
(define b1Wires (make-wire-set (list 1)))
(define sum1Wires (make-wire-set (list 0)))
(define outputCarry1Wire (make-wire))

(ripple-carry-adder a1Wires b1Wires sum1Wires outputCarry1Wire)
(print-adder-result a1Wires b1Wires sum1Wires outputCarry1Wire)

(set-signal! (car b1Wires) 0)
(print-adder-result a1Wires b1Wires sum1Wires outputCarry1Wire)

(set-signal! (car a1Wires) 0)
(print-adder-result a1Wires b1Wires sum1Wires outputCarry1Wire)

(set-signal! (car b1Wires) 1)
(print-adder-result a1Wires b1Wires sum1Wires outputCarry1Wire)

; Test all combinations of inputs into a single adder (for 2 bit numbers)

(define aWires (make-wire-set (list 0 0)))
(define bWires (make-wire-set (list 0 0)))
(define sumWires (make-wire-set (list 0 0)))
(define outputCarryWire (make-wire))

(ripple-carry-adder aWires bWires sumWires outputCarryWire)
(print-adder-result aWires bWires sumWires outputCarryWire)

(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car bWires) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)

(set-signal! (car (cdr aWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)

(set-signal! (car bWires) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car bWires) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)

(set-signal! (car (cdr aWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car aWires) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)

(set-signal! (car bWires) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car bWires) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)

(set-signal! (car (cdr aWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)

(set-signal! (car bWires) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car bWires) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)

(set-signal! (car (cdr aWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)

(set-signal! (car bWires) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 0)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car bWires) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)
(set-signal! (car (cdr bWires)) 1)
(print-adder-result aWires bWires sumWires outputCarryWire)

; Test single adder
(test-single-ripple-carry-adder)

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

; Test connecting the same input wire to two different devices to see if a change
; of signal on the input wire is detected by both devices

(define output1 (make-wire))
(define output2 (make-wire))
(define inputA (make-wire))
(define inputB (make-wire))
(define or-gate1 (or-gate inputA inputB output1))
(define or-gate2 (or-gate inputA inputB output2))
(display "Or-gates Ouput signals: ")
(output1 'print)
(display " and ")
(output2 'print)
(newline)
(displayln "Set inputA to 1")
(set-signal! inputA 1)
(display "Or-gates Ouput signals: ")
(output1 'print)
(display " and ")
(output2 'print)
(newline)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
'ok
InputA: 1 and InputB: 1
--------------------------------- 
Computed Sum: 0 and Carry Bit: 1, Delay proc called 16 time(s)
--------------------------------- 
InputA: 1 and InputB: 0
--------------------------------- 
Computed Sum: 1 and Carry Bit: 0, Delay proc called 24 time(s)
--------------------------------- 
InputA: 0 and InputB: 0
--------------------------------- 
Computed Sum: 0 and Carry Bit: 0, Delay proc called 27 time(s)
--------------------------------- 
InputA: 0 and InputB: 1
--------------------------------- 
Computed Sum: 1 and Carry Bit: 0, Delay proc called 33 time(s)
--------------------------------- 
'ok
InputA: 00 and InputB: 00
--------------------------------- 
Computed Sum: 00 and Carry Bit: 0, Delay proc called 65 time(s)
--------------------------------- 
InputA: 00 and InputB: 01
--------------------------------- 
Computed Sum: 01 and Carry Bit: 0, Delay proc called 71 time(s)
--------------------------------- 
InputA: 00 and InputB: 00
--------------------------------- 
Computed Sum: 00 and Carry Bit: 0, Delay proc called 77 time(s)
--------------------------------- 
InputA: 00 and InputB: 10
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 83 time(s)
--------------------------------- 
InputA: 00 and InputB: 11
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 89 time(s)
--------------------------------- 
InputA: 01 and InputB: 11
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 102 time(s)
--------------------------------- 
InputA: 01 and InputB: 01
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 110 time(s)
--------------------------------- 
InputA: 01 and InputB: 00
--------------------------------- 
Computed Sum: 01 and Carry Bit: 0, Delay proc called 124 time(s)
--------------------------------- 
InputA: 01 and InputB: 01
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 138 time(s)
--------------------------------- 
InputA: 01 and InputB: 00
--------------------------------- 
Computed Sum: 01 and Carry Bit: 0, Delay proc called 152 time(s)
--------------------------------- 
InputA: 01 and InputB: 10
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 158 time(s)
--------------------------------- 
InputA: 01 and InputB: 11
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 174 time(s)
--------------------------------- 
InputA: 00 and InputB: 11
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 187 time(s)
--------------------------------- 
InputA: 10 and InputB: 11
--------------------------------- 
Computed Sum: 01 and Carry Bit: 1, Delay proc called 192 time(s)
--------------------------------- 
InputA: 10 and InputB: 01
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 200 time(s)
--------------------------------- 
InputA: 10 and InputB: 00
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 206 time(s)
--------------------------------- 
InputA: 10 and InputB: 01
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 212 time(s)
--------------------------------- 
InputA: 10 and InputB: 00
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 218 time(s)
--------------------------------- 
InputA: 10 and InputB: 10
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 226 time(s)
--------------------------------- 
InputA: 10 and InputB: 11
--------------------------------- 
Computed Sum: 01 and Carry Bit: 1, Delay proc called 232 time(s)
--------------------------------- 
InputA: 11 and InputB: 11
--------------------------------- 
Computed Sum: 10 and Carry Bit: 1, Delay proc called 247 time(s)
--------------------------------- 
InputA: 11 and InputB: 01
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 257 time(s)
--------------------------------- 
InputA: 11 and InputB: 00
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 273 time(s)
--------------------------------- 
InputA: 11 and InputB: 01
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 289 time(s)
--------------------------------- 
InputA: 11 and InputB: 00
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 305 time(s)
--------------------------------- 
InputA: 11 and InputB: 10
--------------------------------- 
Computed Sum: 01 and Carry Bit: 1, Delay proc called 313 time(s)
--------------------------------- 
InputA: 11 and InputB: 11
--------------------------------- 
Computed Sum: 10 and Carry Bit: 1, Delay proc called 331 time(s)
--------------------------------- 
InputA: 11 and InputB: 11
--------------------------------- 
Computed Sum: 10 and Carry Bit: 1, Delay proc called 331 time(s)
--------------------------------- 
InputA: 11 and InputB: 01
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 341 time(s)
--------------------------------- 
InputA: 11 and InputB: 00
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 357 time(s)
--------------------------------- 
InputA: 11 and InputB: 01
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 373 time(s)
--------------------------------- 
InputA: 11 and InputB: 00
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 389 time(s)
--------------------------------- 
InputA: 11 and InputB: 10
--------------------------------- 
Computed Sum: 01 and Carry Bit: 1, Delay proc called 397 time(s)
--------------------------------- 
InputA: 11 and InputB: 11
--------------------------------- 
Computed Sum: 10 and Carry Bit: 1, Delay proc called 415 time(s)
--------------------------------- 
Setting up a new ripple-carry-adder...done.
InputA: 01000 and InputB: 00000
--------------------------------- 
Computed Sum: 01000 and Carry Bit: 0, Delay proc called 3 time(s)
--------------------------------- 
InputA: 01000 and InputB: 10000
--------------------------------- 
Computed Sum: 11000 and Carry Bit: 0, Delay proc called 6 time(s)
--------------------------------- 
InputA: 11000 and InputB: 10000
--------------------------------- 
Computed Sum: 01000 and Carry Bit: 1, Delay proc called 5 time(s)
--------------------------------- 
InputA: 11000 and InputB: 10001
--------------------------------- 
Computed Sum: 01001 and Carry Bit: 1, Delay proc called 6 time(s)
--------------------------------- 
InputA: 10000 and InputB: 10001
--------------------------------- 
Computed Sum: 00001 and Carry Bit: 1, Delay proc called 3 time(s)
--------------------------------- 
InputA: 10000 and InputB: 10101
--------------------------------- 
Computed Sum: 00101 and Carry Bit: 1, Delay proc called 6 time(s)
--------------------------------- 
InputA: 00000 and InputB: 10101
--------------------------------- 
Computed Sum: 10101 and Carry Bit: 0, Delay proc called 5 time(s)
--------------------------------- 
InputA: 00000 and InputB: 10100
--------------------------------- 
Computed Sum: 10100 and Carry Bit: 0, Delay proc called 6 time(s)
--------------------------------- 
InputA: 00100 and InputB: 10100
--------------------------------- 
Computed Sum: 11000 and Carry Bit: 0, Delay proc called 11 time(s)
--------------------------------- 
InputA: 00100 and InputB: 11100
--------------------------------- 
Computed Sum: 00000 and Carry Bit: 1, Delay proc called 16 time(s)
--------------------------------- 
InputA: 00000 and InputB: 11100
--------------------------------- 
Computed Sum: 11100 and Carry Bit: 0, Delay proc called 21 time(s)
--------------------------------- 
InputA: 00000 and InputB: 01100
--------------------------------- 
Computed Sum: 01100 and Carry Bit: 0, Delay proc called 6 time(s)
--------------------------------- 
InputA: 10000 and InputB: 01100
--------------------------------- 
Computed Sum: 11100 and Carry Bit: 0, Delay proc called 3 time(s)
--------------------------------- 
InputA: 10000 and InputB: 11100
--------------------------------- 
Computed Sum: 01100 and Carry Bit: 1, Delay proc called 8 time(s)
--------------------------------- 
InputA: 10001 and InputB: 11100
--------------------------------- 
Computed Sum: 01101 and Carry Bit: 1, Delay proc called 3 time(s)
--------------------------------- 
InputA: 10001 and InputB: 10100
--------------------------------- 
Computed Sum: 00101 and Carry Bit: 1, Delay proc called 6 time(s)
--------------------------------- 
InputA: 00001 and InputB: 10100
--------------------------------- 
Computed Sum: 10101 and Carry Bit: 0, Delay proc called 5 time(s)
--------------------------------- 
InputA: 00001 and InputB: 10000
--------------------------------- 
Computed Sum: 10001 and Carry Bit: 0, Delay proc called 6 time(s)
--------------------------------- 
InputA: 00011 and InputB: 10000
--------------------------------- 
Computed Sum: 10011 and Carry Bit: 0, Delay proc called 3 time(s)
--------------------------------- 
InputA: 00011 and InputB: 00000
--------------------------------- 
Computed Sum: 00011 and Carry Bit: 0, Delay proc called 6 time(s)
--------------------------------- 
InputA: 10011 and InputB: 00000
--------------------------------- 
Computed Sum: 10011 and Carry Bit: 0, Delay proc called 3 time(s)
--------------------------------- 
InputA: 10011 and InputB: 00010
--------------------------------- 
Computed Sum: 10101 and Carry Bit: 0, Delay proc called 14 time(s)
--------------------------------- 
InputA: 00011 and InputB: 00010
--------------------------------- 
Computed Sum: 00101 and Carry Bit: 0, Delay proc called 3 time(s)
--------------------------------- 
InputA: 00011 and InputB: 00000
--------------------------------- 
Computed Sum: 00011 and Carry Bit: 0, Delay proc called 14 time(s)
--------------------------------- 
InputA: 01011 and InputB: 00000
--------------------------------- 
Computed Sum: 01011 and Carry Bit: 0, Delay proc called 3 time(s)
--------------------------------- 
InputA: 01011 and InputB: 00100
--------------------------------- 
Computed Sum: 01111 and Carry Bit: 0, Delay proc called 6 time(s)
--------------------------------- 
InputA: 11011 and InputB: 00100
--------------------------------- 
Computed Sum: 11111 and Carry Bit: 0, Delay proc called 3 time(s)
--------------------------------- 
InputA: 11011 and InputB: 10100
--------------------------------- 
Computed Sum: 01111 and Carry Bit: 1, Delay proc called 8 time(s)
--------------------------------- 
InputA: 11001 and InputB: 10100
--------------------------------- 
Computed Sum: 01101 and Carry Bit: 1, Delay proc called 3 time(s)
--------------------------------- 
InputA: 11001 and InputB: 00100
--------------------------------- 
Computed Sum: 11101 and Carry Bit: 0, Delay proc called 8 time(s)
--------------------------------- 
InputA: 11000 and InputB: 00100
--------------------------------- 
Computed Sum: 11100 and Carry Bit: 0, Delay proc called 3 time(s)
--------------------------------- 
InputA: 11000 and InputB: 00000
--------------------------------- 
Computed Sum: 11000 and Carry Bit: 0, Delay proc called 6 time(s)
--------------------------------- 
InputA: 11010 and InputB: 00000
--------------------------------- 
Computed Sum: 11010 and Carry Bit: 0, Delay proc called 3 time(s)
--------------------------------- 
InputA: 11010 and InputB: 10000
--------------------------------- 
Computed Sum: 01010 and Carry Bit: 1, Delay proc called 8 time(s)
--------------------------------- 
InputA: 11000 and InputB: 10000
--------------------------------- 
Computed Sum: 01000 and Carry Bit: 1, Delay proc called 3 time(s)
--------------------------------- 
InputA: 11000 and InputB: 10010
--------------------------------- 
Computed Sum: 01010 and Carry Bit: 1, Delay proc called 6 time(s)
--------------------------------- 
InputA: 10000 and InputB: 10010
--------------------------------- 
Computed Sum: 00010 and Carry Bit: 1, Delay proc called 3 time(s)
--------------------------------- 
InputA: 10000 and InputB: 10000
--------------------------------- 
Computed Sum: 00000 and Carry Bit: 1, Delay proc called 6 time(s)
--------------------------------- 
InputA: 10001 and InputB: 10000
--------------------------------- 
Computed Sum: 00001 and Carry Bit: 1, Delay proc called 3 time(s)
--------------------------------- 
InputA: 10001 and InputB: 11000
--------------------------------- 
Computed Sum: 01001 and Carry Bit: 1, Delay proc called 6 time(s)
--------------------------------- 
"Finished running single full adder tests"
Adding 0 and 0
--------------------------------- 
Computed Sum: 0 and Carry Bit: 0, Delay proc called 16 time(s)
--------------------------------- 
Adding 0 and 1
--------------------------------- 
Computed Sum: 1 and Carry Bit: 0, Delay proc called 16 time(s)
--------------------------------- 
Adding 1 and 0
--------------------------------- 
Computed Sum: 1 and Carry Bit: 0, Delay proc called 16 time(s)
--------------------------------- 
Adding 1 and 1
--------------------------------- 
Computed Sum: 0 and Carry Bit: 1, Delay proc called 16 time(s)
--------------------------------- 
Adding 00 and 00
--------------------------------- 
Computed Sum: 00 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 01 and 00
--------------------------------- 
Computed Sum: 01 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 10 and 00
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 11 and 00
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 00 and 01
--------------------------------- 
Computed Sum: 01 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 01 and 01
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 10 and 01
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 11 and 01
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 32 time(s)
--------------------------------- 
Adding 00 and 10
--------------------------------- 
Computed Sum: 10 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 01 and 10
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 10 and 10
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 32 time(s)
--------------------------------- 
Adding 11 and 10
--------------------------------- 
Computed Sum: 01 and Carry Bit: 1, Delay proc called 32 time(s)
--------------------------------- 
Adding 00 and 11
--------------------------------- 
Computed Sum: 11 and Carry Bit: 0, Delay proc called 32 time(s)
--------------------------------- 
Adding 01 and 11
--------------------------------- 
Computed Sum: 00 and Carry Bit: 1, Delay proc called 32 time(s)
--------------------------------- 
Adding 10 and 11
--------------------------------- 
Computed Sum: 01 and Carry Bit: 1, Delay proc called 32 time(s)
--------------------------------- 
Adding 11 and 11
--------------------------------- 
Computed Sum: 10 and Carry Bit: 1, Delay proc called 32 time(s)
--------------------------------- 
Adding 1011011011 and 1110011100
--------------------------------- 
Computed Sum: 1001110111 and Carry Bit: 1, Delay proc called 160 time(s)
--------------------------------- 
Or-gates Ouput signals: 0 and 0
Set inputA to 1
Or-gates Ouput signals: 1 and 1
> 
