#lang racket

; Exercise 3.34.  Louis Reasoner wants to build a squarer, a constraint device with two terminals
; such that the value of connector b on the second terminal will always be the square of the value
; a on the first terminal. He proposes the following simple device made from a multiplier:

; (define (squarer a b)
;   (multiplier a a b))

; There is a serious flaw in this idea. Explain.

; EXPLANATION
;
; See tests below. After creating the squarer, when we set the value of 'a', 'b' is set correctly to
; be the square of a. However, the reverse does not work. When b is set, a does not change. So the
; non-directionality of computation that we expect from constraint-based systems is not there.
;
; The reason for this can be seen by looking at the logic of the procedure "process-new-value" in
; the multiplier. This procedure expects two of the multiplier's three pieces of data (m1, m2 and p) to
; be valid before it computes and sets the third value. In the squarer defined above, since m1 and m2
; are the same connector, they are both without values when we set p externally. So the process-new-value
; proc is unable to set the value of the multiplier's input connectors. (All the conditional checks
; in process-new-value fail.)

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (averager a b c)
	(let ((u (make-connector)) (v (make-connector)))
		(adder a b u)
		(multiplier c v u)
		(constant 2 v)
		'ok
	)
)

(define (adder a1 a2 sum)

	(define (process-new-value)
		(cond
			((and (has-value? a1) (has-value? a2))
				(set-value!
					sum
					(+ (get-value a1) (get-value a2))
					me
				)
			)
			((and (has-value? a1) (has-value? sum))
				(set-value!
					a2
					(- (get-value sum) (get-value a1))
					me
				)
			)
			((and (has-value? a2) (has-value? sum))
				(set-value!
					a1
					(- (get-value sum) (get-value a2))
					me
				)
			)
		)
	)

	(define (process-forget-value)
		(forget-value! sum me)
		(forget-value! a1 me)
		(forget-value! a2 me)
		(process-new-value)
	)

	(define (me request)
		(cond
			((eq? request 'I-have-a-value)  
				(process-new-value)
			)
			((eq? request 'I-lost-my-value) 
				(process-forget-value)
			)
			(else 
				(error "Unknown request -- ADDER" request)
			)
		)
	)

	(connect a1 me)
	(connect a2 me)
	(connect sum me)
	me
)

(define (multiplier m1 m2 product)

	(define (process-new-value)
		(cond
			((or
				(and (has-value? m1) (= (get-value m1) 0))
				(and (has-value? m2) (= (get-value m2) 0))
			 )
				(set-value! product 0 me)
			)
			((and (has-value? m1) (has-value? m2))
				(set-value!
					product
					(* (get-value m1) (get-value m2))
					me
				)
			)
			((and (has-value? product) (has-value? m1))
				(set-value!
					m2
					(/ (get-value product) (get-value m1))
					me
				)
			)
			((and (has-value? product) (has-value? m2))
				(set-value!
					m1
					(/ (get-value product) (get-value m2))
					me
				)
			)
		)
	)

	(define (process-forget-value)
		(forget-value! product me)
		(forget-value! m1 me)
		(forget-value! m2 me)
		(process-new-value)
	)

	(define (me request)
		(cond
			((eq? request 'I-have-a-value)
				(process-new-value)
			)
			((eq? request 'I-lost-my-value)
				(process-forget-value)
			)
			(else
				(error "Unknown request -- MULTIPLIER" request)
			)
		)
	)

	(connect m1 me)
	(connect m2 me)
	(connect product me)
	me
)

(define (constant value connector)

	(define (me request)
		(error "Unknown request -- CONSTANT" request)
	)

	(connect connector me)
	(set-value! connector value me)
	me
)

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant)
	((connector 'set-value!) new-value informant)
)
(define (forget-value! connector retractor)
	((connector 'forget) retractor)
)
(define (connect connector new-constraint)
	((connector 'connect) new-constraint)
)

(define (inform-about-value constraint)
	(constraint 'I-have-a-value)
)
(define (inform-about-no-value constraint)
	(constraint 'I-lost-my-value)
)

(define (make-connector)
	(let ((value false) (informant false) (constraints '()))

		(define (set-my-value newval setter)
			(cond
				((not (has-value? me))
					(set! value newval)
					(set! informant setter)
					(for-each-except
						setter
						inform-about-value
						constraints
					)
				)
				((not (= value newval))
					(error "Contradiction" (list value newval))
				)
				(else
					'ignored
				)
			)
		)

		(define (forget-my-value retractor)
			(if (eq? retractor informant)
				(begin (set! informant false)
					(for-each-except
						retractor
						inform-about-no-value
						constraints
					)
				)
				'ignored
			)
		)
		
		(define (connect new-constraint)
			(if (not (memq new-constraint constraints))
				(set! constraints (cons new-constraint constraints))
				(void)
			)
			(if (has-value? me)
				(inform-about-value new-constraint)
				(void)
			)
			'done
		)

		(define (me request)
			(cond
				((eq? request 'has-value?)
					(if informant true false)
				)
				((eq? request 'value) value)
				((eq? request 'set-value!) set-my-value)
				((eq? request 'forget) forget-my-value)
				((eq? request 'connect) connect)
				(else
					(error "Unknown operation -- CONNECTOR" request)
				)
			)
		)
		me
	)
)

(define (for-each-except exception procedure list)

	(define (loop items)
		(cond
			((null? items) 'done)
			((eq? (car items) exception) (loop (cdr items)))
			(else
				(procedure (car items))
				(loop (cdr items))
			)
		)
	)

	(loop list)
)

(define (probe name connector)

	(define (print-probe value)
		(display "Probe: ")
		(display name)
		(display " = ")
		(display value)
		(newline)
	)

	(define (process-new-value)
		(print-probe (get-value connector))
	)

	(define (process-forget-value)
		(print-probe "?")
	)

	(define (me request)
		(cond
			((eq? request 'I-have-a-value)
				(process-new-value)
			)
			((eq? request 'I-lost-my-value)
				(process-forget-value)
			)
			(else
				(error "Unknown request -- PROBE" request)
			)
		)
	)

	(connect connector me)
	me
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
(define a (make-connector))
(define b (make-connector))
(probe 'a a)
(probe 'b b)

(define (squarer a b)
	(multiplier a a b)
)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
#<procedure:me>
#<procedure:me>
> (squarer a b)
#<procedure:me>
> (set-value! a 36 'user)
Probe: b = 1296
Probe: a = 36
'done
> (forget-value! a 'user)
Probe: b = ?
Probe: a = ?
'done
> (set-value! a -19 'user)
Probe: b = 361
Probe: a = -19
'done
> (forget-value! a 'user)
Probe: b = ?
Probe: a = ?
'done
> (set-value! b 49 'user)
Probe: b = 49
'done
> (get-value a)
-19
> 
