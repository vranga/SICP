#lang racket

; Exercise 3.37.  The celsius-fahrenheit-converter procedure is cumbersome when compared with a more
; expression-oriented style of definition, such as

; (define (celsius-fahrenheit-converter x)
;   (c+ (c* (c/ (cv 9) (cv 5))
;           x)
;       (cv 32)))
; (define C (make-connector))
; (define F (celsius-fahrenheit-converter C))

; Here c+, c*, etc. are the ``constraint'' versions of the arithmetic operations. For example,
; c+ takes two connectors as arguments and returns a connector that is related to these by an adder
; constraint:

; (define (c+ x y)
;   (let ((z (make-connector)))
;     (adder x y z)
;     z))

; Define analogous procedures c-, c*, c/, and cv (constant value) that enable us to define
; compound constraints as in the converter example above.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (celsius-fahrenheit-converter x)
	(c+
		(c*
			(c/ (cv 9) (cv 5))
			x
		)
		(cv 32)
	)
)

(define (c+ x y)
	(let ((z (make-connector)))
		(adder x y z)
		z
	)
)

(define (c* x y)
	(let ((z (make-connector)))
		(multiplier x y z)
		z
	)
)

(define (c/ x y)
	(let ((z (make-connector)))
		(divider x y z)
		z
	)
)

(define (cv x)
	(let ((z (make-connector)))
		(constant x z)
		z
	)
)

(define (squarer a b)

	(define (process-new-value)
		(cond
			((has-value? a)
				(set-value!
					b
					(* (get-value a) (get-value a))
					me
				)
			)
			((has-value? b)
				(if (< (get-value b) 0)
					(error "square less than 0 -- SQUARER" (get-value b))
					(set-value!
						a
						(sqrt (get-value b))
						me
					)
				)
			)
		)
	)

	(define (process-forget-value)
		(forget-value! b me)
		(forget-value! a me)
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

	(connect a me)
	(connect b me)
	me
)

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

(define (divider d1 d2 result)

	(define (process-new-value)
		(cond
			((and (has-value? d2) (= (get-value d2) 0))
				(error "divisor equal to 0 -- DIVIDER" (get-value d2))
			)
			((and (has-value? d1) (= (get-value d1) 0))
				(set-value! result 0 me)
			)
			((and (has-value? d1) (has-value? d2))
				(set-value!
					result
					(/ (get-value d1) (get-value d2))
					me
				)
			)
			((and (has-value? result) (has-value? d1))
				(set-value!
					d2
					(/ (get-value d1) (get-value result))
					me
				)
			)
			((and (has-value? result) (has-value? d2))
				(set-value!
					d1
					(* (get-value result) (get-value d2))
					me
				)
			)
		)
	)

	(define (process-forget-value)
		(forget-value! result me)
		(forget-value! d1 me)
		(forget-value! d2 me)
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
				(error "Unknown request -- DIVIDER" request)
			)
		)
	)

	(connect d1 me)
	(connect d2 me)
	(connect result me)
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

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define C (make-connector))
> (define F (celsius-fahrenheit-converter C))
> (set-value! C 0 'user)
'done
> (get-value F)
32
> (probe 'Celsius C)
Probe: Celsius = 0
#<procedure:me>
> (probe 'Fahrenheit F)
Probe: Fahrenheit = 32
#<procedure:me>
> (forget-value! C 'user)
Probe: Celsius = ?
Probe: Fahrenheit = ?
'done
> (set-value! C 100 'user)
Probe: Celsius = 100
Probe: Fahrenheit = 212
'done
> (forget-value! C 'user)
Probe: Celsius = ?
Probe: Fahrenheit = ?
'done
> (set-value! F 0 'user)
Probe: Fahrenheit = 0
Probe: Celsius = -160/9
'done
> (forget-value! F 'user)
Probe: Fahrenheit = ?
Probe: Celsius = ?
'done
> (set-value! F 98.4 'user)
Probe: Fahrenheit = 98.4
Probe: Celsius = 36.88888888888889
'done
> (forget-value! F 'user)
Probe: Fahrenheit = ?
Probe: Celsius = ?
'done
> (set-value! F 212 'user)
Probe: Fahrenheit = 212
Probe: Celsius = 100
'done
> 
