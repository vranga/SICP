#lang racket

; Exercise 4.4.  Recall the definitions of the special forms and and or from chapter 1:

; and: The expressions are evaluated from left to right. If any expression evaluates to
; false, false is returned; any remaining expressions are not evaluated. If all the
; expressions evaluate to true values, the value of the last expression is returned. If
; there are no expressions then true is returned.

; or: The expressions are evaluated from left to right. If any expression evaluates to a
; true value, that value is returned; any remaining expressions are not evaluated. If all
; expressions evaluate to false, or if there are no expressions, then false is returned.

; Install and and or as new special forms for the evaluator by defining appropriate syntax
; procedures and evaluation procedures eval-and and eval-or. Alternatively, show how to
; implement and and or as derived expressions.

; S O L U T I O N

; This solution implements 'and' and 'or' as derived expressions using the already existing
; 'if' expression

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (EVAL expression env)
	(cond
		((self-evaluating? expression) expression)
		((quoted? expression) (text-of-quotation expression))
		((variable? expression) (lookup-variable-value expression env))
		((pair? expression)
			(let ((handler (get (operator expression) 'eval)))
				(if (not (null? handler))
					; handler found so pass the expression to it
					(handler expression env)
					; handler not found so it must be a procedure application
					(begin
						(display "Calling APPLY on: ")
						(display expression)
						(newline)
						(APPLY
							(EVAL (operator expression) env)
							(list-of-values (operands expression) env)
						)
					)
				)
			)
		)
		(else
			(error "Unknown expression type -- EVAL" expression)
		)
	)
)

; Self Evaluating Expressions
(define (self-evaluating? expression)
	(cond
		((number? expression) true)
		((string? expression) true)
		(else false)
	)
)

; Quoted Expressions
(define (quoted? expression)
	(tagged-list? expression 'quote)
)

(define (text-of-quotation expression) (cadr expression))

; Assignment Expressions
(define (assignment? expression)
	(tagged-list? expression 'set!)
)
(define (assignment-variable expression) (cadr expression))
(define (assignment-value expression) (caddr expression))

(define (EVAL-assignment expression env)
	(display "In proc EVAL-assignment to evaluate: ")
	(display expression)
	(newline)
	(set-variable-value!
		(assignment-variable expression)
		(EVAL (assignment-value expression) env)
		env
	)
	'ok
)

(define (set-variable-value! var val env)
	(display "In proc set-variable-value! to set ")
	(display var)
	(display " to ")
	(display val)
	(newline)
	(define (env-loop env)
		(define (scan vars vals)
			(cond
				((null? vars)
					(env-loop (enclosing-environment env))
				)
				((eq? var (car vars))
					(set-mcar! vals val)
				)
				(else (scan (mcdr vars) (mcdr vals)))
			)
		)
		(if (eq? env the-empty-environment)
			(error "Unbound variable -- SET!" var)
			(let ((frame (first-frame env)))
				(scan
					(frame-variables frame)
					(frame-values frame)
				)
			)
		)
	)
	(env-loop env)
)

; Definition Expressions
(define (definition? expression)
	(tagged-list? expression 'define)
)

(define (definition-variable expression)
	(if (symbol? (cadr expression))
		(cadr expression)
		(caadr expression)
	)
)

(define (definition-value expression)
	(if (symbol? (cadr expression))
		(caddr expression)
		(make-lambda
			(cdadr expression)		; formal parameters
			(cddr expression)		; body
		)
	)
)

(define (EVAL-definition expression env)
	(display "In proc EVAL-definition to evaluate: ")
	(display expression)
	(newline)
	(define-variable!
		(definition-variable expression)
		(EVAL (definition-value expression) env)
		env
	)
	'ok
)

(define (define-variable! var val env)
	(display "In proc define-variable! to define ")
	(display var)
	(display " as ")
	(display val)
	(newline)
	(let ((frame (first-frame env)))
		(define (scan vars vals)
			(cond
				((null? vars)
					(add-binding-to-frame! var val frame)
				)
				((eq? var (car vars))
					(set-mcar! vals val)
				)
				(else (scan (cdr vars) (mcdr vals)))
			)
		)
		(scan
			(frame-variables frame)
			(frame-values frame)
		)
	)
)

; Variable Expressions
(define (variable? expression) (symbol? expression))

(define (lookup-variable-value var env)
	(display "In proc lookup-variable-value to lookup: ")
	(display var)
	(newline)
	(define (env-loop env)
		(define (scan vars vals)
			(cond
				((null? vars)
					(env-loop (enclosing-environment env))
				)
				((eq? var (car vars))
					(begin
						(display "Found value ")
						(display (mcar vals))
						(newline)
						(mcar vals)
					)
				)
				(else
					(scan (cdr vars) (mcdr vals))
				)
			)
		)
		(if (eq? env the-empty-environment)
			(error "Unbound variable" var)
			(let ((frame (first-frame env)))
				(scan
					(frame-variables frame)
					(frame-values frame)
				)
			)
		)
	)
	(env-loop env)
)

; 'if' Expressions
(define (if? expression) (tagged-list? expression 'if))
(define (if-predicate expression) (cadr expression))
(define (if-consequent expression) (caddr expression))
(define (if-alternative expression)
	(if (not (null? (cdddr expression)))
		(cadddr expression)
		'false
	)
)
(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative)
)

(define (EVAL-if expression env)
	(display "In proc EVAL-if to evaluate: ")
	(display expression)
	(newline)
	(if (true? (EVAL (if-predicate expression) env))
		(EVAL (if-consequent expression) env)
		(EVAL (if-alternative expression) env)
	)
)

; lambda Expressions
(define (lambda? expression) (tagged-list? expression 'lambda))
(define (lambda-parameters expression) (cadr expression))
(define (lambda-body expression) (cddr expression))
(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body))
)

(define (EVAL-lambda expression env)
	(display "In proc EVAL-lambda to evaluate: ")
	(display expression)
	(newline)
	(make-procedure
		(lambda-parameters expression)
		(lambda-body expression)
		env
	)
)

(define (make-procedure parameters body env)
	(list 'procedure parameters body env)
)

; "begin" Expressions
(define (begin? expression) (tagged-list? expression 'begin))
(define (begin-actions expression) (cdr expression))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (EVAL-sequence exps env)
	(cond
		((last-exp? exps) (EVAL (first-exp exps) env))
		(else (EVAL (first-exp exps) env)
			(EVAL-sequence (rest-exps exps) env)
		)
	)
)
(define (sequence->exp seq)
	(cond
		((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))
	)
)
(define (make-begin seq) (cons 'begin seq))

(define (EVAL-begin expression env)
	(display "In proc EVAL-begin to evaluate: ")
	(display expression)
	(newline)
	(EVAL-sequence (begin-actions expression) env)
)

; "cond" Expressions
(define (cond? expression) (tagged-list? expression 'cond))
(define (cond-clauses expression) (cdr expression))
(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else)
)
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if expression)
	(expand-cond-clauses (cond-clauses expression))
)

(define (expand-cond-clauses clauses)
	(if (null? clauses)
		'false ; no else clause
		(let ((first (car clauses)) (rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clause isn't last -- COND->IF" clauses)
				)
				(make-if
					(cond-predicate first)
					(sequence->exp (cond-actions first))
					(expand-cond-clauses rest)
				)
			)
		)
	)
)

(define (EVAL-cond expression env)
	(display "In proc EVAL-cond to evaluate: ")
	(display expression)
	(newline)
	(EVAL (cond->if expression) env)
)

; "and" Expressions
(define (and-clauses expression) (cdr expression))

; Expression is of the form: (and exp1 exp2 .... expn)
; This expression can be re-written as:
; (if exp1
;	(if exp2
;		(if exp3
;			...
;				... (if expn
;						true
;						false
;					)
;		)
;		false
;	)
;	false
; )
(define (and->if expression)
	(expand-and-clauses (and-clauses expression))
)

(define (expand-and-clauses clauses)
	(define (last-and-clause? clauses)
		(if (null? (cdr clauses))
			true
			false
		)
	)

	(cond
		((null? clauses) null)
		((last-and-clause? clauses)
			(make-if
				(car clauses) ;predicate
				'true
				'false
			)
		)
		(else
			(make-if 
				(car clauses) ;predicate
				(expand-and-clauses (cdr clauses)) ;consequent
				'false ;alternative
			)
		)
	)
)

(define (EVAL-and expression env)
	(display "In proc EVAL-and to evaluate: ")
	(display expression)
	(newline)
	(display "AND expression in IF form:")
	(newline)
	(display (and->if expression))
	(newline)
	(EVAL (and->if expression) env)
)

; "or" Expressions
(define (or-clauses expression) (cdr expression))

; Expression is of the form: (or exp1 exp2 .... expn)
; This expression can be re-written as:
; (if exp1
;	true
;	(if exp2
;		true
;		(if exp3
;			...
;				... (if expn
;						true
;						false
;					)
;		)
;	)
; )
(define (or->if expression)
	(expand-or-clauses (or-clauses expression))
)

(define (expand-or-clauses clauses)
	(define (last-or-clause? clauses)
		(if (null? (cdr clauses))
			true
			false
		)
	)

	(cond
		((null? clauses) null)
		((last-or-clause? clauses)
			(make-if
				(car clauses) ;predicate
				'true
				'false
			)
		)
		(else
			(make-if 
				(car clauses) ;predicate
				'true ;consequent
				(expand-or-clauses (cdr clauses)) ;alternative
			)
		)
	)
)

(define (EVAL-or expression env)
	(display "In proc EVAL-or to evaluate: ")
	(display expression)
	(newline)
	(display "OR expression in IF form:")
	(newline)
	(display (or->if expression))
	(newline)
	(EVAL (or->if expression) env)
)

; Compound Procedures
(define (application? expression) (pair? expression))
(define (operator expression) (car expression))
(define (operands expression) (cdr expression))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-values exps env)
	(evaluate-l-to-r-list-of-values exps env)
)

(define (tagged-list? expression tag)
	(if (pair? expression)
		(eq? (car expression) tag)
		false
	)
)

; Primitive Procedures Support
(define (primitive-procedure? proc)
	(tagged-list? proc 'primitive)
)

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
	; The following 'apply' proc is the underlying Racket apply
	(apply
		(primitive-implementation proc)
		args
	)
)

(define (compound-procedure? p)
	(tagged-list? p 'procedure)
)

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define primitive-procedures
	(list
		(list 'car car)
		(list 'cdr cdr)
		(list 'cons cons)
		(list 'null? null?)
		(list 'display display)
		(list '> >)
		(list '< <)
		(list '= =)
	)
)

(define (primitive-procedure-names)
	(map car primitive-procedures)
)

(define (primitive-procedure-objects)
	(list->mlist
		(map (lambda (proc) (list 'primitive (car (cdr proc)))) primitive-procedures)
	)
)

; Environment related procedures

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
	(set-mcar! frame (cons var (mcar frame)))
	(set-mcdr! frame (mcons val (mcdr frame)))
)

(define (extend-environment vars vals base-env)
	(if (= (length vars) (mlength vals))
		(cons (make-frame vars vals) base-env)
		(if (< (length vars) (mlength vals))
			(error "Too many arguments supplied" vars vals)
			(error "Too few arguments supplied" vars vals)
		)
	)
)

(define (make-frame variables values)
	(mcons variables values)
)

(define (setup-environment)
	(let ((initial-env (extend-environment	(primitive-procedure-names)
											(primitive-procedure-objects)
											the-empty-environment)))
		(define-variable! 'true true initial-env)
		(define-variable! 'false false initial-env)
		initial-env
	)
)

(define the-global-environment (setup-environment))

; Driver Loop
(define input-prompt ";;; EVAL input:")
(define output-prompt ";;; EVAL value:")
(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		(if (eq? input 'quit)
			'Done
			(let ((output (EVAL input the-global-environment)))
				(announce-output output-prompt)
				(user-print output)
				(driver-loop)
			)
		)
	)
)

(define (prompt-for-input string)
	(newline)
	(newline)
	(display string)
	(newline)
)

(define (announce-output string)
	(newline)
	(display string)
	(newline)
)

(define (user-print object)
	(if (compound-procedure? object)
		(display
			(list
				'compound-procedure
				(procedure-parameters object)
				(procedure-body object)
				'<procedure-env>
			)
		)
		(display object)
	)
)

; Other
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (evaluate-l-to-r-list-of-values exps env)
	(if (no-operands? exps)
		'()
		(let ((first (EVAL (first-operand exps) env)))
			(cons
				first
				(evaluate-l-to-r-list-of-values (rest-operands exps) env)
			)
		)
	)
)

(define (evaluate-r-to-l-list-of-values exps env)
	(if (no-operands? exps)
		'()
		(let ((rest (evaluate-r-to-l-list-of-values (rest-operands exps) env)))
			(cons
				(EVAL (first-operand exps) env)
				rest
			)
		)
	)
)

; Data-driven proc table definitions

(define op-table (mlist))

(define (get type operation)
;	Example of data in the op-table
; 	(define op-table
; 		(list
; 			(cons
; 				'deriv
; 				(list
; 					(cons '+ deriv-of-sum)
; 					(cons '* deriv-of-product)
; 					(cons '** deriv-of-exponentiation)
; 				)
; 			)
; 		)
; 	)

	(define (find-type-in-op-list op-list t)
		(cond
			((null? op-list)
				(begin
					; (display "Type not found: ")
					; (display t)
					; (newline)
					null
				)
			)
			((not (mpair? op-list)) (error "op-list not a pair!"))
			(else
				(if (eq? t (mcar (mcar op-list)))
					(mcdr (mcar op-list))
					(find-type-in-op-list (mcdr op-list) t)
				)
			)
		)
	)

	(find-type-in-op-list (mcdr (find-op-row operation op-table)) type)
)

(define (put type operation proc)
	(cond
		((null? op-table)
			(set! op-table (mlist (mcons operation (mlist (mcons type proc)))))
		)
		(else
			(let ((row (find-op-row operation op-table)))
				(if	(not (null? row))
					; Found the operation in the op-table, so add the type and proc to it
					(mappend! (mcdr row) (mlist (mcons type proc)))
					; Did not find the operation so create a new row for the operation
					(mappend! op-table (mlist (mcons operation (mlist (mcons type proc)))))
				)
			)
		)
	)
	(void)
)

(define (find-op-row oper table)
	(cond
		((null? table) null)
		((not (mpair? table)) (error "op-table not a pair!"))
		(else
			(if (eq? oper (mcar (mcar table)))
				(mcar table)
				(find-op-row oper (mcdr table))
			)
		)
	)
)

(put 'variable 'eval lookup-variable-value)
(put 'set! 'eval EVAL-assignment)
(put 'define 'eval EVAL-definition)
(put 'if 'eval EVAL-if)
(put 'cond 'eval EVAL-cond)
(put 'and 'eval EVAL-and)
(put 'or 'eval EVAL-or)
(put 'begin 'eval EVAL-begin)
(put 'lambda 'eval EVAL-lambda)

; procedure "APPLY"
(define (APPLY procedure arguments)
	(cond
		((primitive-procedure? procedure)
			(apply-primitive-procedure procedure arguments)
		)
		((compound-procedure? procedure)
			(EVAL-sequence
				(procedure-body procedure)
				(extend-environment
					(procedure-parameters procedure)
					(list->mlist arguments)
					(procedure-environment procedure)
				)
			)
		)
		(else
			(error "Unknown procedure type -- APPLY" procedure)
		)
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

Welcome to DrRacket, version 8.1 [cs].
Language: racket, with debugging; memory limit: 128 MB.
In proc define-variable! to define true as #t
In proc define-variable! to define false as #f
> (driver-loop)


;;; EVAL input:
(and (> 10 6) (< 9 19) (= 5 5.0) (< 20 22))
In proc EVAL-and to evaluate: (and (> 10 6) (< 9 19) (= 5 5.0) (< 20 22))
AND expression in IF form:
(if (> 10 6) (if (< 9 19) (if (= 5 5.0) (if (< 20 22) true false) false) false) false)
In proc EVAL-if to evaluate: (if (> 10 6) (if (< 9 19) (if (= 5 5.0) (if (< 20 22) true false) false) false) false)
Calling APPLY on: (> 10 6)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 9 19) (if (= 5 5.0) (if (< 20 22) true false) false) false)
Calling APPLY on: (< 9 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (= 5 5.0) (if (< 20 22) true false) false)
Calling APPLY on: (= 5 5.0)
In proc lookup-variable-value to lookup: =
Found value (primitive #<procedure:=>)
In proc EVAL-if to evaluate: (if (< 20 22) true false)
Calling APPLY on: (< 20 22)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(and (> 10 6) (< 9 19) (= 5 5.0))
In proc EVAL-and to evaluate: (and (> 10 6) (< 9 19) (= 5 5.0))
AND expression in IF form:
(if (> 10 6) (if (< 9 19) (if (= 5 5.0) true false) false) false)
In proc EVAL-if to evaluate: (if (> 10 6) (if (< 9 19) (if (= 5 5.0) true false) false) false)
Calling APPLY on: (> 10 6)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 9 19) (if (= 5 5.0) true false) false)
Calling APPLY on: (< 9 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (= 5 5.0) true false)
Calling APPLY on: (= 5 5.0)
In proc lookup-variable-value to lookup: =
Found value (primitive #<procedure:=>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(and (> 10 6) (< 9 19) (= 6 5.0))
In proc EVAL-and to evaluate: (and (> 10 6) (< 9 19) (= 6 5.0))
AND expression in IF form:
(if (> 10 6) (if (< 9 19) (if (= 6 5.0) true false) false) false)
In proc EVAL-if to evaluate: (if (> 10 6) (if (< 9 19) (if (= 6 5.0) true false) false) false)
Calling APPLY on: (> 10 6)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 9 19) (if (= 6 5.0) true false) false)
Calling APPLY on: (< 9 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (= 6 5.0) true false)
Calling APPLY on: (= 6 5.0)
In proc lookup-variable-value to lookup: =
Found value (primitive #<procedure:=>)
In proc lookup-variable-value to lookup: false
Found value #f

;;; EVAL value:
#f

;;; EVAL input:
(and (> 10 6) (< 29 19) (= 5 5.0))
In proc EVAL-and to evaluate: (and (> 10 6) (< 29 19) (= 5 5.0))
AND expression in IF form:
(if (> 10 6) (if (< 29 19) (if (= 5 5.0) true false) false) false)
In proc EVAL-if to evaluate: (if (> 10 6) (if (< 29 19) (if (= 5 5.0) true false) false) false)
Calling APPLY on: (> 10 6)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 29 19) (if (= 5 5.0) true false) false)
Calling APPLY on: (< 29 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc lookup-variable-value to lookup: false
Found value #f

;;; EVAL value:
#f

;;; EVAL input:
(and (< 10 6) (< 9 19) (= 5 5.0))
In proc EVAL-and to evaluate: (and (< 10 6) (< 9 19) (= 5 5.0))
AND expression in IF form:
(if (< 10 6) (if (< 9 19) (if (= 5 5.0) true false) false) false)
In proc EVAL-if to evaluate: (if (< 10 6) (if (< 9 19) (if (= 5 5.0) true false) false) false)
Calling APPLY on: (< 10 6)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc lookup-variable-value to lookup: false
Found value #f

;;; EVAL value:
#f

;;; EVAL input:
(and (> 10 6) (< 9 19) (= 15 5.0))
In proc EVAL-and to evaluate: (and (> 10 6) (< 9 19) (= 15 5.0))
AND expression in IF form:
(if (> 10 6) (if (< 9 19) (if (= 15 5.0) true false) false) false)
In proc EVAL-if to evaluate: (if (> 10 6) (if (< 9 19) (if (= 15 5.0) true false) false) false)
Calling APPLY on: (> 10 6)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 9 19) (if (= 15 5.0) true false) false)
Calling APPLY on: (< 9 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (= 15 5.0) true false)
Calling APPLY on: (= 15 5.0)
In proc lookup-variable-value to lookup: =
Found value (primitive #<procedure:=>)
In proc lookup-variable-value to lookup: false
Found value #f

;;; EVAL value:
#f

;;; EVAL input:
(and (< 10 16) (> 9 -9) (= 5 5.0))
In proc EVAL-and to evaluate: (and (< 10 16) (> 9 -9) (= 5 5.0))
AND expression in IF form:
(if (< 10 16) (if (> 9 -9) (if (= 5 5.0) true false) false) false)
In proc EVAL-if to evaluate: (if (< 10 16) (if (> 9 -9) (if (= 5 5.0) true false) false) false)
Calling APPLY on: (< 10 16)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (> 9 -9) (if (= 5 5.0) true false) false)
Calling APPLY on: (> 9 -9)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (= 5 5.0) true false)
Calling APPLY on: (= 5 5.0)
In proc lookup-variable-value to lookup: =
Found value (primitive #<procedure:=>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(or (= 10 6) (> 9 19) (< 5 5.0) (> 1 2) (> 20 19))
In proc EVAL-or to evaluate: (or (= 10 6) (> 9 19) (< 5 5.0) (> 1 2) (> 20 19))
OR expression in IF form:
(if (= 10 6) true (if (> 9 19) true (if (< 5 5.0) true (if (> 1 2) true (if (> 20 19) true false)))))
In proc EVAL-if to evaluate: (if (= 10 6) true (if (> 9 19) true (if (< 5 5.0) true (if (> 1 2) true (if (> 20 19) true false)))))
Calling APPLY on: (= 10 6)
In proc lookup-variable-value to lookup: =
Found value (primitive #<procedure:=>)
In proc EVAL-if to evaluate: (if (> 9 19) true (if (< 5 5.0) true (if (> 1 2) true (if (> 20 19) true false))))
Calling APPLY on: (> 9 19)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 5 5.0) true (if (> 1 2) true (if (> 20 19) true false)))
Calling APPLY on: (< 5 5.0)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (> 1 2) true (if (> 20 19) true false))
Calling APPLY on: (> 1 2)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (> 20 19) true false)
Calling APPLY on: (> 20 19)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(or (> 10 6) (< 9 19) (= 5 35.0))
In proc EVAL-or to evaluate: (or (> 10 6) (< 9 19) (= 5 35.0))
OR expression in IF form:
(if (> 10 6) true (if (< 9 19) true (if (= 5 35.0) true false)))
In proc EVAL-if to evaluate: (if (> 10 6) true (if (< 9 19) true (if (= 5 35.0) true false)))
Calling APPLY on: (> 10 6)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(or (> 10 6) (> 9 19) (= 5 5.0))
In proc EVAL-or to evaluate: (or (> 10 6) (> 9 19) (= 5 5.0))
OR expression in IF form:
(if (> 10 6) true (if (> 9 19) true (if (= 5 5.0) true false)))
In proc EVAL-if to evaluate: (if (> 10 6) true (if (> 9 19) true (if (= 5 5.0) true false)))
Calling APPLY on: (> 10 6)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(or (> 10 56) (< 9 19) (= 5 5.0))
In proc EVAL-or to evaluate: (or (> 10 56) (< 9 19) (= 5 5.0))
OR expression in IF form:
(if (> 10 56) true (if (< 9 19) true (if (= 5 5.0) true false)))
In proc EVAL-if to evaluate: (if (> 10 56) true (if (< 9 19) true (if (= 5 5.0) true false)))
Calling APPLY on: (> 10 56)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 9 19) true (if (= 5 5.0) true false))
Calling APPLY on: (< 9 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(or (< 10 6) (< 79 19) (= 55 5.0))
In proc EVAL-or to evaluate: (or (< 10 6) (< 79 19) (= 55 5.0))
OR expression in IF form:
(if (< 10 6) true (if (< 79 19) true (if (= 55 5.0) true false)))
In proc EVAL-if to evaluate: (if (< 10 6) true (if (< 79 19) true (if (= 55 5.0) true false)))
Calling APPLY on: (< 10 6)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (< 79 19) true (if (= 55 5.0) true false))
Calling APPLY on: (< 79 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (= 55 5.0) true false)
Calling APPLY on: (= 55 5.0)
In proc lookup-variable-value to lookup: =
Found value (primitive #<procedure:=>)
In proc lookup-variable-value to lookup: false
Found value #f

;;; EVAL value:
#f

;;; EVAL input:
(or (> 10 6) (< 79 19) (= 55 5.0))
In proc EVAL-or to evaluate: (or (> 10 6) (< 79 19) (= 55 5.0))
OR expression in IF form:
(if (> 10 6) true (if (< 79 19) true (if (= 55 5.0) true false)))
In proc EVAL-if to evaluate: (if (> 10 6) true (if (< 79 19) true (if (= 55 5.0) true false)))
Calling APPLY on: (> 10 6)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(or (> 10 60) (< 9 19) (= 55 5.0))
In proc EVAL-or to evaluate: (or (> 10 60) (< 9 19) (= 55 5.0))
OR expression in IF form:
(if (> 10 60) true (if (< 9 19) true (if (= 55 5.0) true false)))
In proc EVAL-if to evaluate: (if (> 10 60) true (if (< 9 19) true (if (= 55 5.0) true false)))
Calling APPLY on: (> 10 60)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 9 19) true (if (= 55 5.0) true false))
Calling APPLY on: (< 9 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(or (> 10 60) (< 90 19) (= 5 5.0))
In proc EVAL-or to evaluate: (or (> 10 60) (< 90 19) (= 5 5.0))
OR expression in IF form:
(if (> 10 60) true (if (< 90 19) true (if (= 5 5.0) true false)))
In proc EVAL-if to evaluate: (if (> 10 60) true (if (< 90 19) true (if (= 5 5.0) true false)))
Calling APPLY on: (> 10 60)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 90 19) true (if (= 5 5.0) true false))
Calling APPLY on: (< 90 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (= 5 5.0) true false)
Calling APPLY on: (= 5 5.0)
In proc lookup-variable-value to lookup: =
Found value (primitive #<procedure:=>)
In proc lookup-variable-value to lookup: true
Found value #t

;;; EVAL value:
#t

;;; EVAL input:
(or (> 10 600) (< 90 19) (= 65 5.0))
In proc EVAL-or to evaluate: (or (> 10 600) (< 90 19) (= 65 5.0))
OR expression in IF form:
(if (> 10 600) true (if (< 90 19) true (if (= 65 5.0) true false)))
In proc EVAL-if to evaluate: (if (> 10 600) true (if (< 90 19) true (if (= 65 5.0) true false)))
Calling APPLY on: (> 10 600)
In proc lookup-variable-value to lookup: >
Found value (primitive #<procedure:>>)
In proc EVAL-if to evaluate: (if (< 90 19) true (if (= 65 5.0) true false))
Calling APPLY on: (< 90 19)
In proc lookup-variable-value to lookup: <
Found value (primitive #<procedure:<>)
In proc EVAL-if to evaluate: (if (= 65 5.0) true false)
Calling APPLY on: (= 65 5.0)
In proc lookup-variable-value to lookup: =
Found value (primitive #<procedure:=>)
In proc lookup-variable-value to lookup: false
Found value #f

;;; EVAL value:
#f

;;; EVAL input:
(define y 8)
In proc EVAL-definition to evaluate: (define y 8)
In proc define-variable! to define y as 8

;;; EVAL value:
ok

;;; EVAL input:
y
In proc lookup-variable-value to lookup: y
Found value 8

;;; EVAL value:
8

;;; EVAL input:
(set! y 89)
In proc EVAL-assignment to evaluate: (set! y 89)
In proc set-variable-value! to set y to 89

;;; EVAL value:
ok

;;; EVAL input:
y
In proc lookup-variable-value to lookup: y
Found value 89

;;; EVAL value:
89

;;; EVAL input:
(define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
In proc EVAL-definition to evaluate: (define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
In proc EVAL-lambda to evaluate: (lambda (x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
In proc define-variable! to define append as (procedure (x y) ((if (null? x) y (cons (car x) (append (cdr x) y)))) ({(y false true car cdr cons null? display > < =) 89 #f #t (primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:display>) (primitive #<procedure:>>) (primitive #<procedure:<>) (primitive #<procedure:=>)}))

;;; EVAL value:
ok

;;; EVAL input:
(append '(q w e r t y u) '(z x c v b n m))
Calling APPLY on: (append '(q w e r t y u) '(z x c v b n m))
In proc lookup-variable-value to lookup: append
Found value #0=(procedure (x y) ((if (null? x) y (cons (car x) (append (cdr x) y)))) ({(append y false true car cdr cons null? display > < =) #0# 89 #f #t (primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:display>) (primitive #<procedure:>>) (primitive #<procedure:<>) (primitive #<procedure:=>)}))
In proc EVAL-if to evaluate: (if (null? x) y (cons (car x) (append (cdr x) y)))
Calling APPLY on: (null? x)
In proc lookup-variable-value to lookup: null?
Found value (primitive #<procedure:null?>)
In proc lookup-variable-value to lookup: x
Found value (q w e r t y u)
Calling APPLY on: (cons (car x) (append (cdr x) y))
In proc lookup-variable-value to lookup: cons
Found value (primitive #<procedure:cons>)
Calling APPLY on: (car x)
In proc lookup-variable-value to lookup: car
Found value (primitive #<procedure:car>)
In proc lookup-variable-value to lookup: x
Found value (q w e r t y u)
Calling APPLY on: (append (cdr x) y)
In proc lookup-variable-value to lookup: append
Found value #0=(procedure (x y) ((if (null? x) y (cons (car x) (append (cdr x) y)))) ({(append y false true car cdr cons null? display > < =) #0# 89 #f #t (primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:display>) (primitive #<procedure:>>) (primitive #<procedure:<>) (primitive #<procedure:=>)}))
Calling APPLY on: (cdr x)
In proc lookup-variable-value to lookup: cdr
Found value (primitive #<procedure:cdr>)
In proc lookup-variable-value to lookup: x
Found value (q w e r t y u)
In proc lookup-variable-value to lookup: y
Found value (z x c v b n m)
In proc EVAL-if to evaluate: (if (null? x) y (cons (car x) (append (cdr x) y)))
Calling APPLY on: (null? x)
In proc lookup-variable-value to lookup: null?
Found value (primitive #<procedure:null?>)
In proc lookup-variable-value to lookup: x
Found value (w e r t y u)
Calling APPLY on: (cons (car x) (append (cdr x) y))
In proc lookup-variable-value to lookup: cons
Found value (primitive #<procedure:cons>)
Calling APPLY on: (car x)
In proc lookup-variable-value to lookup: car
Found value (primitive #<procedure:car>)
In proc lookup-variable-value to lookup: x
Found value (w e r t y u)
Calling APPLY on: (append (cdr x) y)
In proc lookup-variable-value to lookup: append
Found value #0=(procedure (x y) ((if (null? x) y (cons (car x) (append (cdr x) y)))) ({(append y false true car cdr cons null? display > < =) #0# 89 #f #t (primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:display>) (primitive #<procedure:>>) (primitive #<procedure:<>) (primitive #<procedure:=>)}))
Calling APPLY on: (cdr x)
In proc lookup-variable-value to lookup: cdr
Found value (primitive #<procedure:cdr>)
In proc lookup-variable-value to lookup: x
Found value (w e r t y u)
In proc lookup-variable-value to lookup: y
Found value (z x c v b n m)
In proc EVAL-if to evaluate: (if (null? x) y (cons (car x) (append (cdr x) y)))
Calling APPLY on: (null? x)
In proc lookup-variable-value to lookup: null?
Found value (primitive #<procedure:null?>)
In proc lookup-variable-value to lookup: x
Found value (e r t y u)
Calling APPLY on: (cons (car x) (append (cdr x) y))
In proc lookup-variable-value to lookup: cons
Found value (primitive #<procedure:cons>)
Calling APPLY on: (car x)
In proc lookup-variable-value to lookup: car
Found value (primitive #<procedure:car>)
In proc lookup-variable-value to lookup: x
Found value (e r t y u)
Calling APPLY on: (append (cdr x) y)
In proc lookup-variable-value to lookup: append
Found value #0=(procedure (x y) ((if (null? x) y (cons (car x) (append (cdr x) y)))) ({(append y false true car cdr cons null? display > < =) #0# 89 #f #t (primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:display>) (primitive #<procedure:>>) (primitive #<procedure:<>) (primitive #<procedure:=>)}))
Calling APPLY on: (cdr x)
In proc lookup-variable-value to lookup: cdr
Found value (primitive #<procedure:cdr>)
In proc lookup-variable-value to lookup: x
Found value (e r t y u)
In proc lookup-variable-value to lookup: y
Found value (z x c v b n m)
In proc EVAL-if to evaluate: (if (null? x) y (cons (car x) (append (cdr x) y)))
Calling APPLY on: (null? x)
In proc lookup-variable-value to lookup: null?
Found value (primitive #<procedure:null?>)
In proc lookup-variable-value to lookup: x
Found value (r t y u)
Calling APPLY on: (cons (car x) (append (cdr x) y))
In proc lookup-variable-value to lookup: cons
Found value (primitive #<procedure:cons>)
Calling APPLY on: (car x)
In proc lookup-variable-value to lookup: car
Found value (primitive #<procedure:car>)
In proc lookup-variable-value to lookup: x
Found value (r t y u)
Calling APPLY on: (append (cdr x) y)
In proc lookup-variable-value to lookup: append
Found value #0=(procedure (x y) ((if (null? x) y (cons (car x) (append (cdr x) y)))) ({(append y false true car cdr cons null? display > < =) #0# 89 #f #t (primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:display>) (primitive #<procedure:>>) (primitive #<procedure:<>) (primitive #<procedure:=>)}))
Calling APPLY on: (cdr x)
In proc lookup-variable-value to lookup: cdr
Found value (primitive #<procedure:cdr>)
In proc lookup-variable-value to lookup: x
Found value (r t y u)
In proc lookup-variable-value to lookup: y
Found value (z x c v b n m)
In proc EVAL-if to evaluate: (if (null? x) y (cons (car x) (append (cdr x) y)))
Calling APPLY on: (null? x)
In proc lookup-variable-value to lookup: null?
Found value (primitive #<procedure:null?>)
In proc lookup-variable-value to lookup: x
Found value (t y u)
Calling APPLY on: (cons (car x) (append (cdr x) y))
In proc lookup-variable-value to lookup: cons
Found value (primitive #<procedure:cons>)
Calling APPLY on: (car x)
In proc lookup-variable-value to lookup: car
Found value (primitive #<procedure:car>)
In proc lookup-variable-value to lookup: x
Found value (t y u)
Calling APPLY on: (append (cdr x) y)
In proc lookup-variable-value to lookup: append
Found value #0=(procedure (x y) ((if (null? x) y (cons (car x) (append (cdr x) y)))) ({(append y false true car cdr cons null? display > < =) #0# 89 #f #t (primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:display>) (primitive #<procedure:>>) (primitive #<procedure:<>) (primitive #<procedure:=>)}))
Calling APPLY on: (cdr x)
In proc lookup-variable-value to lookup: cdr
Found value (primitive #<procedure:cdr>)
In proc lookup-variable-value to lookup: x
Found value (t y u)
In proc lookup-variable-value to lookup: y
Found value (z x c v b n m)
In proc EVAL-if to evaluate: (if (null? x) y (cons (car x) (append (cdr x) y)))
Calling APPLY on: (null? x)
In proc lookup-variable-value to lookup: null?
Found value (primitive #<procedure:null?>)
In proc lookup-variable-value to lookup: x
Found value (y u)
Calling APPLY on: (cons (car x) (append (cdr x) y))
In proc lookup-variable-value to lookup: cons
Found value (primitive #<procedure:cons>)
Calling APPLY on: (car x)
In proc lookup-variable-value to lookup: car
Found value (primitive #<procedure:car>)
In proc lookup-variable-value to lookup: x
Found value (y u)
Calling APPLY on: (append (cdr x) y)
In proc lookup-variable-value to lookup: append
Found value #0=(procedure (x y) ((if (null? x) y (cons (car x) (append (cdr x) y)))) ({(append y false true car cdr cons null? display > < =) #0# 89 #f #t (primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:display>) (primitive #<procedure:>>) (primitive #<procedure:<>) (primitive #<procedure:=>)}))
Calling APPLY on: (cdr x)
In proc lookup-variable-value to lookup: cdr
Found value (primitive #<procedure:cdr>)
In proc lookup-variable-value to lookup: x
Found value (y u)
In proc lookup-variable-value to lookup: y
Found value (z x c v b n m)
In proc EVAL-if to evaluate: (if (null? x) y (cons (car x) (append (cdr x) y)))
Calling APPLY on: (null? x)
In proc lookup-variable-value to lookup: null?
Found value (primitive #<procedure:null?>)
In proc lookup-variable-value to lookup: x
Found value (u)
Calling APPLY on: (cons (car x) (append (cdr x) y))
In proc lookup-variable-value to lookup: cons
Found value (primitive #<procedure:cons>)
Calling APPLY on: (car x)
In proc lookup-variable-value to lookup: car
Found value (primitive #<procedure:car>)
In proc lookup-variable-value to lookup: x
Found value (u)
Calling APPLY on: (append (cdr x) y)
In proc lookup-variable-value to lookup: append
Found value #0=(procedure (x y) ((if (null? x) y (cons (car x) (append (cdr x) y)))) ({(append y false true car cdr cons null? display > < =) #0# 89 #f #t (primitive #<procedure:car>) (primitive #<procedure:cdr>) (primitive #<procedure:cons>) (primitive #<procedure:null?>) (primitive #<procedure:display>) (primitive #<procedure:>>) (primitive #<procedure:<>) (primitive #<procedure:=>)}))
Calling APPLY on: (cdr x)
In proc lookup-variable-value to lookup: cdr
Found value (primitive #<procedure:cdr>)
In proc lookup-variable-value to lookup: x
Found value (u)
In proc lookup-variable-value to lookup: y
Found value (z x c v b n m)
In proc EVAL-if to evaluate: (if (null? x) y (cons (car x) (append (cdr x) y)))
Calling APPLY on: (null? x)
In proc lookup-variable-value to lookup: null?
Found value (primitive #<procedure:null?>)
In proc lookup-variable-value to lookup: x
Found value ()
In proc lookup-variable-value to lookup: y
Found value (z x c v b n m)

;;; EVAL value:
(q w e r t y u z x c v b n m)

;;; EVAL input:
.
