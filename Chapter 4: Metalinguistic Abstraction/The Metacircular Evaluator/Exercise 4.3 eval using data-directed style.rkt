#lang racket

; Exercise 4.3. Rewrite eval so that the dispatch is done in data-directed style. Compare
; this with the data-directed differentiation procedure of exercise 2.73. (You may use the
; car of a compound expression as the type of the expression, as is appropriate for the syntax
; implemented in this section.).

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (eval exp env)
	(cond
		((self-evaluating? exp) exp)
		((quoted? exp) (text-of-quotation exp))
		; ((variable? exp) (lookup-variable-value exp env))
		; ((assignment? exp) (eval-assignment exp env))
		; ((definition? exp) (eval-definition exp env))
		; ((if? exp) (eval-if exp env))
		; ((lambda? exp)
		;	(make-procedure
		;		(lambda-parameters exp)
		;		(lambda-body exp)
		;		env
		;	)
		;)
		; ((begin? exp) (eval-sequence (begin-actions exp) env))
		; ((cond? exp) (eval (cond->if exp) env))
		; ((application? exp)
		;	(apply
		;		(eval (operator exp) env)
		;		(list-of-values (operands exp) env)
		;	)
		; )
		(else
			; (error "Unknown expression type -- EVAL" exp)
			(let ((handler (get (operator exp) 'eval)))
				(if (not (null? handler))
					(handler exp env)
					(apply
						(eval (operator exp) env)
						(list-of-values (operands exp) env)
					)
				)
			)
		)
	)
)

(define (operator exp)
	(cond
		((variable? exp) 'variable)
		(else
			(car exp)
		)
	)
)

(define (operands exp) (cdr exp))

(define (self-evaluating? exp)
	(cond
		((number? exp) true)
		((string? exp) true)
		(else false)
	)
)

(define (quoted? exp)
	(tagged-list? exp 'quote)
)

(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp)
	(tagged-list? exp 'set!)
)
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
	(display "In proc eval-assignment to execute: ")
	(display exp)
	(newline)
	(set-variable-value!
		(assignment-variable exp)
		(eval (assignment-value exp) env)
		env
	)
	'ok
)

(define (definition? exp)
	(tagged-list? exp 'define)
)

(define (definition-variable exp)
	(if (symbol? (cadr exp))
		(cadr exp)
		(caadr exp)
	)
)

(define (definition-value exp)
	(if (symbol? (cadr exp))
		(caddr exp)
		(make-lambda
			(cdadr exp)		; formal parameters
			(cddr exp)		; body
		)
	)
)

(define (eval-definition exp env)
	(display "In proc eval-definition to execute: ")
	(display exp)
	(newline)
	(define-variable!
		(definition-variable exp)
		(eval (definition-value exp) env)
		env
	)
	'ok
)

(define (variable? exp) (symbol? exp))

(define (lookup-variable-value var env)
	(display "In proc lookup-variable-value to execute: ")
	(display exp)
	(newline)
	(define (env-loop env)
		(define (scan vars vals)
			(cond
				((null? vars)
					(env-loop (enclosing-environment env))
				)
				((eq? var (car vars))
					(car vals)
				)
				(else
					(scan (cdr vars) (cdr vals))
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

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
	(if (not (null? (cdddr exp)))
		(cadddr exp)
		'false
	)
)
(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative)
)

(define (eval-if exp env)
	(display "In proc eval-if to execute: ")
	(display exp)
	(newline)
	(if (true? (eval (if-predicate exp) env))
		(eval (if-consequent exp) env)
		(eval (if-alternative exp) env)
	)
)

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body))
)

(define (eval-lambda exp env)
	(display "In proc eval-lambda to execute: ")
	(display exp)
	(newline)
	(make-procedure
		(lambda-parameters exp)
		(lambda-body exp)
		env
	)
)

(define (make-procedure parameters body env)
	(list 'procedure parameters body env)
)

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
	(cond
		((last-exp? exps) (eval (first-exp exps) env))
		(else (eval (first-exp exps) env)
			(eval-sequence (rest-exps exps) env)
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

(define (eval-begin exp env)
	(display "In proc eval-begin to execute: ")
	(display exp)
	(newline)
	(eval-sequence (begin-actions exp) env)
)

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else)
)
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
	(expand-clauses (cond-clauses exp))
)

(define (expand-clauses clauses)
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
					(expand-clauses rest)
				)
			)
		)
	)
)

(define (eval-cond exp env)
	(display "In proc eval-cond to execute: ")
	(display exp)
	(newline)
	(eval (cond->if exp) env)
)

(define (application? exp) (pair? exp))

(define (list-of-values exps env)
	(evaluate-l-to-r-list-of-values exps env)
)

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false
	)
)

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(define (scan vars vals)
			(cond
				((null? vars)
					(add-binding-to-frame! var val frame)
				)
				((eq? var (car vars))
					(set-mcar! vals val)
				)
				(else (scan (cdr vars) (cdr vals)))
			)
		)
		(scan
			(frame-variables frame)
			(frame-values frame)
		)
	)
)

(define (set-variable-value! var val env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond
				((null? vars)
					(env-loop (enclosing-environment env))
				)
				((eq? var (car vars))
					(set-mcar! vals val)
				)
				(else (scan (cdr vars) (cdr vals)))
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

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
	(set-mcar! frame (cons var (car frame)))
	(set-mcdr! frame (cons val (cdr frame)))
)

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (evaluate-l-to-r-list-of-values exps env)
	(if (no-operands? exps)
		'()
		(let ((first (eval (first-operand exps) env)))
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
				(eval (first-operand exps) env)
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
					(display "Type not found: ")
					(display t)
					(newline)
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
(put 'set! 'eval eval-assignment)
(put 'define 'eval eval-definition)
(put 'if 'eval eval-if)
(put 'cond 'eval eval-cond)
(put 'begin 'eval eval-begin)
(put 'lambda 'eval eval-lambda)

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

(define a '(set! x 5))
(define d '(define y 6))
(define i '(if true (display 'yes) (display 'no)))
(define v 'mud)
(define l '(lambda (x) (display "Executing lambda proc")))
(define b '(begin (display '(hi there))))
(define c '(cond ((> 5 2) (display 'yes)) (else (display 'no))))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 4096 MB.
> (eval a 'env)
In proc eval-assignment to execute: (set! x 5)
. . car: contract violation
  expected: pair?
  given: 'env
> (eval d 'env)
In proc eval-definition to execute: (define y 6)
. . car: contract violation
  expected: pair?
  given: 'env
> (eval i 'env)
In proc eval-if to execute: (if true (display 'yes) (display 'no))
In proc lookup-variable-value to execute: #<procedure:exp>
. . car: contract violation
  expected: pair?
  given: 'env
> (eval v 'env)
In proc lookup-variable-value to execute: #<procedure:exp>
. . car: contract violation
  expected: pair?
  given: 'env
> (eval l 'env)
In proc eval-lambda to execute: (lambda (x) (display Executing lambda proc))
'(procedure (x) ((display "Executing lambda proc")) env)
> (eval b 'env)
In proc eval-begin to execute: (begin (display '(hi there)))
Type not found: display
In proc lookup-variable-value to execute: #<procedure:exp>
. . car: contract violation
  expected: pair?
  given: 'env
> (eval c 'env)
In proc eval-cond to execute: (cond ((> 5 2) (display 'yes)) (else (display 'no)))
In proc eval-if to execute: (if (> 5 2) (display 'yes) (display 'no))
Type not found: >
In proc lookup-variable-value to execute: #<procedure:exp>
. . car: contract violation
  expected: pair?
  given: 'env
> 
