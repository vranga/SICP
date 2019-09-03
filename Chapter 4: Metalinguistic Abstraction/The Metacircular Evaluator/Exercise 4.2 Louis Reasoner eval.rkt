#lang racket

; Exercise 4.2.  Louis Reasoner plans to reorder the cond clauses in eval so that the clause
; for procedure applications appears before the clause for assignments. He argues that this
; will make the interpreter more efficient: Since programs usually contain more applications
; than assignments, definitions, and so on, his modified eval will usually check fewer
; clauses than the original eval before identifying the type of an expression.

; a. What is wrong with Louis's plan? (Hint: What will Louis's evaluator do with the
; expression (define x 3)?)

; b. Louis is upset that his plan didn't work. He is willing to go to any lengths to make
; his evaluator recognize procedure applications before it checks for most other kinds of
; expressions. Help him by changing the syntax of the evaluated language so that procedure
; applications start with call. For example, instead of (factorial 3) we will now have to
; write (call factorial 3) and instead of (+ 1 2) we will have to write (call + 1 2).

; E X P L A N A T I O N
; a. Louis Reasoner's implementation with the reordered cond clauses will cause infinite
; loops. When (define x 3) is evaluated, it will be interpreted as a procedure application
; so eval will be called on it again causing an infinite loop.
;

; S O L U T I O N

(define (eval exp env)
	(cond
		((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((application? exp)
			(apply
				(eval (operator exp) env)
				(list-of-values (operands exp) env)
			)
		)
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
			(make-procedure
				(lambda-parameters exp)
				(lambda-body exp)
				env
			)
		)
		((begin? exp) (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		(else
			(error "Unknown expression type -- EVAL" exp)
		)
	)
)

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
	(define-variable!
		(definition-variable exp)
		(eval (definition-value exp) env)
		env
	)
	'ok
)

(define (variable? exp) (symbol? exp))

(define (lookup-variable-value var env)
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

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

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
