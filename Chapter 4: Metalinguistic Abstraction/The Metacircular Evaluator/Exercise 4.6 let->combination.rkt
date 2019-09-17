#lang racket

; Exercise 4.6.  Let expressions are derived expressions, because

; (let ((<var1> <exp1>) ... (<varn> <expn>))
;   <body>)

; is equivalent to

; ((lambda (<var1> ... <varn>)
;    <body>)
;  <exp1>
 
;  <expn>)

; Implement a syntactic transformation let->combination that reduces evaluating let
; expressions to evaluating combinations of the type shown above, and add the appropriate
; clause to eval to handle let expressions.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (EVAL exp env)
	(cond
		((self-evaluating? exp) exp)
		((quoted? exp) (text-of-quotation exp))
		((variable? exp) (lookup-variable-value exp env))
		((pair? exp)
			(let ((handler (get (operator exp) 'eval)))
				(if (not (null? handler))
					; handler found so pass the expression to it
					(handler exp env)
					; handler not found so it must be a procedure application
					(APPLY
						(EVAL (operator exp) env)
						(list-of-values (operands exp) env)
					)
				)
			)
		)
		(else
			(error "Unknown expression type -- EVAL" exp)
		)
	)
)

; Self Evaluating Expressions
(define (self-evaluating? exp)
	(cond
		((number? exp) true)
		((string? exp) true)
		(else false)
	)
)

; Quoted Expressions
(define (quoted? exp)
	(tagged-list? exp 'quote)
)

(define (text-of-quotation exp) (cadr exp))

; Assignment Expressions
(define (assignment? exp)
	(tagged-list? exp 'set!)
)
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (EVAL-assignment exp env)
	(display "In proc EVAL-assignment to execute: ")
	(display exp)
	(newline)
	(set-variable-value!
		(assignment-variable exp)
		(EVAL (assignment-value exp) env)
		env
	)
	'ok
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

(define (EVAL-definition exp env)
	(display "In proc EVAL-definition to execute: ")
	(display exp)
	(newline)
	(define-variable!
		(definition-variable exp)
		(EVAL (definition-value exp) env)
		env
	)
	'ok
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
					(mcar vals)
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

(define (EVAL-if exp env)
	(display "In proc EVAL-if to execute: ")
	(display exp)
	(newline)
	(if (true? (EVAL (if-predicate exp) env))
		(EVAL (if-consequent exp) env)
		(EVAL (if-alternative exp) env)
	)
)

; lambda Expressions
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body))
)

(define (EVAL-lambda exp env)
	(display "In proc EVAL-lambda to execute: ")
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

; let Expressions
(define (let? exp) (tagged-list? exp 'let))
(define (let-var-mappings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
	(let (
			(parameters (map car (let-var-mappings exp)))
			(arguments (map cadr (let-var-mappings exp)))
		 )
		(cons (make-lambda parameters (let-body exp)) arguments)
	)
)

(define (EVAL-let exp env)
	(display "In proc EVAL-let to execute: ")
	(display exp)
	(newline)
	(EVAL (let->combination exp) env)
)

; "begin" Expressions
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
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

(define (EVAL-begin exp env)
	(display "In proc EVAL-begin to execute: ")
	(display exp)
	(newline)
	(EVAL-sequence (begin-actions exp) env)
)

; "cond" Expressions
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

; The following statement:
; (cond ((< a b) exp1)
; 		((assoc 'b '((a 1) (b 2))) => single-argument-proc)
; 		((< m n) exp2)
;       (else exp3))
; is equivalent to:
; (if (< a b)
;	exp1
;	(if (not (eq? (assoc 'b '((a 1) (b 2))) false))
;		(single-argument-proc (assoc 'b '((a 1) (b 2))))
;		(if (< m n)
;			exp2
;			exp3
;		)
;	)
; )

(define (expand-clauses clauses)
	(if (null? clauses)
		'false ; no else clause
		(let ((first (car clauses)) (rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clause isn't last -- COND->IF" clauses)
				)
				(if (special-cond-clause-syntax? first)
					; <handle the <test> => <recipient> syntax>
					(make-if
						(list 'not (list 'eq? (cond-predicate first) 'false))
						(list (recipient-proc first) (cond-predicate first))
						(expand-clauses rest)
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
)

(define (special-cond-clause-syntax? clause)
	(eq? (car (cdr clause)) `=>)
)

(define (recipient-proc clause)
	(caddr clause)
)

(define (EVAL-cond exp env)
	(display "In proc EVAL-cond to execute: ")
	(display exp)
	(newline)
	(EVAL (cond->if exp) env)
)

; "and" Expressions
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
(define (and? exp) (tagged-list? exp 'and))
(define (and-first-predicate exp)
	(if (not (null? (cdr exp)))
		(cadr exp)
		'()
	)
)
(define (and-rest-predicates exp)
	(if (not (null? (cdr exp)))
		(cddr exp)
		'()
	)
)

(define (EVAL-and exp env)
	(display "In proc EVAL-and to execute: ")
	(display exp)
	(newline)
	(if (null? exp)
		; If there are no expressions, return true
		true
		(let ((fp (and-first-predicate exp)) (rp (and-rest-predicates exp)))
			(if (not (null? fp))
				(if (true? (EVAL (and-first-predicate exp) env))
					(EVAL-and (make-and (and-rest-predicates exp)) env)
					false
				)
				true
			)
		)
	)
)

(define (make-and exp)
	(cons 'and exp)
)

; "or" Expressions
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
(define (or? exp) (tagged-list? exp 'or))
(define (or-first-predicate exp)
	(if (not (null? (cdr exp)))
		(cadr exp)
		'()
	)
)
(define (or-rest-predicates exp)
	(if (not (null? (cdr exp)))
		(cddr exp)
		'()
	)
)

(define (EVAL-or exp env)
	(display "In proc EVAL-or to execute: ")
	(display exp)
	(newline)
	(if (null? exp)
		; If there are no expressions, return false
		false
		(let ((fp (or-first-predicate exp)) (rp (or-rest-predicates exp)))
			(if (not (null? fp))
				(if (true? (EVAL (or-first-predicate exp) env))
					true
					(EVAL-or (make-or (or-rest-predicates exp)) env)
				)
				false
			)
		)
	)
)

(define (make-or exp)
	(cons 'or exp)
)

; Compound Procedures
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (compound-procedure? p)
	(tagged-list? p 'procedure)
)

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

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

(define primitive-procedures
	(list
		(list 'car car)
		(list 'cdr cdr)
		(list 'cadr cadr)
		(list 'cons cons)
		(list 'not not)
		(list 'eq? eq?)
		(list 'null? null?)
		(list 'display display)
		(list 'assoc assoc)
		(list '> >)
		(list '< <)
		(list '= =)
		(list '* *)
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

; Other
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (list-of-values exps env)
	(evaluate-l-to-r-list-of-values exps env)
)

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false
	)
)

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
(put 'set! 'eval EVAL-assignment)
(put 'define 'eval EVAL-definition)
(put 'if 'eval EVAL-if)
(put 'cond 'eval EVAL-cond)
(put 'and 'eval EVAL-and)
(put 'or 'eval EVAL-or)
(put 'begin 'eval EVAL-begin)
(put 'lambda 'eval EVAL-lambda)
(put 'let 'eval EVAL-let)

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

; Driver Loop
(define input-prompt ";;; EVAL input:")
(define output-prompt ";;; EVAL value:")
(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		(let ((output (EVAL input the-global-environment)))
			(announce-output output-prompt)
			(user-print output)
		)
	)
	(driver-loop)
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

(define letexp1
	'(let ((frame (first-frame env)))
		(scan
			(frame-variables frame)
			(frame-values frame)
		)
	)
)

(define letexp2
	'(let ((first (car clauses)) (rest (cdr clauses)))
		(if (cond-else-clause? first)
			(if (null? rest)
				(sequence->exp (cond-actions first))
				(error "ELSE clause isn't last -- COND->IF" clauses)
			)
			(if (special-cond-clause-syntax? first)
				; <handle the <test> => <recipient> syntax>
				(make-if
					(list 'not (list 'eq? (cond-predicate first) 'false))
					(list (recipient-proc first) (cond-predicate first))
					(expand-clauses rest)
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

(define letexp3 '(let ((a 10) (b 20) (c 30) (d 45)) (* a b c d)))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 4096 MB.
> (let->combination letexp1)
'((lambda (frame) (scan (frame-variables frame) (frame-values frame))) (first-frame env))
> (let->combination letexp2)
'((lambda (first rest)
    (if (cond-else-clause? first)
      (if (null? rest) (sequence->exp (cond-actions first)) (error "ELSE clause isn't last -- COND->IF" clauses))
      (if (special-cond-clause-syntax? first)
        (make-if
         (list 'not (list 'eq? (cond-predicate first) 'false))
         (list (recipient-proc first) (cond-predicate first))
         (expand-clauses rest))
        (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))))
  (car clauses)
  (cdr clauses))
> (let->combination letexp3)
'((lambda (a b c d) (* a b c d)) 10 20 30 45)
> (driver-loop)


;;; EVAL input:
(let ((a 10) (b 20) (c 30) (d 45)) (* a b c d))
In proc EVAL-let to execute: (let ((a 10) (b 20) (c 30) (d 45)) (* a b c d))
Type not found: (lambda (a b c d) (* a b c d))
In proc EVAL-lambda to execute: (lambda (a b c d) (* a b c d))
Type not found: *
In proc lookup-variable-value to execute: #<procedure:exp>
In proc lookup-variable-value to execute: #<procedure:exp>
In proc lookup-variable-value to execute: #<procedure:exp>
In proc lookup-variable-value to execute: #<procedure:exp>
In proc lookup-variable-value to execute: #<procedure:exp>

;;; EVAL value:
270000

;;; EVAL input:
.
