#lang racket

; Exercise 4.18.  Consider an alternative strategy for scanning out definitions that translates
; the example in the text to

; (lambda <vars>
; 	(let ((u '*unassigned*) (v '*unassigned*))
; 		(let ((a <e1>) (b <e2>))
; 			(set! u a)
; 			(set! v b)
; 		)
; 		<e3>
; 	)
; )

; Here a and b are meant to represent new variable names, created by the interpreter, that do
; not appear in the user's program. Consider the solve procedure from section 3.5.4:

; (define (solve f y0 dt)
; 	(define y (integral (delay dy) y0 dt))
; 	(define dy (stream-map f y))
; 	y
; )

; Will this procedure work if internal definitions are scanned out as shown in this exercise?
; What if they are scanned out as shown in the text? Explain.

; S O L U T I O N

; The transformed procedure (if internal definitions are scanned out as shown in this exercise):

; (define (solve f y0 dt)
; 	(let ((y '*unassigned*) (dy '*unassigned*))
; 		(let ((a (integral (delay dy) y0 dt)) (b (stream-map f y)))
; 			(set! y a)
; 			(set! dy b)
; 		)
; 		y
; 	)
; )

; The transformed procedure (if internal definitions are scanned out as 
; implemented in Exercise 4.16 and also the code below in this file):

; (define (solve f y0 dt)
; 	(let ((y '*unassigned*) (dy '*unassigned*))
;		(set! y (integral (delay dy) y0 dt))
; 		(set! dy (stream-map f y))
;		y
; 	)
; )

; This program scans out internal definitions as shown in this exercise.

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (EVAL expression env)
	; (with-handlers ([exn:fail? (lambda (exn) 
	; 		(display "Failed to evaluate: ") (displayln expression) (exn-message exn))])
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
							; (display "Calling APPLY on: ")
							; (display expression)
							; (newline)
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
	; )
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

(define (make-assignment variable-name value)
	(list 'set! variable-name value)
)

(define (EVAL-assignment expression env)
	; (display "In proc EVAL-assignment to evaluate: ")
	; (display expression)
	; (newline)
	(set-variable-value!
		(assignment-variable expression)
		(EVAL (assignment-value expression) env)
		env
	)
	(string-append "Assigned value to: " (~a (assignment-variable expression)))
)

(define (set-variable-value! var val env)
	; (display "In proc set-variable-value! to set ")
	; (display var)
	; (display " to ")
	; (display val)
	; (newline)

	(let ((f (find-variable-position-in-env var env)))
		(if (null? f)
			(error "Cannot set variable before definition" var)
			(set-mcar! f (cons var val))
		)
	)
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

(define (make-definition variable-name value)
	(list 'define variable-name value)
)

(define (EVAL-definition expression env)
	; (display "In proc EVAL-definition to evaluate: ")
	; (display expression)
	; (newline)
	(define-variable!
		(definition-variable expression)
		(EVAL (definition-value expression) env)
		env
	)
	(string-append "Defined the variable: " (~a (definition-variable expression)))
)

(define (define-variable! var val env)
	; (display "In proc define-variable! to define ")
	; (display var)
	; (display " as ")
	; (display val)
	; (newline)

	(let ((ref (find-variable-position-in-frame var (first-frame env))))
		(if (null? ref)
			(add-binding-to-env! var val env)
			(set-mcar! ref (cons var val))
		)
	)
)

; Un-definition Expressions (to remove bindings from the environment)
(define (un-definition? expression)
	(tagged-list? expression 'make-unbound!)
)

(define (un-definition-variable expression)
	(cadr expression)
)

(define (EVAL-un-definition expression env)
	; (display "In proc EVAL-un-definition to evaluate: ")
	; (display expression)
	; (newline)
	(make-variable-unbound!
		(un-definition-variable expression)
		env
	)
	(string-append "Un-defined the variable: " (~a (un-definition-variable expression)))
)

(define (make-variable-unbound! var env)
	; Look for the variable only within the supplied frame (i.e. don't look beyond this
	; frame) and if found remove it and its value from this frame

	; (display "In proc make-variable-unbound! to remove ")
	; (display var)
	; (newline)
	(define (scan frame)
		(cond
			((null? frame) (error "Variable does not exist in supplied frame:" var))
			((eq? var (name-in-binding (first-binding frame)))
				; The first binding contains this variable. We can either null it out
				; or physically remove it. I choose to null it out.
				(set-mcar! frame (make-binding null null))
			)
			((null? (rest-bindings frame))
				; First binding does not contain this variable and we have reached the
				; last binding. So the variable does not exist in this frame
				(error "Variable does not exist in supplied frame:" var)
			)
			; If the second binding contains this variable, then remove the binding from
			; the frame by making the previous item point to the item ahead of the binding
			; being removed
			((eq? var (name-in-binding (first-binding (rest-bindings frame))))
				(set-mcdr! frame (rest-bindings (rest-bindings frame)))
			)
			(else (scan (rest-bindings frame)))
		)
	)

	(scan (first-frame env))
)

; Variable Expressions
(define (variable? expression) (symbol? expression))

(define (lookup-variable-value var env)
	; (display "In proc lookup-variable-value to lookup: ")
	; (display var)
	; (newline)
	(let ((ref (find-variable-position-in-env var env)))
		(if (null? ref)
			(error "Unbound name" var)
			(if (eq? (cdr (mcar ref)) '*unassigned*)
				(error "Cannot lookup unassigned name" var)
				(cdr (mcar ref))
			)
		)
	)
)

; 'delay' Expressions
; If 'delay' were a procedure, then, according to this model of evaluation, evaluating
; (delay <expression>) would automatically cause <expression> to be evaluated, which we
; don't want. So 'delay' needs to be a special form.
(define (delay? expression)
	(tagged-expression? expression 'delay)
)

(define (delay-predicate expression)
	(cadr expression)
)

(define (make-delay expression)
	(list 'delay expression)
)

(define (EVAL-delay expression env)
	(make-lambda null (list (delay-predicate expression)))
)

; 'force' Expressions
(define (force? expression)
	(tagged-expression? expression 'force)
)

(define (force-predicate expression)
	(cadr expression)
)

(define (make-force expression)
	(list 'force expression)
)

(define (EVAL-force expression env)
	; Note:
	; The 'force-predicate' could be one of two expression types:
	; 1. A "delay" expression
	; 2. A variable that points to a lambda expression (possibly produced by an earlier
	; "delay" call on some expression)
	; 3. A lambda expression directly

	; Case 1 ("delay" expression)
	; The inner EVAL produces a lambda expression
	; The outer EVAL creates a compound-procedure object from the lambda expression
	; supplied to it. Then APPLY executes the compound procedure

	; Case 2 (variable pointing to a lambda expression)
	; The inner EVAL looks up the variable and fetches the lambda expression from the
	; environment frame
	; The outer EVAL creates a compound-procedure object from the lambda expression
	; supplied to it. Then APPLY executes the compound procedure

	; Case 3 (A lambda expression)
	; In this case, we just need to call EVAL once to create the compound-procedure object
	; and then call APPLY on it

	(cond
		((or (delay? (force-predicate expression)) (variable? (force-predicate expression)))
			(APPLY
				(EVAL (EVAL (force-predicate expression) env) env)
				null
			)
		)
		((lambda? (force-predicate expression))
			(APPLY
				(EVAL (force-predicate expression) env)
				null
			)
		)
		(else
			(error "Invalid force predicate" expression)
		)
	)
)

; 'cons-stream' Expressions
(define (cons-stream? expression)
	(tagged-list? expression 'cons-stream)
)
(define (cons-stream-first-arg expression) (cadr expression))
(define (cons-stream-second-arg expression) (caddr expression))

(define (EVAL-cons-stream expression env)

	(define (substitute-names-from-env-for-values expression env)

		(define (substitute-names-from-frame-for-values expression frame)

			(define (substitute-name-for-value expression name value)
				(cond
					((or (self-evaluating? expression) (null? expression)) expression)
					((quoted? expression) expression)
					((variable? expression)
						(if (eq? expression name)
							value
							expression
						)
					)
					((pair? expression)
						(cons
							(substitute-name-for-value (car expression) name value)
							(substitute-name-for-value (cdr expression) name value)
						)
					)
					(else
						(error "Invalid argument to procedure substitute-name-for-value" expression)
					)
				)
			)

			(if (null? frame)
				expression
				(let ((first-binding (first-binding frame)))
					(if (null? first-binding)
						expression
						(let ((new-expression (substitute-name-for-value expression (name-in-binding first-binding) (value-in-binding first-binding))))
							(substitute-names-from-frame-for-values new-expression (rest-bindings frame))
						)
					)
				)
			)
		)

		(substitute-names-from-frame-for-values expression (first-frame env))
	)

	; We need to impose certain restrictions on the second argument to cons-stream
	; The second argument should be an expression that produces a stream. So I impose 
	; the condition that it should be a compound procedure
	(let ((first-arg (cons-stream-first-arg expression))
		  (second-arg (cons-stream-second-arg expression)))
		(cond
			((or (self-evaluating? second-arg) (quoted? second-arg))
				(error "Invalid second argument to cons-stream" expression)
			)
			((variable? second-arg)
				(error "Invalid second argument to cons-stream" expression)
			)
			((pair? second-arg)
				; We expect it to be a compound procedure and not a special form.
				; So a handler should not be found for it
				(let ((handler (get (operator second-arg) 'eval)))
					(if (not (null? handler))
						; handler found
						(error "Invalid second argument to cons-stream" expression)
						; handler not found so it must be a compound procedure
						(cons
							'stream-object
							(cons
								(EVAL first-arg env)
								(list
									(EVAL-delay
										(make-delay
								 			(cons
								 				(operator second-arg)
								 				; Note that the delayed expression will be evaluated in a different environment later
								 				; This future environment will not contain the bindings that are needed to evaulate the
												; operands. So we pre-process the operands by substituting any occurrences of names that
												; exist in the current environment (current frame only) with their corresponding values
								 				(substitute-names-from-env-for-values (operands second-arg) env)
								 			)
										)
										env
									)
								)
							)
						)
					)
				)
			)
			(else
				(error "Invalid second argument to cons-stream" expression)
			)
		)
	)
)

; 'stream-object' Expressions
(define (stream-object? expression)
	(tagged-expression? expression 'stream-object)
)
(define (EVAL-stream-object expression env)
	; stream objects can be understood only by stream-car and stream-cdr.
	; Don't evaluate a stream object directly.
	expression
)

; 'stream-car' Expressions
(define (stream-car? expression)
	(tagged-expression? expression 'stream-car)
)
(define (stream-car-predicate expression) (cadr expression))
(define (EVAL-stream-car expression env)
	(cadr (EVAL (stream-car-predicate expression) env))
)

; 'stream-cdr' Expressions
(define (stream-cdr? expression)
	(tagged-expression? expression 'stream-cdr)
)
(define (stream-cdr-predicate expression) (cadr expression))
(define (EVAL-stream-cdr expression env)
	(EVAL-force (make-force (caddr (EVAL (stream-cdr-predicate expression) env))) env)
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
	(displayln "Making if expression with: ")
	(displayln "Predicate:")
	(displayln predicate)
	(displayln "Consequent")
	(displayln consequent)
	(displayln "Alternative")
	(displayln alternative)
	(list 'if predicate consequent alternative)
)

(define (EVAL-if expression env)
	; (display "In proc EVAL-if to evaluate: ")
	; (display expression)
	; (newline)
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
	; (display "In proc EVAL-lambda to evaluate: ")
	; (display expression)
	; (newline)
	(if (null? (lambda-body expression))
		(error "No expressions in lambda body" expression)
		(make-procedure
			(lambda-parameters expression)
			(lambda-body expression)
			env
		)
	)
)

(define (scan-out-defines procedure-body)
	(define (contains-internal-definitions? block-of-expressions)
		(if (null? block-of-expressions)
			false
			(if (definition? (car block-of-expressions))
				true
				(contains-internal-definitions? (cdr block-of-expressions))
			)
		)
	)
	(define (scan-for-internal-definitions block-of-expressions)
		(if (null? block-of-expressions)
			null
			(if (definition? (car block-of-expressions))
				(cons
					(car block-of-expressions)
					(scan-for-internal-definitions (cdr block-of-expressions))
				)
				(scan-for-internal-definitions (cdr block-of-expressions))
			)
		)
	)
	(define (make-unassigned-var-bindings-for-let variable-definitions)
		(if (null? variable-definitions)
			null
			(cons
				(make-unassigned-var-binding (car variable-definitions))
				(make-unassigned-var-bindings-for-let (cdr variable-definitions))
			)
		)
	)
	(define	(make-unassigned-var-binding var-definition)
		(list (definition-variable var-definition) ''*unassigned*)
	)
	(define (replace-defines-with-set!s block-of-expressions)
		(if (null? block-of-expressions)
			null
			(if (definition? (car block-of-expressions))
				(cons
					(make-assignment
						(definition-variable (car block-of-expressions))
						(definition-value (car block-of-expressions))
					)
					(replace-defines-with-set!s (cdr block-of-expressions))
				)
				(cons
					(car block-of-expressions)
					(replace-defines-with-set!s (cdr block-of-expressions))
				)
			)
		)
	)

	(if (contains-internal-definitions? procedure-body)
		(list (make-let
			(make-unassigned-var-bindings-for-let
				(scan-for-internal-definitions procedure-body)
			)
			(replace-defines-with-set!s procedure-body)
		))
		procedure-body
	)
)

(define (make-procedure parameters body env)
	; (let ((transformed-body (scan-out-defines body)))
	(let ((transformed-body body))
		(displayln "Making procedure with: ")
		(displayln "Parameters:")
		(displayln parameters)
		(displayln "Body:")
		(displayln transformed-body)
		(list 'procedure parameters transformed-body env)
	)
)

; let Expressions
(define (let? expression) (tagged-list? expression 'let))
(define (named-let? expression)
	(if (let? expression)
		(if (and (not (null? (cdddr expression))) (symbol? (cadr expression)))
			true
			false
		)
		false
	)
)
(define (let-var-bindings expression)
	(if (named-let? expression)
		(caddr expression)
		(cadr expression)
	)
)
(define (let-body expression)
	(if (named-let? expression)
		(cdddr expression)
		(cddr expression)
	)
)
(define (named-let-proc-name expression)
	(cadr expression)
)

(define (let->combination expression)
	(let (
			(parameters (map car (let-var-bindings expression)))
			(arguments (map cadr (let-var-bindings expression)))
		 )
		(if (named-let? expression)
			; The idea is to create the following:
			; (begin
			;	(define <named-let-proc-name> (lambda () ...))
			;	(<named-let-proc-name> arguments)
			; )
			(make-begin
				(list
					(make-definition
						(named-let-proc-name expression)
						(make-lambda parameters (let-body expression))
					)
					(cons (named-let-proc-name expression) arguments)
				)
			)
			; Ordinary let
			(cons (make-lambda parameters (let-body expression)) arguments)
		)
	)
)

(define (make-let var-bindings body)
	(displayln "Making let expression with: ")
	(displayln "Var-bindings:")
	(displayln var-bindings)
	(displayln "Body:")
	(displayln body)
	(cons 'let (cons var-bindings body))
)

(define (EVAL-let expression env)
	(EVAL (let->combination expression) env)
)

; let* Expressions
(define (let*? expression) (tagged-list? expression 'let*))
(define (let*-var-bindings expression) (cadr expression))
(define (let*-first-var-binding var-bindings) (car var-bindings))
(define (let*-rest-var-bindings var-bindings) (cdr var-bindings))
(define (let*-body expression) (cddr expression))

; (let* ((x 3)
;        (y (+ x 2))
;        (z (+ x y 5)))
;   (* x z))

; can be written as:

; (let ((x 3))
; 	(let ((y (+ x 2)))
; 		(let ((z (+ x y 5)))
; 			(* x z)
; 		)
; 	)
; )

(define (let*->nested-lets expression)
	(if (not (null? (let*-rest-var-bindings (let*-var-bindings expression))))
		(make-let
			(list (let*-first-var-binding (let*-var-bindings expression)))
			(list (let*->nested-lets
				(make-let* (let*-rest-var-bindings (let*-var-bindings expression)) (let*-body expression))
			))
		)
		(make-let
			(list (let*-first-var-binding (let*-var-bindings expression)))
			(let*-body expression)
		)
	)
)

(define (make-let* var-bindings body)
	(displayln "Making let* expression with: ")
	(displayln "Var-bindings:")
	(displayln var-bindings)
	(displayln "Body:")
	(displayln body)
	(cons 'let* (cons var-bindings body))
)

(define (EVAL-let* expression env)
	(display "In proc EVAL-let* to evaluate: ")
	(display expression)
	(newline)
	(EVAL (let*->nested-lets expression) env)
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
		(else
			(EVAL (first-exp exps) env)
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
(define (make-begin seq)
	(displayln "Making begin expression with sequence: ")
	(displayln seq)
	(cons 'begin seq)
)

(define (EVAL-begin expression env)
	; (display "In proc EVAL-begin to evaluate: ")
	; (display expression)
	; (newline)
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

(define (expand-cond-clauses clauses)
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
						(expand-cond-clauses rest)
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
)

(define (special-cond-clause-syntax? clause)
	(eq? (car (cdr clause)) `=>)
)

(define (recipient-proc clause)
	(caddr clause)
)

(define (EVAL-cond expression env)
	(display "In proc EVAL-cond to evaluate: ")
	(displayln expression)
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
	(displayln expression)
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
	(displayln expression)
	(EVAL (or->if expression) env)
)

; do blocks
(define (EVAL-do expression env)
	(cond
		((do-while? expression) (EVAL-do-while expression env))
		((do-until? expression) (EVAL-do-until expression env))
		(else
			(error "Invalid do expression: " expression)
		)
	)
)

; do-while blocks
; The 'do-while' construct can be as follows:
;
; (do
;   (<one or more statements>)
;   while (condition)
; )
; 
(define (do-while? expression)
	(and
		(tagged-list? expression 'do)
		(eq? (last-but-one-term expression) 'while)
	)
)

(define (do-while-statement-block expression)
	(define (statement-block expression)
		(if (not (eq? (car expression) 'while))
			(cons (car expression) (statement-block (cdr expression)))
			null
		)
	)
	(statement-block (cdr expression))
)

(define (do-while-condition expression)
	(define (condition expression)
		(if (eq? (car expression) 'while)
			(if (not (pair? (cdr expression)))
				(error "Invalid do-while block: No terminating condition found")
				(cadr expression)
			)
			(condition (cdr expression))
		)
	)
	(condition (cdr expression))
)

(define (do-while->combination expression)
	; Use the while block construct to convert it as follows:
	; (begin
	;   <statements>
	;   (while (condition)
	;     <statements>
	;   )
	; )
	(make-begin
		(append
			(do-while-statement-block expression)
			(list
				(make-while
					(do-while-condition expression)
					(do-while-statement-block expression)
				)
			)
		)
	)
)

(define (EVAL-do-while expression env)
	(display "In proc EVAL-do-while to evaluate: ")
	(displayln expression)
	(EVAL (do-while->combination expression) env)
)

; do-until blocks
; The 'do-until' construct can be as follows:
;
; (do
;   (<one or more statements>)
;   until (condition)
; )
; 
(define (do-until? expression)
	(and
		(tagged-list? expression 'do)
		(eq? (last-but-one-term expression) 'until)
	)
)

(define (do-until-statement-block expression)
	(define (statement-block expression)
		(if (not (eq? (car expression) 'until))
			(cons (car expression) (statement-block (cdr expression)))
			null
		)
	)
	(statement-block (cdr expression))
)

(define (do-until-condition expression)
	(define (condition expression)
		(if (eq? (car expression) 'until)
			(if (not (pair? (cdr expression)))
				(error "Invalid do-until block: No terminating condition found")
				(cadr expression)
			)
			(condition (cdr expression))
		)
	)
	(condition (cdr expression))
)

(define (do-until->combination expression)
	; Use the while block construct to convert it as follows:
	; (begin
	;   <statements>
	;   (while (not (condition))
	;     <statements>
	;   )
	; )
	(make-begin
		(append
			(do-until-statement-block expression)
			(list
				(make-while
					(list 'not (do-until-condition expression))
					(do-until-statement-block expression)
				)
			)
		)
	)
)

(define (EVAL-do-until expression env)
	(display "In proc EVAL-do-until to evaluate: ")
	(displayln expression)
	(EVAL (do-until->combination expression) env)
)

; for blocks
; The 'for' construct can be as follows:
;
; (for (count-var <start>) (count-var <end>) inc
;   (<one or more statements>)
; )
;

(define (for? expression) (tagged-list? expression 'for))

(define (for-count-var expression)
	(caadr expression)
)

(define (for-count-start expression)
	(cadr (cadr expression))
)

(define (for-count-end expression)
	(cadr (caddr expression))
)

(define (for-inc-proc expression)
	(cadddr expression)
)

(define (for-statement-block expression)
	(cddddr expression)
)

(define (for->combination expression)
	; The derived expression for this would be:
	;
	; (begin
	;   (define count-var <start>)
	;   (define (for-block)
	;     (if (<= count-var <end>)
	;		(begin
	;         <statements>
	;         (set! count-var (inc count-var))
	;         (for-block)
	;		)
	;       'done
	;     )
	;   )
	;   (for-block)
	; )
	;
	; which is the same as:
	;
	; (begin
	;   (define count-var <start>)
	;   (define for-block
	;     (lambda ()
	;       (if (<= count-var <end>)
	;         (begin
	;           <statements>
	;           (set! count-var (inc count-var))
	;           (for-block)
	;         )
	;         'done
	;       )
	;     )
	;   )
	;   (for-block)
	; )
	;
	; where the variable 'count-var' is available for use inside the statement block and
	; 'inc' is a one-argument proc that increments what is supplied to it. The user of 
	; the 'for' construct must supply a definition for inc
	
	(make-begin
		(list ; 3 items
			(make-definition
				(for-count-var expression)
				(for-count-start expression)
			)
			(make-definition
				'for-block
				(make-lambda
					null
					(list
						(make-if
							(list '<= (for-count-var expression) (for-count-end expression))
							(make-begin
								(append
									(for-statement-block expression)
									(list
										(make-assignment
											(for-count-var expression)
											(list (for-inc-proc expression) (for-count-var expression))
										)
										'(for-block)
									)
								)
							)
							''done
						)
					)
				)
			)
			'(for-block)
		)
	)
)

(define (EVAL-for expression env)
	(display "In proc EVAL-for to evaluate: ")
	(displayln expression)
	(EVAL (for->combination expression) env)
)

; while blocks
; The 'while' construct can be as follows:
;
; (while (condition)
;    <one or more statements>
; )

(define (while? expression) (tagged-list? expression 'while))

(define (while-statement-block expression)
	(cddr expression)
)

(define (while-condition expression)
	(cadr expression)
)

(define (while->combination expression)
	; The derived expression for this would be:
	;
	; (define (while-block)
	;  (if (condition)
	;    (begin
	;      <statements>
	;      (while-block)
	;    )
	;	 'done
	;  )
	; )
	; (while-block)
	;
	; which is the same as:
	;
	; (define while-block
	;  (lamdba ()
	;    (if (condition)
	;      (begin
	;        <statements>
	;        (while-block)
	;      )
	;      'done
	;    )
	;  )
	; )
	; (while-block)
	(make-begin
		(list
			(make-definition
				'while-block
				(make-lambda
					null
					(list
						(make-if
							(while-condition expression)
							(make-begin
								(append
									(while-statement-block expression)
									(list '(while-block))
								)
							)
							''done
						)
					)
				)
			)
			'(while-block)
		)
	)
)

(define (make-while condition statements)
	(displayln "Making while expression with: ")
	(displayln "Condition:")
	(displayln condition)
	(displayln "Statements:")
	(displayln statements)
	(cons 'while (cons condition statements))
)

(define (EVAL-while expression env)
	(display "In proc EVAL-while to evaluate: ")
	(displayln expression)
	(EVAL (while->combination expression) env)
)

; Compound Procedures
(define (application? expression) (pair? expression))
(define (operator expression) (car expression))
(define (operands expression) (cdr expression))
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
	; (display "Applying primitive proc: ")
	; (display proc)
	; (newline)
	(apply
		(primitive-implementation proc)
		args
	)
)

(define primitive-procedures
	(list
		(list 'abs abs)
		(list 'assoc assoc)
		(list 'car car)
		(list 'cadr cadr)
		(list 'cdr cdr)
		(list 'cons cons)
		(list 'display display)
		(list 'displayln displayln)
		(list 'eq? eq?)
		(list 'list list)
		(list 'newline newline)
		(list 'not not)
		(list 'null? null?)
		(list 'void void)
		(list '> >)
		(list '< <)
		(list '>= >=)
		(list '<= <=)
		(list '= =)
		(list '+ +)
		(list '- -)
		(list '* *)
		(list '/ /)
	)
)

(define (primitive-procedure-names)
	(map car primitive-procedures)
)

(define (primitive-procedure-objects)
	(map (lambda (proc) (list 'primitive (car (cdr proc)))) primitive-procedures)
)

; Environment related procedures
(define (make-binding name value) (cons name value))
(define (name-in-binding binding) (car binding))
(define (value-in-binding binding) (cdr binding))
(define (first-binding frame) (mcar frame))
(define (rest-bindings frame) (mcdr frame))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (add-binding-to-env! var val env)
	(mappend! (first-frame env) (mlist (cons var val)))
)

(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
		(cons (make-frame vars vals) base-env)
		(if (< (length vars) (length vals))
			(error "Too many arguments supplied" vars vals)
			(error "Too few arguments supplied" vars vals)
		)
	)
)

(define (make-frame variables values)
	; Internal structure of frame: list of bindings (instead of a pair of lists)
	(define (make-frame-internal vars vals)
		(if (null? vars)
			null
			(mcons
				(make-binding (car vars) (car vals))
				(make-frame-internal (cdr vars) (cdr vals))
			)
		)
	)
	(if (null? variables)
		; Create an empty frame
		(mcons
			(make-binding null null)
			null
		)
		(make-frame-internal variables values)
	)
)

(define (find-variable-position-in-env var env)
	; Traverse the supplied environment to find the frame that contains a binding for 'var'
	(define (env-loop env)
		(if (eq? env the-empty-environment)
			null
			(let ((frame (first-frame env)))
				(let ((variable-position (find-variable-position-in-frame var frame)))
					(if (eq? variable-position null)
						(env-loop (enclosing-environment env))
						variable-position
					)
				)
			)
		)
	)

	(env-loop env)
)

(define (find-variable-position-in-frame var frame)
	; Look for the variable only within the supplied frame (i.e. don't look beyond this
	; frame)
	(define (scan frame)
		(cond
			((null? frame) null)
			((eq? var (name-in-binding (first-binding frame))) frame)
			(else (scan (rest-bindings frame)))
		)
	)

	(scan frame)
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

(define (print-environment env)
	(if (eq? env the-empty-environment)
		(displayln "[No more frames to display]")
		(if (eq? (enclosing-environment env) the-empty-environment)
			(displayln "[Last Frame (Not displaying this frame containing primitive procedures)]")
			(let ((frame (first-frame env)))
				(print-frame frame)
				(print-environment (enclosing-environment env))
			)
		)
	)
)

(define (print-frame frame)
	(if (null? frame)
		(displayln "[End of Frame]")
		(begin
			(print-binding (first-binding frame))
			(print-frame (rest-bindings frame))
		)
	)
)

(define (print-binding binding)
	(display "Name: ")
	(displayln (car binding))
	(display "Value: ")
	(displayln (cdr binding))
)

; Other
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (list-of-values exps env)
	(evaluate-l-to-r-list-of-values exps env)
)

(define (tagged-list? expression tag)
	(if (pair? expression)
		(eq? (car expression) tag)
		false
	)
)

(define (tagged-expression? expression tag)
	; A tagged expression is a tagged list which has exactly one item after the tag
	(if (tagged-list? expression tag)
		(if (null? (cdr expression))
			false
			(if (null? (cddr expression)) ; ensure that there is only one item after the tag
				true
				false
			)
		)
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

(define (last-but-one-term expression)
	(if (pair? expression)
		(if (pair? (cdr expression))
			(if (null? (cddr expression))
				(car expression)
				(last-but-one-term (cdr expression))
			)
			(error "Invalid expression: " expression)
		)
		(error "Invalid expression: " expression)
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

(put 'and 'eval EVAL-and)
(put 'begin 'eval EVAL-begin)
(put 'cond 'eval EVAL-cond)
(put 'cons-stream 'eval EVAL-cons-stream)
(put 'define 'eval EVAL-definition)
(put 'delay 'eval EVAL-delay)
(put 'do 'eval EVAL-do)
(put 'for 'eval EVAL-for)
(put 'force 'eval EVAL-force)
(put 'if 'eval EVAL-if)
(put 'lambda 'eval EVAL-lambda)
(put 'let 'eval EVAL-let)
(put 'let* 'eval EVAL-let*)
(put 'make-unbound! 'eval EVAL-un-definition)
(put 'or 'eval EVAL-or)
(put 'set! 'eval EVAL-assignment)
(put 'stream-car 'eval EVAL-stream-car)
(put 'stream-cdr 'eval EVAL-stream-cdr)
(put 'stream-object 'eval EVAL-stream-object)
(put 'variable 'eval lookup-variable-value)
(put 'while 'eval EVAL-while)

; procedure "APPLY"
(define (APPLY procedure arguments)
	(cond
		((primitive-procedure? procedure)
			(apply-primitive-procedure procedure arguments)
		)
		((compound-procedure? procedure)
			(begin
				(let ((new-execution-environment (extend-environment
						(procedure-parameters procedure)
						arguments
						(procedure-environment procedure)
						)))
					; (displayln "Extended the environment:")
					; (print-environment new-execution-environment)
					(EVAL-sequence
						(procedure-body procedure)
						new-execution-environment
					)
				)
			)
		)
		(else
			(error "Unknown procedure type -- APPLY" procedure)
		)
	)
)

; Driver Loop
(define input-prompt "[Metacircular Evaluator Input] >>>")
(define output-prompt "[Metacircular Evaluator Output] >>> ")

(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		(display "Starting to evaluate: ")
		(displayln (~a input))
		(if (eq? input 'quit)
			'Done
			(let ((output (EVAL input the-global-environment)))
				(newline)
				(display "Finished evaluating: ")
				(displayln (~a input))
				(announce-output output-prompt)
				(user-print output)
				(newline)
				(displayln "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
				(driver-loop)
			)
		)
	)
)

(define (prompt-for-input string)
	(newline)
	(display string)
	(newline)
)

(define (announce-output string)
	(newline)
	(display string)
)

(define (user-print object)
	(if (compound-procedure? object)
		(display
			(list
				'procedure
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
(driver-loop)

; Test Results

Welcome to DrRacket, version 8.1 [cs].
Language: racket, with debugging; memory limit: 128 MB.

[Metacircular Evaluator Input] >>>
(define b 69)
Starting to evaluate: (define b 69)

Finished evaluating: (define b 69)

[Metacircular Evaluator Output] >>> Defined the variable: b
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
b
Starting to evaluate: b

Finished evaluating: b

[Metacircular Evaluator Output] >>> 69
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
(force (delay b))
Starting to evaluate: (force (delay b))
Making procedure with: 
Parameters:
()
Body:
(b)

Finished evaluating: (force (delay b))

[Metacircular Evaluator Output] >>> 69
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
(delay b)
Starting to evaluate: (delay b)

Finished evaluating: (delay b)

[Metacircular Evaluator Output] >>> (lambda () b)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
((lambda (x) (* x x x)) 11)
Starting to evaluate: ((lambda (x) (* x x x)) 11)
Making procedure with: 
Parameters:
(x)
Body:
((* x x x))

Finished evaluating: ((lambda (x) (* x x x)) 11)

[Metacircular Evaluator Output] >>> 1331
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
(delay ((lambda (x) (* x x x)) 11))
Starting to evaluate: (delay ((lambda (x) (* x x x)) 11))

Finished evaluating: (delay ((lambda (x) (* x x x)) 11))

[Metacircular Evaluator Output] >>> (lambda () ((lambda (x) (* x x x)) 11))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
((lambda () ((lambda (x) (* x x x)) 11)))
Starting to evaluate: ((lambda () ((lambda (x) (* x x x)) 11)))
Making procedure with: 
Parameters:
()
Body:
(((lambda (x) (* x x x)) 11))
Making procedure with: 
Parameters:
(x)
Body:
((* x x x))

Finished evaluating: ((lambda () ((lambda (x) (* x x x)) 11)))

[Metacircular Evaluator Output] >>> 1331
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
(delay ((lambda (x) (* x x x)) 11))
Starting to evaluate: (delay ((lambda (x) (* x x x)) 11))

Finished evaluating: (delay ((lambda (x) (* x x x)) 11))

[Metacircular Evaluator Output] >>> (lambda () ((lambda (x) (* x x x)) 11))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
(force (delay ((lambda (x) (* x x x)) 11)))
Starting to evaluate: (force (delay ((lambda (x) (* x x x)) 11)))
Making procedure with: 
Parameters:
()
Body:
(((lambda (x) (* x x x)) 11))
Making procedure with: 
Parameters:
(x)
Body:
((* x x x))

Finished evaluating: (force (delay ((lambda (x) (* x x x)) 11)))

[Metacircular Evaluator Output] >>> 1331
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
(force (delay (force (delay b))))
Starting to evaluate: (force (delay (force (delay b))))
Making procedure with: 
Parameters:
()
Body:
((force (delay b)))
Making procedure with: 
Parameters:
()
Body:
(b)

Finished evaluating: (force (delay (force (delay b))))

[Metacircular Evaluator Output] >>> 69
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
(define the-empty-stream '())
(define (stream-empty? s)
	(if (eq? s the-empty-stream)
		true
		false
	)
)

(define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream
			low
			(stream-enumerate-interval (+ low 1) high)
		)
	)
)

(define (add-streams s1 s2)
	(if (stream-empty? s2)
		s1
		(if (stream-empty? s1)
			s2
			(cons-stream
				(+ (stream-car s1) (stream-car s2))
				(add-streams (stream-cdr s1) (stream-cdr s2))
			)
		)
	)	
)

(define stream-of-five-elements (stream-enumerate-interval 5 9))
stream-of-five-elements
(stream-cdr stream-of-five-elements)
(stream-cdr (stream-cdr stream-of-five-elements))
(stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))
(stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements))))
(stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))))

(stream-car stream-of-five-elements)
(stream-car (stream-cdr stream-of-five-elements))
(stream-car (stream-cdr (stream-cdr stream-of-five-elements)))
(stream-car (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements))))
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))))

(define one-to-six (stream-enumerate-interval 1 6))
(define eleven-to-sixteen (stream-enumerate-interval 11 16))
(define sum-stream (add-streams one-to-six eleven-to-sixteen))
sum-stream
(stream-car sum-stream)
(stream-car (stream-cdr sum-stream))
(stream-car (stream-cdr (stream-cdr sum-stream)))
(stream-car (stream-cdr (stream-cdr (stream-cdr sum-stream))))
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr sum-stream)))))
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr sum-stream))))))
Starting to evaluate: (define the-empty-stream (quote ()))

Finished evaluating: (define the-empty-stream (quote ()))

[Metacircular Evaluator Output] >>> Defined the variable: the-empty-stream
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (stream-empty? s) (if (eq? s the-empty-stream) true false))
Making procedure with: 
Parameters:
(s)
Body:
((if (eq? s the-empty-stream) true false))

Finished evaluating: (define (stream-empty? s) (if (eq? s the-empty-stream) true false))

[Metacircular Evaluator Output] >>> Defined the variable: stream-empty?
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (stream-enumerate-interval low high) (if (> low high) the-empty-stream (cons-stream low (stream-enumerate-interval (+ low 1) high))))
Making procedure with: 
Parameters:
(low high)
Body:
((if (> low high) the-empty-stream (cons-stream low (stream-enumerate-interval (+ low 1) high))))

Finished evaluating: (define (stream-enumerate-interval low high) (if (> low high) the-empty-stream (cons-stream low (stream-enumerate-interval (+ low 1) high))))

[Metacircular Evaluator Output] >>> Defined the variable: stream-enumerate-interval
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (add-streams s1 s2) (if (stream-empty? s2) s1 (if (stream-empty? s1) s2 (cons-stream (+ (stream-car s1) (stream-car s2)) (add-streams (stream-cdr s1) (stream-cdr s2))))))
Making procedure with: 
Parameters:
(s1 s2)
Body:
((if (stream-empty? s2) s1 (if (stream-empty? s1) s2 (cons-stream (+ (stream-car s1) (stream-car s2)) (add-streams (stream-cdr s1) (stream-cdr s2))))))

Finished evaluating: (define (add-streams s1 s2) (if (stream-empty? s2) s1 (if (stream-empty? s1) s2 (cons-stream (+ (stream-car s1) (stream-car s2)) (add-streams (stream-cdr s1) (stream-cdr s2))))))

[Metacircular Evaluator Output] >>> Defined the variable: add-streams
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define stream-of-five-elements (stream-enumerate-interval 5 9))

Finished evaluating: (define stream-of-five-elements (stream-enumerate-interval 5 9))

[Metacircular Evaluator Output] >>> Defined the variable: stream-of-five-elements
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: stream-of-five-elements

Finished evaluating: stream-of-five-elements

[Metacircular Evaluator Output] >>> (stream-object 5 (lambda () (stream-enumerate-interval (+ 5 1) 9)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-cdr stream-of-five-elements)
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 9))

Finished evaluating: (stream-cdr stream-of-five-elements)

[Metacircular Evaluator Output] >>> (stream-object 6 (lambda () (stream-enumerate-interval (+ 6 1) 9)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-cdr (stream-cdr stream-of-five-elements))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 6 1) 9))

Finished evaluating: (stream-cdr (stream-cdr stream-of-five-elements))

[Metacircular Evaluator Output] >>> (stream-object 7 (lambda () (stream-enumerate-interval (+ 7 1) 9)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 6 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 7 1) 9))

Finished evaluating: (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))

[Metacircular Evaluator Output] >>> (stream-object 8 (lambda () (stream-enumerate-interval (+ 8 1) 9)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 6 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 7 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 8 1) 9))

Finished evaluating: (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements))))

[Metacircular Evaluator Output] >>> (stream-object 9 (lambda () (stream-enumerate-interval (+ 9 1) 9)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 6 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 7 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 8 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 9 1) 9))

Finished evaluating: (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))))

[Metacircular Evaluator Output] >>> ()
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car stream-of-five-elements)

Finished evaluating: (stream-car stream-of-five-elements)

[Metacircular Evaluator Output] >>> 5
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car (stream-cdr stream-of-five-elements))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 9))

Finished evaluating: (stream-car (stream-cdr stream-of-five-elements))

[Metacircular Evaluator Output] >>> 6
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car (stream-cdr (stream-cdr stream-of-five-elements)))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 6 1) 9))

Finished evaluating: (stream-car (stream-cdr (stream-cdr stream-of-five-elements)))

[Metacircular Evaluator Output] >>> 7
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 6 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 7 1) 9))

Finished evaluating: (stream-car (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements))))

[Metacircular Evaluator Output] >>> 8
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 6 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 7 1) 9))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 8 1) 9))

Finished evaluating: (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr stream-of-five-elements)))))

[Metacircular Evaluator Output] >>> 9
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define one-to-six (stream-enumerate-interval 1 6))

Finished evaluating: (define one-to-six (stream-enumerate-interval 1 6))

[Metacircular Evaluator Output] >>> Defined the variable: one-to-six
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define eleven-to-sixteen (stream-enumerate-interval 11 16))

Finished evaluating: (define eleven-to-sixteen (stream-enumerate-interval 11 16))

[Metacircular Evaluator Output] >>> Defined the variable: eleven-to-sixteen
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define sum-stream (add-streams one-to-six eleven-to-sixteen))

Finished evaluating: (define sum-stream (add-streams one-to-six eleven-to-sixteen))

[Metacircular Evaluator Output] >>> Defined the variable: sum-stream
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: sum-stream

Finished evaluating: sum-stream

[Metacircular Evaluator Output] >>> (stream-object 12 (lambda () (add-streams (stream-cdr (stream-object 1 (lambda () (stream-enumerate-interval (+ 1 1) 6)))) (stream-cdr (stream-object 11 (lambda () (stream-enumerate-interval (+ 11 1) 16)))))))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car sum-stream)

Finished evaluating: (stream-car sum-stream)

[Metacircular Evaluator Output] >>> 12
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car (stream-cdr sum-stream))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 1 (lambda () (stream-enumerate-interval (+ 1 1) 6)))) (stream-cdr (stream-object 11 (lambda () (stream-enumerate-interval (+ 11 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 1 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 11 1) 16))

Finished evaluating: (stream-car (stream-cdr sum-stream))

[Metacircular Evaluator Output] >>> 14
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car (stream-cdr (stream-cdr sum-stream)))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 1 (lambda () (stream-enumerate-interval (+ 1 1) 6)))) (stream-cdr (stream-object 11 (lambda () (stream-enumerate-interval (+ 11 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 1 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 11 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 2 (lambda () (stream-enumerate-interval (+ 2 1) 6)))) (stream-cdr (stream-object 12 (lambda () (stream-enumerate-interval (+ 12 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 2 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 12 1) 16))

Finished evaluating: (stream-car (stream-cdr (stream-cdr sum-stream)))

[Metacircular Evaluator Output] >>> 16
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car (stream-cdr (stream-cdr (stream-cdr sum-stream))))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 1 (lambda () (stream-enumerate-interval (+ 1 1) 6)))) (stream-cdr (stream-object 11 (lambda () (stream-enumerate-interval (+ 11 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 1 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 11 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 2 (lambda () (stream-enumerate-interval (+ 2 1) 6)))) (stream-cdr (stream-object 12 (lambda () (stream-enumerate-interval (+ 12 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 2 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 12 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 3 (lambda () (stream-enumerate-interval (+ 3 1) 6)))) (stream-cdr (stream-object 13 (lambda () (stream-enumerate-interval (+ 13 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 3 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 13 1) 16))

Finished evaluating: (stream-car (stream-cdr (stream-cdr (stream-cdr sum-stream))))

[Metacircular Evaluator Output] >>> 18
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr sum-stream)))))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 1 (lambda () (stream-enumerate-interval (+ 1 1) 6)))) (stream-cdr (stream-object 11 (lambda () (stream-enumerate-interval (+ 11 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 1 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 11 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 2 (lambda () (stream-enumerate-interval (+ 2 1) 6)))) (stream-cdr (stream-object 12 (lambda () (stream-enumerate-interval (+ 12 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 2 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 12 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 3 (lambda () (stream-enumerate-interval (+ 3 1) 6)))) (stream-cdr (stream-object 13 (lambda () (stream-enumerate-interval (+ 13 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 3 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 13 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 4 (lambda () (stream-enumerate-interval (+ 4 1) 6)))) (stream-cdr (stream-object 14 (lambda () (stream-enumerate-interval (+ 14 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 4 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 14 1) 16))

Finished evaluating: (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr sum-stream)))))

[Metacircular Evaluator Output] >>> 20
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr sum-stream))))))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 1 (lambda () (stream-enumerate-interval (+ 1 1) 6)))) (stream-cdr (stream-object 11 (lambda () (stream-enumerate-interval (+ 11 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 1 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 11 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 2 (lambda () (stream-enumerate-interval (+ 2 1) 6)))) (stream-cdr (stream-object 12 (lambda () (stream-enumerate-interval (+ 12 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 2 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 12 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 3 (lambda () (stream-enumerate-interval (+ 3 1) 6)))) (stream-cdr (stream-object 13 (lambda () (stream-enumerate-interval (+ 13 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 3 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 13 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 4 (lambda () (stream-enumerate-interval (+ 4 1) 6)))) (stream-cdr (stream-object 14 (lambda () (stream-enumerate-interval (+ 14 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 4 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 14 1) 16))
Making procedure with: 
Parameters:
()
Body:
((add-streams (stream-cdr (stream-object 5 (lambda () (stream-enumerate-interval (+ 5 1) 6)))) (stream-cdr (stream-object 15 (lambda () (stream-enumerate-interval (+ 15 1) 16))))))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 5 1) 6))
Making procedure with: 
Parameters:
()
Body:
((stream-enumerate-interval (+ 15 1) 16))

Finished evaluating: (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr sum-stream))))))

[Metacircular Evaluator Output] >>> 22
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
.
