#lang racket

; Exercise 4.9.  Many languages support a variety of iteration constructs, such as do, for,
; while, and until. In Scheme, iterative processes can be expressed in terms of ordinary
; procedure calls, so special iteration constructs provide no essential gain in computational
; power. On the other hand, such constructs are often convenient. Design some iteration
; constructs, give examples of their use, and show how to implement them as derived
; expressions.

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

(define (make-assignment variable-name value)
	(list 'set! variable-name value)
)

(define (EVAL-assignment exp env)
	; (display "In proc EVAL-assignment to evaluate: ")
	; (display exp)
	; (newline)
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
				(else (scan (cdr vars) (mcdr vals)))
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

(define (make-definition variable-name value)
	(list 'define variable-name value)
)

(define (EVAL-definition exp env)
	; (display "In proc EVAL-definition to evaluate: ")
	; (display exp)
	; (newline)
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
	; (display "In proc lookup-variable-value to evaluate: ")
	; (display var)
	; (newline)
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
	; (display "In proc EVAL-if to evaluate: ")
	; (display exp)
	; (newline)
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
	; (display "In proc EVAL-lambda to evaluate: ")
	; (display exp)
	; (newline)
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
(define (named-let? exp)
	(if (let? exp)
		(if (not (null? (cdddr exp)))
			true
			false
		)
		false
	)
)
(define (let-var-bindings exp)
	(if (named-let? exp)
		(caddr exp)
		(cadr exp)
	)
)
(define (let-body exp)
	(if (named-let? exp)
		(cdddr exp)
		(cddr exp)
	)
)
(define (named-let-proc-name exp)
	(cadr exp)
)

(define (let->combination exp)
	(let (
			(parameters (map car (let-var-bindings exp)))
			(arguments (map cadr (let-var-bindings exp)))
		 )
		(if (named-let? exp)
			; The idea is to create the following:
			; (begin
			;	(define <named-let-proc-name> (lambda () ...))
			;	(<named-let-proc-name> arguments)
			; )
			(make-begin
				(list
					(make-definition
						(named-let-proc-name exp)
						(make-lambda parameters (let-body exp))
					)
					(cons (named-let-proc-name exp) arguments)
				)
			)
			; Ordinary let
			(cons (make-lambda parameters (let-body exp)) arguments)
		)
	)
)

(define (make-let var-bindings body)
	(list 'let var-bindings body)
)

(define (EVAL-let exp env)
	(display "In proc EVAL-let to evaluate: ")
	(display exp)
	(newline)
	(display "Converted ")
	(display exp)
	(display " to: ")
	(newline)
	(display (let->combination exp))
	(newline)
	(EVAL (let->combination exp) env)
)

; let* Expressions
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-var-bindings exp) (cadr exp))
(define (let*-first-var-binding var-bindings) (car var-bindings))
(define (let*-rest-var-bindings var-bindings) (cdr var-bindings))
(define (let*-body exp) (caddr exp))

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

(define (let*->nested-lets exp)
	(if (not (null? (let*-rest-var-bindings (let*-var-bindings exp))))
		(make-let
			(list (let*-first-var-binding (let*-var-bindings exp)))
			(let*->nested-lets
				(make-let* (let*-rest-var-bindings (let*-var-bindings exp)) (let*-body exp))
			)
		)
		(make-let
			(list (let*-first-var-binding (let*-var-bindings exp)))
			(let*-body exp)
		)
	)
)

(define (make-let* var-bindings body)
	(list 'let* var-bindings body)
)

(define (EVAL-let* exp env)
	(display "In proc EVAL-let* to evaluate: ")
	(display exp)
	(newline)
	(EVAL (let*->nested-lets exp) env)
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
(define (make-begin seq) (cons 'begin seq))

(define (EVAL-begin exp env)
	; (display "In proc EVAL-begin to evaluate: ")
	; (display exp)
	; (newline)
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
	(display "In proc EVAL-cond to evaluate: ")
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

(define (make-and exp)
	(cons 'and exp)
)

(define (EVAL-and exp env)
	(display "In proc EVAL-and to evaluate: ")
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

(define (make-or exp)
	(cons 'or exp)
)

(define (EVAL-or exp env)
	(display "In proc EVAL-or to evaluate: ")
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

; do blocks
(define (EVAL-do exp env)
	(cond
		((do-while? exp) (EVAL-do-while exp env))
		((do-until? exp) (EVAL-do-until exp env))
		(else
			(error "Invalid do expression: " exp)
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
(define (do-while? exp)
	(and
		(tagged-list? exp 'do)
		(eq? (last-but-one-term exp) 'while)
	)
)

(define (do-while-statement-block exp)
	(define (statement-block exp)
		(if (not (eq? (car exp) 'while))
			(cons (car exp) (statement-block (cdr exp)))
			null
		)
	)
	(statement-block (cdr exp))
)

(define (do-while-condition exp)
	(define (condition exp)
		(if (eq? (car exp) 'while)
			(if (not (pair? (cdr exp)))
				(error "Invalid do-while block: No terminating condition found")
				(cadr exp)
			)
			(condition (cdr exp))
		)
	)
	(condition (cdr exp))
)

(define (do-while->combination exp)
	; Use the while block construct to convert it as follows:
	; (begin
	;   <statements>
	;   (while (condition)
	;     <statements>
	;   )
	; )
	(make-begin
		(append
			(do-while-statement-block exp)
			(list
				(make-while
					(do-while-condition exp)
					(do-while-statement-block exp)
				)
			)
		)
	)
)

(define (EVAL-do-while exp env)
	; (display "In proc EVAL-do-while to evaluate: ")
	; (display exp)
	; (newline)
	(display "EVAL-do-while converted ")
	(display exp)
	(display " to: ")
	(newline)
	(display (do-while->combination exp))
	(newline)
	(EVAL (do-while->combination exp) env)
)

; do-until blocks
; The 'do-until' construct can be as follows:
;
; (do
;   (<one or more statements>)
;   until (condition)
; )
; 
(define (do-until? exp)
	(and
		(tagged-list? exp 'do)
		(eq? (last-but-one-term exp) 'until)
	)
)

(define (do-until-statement-block exp)
	(define (statement-block exp)
		(if (not (eq? (car exp) 'until))
			(cons (car exp) (statement-block (cdr exp)))
			null
		)
	)
	(statement-block (cdr exp))
)

(define (do-until-condition exp)
	(define (condition exp)
		(if (eq? (car exp) 'until)
			(if (not (pair? (cdr exp)))
				(error "Invalid do-until block: No terminating condition found")
				(cadr exp)
			)
			(condition (cdr exp))
		)
	)
	(condition (cdr exp))
)

(define (do-until->combination exp)
	; Use the while block construct to convert it as follows:
	; (begin
	;   <statements>
	;   (while (not (condition))
	;     <statements>
	;   )
	; )
	(make-begin
		(append
			(do-until-statement-block exp)
			(list
				(make-while
					(list 'not (do-until-condition exp))
					(do-until-statement-block exp)
				)
			)
		)
	)
)

(define (EVAL-do-until exp env)
	; (display "In proc EVAL-do-until to evaluate: ")
	; (display exp)
	; (newline)
	(display "EVAL-do-until converted ")
	(display exp)
	(display " to: ")
	(newline)
	(display (do-until->combination exp))
	(newline)
	(EVAL (do-until->combination exp) env)
)

; for blocks
; The 'for' construct can be as follows:
;
; (for (count-var <start>) (count-var <end>) inc
;   (<one or more statements>)
; )
;

(define (for? exp) (tagged-list? exp 'for))

(define (for-count-var exp)
	(caadr exp)
)

(define (for-count-start exp)
	(cadr (cadr exp))
)

(define (for-count-end exp)
	(cadr (caddr exp))
)

(define (for-inc-proc exp)
	(cadddr exp)
)

(define (for-statement-block exp)
	(cddddr exp)
)

(define (for->combination exp)
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
				(for-count-var exp)
				(for-count-start exp)
			)
			(make-definition
				'for-block
				(make-lambda
					null
					(list
						(make-if
							(list '<= (for-count-var exp) (for-count-end exp))
							(make-begin
								(append
									(for-statement-block exp)
									(list
										(make-assignment
											(for-count-var exp)
											(list (for-inc-proc exp) (for-count-var exp))
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

(define (EVAL-for exp env)
	; (display "In proc EVAL-for to evaluate: ")
	; (display exp)
	; (newline)
	(display "EVAL-for converted ")
	(display exp)
	(display " to: ")
	(newline)
	(display (for->combination exp))
	(newline)
	(EVAL (for->combination exp) env)
)

; while blocks
; The 'while' construct can be as follows:
;
; (while (condition)
;    <one or more statements>
; )

(define (while? exp) (tagged-list? exp 'while))

(define (while-statement-block exp)
	(cddr exp)
)

(define (while-condition exp)
	(cadr exp)
)

(define (while->combination exp)
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
							(while-condition exp)
							(make-begin
								(append
									(while-statement-block exp)
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
	(cons 'while (cons condition statements))
)

(define (EVAL-while exp env)
	; (display "In proc EVAL-while to evaluate: ")
	; (display exp)
	; (newline)
	(display "EVAL-while converted ")
	(display exp)
	(display " to: ")
	(newline)
	(display (while->combination exp))
	(newline)
	(EVAL (while->combination exp) env)
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
		(list 'car car)
		(list 'cdr cdr)
		(list 'cadr cadr)
		(list 'cons cons)
		(list 'not not)
		(list 'eq? eq?)
		(list 'null? null?)
		(list 'display display)
		(list 'newline newline)
		(list 'assoc assoc)
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

(define (last-but-one-term exp)
	(if (pair? exp)
		(if (pair? (cdr exp))
			(if (null? (cddr exp))
				(car exp)
				(last-but-one-term (cdr exp))
			)
			(error "Invalid expression: " exp)
		)
		(error "Invalid expression: " exp)
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
(put 'let 'eval EVAL-let)
(put 'let* 'eval EVAL-let*)
(put 'do 'eval EVAL-do)
(put 'for 'eval EVAL-for)
(put 'while 'eval EVAL-while)

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
(define input-prompt "[Metacircular Evaluator Input] >>>")
(define output-prompt "[Metacircular Evaluator Output] >>> ")
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
	(display string)
	(newline)
)

(define (announce-output string)
	(newline)
	(display string)
	; (newline)
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

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 4096 MB.
> (driver-loop)

[Metacircular Evaluator Input] >>>
(define x 1)

[Metacircular Evaluator Output] >>> ok
[Metacircular Evaluator Input] >>>
x

[Metacircular Evaluator Output] >>> 1
[Metacircular Evaluator Input] >>>
(do
    (display 'x:)
    (display x)
    (newline)
    (set! x (+ x 2))
    (display '(x after setting:))
    (display x)
    (newline)
    while (< x 10)
)
EVAL-do-while converted (do (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) while (< x 10)) to: 
(begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while (< x 10) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline)))
x:1
(x after setting:)3
EVAL-while converted (while (< x 10) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline)) to: 
(begin (define while-block (lambda () (if (< x 10) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))) (while-block))
x:3
(x after setting:)5
x:5
(x after setting:)7
x:7
(x after setting:)9
x:9
(x after setting:)11

[Metacircular Evaluator Output] >>> done
[Metacircular Evaluator Input] >>>
(define (inc val)
    (+ val 1)
)

[Metacircular Evaluator Output] >>> ok
[Metacircular Evaluator Input] >>>
(inc 100)

[Metacircular Evaluator Output] >>> 101
[Metacircular Evaluator Input] >>>
(for (i 1) (i 40) inc
    (display i)
    (newline)
)
EVAL-for converted (for (i 1) (i 40) inc (display i) (newline)) to: 
(begin (define i 1) (define for-block (lambda () (if (<= i 40) (begin (display i) (newline) (set! i (inc i)) (for-block)) 'done))) (for-block))
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40

[Metacircular Evaluator Output] >>> done
[Metacircular Evaluator Input] >>>
(define x 1)

[Metacircular Evaluator Output] >>> ok
[Metacircular Evaluator Input] >>>
x

[Metacircular Evaluator Output] >>> 1
[Metacircular Evaluator Input] >>>
x

[Metacircular Evaluator Output] >>> 1
[Metacircular Evaluator Input] >>>
(do
    (display 'x:)
    (display x)
    (newline)
    (set! x (+ x 2))
    (display '(x after setting:))
    (display x)
    (newline)
    until (> x 30)
)
EVAL-do-until converted (do (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) until (> x 30)) to: 
(begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while (not (> x 30)) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline)))
x:1
(x after setting:)3
EVAL-while converted (while (not (> x 30)) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline)) to: 
(begin (define while-block (lambda () (if (not (> x 30)) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))) (while-block))
x:3
(x after setting:)5
x:5
(x after setting:)7
x:7
(x after setting:)9
x:9
(x after setting:)11
x:11
(x after setting:)13
x:13
(x after setting:)15
x:15
(x after setting:)17
x:17
(x after setting:)19
x:19
(x after setting:)21
x:21
(x after setting:)23
x:23
(x after setting:)25
x:25
(x after setting:)27
x:27
(x after setting:)29
x:29
(x after setting:)31

[Metacircular Evaluator Output] >>> done
[Metacircular Evaluator Input] >>>
x

[Metacircular Evaluator Output] >>> 31
[Metacircular Evaluator Input] >>>
(while (< x 50)
    (display x)
    (newline)
    (set! x (+ x 3))
)
EVAL-while converted (while (< x 50) (display x) (newline) (set! x (+ x 3))) to: 
(begin (define while-block (lambda () (if (< x 50) (begin (display x) (newline) (set! x (+ x 3)) (while-block)) 'done))) (while-block))
31
34
37
40
43
46
49

[Metacircular Evaluator Output] >>> done
[Metacircular Evaluator Input] >>>
.
