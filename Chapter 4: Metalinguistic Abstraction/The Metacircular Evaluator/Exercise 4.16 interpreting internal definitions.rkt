#lang racket

; Exercise 4.16.  In this exercise we implement the method just described for interpreting
; internal definitions. We assume that the evaluator supports let (see exercise 4.6).

; a. Change lookup-variable-value (section 4.1.3) to signal an error if the value it finds is
; the symbol *unassigned*.

; b. Write a procedure scan-out-defines that takes a procedure body and returns an equivalent
; one that has no internal definitions, by making the transformation described above.

; c. Install scan-out-defines in the interpreter, either in make-procedure or in procedure-body
; (see section 4.1.3). Which place is better? Why?

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (EVAL expression env)
	(with-handlers ([exn:fail? (lambda (exn) 
			(display "Failed to evaluate: ") (displayln expression) (exn-message exn))])
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
			(error "Unbound variable" var)
			(if (eq? (cdr (mcar ref)) '*unassigned*)
				(error "Cannot lookup unassigned variable " var)
				(cdr (mcar ref))
			)
		)
	)
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
	(let ((transformed-body (scan-out-defines body)))
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

(put 'variable 'eval lookup-variable-value)
(put 'set! 'eval EVAL-assignment)
(put 'define 'eval EVAL-definition)
(put 'make-unbound! 'eval EVAL-un-definition)
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
				(announce-output output-prompt)
				(user-print output)
				(newline)
				(display "Finished evaluating: ")
				(displayln (~a input))
				(displayln "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
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
(driver-loop)

; Test Results

Welcome to DrRacket, version 8.1 [cs].
Language: racket, with debugging; memory limit: 128 MB.

[Metacircular Evaluator Input] >>>
quit
Starting to evaluate: quit
'Done
> (define cond1
	'(cond
		((eq? a b) (proc1 a))
		((> x y) (display x))
		(else
			(display (+ x y))
		)
	)
)

(define cond2
	'(cond
		((eq? a b) (proc1 a))
		((> x y) (display x))
		((assoc 'b '((a 1) (b 2))) => cadr)
		(else
			(display (+ x y))
		)
	)
)

(cond->if cond1)
(cond->if cond2)

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

(define letexp4 '(let ((a 10) (b 20) (c 30) (d 45)) (displayln 'Multiplying) (* a b c d)))

(define letexp5
	'(let ((f (square 4)))
		(+
			(cube f)
			(* 2 f)
		)
	)
)

(let->combination letexp1)
(let->combination letexp2)
(let->combination letexp3)
(let->combination letexp4)
(let->combination letexp5)

(define letstar1
	'(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
		(* x z)
	 )
)

(define letstar2
	'(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
		(displayln 'In-letstar2)
		(* x z)
	 )
)

(define letstarnested
	'(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
		(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
			(displayln 'In-letstar2)
			(* x z)
	 	)
	 )
)

(define name-let-exp
	'(let fib-iter ((a 1) (b 0) (count n))
		(if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1))
		)
	)
)

letstar1
(make-let* (let*-var-bindings letstar1) (let*-body letstar1))
letstar2
(make-let* (let*-var-bindings letstar2) (let*-body letstar2))
letstarnested
(make-let* (let*-var-bindings letstarnested) (let*-body letstarnested))
(let*->nested-lets letstar1)
(let*->nested-lets letstar2)
(let*->nested-lets letstarnested)

(define L1 '(lambda (a b) (displayln 'Entered-F1) (* a b)))
(lambda-parameters L1)
(lambda-body L1)
(scan-out-defines (lambda-body L1))
(define L2 '(lambda (b) (* b b)))
(lambda-parameters L2)
(lambda-body L2)
(scan-out-defines (lambda-body L2))
(define L3 '(lambda (a b c d) (define e 3) (define f 4) (displayln 'Entered-F1) (define g 5) (* a b c d e f g h) (define h 6)))
(lambda-parameters L3)
(lambda-body L3)
(scan-out-defines (lambda-body L3))
(define L4 '(lambda (a b) (define e 3) (define f 4) (define g 5) (* a b c d e f g h) (define h 6)))
(lambda-parameters L4)
(lambda-body L4)
(scan-out-defines (lambda-body L4))
(define L5 '(lambda (b) (define e 3) (* b e)))
(lambda-parameters L5)
(lambda-body L5)
(scan-out-defines (lambda-body L5))
(define L6 '(lambda () (define e 3) e))
(lambda-parameters L6)
(lambda-body L6)
(scan-out-defines (lambda-body L6))
(define L7 '(lambda () (display 'Hi)))
(lambda-parameters L7)
(lambda-body L7)
(scan-out-defines (lambda-body L7))
Making if expression with: 
Predicate:
(> x y)
Consequent
(display x)
Alternative
(display (+ x y))
Making if expression with: 
Predicate:
(eq? a b)
Consequent
(proc1 a)
Alternative
(if (> x y) (display x) (display (+ x y)))
'(if (eq? a b) (proc1 a) (if (> x y) (display x) (display (+ x y))))
Making if expression with: 
Predicate:
(not (eq? (assoc 'b '((a 1) (b 2))) false))
Consequent
(cadr (assoc 'b '((a 1) (b 2))))
Alternative
(display (+ x y))
Making if expression with: 
Predicate:
(> x y)
Consequent
(display x)
Alternative
(if (not (eq? (assoc 'b '((a 1) (b 2))) false)) (cadr (assoc 'b '((a 1) (b 2)))) (display (+ x y)))
Making if expression with: 
Predicate:
(eq? a b)
Consequent
(proc1 a)
Alternative
(if (> x y) (display x) (if (not (eq? (assoc 'b '((a 1) (b 2))) false)) (cadr (assoc 'b '((a 1) (b 2)))) (display (+ x y))))
'(if (eq? a b)
   (proc1 a)
   (if (> x y) (display x) (if (not (eq? (assoc 'b '((a 1) (b 2))) false)) (cadr (assoc 'b '((a 1) (b 2)))) (display (+ x y)))))
'((lambda (frame) (scan (frame-variables frame) (frame-values frame))) (first-frame env))
'((lambda (first rest)
    (if (cond-else-clause? first)
      (if (null? rest) (sequence->exp (cond-actions first)) (error "ELSE clause isn't last -- COND->IF" clauses))
      (if (special-cond-clause-syntax? first)
        (make-if (list 'not (list 'eq? (cond-predicate first) 'false)) (list (recipient-proc first) (cond-predicate first)) (expand-clauses rest))
        (make-if (cond-predicate first) (sequence->exp (cond-actions first)) (expand-clauses rest)))))
  (car clauses)
  (cdr clauses))
'((lambda (a b c d) (* a b c d)) 10 20 30 45)
'((lambda (a b c d) (displayln 'Multiplying) (* a b c d)) 10 20 30 45)
'((lambda (f) (+ (cube f) (* 2 f))) (square 4))
'(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
Making let* expression with: 
Var-bindings:
((x 3) (y (+ x 2)) (z (+ x y 5)))
Body:
((* x z))
'(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
'(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z))
Making let* expression with: 
Var-bindings:
((x 3) (y (+ x 2)) (z (+ x y 5)))
Body:
((displayln 'In-letstar2) (* x z))
'(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z))
'(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z)))
Making let* expression with: 
Var-bindings:
((x 3) (y (+ x 2)) (z (+ x y 5)))
Body:
((let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z)))
'(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z)))
Making let* expression with: 
Var-bindings:
((y (+ x 2)) (z (+ x y 5)))
Body:
((* x z))
Making let* expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((* x z))
Making let expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((* x z))
Making let expression with: 
Var-bindings:
((y (+ x 2)))
Body:
((let ((z (+ x y 5))) (* x z)))
Making let expression with: 
Var-bindings:
((x 3))
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
'(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
Making let* expression with: 
Var-bindings:
((y (+ x 2)) (z (+ x y 5)))
Body:
((displayln 'In-letstar2) (* x z))
Making let* expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((displayln 'In-letstar2) (* x z))
Making let expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((displayln 'In-letstar2) (* x z))
Making let expression with: 
Var-bindings:
((y (+ x 2)))
Body:
((let ((z (+ x y 5))) (displayln 'In-letstar2) (* x z)))
Making let expression with: 
Var-bindings:
((x 3))
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (displayln 'In-letstar2) (* x z))))
'(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (displayln 'In-letstar2) (* x z))))
Making let* expression with: 
Var-bindings:
((y (+ x 2)) (z (+ x y 5)))
Body:
((let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z)))
Making let* expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z)))
Making let expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z)))
Making let expression with: 
Var-bindings:
((y (+ x 2)))
Body:
((let ((z (+ x y 5))) (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z))))
Making let expression with: 
Var-bindings:
((x 3))
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z)))))
'(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'In-letstar2) (* x z)))))
'(a b)
'((displayln 'Entered-F1) (* a b))
'((displayln 'Entered-F1) (* a b))
'(b)
'((* b b))
'((* b b))
'(a b c d)
'((define e 3) (define f 4) (displayln 'Entered-F1) (define g 5) (* a b c d e f g h) (define h 6))
Making let expression with: 
Var-bindings:
((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*))
Body:
((set! e 3) (set! f 4) (displayln 'Entered-F1) (set! g 5) (* a b c d e f g h) (set! h 6))
'((let ((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*))
    (set! e 3)
    (set! f 4)
    (displayln 'Entered-F1)
    (set! g 5)
    (* a b c d e f g h)
    (set! h 6)))
'(a b)
'((define e 3) (define f 4) (define g 5) (* a b c d e f g h) (define h 6))
Making let expression with: 
Var-bindings:
((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*))
Body:
((set! e 3) (set! f 4) (set! g 5) (* a b c d e f g h) (set! h 6))
'((let ((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*)) (set! e 3) (set! f 4) (set! g 5) (* a b c d e f g h) (set! h 6)))
'(b)
'((define e 3) (* b e))
Making let expression with: 
Var-bindings:
((e '*unassigned*))
Body:
((set! e 3) (* b e))
'((let ((e '*unassigned*)) (set! e 3) (* b e)))
'()
'((define e 3) e)
Making let expression with: 
Var-bindings:
((e '*unassigned*))
Body:
((set! e 3) e)
'((let ((e '*unassigned*)) (set! e 3) e))
'()
'((display 'Hi))
'((display 'Hi))
> (driver-loop)

[Metacircular Evaluator Input] >>>
(define (square x) (* x x))
(define (cube x) (* (square x) x))

(define a '(set! x 5))
(define d '(define y 6))
(define i '(if true (display 'yes) (display 'no)))
(define v 'mud)
(define l '(lambda (x) (display "Executing lambda proc")))
(define b '(begin (display '(hi there))))
(define c '(cond ((> 5 2) (display 'yes)) (else (display 'no))))

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))
(cond ((assoc 'a '((a 1) (b 2))) => cadr)
      (else false))
(cond ((assoc 'c '((a 1) (b 2))) => cadr)
      (else false))

(and (> 10 6) (< 9 19) (= 5 5.0))
(and (> 10 6) (< 9 19) (= 6 5.0))
(and (> 10 6) (< 29 19) (= 5 5.0))
(and (> 10 6) (< 9 19) (= 15 5.0))
(and (> 10 6) (< 9 19) (= 5 5.0) (< 20 22))

(and (< 10 6) (< 9 19) (= 5 5.0))
(and (< 10 16) (> 9 -9) (= 5 5.0))

(or (> 10 6) (< 9 19) (= 5 5.0))
(or (> 10 6) (< 9 19) (= 5 35.0))
(or (> 10 6) (> 9 19) (= 5 5.0))
(or (> 10 6) (< 79 19) (= 55 5.0))
(or (> 10 56) (< 9 19) (= 5 5.0))
(or (> 10 60) (< 9 19) (= 55 5.0))
(or (> 10 60) (< 90 19) (= 5 5.0))
(or (> 10 600) (< 90 19) (= 65 5.0))

(or (< 10 6) (< 79 19) (= 55 5.0))
(or (= 10 6) (> 9 19) (< 5 5.0) (> 1 2) (> 20 19))

(define y 8)
y
(set! y 89)
y
(define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
(append '(q w e r t y) '(z x c v b n))

(let ((a 10) (b 20) (c 30) (d 45)) (displayln 'Multiplying) (* a b c d))

(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z))
(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
	(displayln 'Multiplying...)
	(* 
		(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z))
		z
	)
)

(let ((f (square 4))) (+ (cube f) (* 2 f)))
(let ((a 10) (b 20) (c 30) (d 45)) (* a b c d))
(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
(define (fib n) (let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 10)
(fib 20)
(fib 25)
(fib 28)
(fib 30)
(fib 50)

(define x 365)

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

(define (inc val)
    (+ val 1)
)

(inc 100)

(for (i 1) (i 40) inc
    (display i)
    (newline)
)

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
x

(while (< x 50)
    (display x)
    (newline)
    (set! x (+ x 3))
)

(define x -10)
x
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
(inc 100)
(for (i 1) (i 40) inc
    (display i)
    (newline)
)
x
(define x 1)
x
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
x
(while (< x 50)
    (display x)
    (newline)
    (set! x (+ x 3))
)
(define x 31)
x
(while (< x 50)
    (display x)
    (newline)
    (set! x (+ x 3))
)
(inc 2001)
(let ((f (square 4))) (+ (cube f) (* 2 f)))
(let ((a 10) (b 20) (c 30) (d 45)) (* a b c d))
(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))

(define x 11)
x
(define (P1 x) (display "Passed in value: ") (displayln x) (set! x 31) (display "Modified value: ") (displayln x))
(P1 x)
x
(set! x (+ x 3))
x
(P1 22)
x
(P1 x)
x

(define x 3)
x
(make-unbound! x)
x
(define x 4)
(define y 5)
x
y
(make-unbound! y)
y
x
(make-unbound! x)
x
(define x 7)
(define y 8)
(define z 9)
x
y
z
(make-unbound! y)
y
x
z

(define (F1 x)

	(define (F2 x)
		(define (F3 x)
			(define (F4 x)
				(define (inc x)
					(displayln "Entered proc (inc x)")
					(display "Passed in value of x: ")
					(displayln x)
					(set! x (* x 3))
					(display "Value of x after tripling: ")
					(displayln x)
					(+ x 1)
				)
				(displayln "Entered proc (F4 x)")
				(display "Passed in value of x: ")
				(displayln x)
				(set! x (* x 3))
				(display "Value of x after tripling: ")
				(displayln x)
				(set! x (inc x))
				(display "Value of x after incrementing: ")
				(displayln x)
				(displayln "Exiting proc (F4 x)")
			)
			(displayln "Entered proc (F3 x)")
			(display "Passed in value of x: ")
			(displayln x)
			(set! x (* x 3))
			(display "Value of x after tripling: ")
			(displayln x)
			(F4 x)
			(display "Value of x after calling (F4 x): ")
			(displayln x)
			(displayln "Exiting proc (F3 x)")
		)
		(displayln "Entered proc (F2 x)")
		(display "Passed in value of x: ")
		(displayln x)
		(set! x (* x 3))
		(display "Value of x after tripling: ")
		(displayln x)
		(F3 x)
		(display "Value of x after calling (F3 x): ")
		(displayln x)
		(displayln "Exiting proc (F2 x)")
	)

	(displayln "Entered proc (F1 x)")
	(display "Passed in value of x: ")
	(displayln x)
	(set! x (* x 3))
	(display "Value of x after tripling: ")
	(displayln x)
	(F2 x)
	(display "Value of x after calling (F2 x): ")
	(displayln x)
	(displayln "Exiting proc (F1 x)")
)

(F1 x)
x
(define x 24)
(define y 84)
x
y
(make-unbound! x)
x
y
z
(make-unbound! y)
y
(define x 2000)
x
(define (F1)
	(define x 100)
	(display "Value of x inside F1: ")
	(displayln x)
	(make-unbound! x)
	(display "Displaying x after unbounding it: ")
	(displayln x)
)
(F1)
x
(make-unbound! x)
x

(define (map proc items)
	(if (null? items)
		'()
		(cons (proc (car items)) (map proc (cdr items)))
	)
)
(map abs (list -10 2.5 -11.6 17))
(map abs (list -10 2.5 -11.6 0.0 17 -.5 -8 -35 96))
(map inc (list -10 2.5 -11.6 0.0 17 -.5 -8 -35 96))

(define (F1 a b) (displayln 'Entered-F1) (* a b))
(F1 8 4)
(define (F2 a) (define b 9) (define c 11) (* a b c))
(F2 6)
((lambda (a b) (displayln 'Entered-F1) (* a b)) 8 5)
((lambda (b) (* b b)) 16)
((lambda (a b) (define e 3) (define f 4) (define g 5) (define h 6) (* a b e f g h)) 2 7)
((lambda (b) (define e 3) (* b e)) 85)
((lambda () (define e 3) e))
((lambda () (display 'Hi)))
(let ((a 1) (b 3) (c 5)) (* a b c))
(let ((b 3) (c 5)) (* b c))
(let ((b 3)) (* b b))
((lambda () (set! b 3) (* b b)))
((lambda () (define b 3) (* b b)))
(define (F2 a) (define b 9) (define c 11) (* a b c))
(F2 456)
((lambda (a b c d) (define e 3) (define f 4) (displayln 'Entered-F1) (define g 5) (define h 6) (* a b c d e f g h)) 3 5 7 4)
((lambda () e (define e 3)))
((lambda (a b c d) (define e 3) (define f 4) (displayln 'Entered-F1) (define g 5) (* a b c d e f g h) (define h 6)) 3 5 7 4)
Starting to evaluate: (define (square x) (* x x))
Making procedure with: 
Parameters:
(x)
Body:
((* x x))

[Metacircular Evaluator Output] >>> Defined the variable: square
Finished evaluating: (define (square x) (* x x))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (cube x) (* (square x) x))
Making procedure with: 
Parameters:
(x)
Body:
((* (square x) x))

[Metacircular Evaluator Output] >>> Defined the variable: cube
Finished evaluating: (define (cube x) (* (square x) x))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define a (quote (set! x 5)))

[Metacircular Evaluator Output] >>> Defined the variable: a
Finished evaluating: (define a (quote (set! x 5)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define d (quote (define y 6)))

[Metacircular Evaluator Output] >>> Defined the variable: d
Finished evaluating: (define d (quote (define y 6)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define i (quote (if true (display (quote yes)) (display (quote no)))))

[Metacircular Evaluator Output] >>> Defined the variable: i
Finished evaluating: (define i (quote (if true (display (quote yes)) (display (quote no)))))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define v (quote mud))

[Metacircular Evaluator Output] >>> Defined the variable: v
Finished evaluating: (define v (quote mud))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define l (quote (lambda (x) (display Executing lambda proc))))

[Metacircular Evaluator Output] >>> Defined the variable: l
Finished evaluating: (define l (quote (lambda (x) (display Executing lambda proc))))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define b (quote (begin (display (quote (hi there))))))

[Metacircular Evaluator Output] >>> Defined the variable: b
Finished evaluating: (define b (quote (begin (display (quote (hi there))))))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define c (quote (cond ((> 5 2) (display (quote yes))) (else (display (quote no))))))

[Metacircular Evaluator Output] >>> Defined the variable: c
Finished evaluating: (define c (quote (cond ((> 5 2) (display (quote yes))) (else (display (quote no))))))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (cond ((assoc (quote b) (quote ((a 1) (b 2)))) => cadr) (else false))
In proc EVAL-cond to evaluate: (cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false))
Making if expression with: 
Predicate:
(not (eq? (assoc 'b '((a 1) (b 2))) false))
Consequent
(cadr (assoc 'b '((a 1) (b 2))))
Alternative
false

[Metacircular Evaluator Output] >>> 2
Finished evaluating: (cond ((assoc (quote b) (quote ((a 1) (b 2)))) => cadr) (else false))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (cond ((assoc (quote a) (quote ((a 1) (b 2)))) => cadr) (else false))
In proc EVAL-cond to evaluate: (cond ((assoc 'a '((a 1) (b 2))) => cadr) (else false))
Making if expression with: 
Predicate:
(not (eq? (assoc 'a '((a 1) (b 2))) false))
Consequent
(cadr (assoc 'a '((a 1) (b 2))))
Alternative
false

[Metacircular Evaluator Output] >>> 1
Finished evaluating: (cond ((assoc (quote a) (quote ((a 1) (b 2)))) => cadr) (else false))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (cond ((assoc (quote c) (quote ((a 1) (b 2)))) => cadr) (else false))
In proc EVAL-cond to evaluate: (cond ((assoc 'c '((a 1) (b 2))) => cadr) (else false))
Making if expression with: 
Predicate:
(not (eq? (assoc 'c '((a 1) (b 2))) false))
Consequent
(cadr (assoc 'c '((a 1) (b 2))))
Alternative
false

[Metacircular Evaluator Output] >>> #f
Finished evaluating: (cond ((assoc (quote c) (quote ((a 1) (b 2)))) => cadr) (else false))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (and (> 10 6) (< 9 19) (= 5 5.0))
In proc EVAL-and to evaluate: (and (> 10 6) (< 9 19) (= 5 5.0))
Making if expression with: 
Predicate:
(= 5 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 9 19)
Consequent
(if (= 5 5.0) true false)
Alternative
false
Making if expression with: 
Predicate:
(> 10 6)
Consequent
(if (< 9 19) (if (= 5 5.0) true false) false)
Alternative
false

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (and (> 10 6) (< 9 19) (= 5 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (and (> 10 6) (< 9 19) (= 6 5.0))
In proc EVAL-and to evaluate: (and (> 10 6) (< 9 19) (= 6 5.0))
Making if expression with: 
Predicate:
(= 6 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 9 19)
Consequent
(if (= 6 5.0) true false)
Alternative
false
Making if expression with: 
Predicate:
(> 10 6)
Consequent
(if (< 9 19) (if (= 6 5.0) true false) false)
Alternative
false

[Metacircular Evaluator Output] >>> #f
Finished evaluating: (and (> 10 6) (< 9 19) (= 6 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (and (> 10 6) (< 29 19) (= 5 5.0))
In proc EVAL-and to evaluate: (and (> 10 6) (< 29 19) (= 5 5.0))
Making if expression with: 
Predicate:
(= 5 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 29 19)
Consequent
(if (= 5 5.0) true false)
Alternative
false
Making if expression with: 
Predicate:
(> 10 6)
Consequent
(if (< 29 19) (if (= 5 5.0) true false) false)
Alternative
false

[Metacircular Evaluator Output] >>> #f
Finished evaluating: (and (> 10 6) (< 29 19) (= 5 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (and (> 10 6) (< 9 19) (= 15 5.0))
In proc EVAL-and to evaluate: (and (> 10 6) (< 9 19) (= 15 5.0))
Making if expression with: 
Predicate:
(= 15 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 9 19)
Consequent
(if (= 15 5.0) true false)
Alternative
false
Making if expression with: 
Predicate:
(> 10 6)
Consequent
(if (< 9 19) (if (= 15 5.0) true false) false)
Alternative
false

[Metacircular Evaluator Output] >>> #f
Finished evaluating: (and (> 10 6) (< 9 19) (= 15 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (and (> 10 6) (< 9 19) (= 5 5.0) (< 20 22))
In proc EVAL-and to evaluate: (and (> 10 6) (< 9 19) (= 5 5.0) (< 20 22))
Making if expression with: 
Predicate:
(< 20 22)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(= 5 5.0)
Consequent
(if (< 20 22) true false)
Alternative
false
Making if expression with: 
Predicate:
(< 9 19)
Consequent
(if (= 5 5.0) (if (< 20 22) true false) false)
Alternative
false
Making if expression with: 
Predicate:
(> 10 6)
Consequent
(if (< 9 19) (if (= 5 5.0) (if (< 20 22) true false) false) false)
Alternative
false

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (and (> 10 6) (< 9 19) (= 5 5.0) (< 20 22))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (and (< 10 6) (< 9 19) (= 5 5.0))
In proc EVAL-and to evaluate: (and (< 10 6) (< 9 19) (= 5 5.0))
Making if expression with: 
Predicate:
(= 5 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 9 19)
Consequent
(if (= 5 5.0) true false)
Alternative
false
Making if expression with: 
Predicate:
(< 10 6)
Consequent
(if (< 9 19) (if (= 5 5.0) true false) false)
Alternative
false

[Metacircular Evaluator Output] >>> #f
Finished evaluating: (and (< 10 6) (< 9 19) (= 5 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (and (< 10 16) (> 9 -9) (= 5 5.0))
In proc EVAL-and to evaluate: (and (< 10 16) (> 9 -9) (= 5 5.0))
Making if expression with: 
Predicate:
(= 5 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(> 9 -9)
Consequent
(if (= 5 5.0) true false)
Alternative
false
Making if expression with: 
Predicate:
(< 10 16)
Consequent
(if (> 9 -9) (if (= 5 5.0) true false) false)
Alternative
false

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (and (< 10 16) (> 9 -9) (= 5 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (> 10 6) (< 9 19) (= 5 5.0))
In proc EVAL-or to evaluate: (or (> 10 6) (< 9 19) (= 5 5.0))
Making if expression with: 
Predicate:
(= 5 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 9 19)
Consequent
true
Alternative
(if (= 5 5.0) true false)
Making if expression with: 
Predicate:
(> 10 6)
Consequent
true
Alternative
(if (< 9 19) true (if (= 5 5.0) true false))

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (or (> 10 6) (< 9 19) (= 5 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (> 10 6) (< 9 19) (= 5 35.0))
In proc EVAL-or to evaluate: (or (> 10 6) (< 9 19) (= 5 35.0))
Making if expression with: 
Predicate:
(= 5 35.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 9 19)
Consequent
true
Alternative
(if (= 5 35.0) true false)
Making if expression with: 
Predicate:
(> 10 6)
Consequent
true
Alternative
(if (< 9 19) true (if (= 5 35.0) true false))

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (or (> 10 6) (< 9 19) (= 5 35.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (> 10 6) (> 9 19) (= 5 5.0))
In proc EVAL-or to evaluate: (or (> 10 6) (> 9 19) (= 5 5.0))
Making if expression with: 
Predicate:
(= 5 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(> 9 19)
Consequent
true
Alternative
(if (= 5 5.0) true false)
Making if expression with: 
Predicate:
(> 10 6)
Consequent
true
Alternative
(if (> 9 19) true (if (= 5 5.0) true false))

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (or (> 10 6) (> 9 19) (= 5 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (> 10 6) (< 79 19) (= 55 5.0))
In proc EVAL-or to evaluate: (or (> 10 6) (< 79 19) (= 55 5.0))
Making if expression with: 
Predicate:
(= 55 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 79 19)
Consequent
true
Alternative
(if (= 55 5.0) true false)
Making if expression with: 
Predicate:
(> 10 6)
Consequent
true
Alternative
(if (< 79 19) true (if (= 55 5.0) true false))

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (or (> 10 6) (< 79 19) (= 55 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (> 10 56) (< 9 19) (= 5 5.0))
In proc EVAL-or to evaluate: (or (> 10 56) (< 9 19) (= 5 5.0))
Making if expression with: 
Predicate:
(= 5 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 9 19)
Consequent
true
Alternative
(if (= 5 5.0) true false)
Making if expression with: 
Predicate:
(> 10 56)
Consequent
true
Alternative
(if (< 9 19) true (if (= 5 5.0) true false))

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (or (> 10 56) (< 9 19) (= 5 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (> 10 60) (< 9 19) (= 55 5.0))
In proc EVAL-or to evaluate: (or (> 10 60) (< 9 19) (= 55 5.0))
Making if expression with: 
Predicate:
(= 55 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 9 19)
Consequent
true
Alternative
(if (= 55 5.0) true false)
Making if expression with: 
Predicate:
(> 10 60)
Consequent
true
Alternative
(if (< 9 19) true (if (= 55 5.0) true false))

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (or (> 10 60) (< 9 19) (= 55 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (> 10 60) (< 90 19) (= 5 5.0))
In proc EVAL-or to evaluate: (or (> 10 60) (< 90 19) (= 5 5.0))
Making if expression with: 
Predicate:
(= 5 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 90 19)
Consequent
true
Alternative
(if (= 5 5.0) true false)
Making if expression with: 
Predicate:
(> 10 60)
Consequent
true
Alternative
(if (< 90 19) true (if (= 5 5.0) true false))

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (or (> 10 60) (< 90 19) (= 5 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (> 10 600) (< 90 19) (= 65 5.0))
In proc EVAL-or to evaluate: (or (> 10 600) (< 90 19) (= 65 5.0))
Making if expression with: 
Predicate:
(= 65 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 90 19)
Consequent
true
Alternative
(if (= 65 5.0) true false)
Making if expression with: 
Predicate:
(> 10 600)
Consequent
true
Alternative
(if (< 90 19) true (if (= 65 5.0) true false))

[Metacircular Evaluator Output] >>> #f
Finished evaluating: (or (> 10 600) (< 90 19) (= 65 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (< 10 6) (< 79 19) (= 55 5.0))
In proc EVAL-or to evaluate: (or (< 10 6) (< 79 19) (= 55 5.0))
Making if expression with: 
Predicate:
(= 55 5.0)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(< 79 19)
Consequent
true
Alternative
(if (= 55 5.0) true false)
Making if expression with: 
Predicate:
(< 10 6)
Consequent
true
Alternative
(if (< 79 19) true (if (= 55 5.0) true false))

[Metacircular Evaluator Output] >>> #f
Finished evaluating: (or (< 10 6) (< 79 19) (= 55 5.0))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (or (= 10 6) (> 9 19) (< 5 5.0) (> 1 2) (> 20 19))
In proc EVAL-or to evaluate: (or (= 10 6) (> 9 19) (< 5 5.0) (> 1 2) (> 20 19))
Making if expression with: 
Predicate:
(> 20 19)
Consequent
true
Alternative
false
Making if expression with: 
Predicate:
(> 1 2)
Consequent
true
Alternative
(if (> 20 19) true false)
Making if expression with: 
Predicate:
(< 5 5.0)
Consequent
true
Alternative
(if (> 1 2) true (if (> 20 19) true false))
Making if expression with: 
Predicate:
(> 9 19)
Consequent
true
Alternative
(if (< 5 5.0) true (if (> 1 2) true (if (> 20 19) true false)))
Making if expression with: 
Predicate:
(= 10 6)
Consequent
true
Alternative
(if (> 9 19) true (if (< 5 5.0) true (if (> 1 2) true (if (> 20 19) true false))))

[Metacircular Evaluator Output] >>> #t
Finished evaluating: (or (= 10 6) (> 9 19) (< 5 5.0) (> 1 2) (> 20 19))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define y 8)

[Metacircular Evaluator Output] >>> Defined the variable: y
Finished evaluating: (define y 8)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: y

[Metacircular Evaluator Output] >>> 8
Finished evaluating: y
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (set! y 89)

[Metacircular Evaluator Output] >>> Assigned value to: y
Finished evaluating: (set! y 89)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: y

[Metacircular Evaluator Output] >>> 89
Finished evaluating: y
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
Making procedure with: 
Parameters:
(x y)
Body:
((if (null? x) y (cons (car x) (append (cdr x) y))))

[Metacircular Evaluator Output] >>> Defined the variable: append
Finished evaluating: (define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (append (quote (q w e r t y)) (quote (z x c v b n)))

[Metacircular Evaluator Output] >>> (q w e r t y z x c v b n)
Finished evaluating: (append (quote (q w e r t y)) (quote (z x c v b n)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let ((a 10) (b 20) (c 30) (d 45)) (displayln (quote Multiplying)) (* a b c d))
Making procedure with: 
Parameters:
(a b c d)
Body:
((displayln 'Multiplying) (* a b c d))
Multiplying

[Metacircular Evaluator Output] >>> 270000
Finished evaluating: (let ((a 10) (b 20) (c 30) (d 45)) (displayln (quote Multiplying)) (* a b c d))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln (quote Multiplying...)) (* x z))
In proc EVAL-let* to evaluate: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z))
Making let* expression with: 
Var-bindings:
((y (+ x 2)) (z (+ x y 5)))
Body:
((displayln 'Multiplying...) (* x z))
Making let* expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((displayln 'Multiplying...) (* x z))
Making let expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((displayln 'Multiplying...) (* x z))
Making let expression with: 
Var-bindings:
((y (+ x 2)))
Body:
((let ((z (+ x y 5))) (displayln 'Multiplying...) (* x z)))
Making let expression with: 
Var-bindings:
((x 3))
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (displayln 'Multiplying...) (* x z))))
Making procedure with: 
Parameters:
(x)
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (displayln 'Multiplying...) (* x z))))
Making procedure with: 
Parameters:
(y)
Body:
((let ((z (+ x y 5))) (displayln 'Multiplying...) (* x z)))
Making procedure with: 
Parameters:
(z)
Body:
((displayln 'Multiplying...) (* x z))
Multiplying...

[Metacircular Evaluator Output] >>> 39
Finished evaluating: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln (quote Multiplying...)) (* x z))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln (quote Multiplying...)) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln (quote Multiplying...)) (* x z)) z))
In proc EVAL-let* to evaluate: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z)) z))
Making let* expression with: 
Var-bindings:
((y (+ x 2)) (z (+ x y 5)))
Body:
((displayln 'Multiplying...) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z)) z))
Making let* expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((displayln 'Multiplying...) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z)) z))
Making let expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((displayln 'Multiplying...) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z)) z))
Making let expression with: 
Var-bindings:
((y (+ x 2)))
Body:
((let ((z (+ x y 5))) (displayln 'Multiplying...) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z)) z)))
Making let expression with: 
Var-bindings:
((x 3))
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (displayln 'Multiplying...) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z)) z))))
Making procedure with: 
Parameters:
(x)
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (displayln 'Multiplying...) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z)) z))))
Making procedure with: 
Parameters:
(y)
Body:
((let ((z (+ x y 5))) (displayln 'Multiplying...) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z)) z)))
Making procedure with: 
Parameters:
(z)
Body:
((displayln 'Multiplying...) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z)) z))
Multiplying...
In proc EVAL-let* to evaluate: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln 'Multiplying...) (* x z))
Making let* expression with: 
Var-bindings:
((y (+ x 2)) (z (+ x y 5)))
Body:
((displayln 'Multiplying...) (* x z))
Making let* expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((displayln 'Multiplying...) (* x z))
Making let expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((displayln 'Multiplying...) (* x z))
Making let expression with: 
Var-bindings:
((y (+ x 2)))
Body:
((let ((z (+ x y 5))) (displayln 'Multiplying...) (* x z)))
Making let expression with: 
Var-bindings:
((x 3))
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (displayln 'Multiplying...) (* x z))))
Making procedure with: 
Parameters:
(x)
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (displayln 'Multiplying...) (* x z))))
Making procedure with: 
Parameters:
(y)
Body:
((let ((z (+ x y 5))) (displayln 'Multiplying...) (* x z)))
Making procedure with: 
Parameters:
(z)
Body:
((displayln 'Multiplying...) (* x z))
Multiplying...

[Metacircular Evaluator Output] >>> 507
Finished evaluating: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln (quote Multiplying...)) (* (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (displayln (quote Multiplying...)) (* x z)) z))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let ((f (square 4))) (+ (cube f) (* 2 f)))
Making procedure with: 
Parameters:
(f)
Body:
((+ (cube f) (* 2 f)))

[Metacircular Evaluator Output] >>> 4128
Finished evaluating: (let ((f (square 4))) (+ (cube f) (* 2 f)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let ((a 10) (b 20) (c 30) (d 45)) (* a b c d))
Making procedure with: 
Parameters:
(a b c d)
Body:
((* a b c d))

[Metacircular Evaluator Output] >>> 270000
Finished evaluating: (let ((a 10) (b 20) (c 30) (d 45)) (* a b c d))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
In proc EVAL-let* to evaluate: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
Making let* expression with: 
Var-bindings:
((y (+ x 2)) (z (+ x y 5)))
Body:
((* x z))
Making let* expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((* x z))
Making let expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((* x z))
Making let expression with: 
Var-bindings:
((y (+ x 2)))
Body:
((let ((z (+ x y 5))) (* x z)))
Making let expression with: 
Var-bindings:
((x 3))
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
Making procedure with: 
Parameters:
(x)
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
Making procedure with: 
Parameters:
(y)
Body:
((let ((z (+ x y 5))) (* x z)))
Making procedure with: 
Parameters:
(z)
Body:
((* x z))

[Metacircular Evaluator Output] >>> 39
Finished evaluating: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (fib n) (let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
Making procedure with: 
Parameters:
(n)
Body:
((let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))

[Metacircular Evaluator Output] >>> Defined the variable: fib
Finished evaluating: (define (fib n) (let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 0)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 0
Finished evaluating: (fib 0)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 1)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 1
Finished evaluating: (fib 1)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 2)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 1
Finished evaluating: (fib 2)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 3)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 2
Finished evaluating: (fib 3)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 4)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 3
Finished evaluating: (fib 4)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 5)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 5
Finished evaluating: (fib 5)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 6)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 8
Finished evaluating: (fib 6)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 7)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 13
Finished evaluating: (fib 7)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 10)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 55
Finished evaluating: (fib 10)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 20)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 6765
Finished evaluating: (fib 20)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 25)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 75025
Finished evaluating: (fib 25)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 28)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 317811
Finished evaluating: (fib 28)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 30)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 832040
Finished evaluating: (fib 30)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (fib 50)
Making begin expression with sequence: 
((define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))
Making procedure with: 
Parameters:
(a b count)
Body:
((if (= count 0) b (fib-iter (+ a b) a (- count 1))))

[Metacircular Evaluator Output] >>> 12586269025
Finished evaluating: (fib 50)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x 365)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x 365)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (do (display (quote x:)) (display x) (newline) (set! x (+ x 2)) (display (quote (x after setting:))) (display x) (newline) while (< x 10))
In proc EVAL-do-while to evaluate: (do (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) while (< x 10))
Making while expression with: 
Condition:
(< x 10)
Statements:
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline))
Making begin expression with sequence: 
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while (< x 10) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline)))
x:365
(x after setting:)367
In proc EVAL-while to evaluate: (while (< x 10) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline))
Making begin expression with sequence: 
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block))
Making if expression with: 
Predicate:
(< x 10)
Consequent
(begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block))
Alternative
'done
Making begin expression with sequence: 
((define while-block (lambda () (if (< x 10) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))) (while-block))
Making procedure with: 
Parameters:
()
Body:
((if (< x 10) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))

[Metacircular Evaluator Output] >>> done
Finished evaluating: (do (display (quote x:)) (display x) (newline) (set! x (+ x 2)) (display (quote (x after setting:))) (display x) (newline) while (< x 10))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (inc val) (+ val 1))
Making procedure with: 
Parameters:
(val)
Body:
((+ val 1))

[Metacircular Evaluator Output] >>> Defined the variable: inc
Finished evaluating: (define (inc val) (+ val 1))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (inc 100)

[Metacircular Evaluator Output] >>> 101
Finished evaluating: (inc 100)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (for (i 1) (i 40) inc (display i) (newline))
In proc EVAL-for to evaluate: (for (i 1) (i 40) inc (display i) (newline))
Making begin expression with sequence: 
((display i) (newline) (set! i (inc i)) (for-block))
Making if expression with: 
Predicate:
(<= i 40)
Consequent
(begin (display i) (newline) (set! i (inc i)) (for-block))
Alternative
'done
Making begin expression with sequence: 
((define i 1) (define for-block (lambda () (if (<= i 40) (begin (display i) (newline) (set! i (inc i)) (for-block)) 'done))) (for-block))
Making procedure with: 
Parameters:
()
Body:
((if (<= i 40) (begin (display i) (newline) (set! i (inc i)) (for-block)) 'done))
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
Finished evaluating: (for (i 1) (i 40) inc (display i) (newline))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (do (display (quote x:)) (display x) (newline) (set! x (+ x 2)) (display (quote (x after setting:))) (display x) (newline) until (> x 30))
In proc EVAL-do-until to evaluate: (do (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) until (> x 30))
Making while expression with: 
Condition:
(not (> x 30))
Statements:
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline))
Making begin expression with sequence: 
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while (not (> x 30)) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline)))
x:367
(x after setting:)369
In proc EVAL-while to evaluate: (while (not (> x 30)) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline))
Making begin expression with sequence: 
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block))
Making if expression with: 
Predicate:
(not (> x 30))
Consequent
(begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block))
Alternative
'done
Making begin expression with sequence: 
((define while-block (lambda () (if (not (> x 30)) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))) (while-block))
Making procedure with: 
Parameters:
()
Body:
((if (not (> x 30)) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))

[Metacircular Evaluator Output] >>> done
Finished evaluating: (do (display (quote x:)) (display x) (newline) (set! x (+ x 2)) (display (quote (x after setting:))) (display x) (newline) until (> x 30))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 369
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (while (< x 50) (display x) (newline) (set! x (+ x 3)))
In proc EVAL-while to evaluate: (while (< x 50) (display x) (newline) (set! x (+ x 3)))
Making begin expression with sequence: 
((display x) (newline) (set! x (+ x 3)) (while-block))
Making if expression with: 
Predicate:
(< x 50)
Consequent
(begin (display x) (newline) (set! x (+ x 3)) (while-block))
Alternative
'done
Making begin expression with sequence: 
((define while-block (lambda () (if (< x 50) (begin (display x) (newline) (set! x (+ x 3)) (while-block)) 'done))) (while-block))
Making procedure with: 
Parameters:
()
Body:
((if (< x 50) (begin (display x) (newline) (set! x (+ x 3)) (while-block)) 'done))

[Metacircular Evaluator Output] >>> done
Finished evaluating: (while (< x 50) (display x) (newline) (set! x (+ x 3)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x -10)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x -10)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> -10
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (do (display (quote x:)) (display x) (newline) (set! x (+ x 2)) (display (quote (x after setting:))) (display x) (newline) while (< x 10))
In proc EVAL-do-while to evaluate: (do (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) while (< x 10))
Making while expression with: 
Condition:
(< x 10)
Statements:
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline))
Making begin expression with sequence: 
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while (< x 10) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline)))
x:-10
(x after setting:)-8
In proc EVAL-while to evaluate: (while (< x 10) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline))
Making begin expression with sequence: 
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block))
Making if expression with: 
Predicate:
(< x 10)
Consequent
(begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block))
Alternative
'done
Making begin expression with sequence: 
((define while-block (lambda () (if (< x 10) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))) (while-block))
Making procedure with: 
Parameters:
()
Body:
((if (< x 10) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))
x:-8
(x after setting:)-6
x:-6
(x after setting:)-4
x:-4
(x after setting:)-2
x:-2
(x after setting:)0
x:0
(x after setting:)2
x:2
(x after setting:)4
x:4
(x after setting:)6
x:6
(x after setting:)8
x:8
(x after setting:)10

[Metacircular Evaluator Output] >>> done
Finished evaluating: (do (display (quote x:)) (display x) (newline) (set! x (+ x 2)) (display (quote (x after setting:))) (display x) (newline) while (< x 10))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (inc 100)

[Metacircular Evaluator Output] >>> 101
Finished evaluating: (inc 100)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (for (i 1) (i 40) inc (display i) (newline))
In proc EVAL-for to evaluate: (for (i 1) (i 40) inc (display i) (newline))
Making begin expression with sequence: 
((display i) (newline) (set! i (inc i)) (for-block))
Making if expression with: 
Predicate:
(<= i 40)
Consequent
(begin (display i) (newline) (set! i (inc i)) (for-block))
Alternative
'done
Making begin expression with sequence: 
((define i 1) (define for-block (lambda () (if (<= i 40) (begin (display i) (newline) (set! i (inc i)) (for-block)) 'done))) (for-block))
Making procedure with: 
Parameters:
()
Body:
((if (<= i 40) (begin (display i) (newline) (set! i (inc i)) (for-block)) 'done))
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
Finished evaluating: (for (i 1) (i 40) inc (display i) (newline))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 10
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x 1)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x 1)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 1
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (do (display (quote x:)) (display x) (newline) (set! x (+ x 2)) (display (quote (x after setting:))) (display x) (newline) until (> x 30))
In proc EVAL-do-until to evaluate: (do (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) until (> x 30))
Making while expression with: 
Condition:
(not (> x 30))
Statements:
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline))
Making begin expression with sequence: 
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while (not (> x 30)) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline)))
x:1
(x after setting:)3
In proc EVAL-while to evaluate: (while (not (> x 30)) (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline))
Making begin expression with sequence: 
((display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block))
Making if expression with: 
Predicate:
(not (> x 30))
Consequent
(begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block))
Alternative
'done
Making begin expression with sequence: 
((define while-block (lambda () (if (not (> x 30)) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))) (while-block))
Making procedure with: 
Parameters:
()
Body:
((if (not (> x 30)) (begin (display 'x:) (display x) (newline) (set! x (+ x 2)) (display '(x after setting:)) (display x) (newline) (while-block)) 'done))
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
Finished evaluating: (do (display (quote x:)) (display x) (newline) (set! x (+ x 2)) (display (quote (x after setting:))) (display x) (newline) until (> x 30))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 31
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (while (< x 50) (display x) (newline) (set! x (+ x 3)))
In proc EVAL-while to evaluate: (while (< x 50) (display x) (newline) (set! x (+ x 3)))
Making begin expression with sequence: 
((display x) (newline) (set! x (+ x 3)) (while-block))
Making if expression with: 
Predicate:
(< x 50)
Consequent
(begin (display x) (newline) (set! x (+ x 3)) (while-block))
Alternative
'done
Making begin expression with sequence: 
((define while-block (lambda () (if (< x 50) (begin (display x) (newline) (set! x (+ x 3)) (while-block)) 'done))) (while-block))
Making procedure with: 
Parameters:
()
Body:
((if (< x 50) (begin (display x) (newline) (set! x (+ x 3)) (while-block)) 'done))
31
34
37
40
43
46
49

[Metacircular Evaluator Output] >>> done
Finished evaluating: (while (< x 50) (display x) (newline) (set! x (+ x 3)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x 31)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x 31)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 31
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (while (< x 50) (display x) (newline) (set! x (+ x 3)))
In proc EVAL-while to evaluate: (while (< x 50) (display x) (newline) (set! x (+ x 3)))
Making begin expression with sequence: 
((display x) (newline) (set! x (+ x 3)) (while-block))
Making if expression with: 
Predicate:
(< x 50)
Consequent
(begin (display x) (newline) (set! x (+ x 3)) (while-block))
Alternative
'done
Making begin expression with sequence: 
((define while-block (lambda () (if (< x 50) (begin (display x) (newline) (set! x (+ x 3)) (while-block)) 'done))) (while-block))
Making procedure with: 
Parameters:
()
Body:
((if (< x 50) (begin (display x) (newline) (set! x (+ x 3)) (while-block)) 'done))
31
34
37
40
43
46
49

[Metacircular Evaluator Output] >>> done
Finished evaluating: (while (< x 50) (display x) (newline) (set! x (+ x 3)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (inc 2001)

[Metacircular Evaluator Output] >>> 2002
Finished evaluating: (inc 2001)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let ((f (square 4))) (+ (cube f) (* 2 f)))
Making procedure with: 
Parameters:
(f)
Body:
((+ (cube f) (* 2 f)))

[Metacircular Evaluator Output] >>> 4128
Finished evaluating: (let ((f (square 4))) (+ (cube f) (* 2 f)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let ((a 10) (b 20) (c 30) (d 45)) (* a b c d))
Making procedure with: 
Parameters:
(a b c d)
Body:
((* a b c d))

[Metacircular Evaluator Output] >>> 270000
Finished evaluating: (let ((a 10) (b 20) (c 30) (d 45)) (* a b c d))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
In proc EVAL-let* to evaluate: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
Making let* expression with: 
Var-bindings:
((y (+ x 2)) (z (+ x y 5)))
Body:
((* x z))
Making let* expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((* x z))
Making let expression with: 
Var-bindings:
((z (+ x y 5)))
Body:
((* x z))
Making let expression with: 
Var-bindings:
((y (+ x 2)))
Body:
((let ((z (+ x y 5))) (* x z)))
Making let expression with: 
Var-bindings:
((x 3))
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
Making procedure with: 
Parameters:
(x)
Body:
((let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
Making procedure with: 
Parameters:
(y)
Body:
((let ((z (+ x y 5))) (* x z)))
Making procedure with: 
Parameters:
(z)
Body:
((* x z))

[Metacircular Evaluator Output] >>> 39
Finished evaluating: (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x 11)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x 11)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 11
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (P1 x) (display Passed in value: ) (displayln x) (set! x 31) (display Modified value: ) (displayln x))
Making procedure with: 
Parameters:
(x)
Body:
((display Passed in value: ) (displayln x) (set! x 31) (display Modified value: ) (displayln x))

[Metacircular Evaluator Output] >>> Defined the variable: P1
Finished evaluating: (define (P1 x) (display Passed in value: ) (displayln x) (set! x 31) (display Modified value: ) (displayln x))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (P1 x)
Passed in value: 11
Modified value: 31

[Metacircular Evaluator Output] >>> #<void>
Finished evaluating: (P1 x)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 11
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (set! x (+ x 3))

[Metacircular Evaluator Output] >>> Assigned value to: x
Finished evaluating: (set! x (+ x 3))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 14
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (P1 22)
Passed in value: 22
Modified value: 31

[Metacircular Evaluator Output] >>> #<void>
Finished evaluating: (P1 22)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 14
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (P1 x)
Passed in value: 14
Modified value: 31

[Metacircular Evaluator Output] >>> #<void>
Finished evaluating: (P1 x)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 14
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x 3)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x 3)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 3
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (make-unbound! x)

[Metacircular Evaluator Output] >>> Un-defined the variable: x
Finished evaluating: (make-unbound! x)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x
Failed to evaluate: x

[Metacircular Evaluator Output] >>> Unbound variable 'x
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x 4)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x 4)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define y 5)

[Metacircular Evaluator Output] >>> Defined the variable: y
Finished evaluating: (define y 5)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 4
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: y

[Metacircular Evaluator Output] >>> 5
Finished evaluating: y
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (make-unbound! y)

[Metacircular Evaluator Output] >>> Un-defined the variable: y
Finished evaluating: (make-unbound! y)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: y
Failed to evaluate: y

[Metacircular Evaluator Output] >>> Unbound variable 'y
Finished evaluating: y
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 4
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (make-unbound! x)

[Metacircular Evaluator Output] >>> Un-defined the variable: x
Finished evaluating: (make-unbound! x)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x
Failed to evaluate: x

[Metacircular Evaluator Output] >>> Unbound variable 'x
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x 7)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x 7)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define y 8)

[Metacircular Evaluator Output] >>> Defined the variable: y
Finished evaluating: (define y 8)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define z 9)

[Metacircular Evaluator Output] >>> Defined the variable: z
Finished evaluating: (define z 9)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 7
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: y

[Metacircular Evaluator Output] >>> 8
Finished evaluating: y
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: z

[Metacircular Evaluator Output] >>> 9
Finished evaluating: z
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (make-unbound! y)

[Metacircular Evaluator Output] >>> Un-defined the variable: y
Finished evaluating: (make-unbound! y)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: y
Failed to evaluate: y

[Metacircular Evaluator Output] >>> Unbound variable 'y
Finished evaluating: y
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 7
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: z

[Metacircular Evaluator Output] >>> 9
Finished evaluating: z
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (F1 x) (define (F2 x) (define (F3 x) (define (F4 x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x))) (displayln Entered proc (F2 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F3 x) (display Value of x after calling (F3 x): ) (displayln x) (displayln Exiting proc (F2 x))) (displayln Entered proc (F1 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F2 x) (display Value of x after calling (F2 x): ) (displayln x) (displayln Exiting proc (F1 x)))
Making let expression with: 
Var-bindings:
((F2 '*unassigned*))
Body:
((set! F2 (lambda (x) (define (F3 x) (define (F4 x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x))) (displayln Entered proc (F2 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F3 x) (display Value of x after calling (F3 x): ) (displayln x) (displayln Exiting proc (F2 x)))) (displayln Entered proc (F1 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F2 x) (display Value of x after calling (F2 x): ) (displayln x) (displayln Exiting proc (F1 x)))
Making procedure with: 
Parameters:
(x)
Body:
((let ((F2 '*unassigned*)) (set! F2 (lambda (x) (define (F3 x) (define (F4 x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x))) (displayln Entered proc (F2 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F3 x) (display Value of x after calling (F3 x): ) (displayln x) (displayln Exiting proc (F2 x)))) (displayln Entered proc (F1 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F2 x) (display Value of x after calling (F2 x): ) (displayln x) (displayln Exiting proc (F1 x))))

[Metacircular Evaluator Output] >>> Defined the variable: F1
Finished evaluating: (define (F1 x) (define (F2 x) (define (F3 x) (define (F4 x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x))) (displayln Entered proc (F2 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F3 x) (display Value of x after calling (F3 x): ) (displayln x) (displayln Exiting proc (F2 x))) (displayln Entered proc (F1 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F2 x) (display Value of x after calling (F2 x): ) (displayln x) (displayln Exiting proc (F1 x)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (F1 x)
Making procedure with: 
Parameters:
(F2)
Body:
((set! F2 (lambda (x) (define (F3 x) (define (F4 x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x))) (displayln Entered proc (F2 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F3 x) (display Value of x after calling (F3 x): ) (displayln x) (displayln Exiting proc (F2 x)))) (displayln Entered proc (F1 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F2 x) (display Value of x after calling (F2 x): ) (displayln x) (displayln Exiting proc (F1 x)))
Making let expression with: 
Var-bindings:
((F3 '*unassigned*))
Body:
((set! F3 (lambda (x) (define (F4 x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x)))) (displayln Entered proc (F2 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F3 x) (display Value of x after calling (F3 x): ) (displayln x) (displayln Exiting proc (F2 x)))
Making procedure with: 
Parameters:
(x)
Body:
((let ((F3 '*unassigned*)) (set! F3 (lambda (x) (define (F4 x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x)))) (displayln Entered proc (F2 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F3 x) (display Value of x after calling (F3 x): ) (displayln x) (displayln Exiting proc (F2 x))))
Entered proc (F1 x)
Passed in value of x: 7
Value of x after tripling: 21
Making procedure with: 
Parameters:
(F3)
Body:
((set! F3 (lambda (x) (define (F4 x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x)))) (displayln Entered proc (F2 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F3 x) (display Value of x after calling (F3 x): ) (displayln x) (displayln Exiting proc (F2 x)))
Making let expression with: 
Var-bindings:
((F4 '*unassigned*))
Body:
((set! F4 (lambda (x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x)))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x)))
Making procedure with: 
Parameters:
(x)
Body:
((let ((F4 '*unassigned*)) (set! F4 (lambda (x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x)))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x))))
Entered proc (F2 x)
Passed in value of x: 21
Value of x after tripling: 63
Making procedure with: 
Parameters:
(F4)
Body:
((set! F4 (lambda (x) (define (inc x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1)) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x)))) (displayln Entered proc (F3 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (F4 x) (display Value of x after calling (F4 x): ) (displayln x) (displayln Exiting proc (F3 x)))
Making let expression with: 
Var-bindings:
((inc '*unassigned*))
Body:
((set! inc (lambda (x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1))) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x)))
Making procedure with: 
Parameters:
(x)
Body:
((let ((inc '*unassigned*)) (set! inc (lambda (x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1))) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x))))
Entered proc (F3 x)
Passed in value of x: 63
Value of x after tripling: 189
Making procedure with: 
Parameters:
(inc)
Body:
((set! inc (lambda (x) (displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1))) (displayln Entered proc (F4 x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (set! x (inc x)) (display Value of x after incrementing: ) (displayln x) (displayln Exiting proc (F4 x)))
Making procedure with: 
Parameters:
(x)
Body:
((displayln Entered proc (inc x)) (display Passed in value of x: ) (displayln x) (set! x (* x 3)) (display Value of x after tripling: ) (displayln x) (+ x 1))
Entered proc (F4 x)
Passed in value of x: 189
Value of x after tripling: 567
Entered proc (inc x)
Passed in value of x: 567
Value of x after tripling: 1701
Value of x after incrementing: 1702
Exiting proc (F4 x)
Value of x after calling (F4 x): 189
Exiting proc (F3 x)
Value of x after calling (F3 x): 63
Exiting proc (F2 x)
Value of x after calling (F2 x): 21
Exiting proc (F1 x)

[Metacircular Evaluator Output] >>> #<void>
Finished evaluating: (F1 x)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 7
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x 24)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x 24)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define y 84)

[Metacircular Evaluator Output] >>> Defined the variable: y
Finished evaluating: (define y 84)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 24
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: y

[Metacircular Evaluator Output] >>> 84
Finished evaluating: y
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (make-unbound! x)

[Metacircular Evaluator Output] >>> Un-defined the variable: x
Finished evaluating: (make-unbound! x)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x
Failed to evaluate: x

[Metacircular Evaluator Output] >>> Unbound variable 'x
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: y

[Metacircular Evaluator Output] >>> 84
Finished evaluating: y
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: z

[Metacircular Evaluator Output] >>> 9
Finished evaluating: z
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (make-unbound! y)

[Metacircular Evaluator Output] >>> Un-defined the variable: y
Finished evaluating: (make-unbound! y)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: y
Failed to evaluate: y

[Metacircular Evaluator Output] >>> Unbound variable 'y
Finished evaluating: y
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define x 2000)

[Metacircular Evaluator Output] >>> Defined the variable: x
Finished evaluating: (define x 2000)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 2000
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (F1) (define x 100) (display Value of x inside F1: ) (displayln x) (make-unbound! x) (display Displaying x after unbounding it: ) (displayln x))
Making let expression with: 
Var-bindings:
((x '*unassigned*))
Body:
((set! x 100) (display Value of x inside F1: ) (displayln x) (make-unbound! x) (display Displaying x after unbounding it: ) (displayln x))
Making procedure with: 
Parameters:
()
Body:
((let ((x '*unassigned*)) (set! x 100) (display Value of x inside F1: ) (displayln x) (make-unbound! x) (display Displaying x after unbounding it: ) (displayln x)))

[Metacircular Evaluator Output] >>> Defined the variable: F1
Finished evaluating: (define (F1) (define x 100) (display Value of x inside F1: ) (displayln x) (make-unbound! x) (display Displaying x after unbounding it: ) (displayln x))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (F1)
Making procedure with: 
Parameters:
(x)
Body:
((set! x 100) (display Value of x inside F1: ) (displayln x) (make-unbound! x) (display Displaying x after unbounding it: ) (displayln x))
Value of x inside F1: 100
Displaying x after unbounding it: 2000

[Metacircular Evaluator Output] >>> #<void>
Finished evaluating: (F1)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x

[Metacircular Evaluator Output] >>> 2000
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (make-unbound! x)

[Metacircular Evaluator Output] >>> Un-defined the variable: x
Finished evaluating: (make-unbound! x)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: x
Failed to evaluate: x

[Metacircular Evaluator Output] >>> Unbound variable 'x
Finished evaluating: x
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (map proc items) (if (null? items) (quote ()) (cons (proc (car items)) (map proc (cdr items)))))
Making procedure with: 
Parameters:
(proc items)
Body:
((if (null? items) '() (cons (proc (car items)) (map proc (cdr items)))))

[Metacircular Evaluator Output] >>> Defined the variable: map
Finished evaluating: (define (map proc items) (if (null? items) (quote ()) (cons (proc (car items)) (map proc (cdr items)))))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (map abs (list -10 2.5 -11.6 17))

[Metacircular Evaluator Output] >>> (10 2.5 11.6 17)
Finished evaluating: (map abs (list -10 2.5 -11.6 17))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (map abs (list -10 2.5 -11.6 0.0 17 -0.5 -8 -35 96))

[Metacircular Evaluator Output] >>> (10 2.5 11.6 0.0 17 0.5 8 35 96)
Finished evaluating: (map abs (list -10 2.5 -11.6 0.0 17 -0.5 -8 -35 96))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (map inc (list -10 2.5 -11.6 0.0 17 -0.5 -8 -35 96))

[Metacircular Evaluator Output] >>> (-9 3.5 -10.6 1.0 18 0.5 -7 -34 97)
Finished evaluating: (map inc (list -10 2.5 -11.6 0.0 17 -0.5 -8 -35 96))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (F1 a b) (displayln (quote Entered-F1)) (* a b))
Making procedure with: 
Parameters:
(a b)
Body:
((displayln 'Entered-F1) (* a b))

[Metacircular Evaluator Output] >>> Defined the variable: F1
Finished evaluating: (define (F1 a b) (displayln (quote Entered-F1)) (* a b))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (F1 8 4)
Entered-F1

[Metacircular Evaluator Output] >>> 32
Finished evaluating: (F1 8 4)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (F2 a) (define b 9) (define c 11) (* a b c))
Making let expression with: 
Var-bindings:
((b '*unassigned*) (c '*unassigned*))
Body:
((set! b 9) (set! c 11) (* a b c))
Making procedure with: 
Parameters:
(a)
Body:
((let ((b '*unassigned*) (c '*unassigned*)) (set! b 9) (set! c 11) (* a b c)))

[Metacircular Evaluator Output] >>> Defined the variable: F2
Finished evaluating: (define (F2 a) (define b 9) (define c 11) (* a b c))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (F2 6)
Making procedure with: 
Parameters:
(b c)
Body:
((set! b 9) (set! c 11) (* a b c))

[Metacircular Evaluator Output] >>> 594
Finished evaluating: (F2 6)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda (a b) (displayln (quote Entered-F1)) (* a b)) 8 5)
Making procedure with: 
Parameters:
(a b)
Body:
((displayln 'Entered-F1) (* a b))
Entered-F1

[Metacircular Evaluator Output] >>> 40
Finished evaluating: ((lambda (a b) (displayln (quote Entered-F1)) (* a b)) 8 5)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda (b) (* b b)) 16)
Making procedure with: 
Parameters:
(b)
Body:
((* b b))

[Metacircular Evaluator Output] >>> 256
Finished evaluating: ((lambda (b) (* b b)) 16)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda (a b) (define e 3) (define f 4) (define g 5) (define h 6) (* a b e f g h)) 2 7)
Making let expression with: 
Var-bindings:
((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*))
Body:
((set! e 3) (set! f 4) (set! g 5) (set! h 6) (* a b e f g h))
Making procedure with: 
Parameters:
(a b)
Body:
((let ((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*)) (set! e 3) (set! f 4) (set! g 5) (set! h 6) (* a b e f g h)))
Making procedure with: 
Parameters:
(e f g h)
Body:
((set! e 3) (set! f 4) (set! g 5) (set! h 6) (* a b e f g h))

[Metacircular Evaluator Output] >>> 5040
Finished evaluating: ((lambda (a b) (define e 3) (define f 4) (define g 5) (define h 6) (* a b e f g h)) 2 7)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda (b) (define e 3) (* b e)) 85)
Making let expression with: 
Var-bindings:
((e '*unassigned*))
Body:
((set! e 3) (* b e))
Making procedure with: 
Parameters:
(b)
Body:
((let ((e '*unassigned*)) (set! e 3) (* b e)))
Making procedure with: 
Parameters:
(e)
Body:
((set! e 3) (* b e))

[Metacircular Evaluator Output] >>> 255
Finished evaluating: ((lambda (b) (define e 3) (* b e)) 85)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda () (define e 3) e))
Making let expression with: 
Var-bindings:
((e '*unassigned*))
Body:
((set! e 3) e)
Making procedure with: 
Parameters:
()
Body:
((let ((e '*unassigned*)) (set! e 3) e))
Making procedure with: 
Parameters:
(e)
Body:
((set! e 3) e)

[Metacircular Evaluator Output] >>> 3
Finished evaluating: ((lambda () (define e 3) e))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda () (display (quote Hi))))
Making procedure with: 
Parameters:
()
Body:
((display 'Hi))
Hi
[Metacircular Evaluator Output] >>> #<void>
Finished evaluating: ((lambda () (display (quote Hi))))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let ((a 1) (b 3) (c 5)) (* a b c))
Making procedure with: 
Parameters:
(a b c)
Body:
((* a b c))

[Metacircular Evaluator Output] >>> 15
Finished evaluating: (let ((a 1) (b 3) (c 5)) (* a b c))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let ((b 3) (c 5)) (* b c))
Making procedure with: 
Parameters:
(b c)
Body:
((* b c))

[Metacircular Evaluator Output] >>> 15
Finished evaluating: (let ((b 3) (c 5)) (* b c))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (let ((b 3)) (* b b))
Making procedure with: 
Parameters:
(b)
Body:
((* b b))

[Metacircular Evaluator Output] >>> 9
Finished evaluating: (let ((b 3)) (* b b))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda () (set! b 3) (* b b)))
Making procedure with: 
Parameters:
()
Body:
((set! b 3) (* b b))

[Metacircular Evaluator Output] >>> 9
Finished evaluating: ((lambda () (set! b 3) (* b b)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda () (define b 3) (* b b)))
Making let expression with: 
Var-bindings:
((b '*unassigned*))
Body:
((set! b 3) (* b b))
Making procedure with: 
Parameters:
()
Body:
((let ((b '*unassigned*)) (set! b 3) (* b b)))
Making procedure with: 
Parameters:
(b)
Body:
((set! b 3) (* b b))

[Metacircular Evaluator Output] >>> 9
Finished evaluating: ((lambda () (define b 3) (* b b)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (define (F2 a) (define b 9) (define c 11) (* a b c))
Making let expression with: 
Var-bindings:
((b '*unassigned*) (c '*unassigned*))
Body:
((set! b 9) (set! c 11) (* a b c))
Making procedure with: 
Parameters:
(a)
Body:
((let ((b '*unassigned*) (c '*unassigned*)) (set! b 9) (set! c 11) (* a b c)))

[Metacircular Evaluator Output] >>> Defined the variable: F2
Finished evaluating: (define (F2 a) (define b 9) (define c 11) (* a b c))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: (F2 456)
Making procedure with: 
Parameters:
(b c)
Body:
((set! b 9) (set! c 11) (* a b c))

[Metacircular Evaluator Output] >>> 45144
Finished evaluating: (F2 456)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda (a b c d) (define e 3) (define f 4) (displayln (quote Entered-F1)) (define g 5) (define h 6) (* a b c d e f g h)) 3 5 7 4)
Making let expression with: 
Var-bindings:
((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*))
Body:
((set! e 3) (set! f 4) (displayln 'Entered-F1) (set! g 5) (set! h 6) (* a b c d e f g h))
Making procedure with: 
Parameters:
(a b c d)
Body:
((let ((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*)) (set! e 3) (set! f 4) (displayln 'Entered-F1) (set! g 5) (set! h 6) (* a b c d e f g h)))
Making procedure with: 
Parameters:
(e f g h)
Body:
((set! e 3) (set! f 4) (displayln 'Entered-F1) (set! g 5) (set! h 6) (* a b c d e f g h))
Entered-F1

[Metacircular Evaluator Output] >>> 151200
Finished evaluating: ((lambda (a b c d) (define e 3) (define f 4) (displayln (quote Entered-F1)) (define g 5) (define h 6) (* a b c d e f g h)) 3 5 7 4)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda () e (define e 3)))
Making let expression with: 
Var-bindings:
((e '*unassigned*))
Body:
(e (set! e 3))
Making procedure with: 
Parameters:
()
Body:
((let ((e '*unassigned*)) e (set! e 3)))
Making procedure with: 
Parameters:
(e)
Body:
(e (set! e 3))
Failed to evaluate: e

[Metacircular Evaluator Output] >>> Assigned value to: e
Finished evaluating: ((lambda () e (define e 3)))
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
Starting to evaluate: ((lambda (a b c d) (define e 3) (define f 4) (displayln (quote Entered-F1)) (define g 5) (* a b c d e f g h) (define h 6)) 3 5 7 4)
Making let expression with: 
Var-bindings:
((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*))
Body:
((set! e 3) (set! f 4) (displayln 'Entered-F1) (set! g 5) (* a b c d e f g h) (set! h 6))
Making procedure with: 
Parameters:
(a b c d)
Body:
((let ((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*)) (set! e 3) (set! f 4) (displayln 'Entered-F1) (set! g 5) (* a b c d e f g h) (set! h 6)))
Making procedure with: 
Parameters:
(e f g h)
Body:
((set! e 3) (set! f 4) (displayln 'Entered-F1) (set! g 5) (* a b c d e f g h) (set! h 6))
Entered-F1
Failed to evaluate: h
Failed to evaluate: (* a b c d e f g h)

[Metacircular Evaluator Output] >>> Assigned value to: h
Finished evaluating: ((lambda (a b c d) (define e 3) (define f 4) (displayln (quote Entered-F1)) (define g 5) (* a b c d e f g h) (define h 6)) 3 5 7 4)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[Metacircular Evaluator Input] >>>
.
