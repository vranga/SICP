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
				(set-mcar! (make-binding null null))
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
			(scan-out-defines
				(lambda-body expression)
			)
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
	(displayln "Making procedure with: ")
	(displayln "Parameters:")
	(displayln parameters)
	(displayln "Body:")
	(displayln body)
	(list 'procedure parameters body env)
)

; let Expressions
(define (let? expression) (tagged-list? expression 'let))
(define (named-let? expression)
	(if (let? expression)
		(if (and (not (null? (cdddr expression))) (symbol? (cdr expression)))
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
'Done
> (define L1 '(lambda (a b) (displayln 'Entered-F1) (* a b)))
> (define L2 '(lambda (b) (* b b)))
> (lambda-parameters L1)
'(a b)
> (lambda-parameters L2)
'(b)
> (lambda-body L2)
'((* b b))
> (lambda-body L1)
'((displayln 'Entered-F1) (* a b))
> (scan-out-defines (lambda-body L1))
'((displayln 'Entered-F1) (* a b))
> (scan-out-defines (lambda-body L2))
'((* b b))
> (define L3 '(lambda (a b c d) (define e 3) (define f 4) (displayln 'Entered-F1) (define g 5) (* a b c d e f g h) (define h 6)))
> (lambda-parameters L3)
'(a b c d)
> (lambda-body L3)
'((define e 3) (define f 4) (displayln 'Entered-F1) (define g 5) (* a b c d e f g h) (define h 6))
> (scan-out-defines (lambda-body L3))
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
> (define L4 '(lambda (a b) (define e 3) (define f 4) (define g 5) (* a b c d e f g h) (define h 6)))
> (lambda-parameters L4)
'(a b)
> (lambda-body L4)
'((define e 3) (define f 4) (define g 5) (* a b c d e f g h) (define h 6))
> (scan-out-defines (lambda-body L4))
Making let expression with: 
Var-bindings:
((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*))
Body:
((set! e 3) (set! f 4) (set! g 5) (* a b c d e f g h) (set! h 6))
'((let ((e '*unassigned*) (f '*unassigned*) (g '*unassigned*) (h '*unassigned*))
    (set! e 3)
    (set! f 4)
    (set! g 5)
    (* a b c d e f g h)
    (set! h 6)))
> (define L5 '(lambda (b) (define e 3) (* b e)))
> (lambda-parameters L5)
'(b)
> (lambda-body L5)
'((define e 3) (* b e))
> (scan-out-defines (lambda-body L5))
Making let expression with: 
Var-bindings:
((e '*unassigned*))
Body:
((set! e 3) (* b e))
'((let ((e '*unassigned*)) (set! e 3) (* b e)))
> (define L6 '(lambda () (define e 3) e))
> (lambda-parameters L6)
'()
> (lambda-body L6)
'((define e 3) e)
> (scan-out-defines (lambda-body L6))
Making let expression with: 
Var-bindings:
((e '*unassigned*))
Body:
((set! e 3) e)
'((let ((e '*unassigned*)) (set! e 3) e))
> (define L7 '(lambda () (display 'Hi)))
> (lambda-parameters L7)
'()
> (lambda-body L7)
'((display 'Hi))
> (scan-out-defines (lambda-body L7))
'((display 'Hi))
> (driver-loop)

[Metacircular Evaluator Input] >>>
(define (F1 a b) (displayln 'Entered-F1) (* a b))
Making procedure with: 
Parameters:
(a b)
Body:
((displayln 'Entered-F1) (* a b))

[Metacircular Evaluator Output] >>> Defined the variable: F1
[Metacircular Evaluator Input] >>>
(F1 8 4)
Entered-F1

[Metacircular Evaluator Output] >>> 32
[Metacircular Evaluator Input] >>>
(define (F2 a) (define b 9) (define c 11) (* a b c))
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
[Metacircular Evaluator Input] >>>
(F2 6)
Making procedure with: 
Parameters:
(b c)
Body:
((set! b 9) (set! c 11) (* a b c))

[Metacircular Evaluator Output] >>> 594
[Metacircular Evaluator Input] >>>
((lambda (a b) (displayln 'Entered-F1) (* a b)) 8 5)
Making procedure with: 
Parameters:
(a b)
Body:
((displayln 'Entered-F1) (* a b))
Entered-F1

[Metacircular Evaluator Output] >>> 40
[Metacircular Evaluator Input] >>>
((lambda (b) (* b b)) 16)
Making procedure with: 
Parameters:
(b)
Body:
((* b b))

[Metacircular Evaluator Output] >>> 256
[Metacircular Evaluator Input] >>>
((lambda (a b) (define e 3) (define f 4) (define g 5) (define h 6) (* a b e f g h)) 2 7)
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
[Metacircular Evaluator Input] >>>
((lambda (b) (define e 3) (* b e)) 85)
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
[Metacircular Evaluator Input] >>>
((lambda () (define e 3) e))
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
[Metacircular Evaluator Input] >>>
((lambda () (display 'Hi)))
Making procedure with: 
Parameters:
()
Body:
((display 'Hi))
Hi
[Metacircular Evaluator Output] >>> #<void>
[Metacircular Evaluator Input] >>>
(let ((a 1) (b 3) (c 5)) (* a b c))
Making procedure with: 
Parameters:
(a b c)
Body:
((* a b c))

[Metacircular Evaluator Output] >>> 15
[Metacircular Evaluator Input] >>>
(let ((b 3) (c 5)) (* b c))
Making procedure with: 
Parameters:
(b c)
Body:
((* b c))

[Metacircular Evaluator Output] >>> 15
[Metacircular Evaluator Input] >>>
(let ((b 3)) (* b b))
Making procedure with: 
Parameters:
(b)
Body:
((* b b))

[Metacircular Evaluator Output] >>> 9
[Metacircular Evaluator Input] >>>
((lambda () (set! b 3) (* b b)))
Making procedure with: 
Parameters:
()
Body:
((set! b 3) (* b b))
Failed to evaluate: (set! b 3)
Failed to evaluate: b
Failed to evaluate: b
Failed to evaluate: (* b b)

[Metacircular Evaluator Output] >>> *: contract violation
  expected: number?
  given: "Unbound variable 'b"
[Metacircular Evaluator Input] >>>
((lambda () (define b 3) (* b b)))
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
[Metacircular Evaluator Input] >>>
(define (F2 a) (define b 9) (define c 11) (* a b c))
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
[Metacircular Evaluator Input] >>>
(F2 456)
Making procedure with: 
Parameters:
(b c)
Body:
((set! b 9) (set! c 11) (* a b c))

[Metacircular Evaluator Output] >>> 45144
[Metacircular Evaluator Input] >>>
((lambda (a b c d) (define e 3) (define f 4) (displayln 'Entered-F1) (define g 5) (define h 6) (* a b c d e f g h)) 3 5 7 4)
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
[Metacircular Evaluator Input] >>>
((lambda () e (define e 3)))
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
[Metacircular Evaluator Input] >>>
.
