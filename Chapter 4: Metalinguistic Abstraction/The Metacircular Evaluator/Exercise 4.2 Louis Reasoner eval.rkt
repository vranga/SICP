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

; S O L U T I O N

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

; Dummy implementation (to allow compilation of the above procedures)
(define (no-operands? exps)
	(if (pair? exps)
		false
		true
	)
)

(define (first-operand exps)
	(car exps)
)

(define (rest-operands exps)
	(cdr exps)
)

(define (eval exp env)
	(display "Evaluating ")
	(display exp)
	(newline)
	exp
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
> (define expressions (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j))
> (define env 'env)
> (evaluate-l-to-r-list-of-values expressions env)
Evaluating a
Evaluating b
Evaluating c
Evaluating d
Evaluating e
Evaluating f
Evaluating g
Evaluating h
Evaluating i
Evaluating j
'(a b c d e f g h i j)
> (evaluate-r-to-l-list-of-values expressions env)
Evaluating j
Evaluating i
Evaluating h
Evaluating g
Evaluating f
Evaluating e
Evaluating d
Evaluating c
Evaluating b
Evaluating a
'(a b c d e f g h i j)
> 
