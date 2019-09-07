#lang racket

; Exercise 2.58.  Suppose we want to modify the differentiation program so that it works
; with ordinary mathematical notation, in which + and * are infix rather than prefix
; operators. Since the differentiation program is defined in terms of abstract data, we
; can modify it to work with different representations of expressions solely by changing
; the predicates, selectors, and constructors that define the representation of the
; algebraic expressions on which the differentiator is to operate.

; a. Show how to do this in order to differentiate algebraic expressions presented in
; infix form, such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + and
; * always take two arguments and that expressions are fully parenthesized.

; b. The problem becomes substantially harder if we allow standard algebraic notation,
; such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that
; multiplication is done before addition. Can you design appropriate predicates,
; selectors, and constructors for this notation such that our derivative program still
; works?

; SOLUTION

; Assume that +, * and ** always take two arguments and that expressions are fully parenthesized.

(define (deriv exp var)
	(cond
		((number? exp) 0)
		((variable? exp) (if (same-variable? exp var) 1 0))
		((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
		((and (exponentiation? exp) (number? (exponent exp)))
			(if (not (= (exponent exp) 0))
				; rule applies for real-number, non-zero exponents only
				(make-product
					(exponent exp)
					(make-product
						(make-exponentiation (base exp) (- (exponent exp) 1))
						(deriv (base exp) var)
					)
				)
				; if the exponent is zero, the expression will be 1 so the derivative 
				; will be zero
				0
			)
		)
		((product? exp)
			(make-sum
				(make-product (multiplier exp) (deriv (multiplicand exp) var))
				(make-product (deriv (multiplier exp) var) (multiplicand exp))
			)
		)
		(else
			(error "unknown expression type -- DERIV" exp)
		)
	)
)

; Infix notation will be of the form (base ** exponent)
(define (exponentiation? expr)
	(and (pair? expr) (eq? (car (cdr expr)) '**))
)

; The base is the first item of the exponentiation list
(define (base expr)
	(car expr)
)

; The exponent is the third item of the exponentiation list
(define (exponent expr)
	(car (cdr (cdr expr)))
)

(define (make-exponentiation base exponent)
	(cond
		; Anything raised to the power 0 is 1
		((=number? exponent 0) 1)
		; Anything raised to the power 1 is the thing itself
		((=number? exponent 1) base)
		; 1 raised to anything is 1
		((=number? base 1) 1)
		((and (number? base) (number? exponent)) (expt base exponent))
		(else (list base '** exponent))
	)
)

(define (=number? exp num)
	(and (number? exp) (= exp num))
)

(define (variable? x)
	(symbol? x)
)

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (make-sum a1 a2)
	(cond
		((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list a1 '+ a2))
	)
)

(define (make-product m1 m2)
	(cond
		((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list m1 '* m2))
	)
)

; Infix notation will be of the form (a1 + a2)
(define (sum? x)
	(and (pair? x) (eq? (car (cdr x)) '+))
)

; The addend is the first item in the list
(define (addend s)
	(car s)
)

; The augend is the third item in the list
(define (augend s)
	(caddr s)
)

; Infix notation will be of the form (a1 * a2)
(define (product? x)
	(and (pair? x) (eq? (car (cdr x)) '*))
)

; The multiplier is the first item in the list
(define (multiplier p)
	(car p)
)

; The multiplicand is the third item in the list
(define (multiplicand p)
	(caddr p)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (deriv '(x + 4) 'x)
(deriv '(x + (x + 4)) 'x)
(deriv '(x * y) 'x)
(deriv '((x * y) * (x + 3)) 'x)
(deriv '(x + (x + (x + x))) 'x)
(deriv '((((x + x) + x) + x) + (x * y)) 'x)
(deriv '((x * y) + ((x + x) + (x + x))) 'x)
(deriv '(x ** -3) 'x)
(deriv '(x ** 3) 'x)
(deriv '(x ** 0) 'x)
(deriv '(x ** 1) 'x)
(deriv '(x ** 2) 'x)
1
2
'y
'(+ (* x y) (* y (+ x 3)))
4
'(+ 1 (+ 1 (+ 1 (+ 1 y))))
'(+ y 4)
'(* -3 (** x -4))
'(* 3 (** x 2))
0
1
'(* 2 x)

> (deriv '((x * y) * (x + 3)) 'x)
'(+ (* x y) (* y (+ x 3)))
'(+ (* x y) (* y (+ x 3)))

> (deriv '(x ** x) 'x)
. . unknown expression type -- DERIV (** x x)
> 
