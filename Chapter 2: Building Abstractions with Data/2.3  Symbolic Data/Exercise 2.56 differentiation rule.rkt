#lang racket

; Exercise 2.56. Show how to extend the basic differentiator to handle more kinds of
; expressions. For instance, implement the differentiation rule

; d(u^n)/dx = n * u^(n-1) * (du/dx)

; by adding a new clause to the deriv program and defining appropriate procedures
; exponentiation?, base, exponent, and make-exponentiation. (You may use the symbol **
; to denote exponentiation.) Build in the rules that anything raised to the power 0 is 1
; and anything raised to the power 1 is the thing itself.

; SOLUTION

(define (deriv exp var)
	(cond ((number? exp) 0)
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

; An exponentiation is a list whose first element is the symbol **
(define (exponentiation? expr)
	(and (pair? expr) (eq? (car expr) '**))
)

; The base is the second item of the exponentiation list
(define (base expr)
	(car (cdr expr))
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
		(else (list '** base exponent))
	)
)

(define (=number? exp num)
	(and (number? exp) (= exp num))
)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (make-sum a1 a2)
	(cond
		((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list '+ a1 a2))
	)
)

(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list '* m1 m2))
	)
)

(define (sum? x)
	(and (pair? x) (eq? (car x) '+))
)

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
	(and (pair? x) (eq? (car x) '*))
)

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (deriv '(** x -3) 'x)
'(* -3 (** x -4))
> (deriv '(** x -2) 'x)
'(* -2 (** x -3))
> (deriv '(** x -1) 'x)
'(* -1 (** x -2))
> (deriv '(** x 0) 'x)
0
> (deriv '(** x 1) 'x)
1
> (deriv '(** x 2) 'x)
'(* 2 x)
> (deriv '(** x 3) 'x)
'(* 3 (** x 2))
> (deriv '(** x 4) 'x)
'(* 4 (** x 3))
> (deriv '(** x 5) 'x)
'(* 5 (** x 4))
> (deriv '(** x 6) 'x)
'(* 6 (** x 5))
> (deriv '(** x .5) 'x)
'(* 0.5 (** x -0.5))
> (deriv '(** x x) 'x)
. . unknown expression type -- DERIV (** x x)
> 
