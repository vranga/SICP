#lang racket

; Exercise 2.57.  Extend the differentiation program to handle sums and products of
; arbitrary numbers of (two or more) terms. Then the last example above could be
; expressed as

; (deriv '(* x y (+ x 3)) 'x)

; Try to do this by changing only the representation for sums and products, without
; changing the deriv procedure at all. For example, the addend of a sum would be the
; first term, and the augend would be the sum of the rest of the terms.

; SOLUTION

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

(define (make-sum a1 a2 . z)
	(cond
		((null? z)
			(cond
				((=number? a1 0) a2)
				((=number? a2 0) a1)
				((and (number? a1) (number? a2)) (+ a1 a2))
				(else
					(list '+ a1 a2)
				)
			)
		)
		((=number? a1 0) (make-sum a2 z))
		((=number? a2 0) (make-sum a1 z))
		((and (number? a1) (number? a2)) (make-sum (+ a1 a2) z))
		(else
			(list '+ a1 (cons a2 z))
		)
	)
)

(define (make-product m1 m2 . z)
	(cond
		((or (=number? m1 0) (=number? m2 0) (=number? z 0)) 0)
		((null? z)
			(cond
				((=number? m1 1) m2)
				((=number? m2 1) m1)
				((and (number? m1) (number? m2)) (* m1 m2))
				(else
					(list '* m1 m2)
				)
			)
		)
		((=number? m1 1) (make-product m2 z))
		((=number? m2 1) (make-product m1 z))
		((and (number? m1) (number? m2)) (make-product (* m1 m2) z))
		(else
			(list '* m1 (cons m2 z))
		)
	)
)

(define (sum? x)
	(and (pair? x) (eq? (car x) '+))
)

(define (addend s)
	(cadr s)
)

(define (augend s)
	; the logic here is to figure out the nature of the augend i.e. is it a single element
	; or is it a list by itself and process accordingly
	(let
		(
			(aug (cdr (cdr s)))
			(fourth-element (cdr (cdr (cdr s))))
		)
		(cond
			((null? fourth-element) (car aug))
			(else
				(cons '+ aug)
			)
		)
	)
)

(define (product? x)
	(and (pair? x) (eq? (car x) '*))
)

(define (multiplier p)
	(cadr p)
)

(define (multiplicand p)
	; the logic here is to figure out the nature of the multiplicand i.e. is it a single element
	; or is it a list by itself and process accordingly
	(let
		(
			(aug (cdr (cdr p)))
			(fourth-element (cdr (cdr (cdr p))))
		)
		(cond
			((null? fourth-element) (car aug))
			(else
				(cons '* aug)
			)
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (deriv '(+ x 4) 'x)
(deriv '(+ x x 4) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(+ x x x x) 'x)
(deriv '(+ x x x x (* x y)) 'x)
(deriv '(+ (* x y) x x x x) 'x)
(deriv '(** x -3) 'x)
(deriv '(** x 3) 'x)
(deriv '(** x 0) 'x)
(deriv '(** x 1) 'x)
(deriv '(** x 2) 'x)
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

> (deriv '(* x y (+ x 3)) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
'(+ (* x y) (* y (+ x 3)))
'(+ (* x y) (* y (+ x 3)))

> (deriv '(** x x) 'x)
. . unknown expression type -- DERIV (** x x)
> 
