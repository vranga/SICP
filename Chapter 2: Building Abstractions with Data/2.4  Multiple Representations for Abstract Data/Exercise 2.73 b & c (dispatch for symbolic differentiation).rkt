#lang racket

; Exercise 2.73.  Section 2.3.2 described a program that performs symbolic
; differentiation:

; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         ((sum? exp)
;          (make-sum (deriv (addend exp) var)
;                    (deriv (augend exp) var)))
;         ((product? exp)
;          (make-sum
;            (make-product (multiplier exp)
;                          (deriv (multiplicand exp) var))
;            (make-product (deriv (multiplier exp) var)
;                          (multiplicand exp))))
;         <more rules can be added here>
;         (else (error "unknown expression type -- DERIV" exp))))

; We can regard this program as performing a dispatch on the type of the expression to
; be differentiated. In this situation the ``type tag'' of the datum is the algebraic
; operator symbol (such as +) and the operation being performed is deriv. We can
; transform this program into data-directed style by rewriting the basic derivative
; procedure as

; (define (deriv exp var)
;    (cond ((number? exp) 0)
;          ((variable? exp) (if (same-variable? exp var) 1 0))
;          (else ((get 'deriv (operator exp)) (operands exp)
;                                             var))))
; (define (operator exp) (car exp))
; (define (operands exp) (cdr exp))

; a.  Explain what was done above. Why can't we assimilate the predicates number? and
; same-variable? into the data-directed dispatch?

; b.  Write the procedures for derivatives of sums and products, and the auxiliary code
; required to install them in the table used by the program above.

; c.  Choose any additional differentiation rule that you like, such as the one for
; exponents (exercise 2.56), and install it in this data-directed system.

; d.  In this simple algebraic manipulator the type of an expression is the algebraic
; operator that binds it together. Suppose, however, we indexed the procedures in the
; opposite way, so that the dispatch line in deriv looked like

; ((get (operator exp) 'deriv) (operands exp) var)

; What corresponding changes to the derivative system are required?

; SOLUTION

(define (deriv exp var)
	(cond
		((number? exp) 0)
		((variable? exp) (if (same-variable? exp var) 1 0))
		(else
			((get 'deriv (operator exp)) (operands exp) var)
		)
	)
)

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (deriv-of-sum exp var)
	; We need to modify this from what it was in Exercise 2.57 because in this implementation
	; there is no '+' in exp. (The calling proc strips this off.) 
	; Since I don't want to change any of the underlying procedures, I am just re-introducing the '+'
	(make-sum (deriv (addend (cons '+ exp)) var) (deriv (augend (cons '+ exp)) var))
)

(define (deriv-of-product exp var)
	; We need to modify this from what it was in Exercise 2.57 because in this implementation
	; there is no '*' in exp. (The calling proc strips this off.) 
	; Since I don't want to change any of the underlying procedures, I am just re-introducing the '*'
	(make-sum
		(make-product (multiplier (cons '* exp)) (deriv (multiplicand (cons '* exp)) var))
		(make-product (deriv (multiplier (cons '* exp)) var) (multiplicand (cons '* exp)))
	)
)

(define (deriv-of-exponentiation exp var)
	; We need to modify this from what it was in Exercise 2.57 because in this implementation
	; there is no '**' in exp. (The calling proc strips this off.) 
	; Since I don't want to change any of the underlying procedures, I am just re-introducing the '**'
	(if (number? (exponent (cons '** exp)))
		(if (not (= (exponent (cons '** exp)) 0))
			; rule applies for real-number, non-zero exponents only
			(make-product
				(exponent (cons '** exp))
				(make-product
					(make-exponentiation (base (cons '** exp)) (- (exponent (cons '** exp)) 1))
					(deriv (base (cons '** exp)) var)
				)
			)
			; if the exponent is zero, the expression will be 1 so the derivative 
			; will be zero
			0
		)
		(error "Exponent needs to be a number")
	)
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

; This is a crude implementation of get on the operation table. The table is hard-coded here so no 'put's are needed
(define (get operation type)
	(define op-table
		(list
			(cons
				'deriv
				(list
					(cons '+ deriv-of-sum)
					(cons '* deriv-of-product)
					(cons '** deriv-of-exponentiation)
				)
			)
		)
	)

	(define (find-op-row oper table)
		(cond
			((not (pair? table)) (error "op-table Not a pair!"))
			((null? table) (error "Operation not found: " oper))
			(else
				(if (eq? oper (car (car table)))
					(car table)
					(find-op-row oper (cdr table))
				)
			)
		)
	)

	(define (find-type-in-op-list op-list t)
		(cond
			((not (pair? op-list)) (error "op-list Not a pair!"))
			((null? op-list) (error "Type not found: " t))
			(else
				(if (eq? t (car (car op-list)))
					(cdr (car op-list))
					(find-type-in-op-list (cdr op-list) t)
				)
			)
		)
	)

	(find-type-in-op-list (cdr (find-op-row operation op-table)) type)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (deriv '(+ x 4) 'x)
1
> (deriv '(+ x x 4) 'x)
2
> (deriv '(* x y) 'x)
'y
> (deriv '(* (* x y) (+ x 3)) 'x)
'(+ (* x y) (* y (+ x 3)))
> (deriv '(+ x x x x) 'x)
4
> (deriv '(+ x x x x (* x y)) 'x)
'(+ 1 (+ 1 (+ 1 (+ 1 y))))
> (deriv '(+ (* x y) x x x x) 'x)
'(+ y 4)
> (deriv '(** x -3) 'x)
'(* -3 (** x -4))
> (deriv '(** x 3) 'x)
'(* 3 (** x 2))
> (deriv '(** x 0) 'x)
0
> (deriv '(** x 1) 'x)
1
> (deriv '(** x 2) 'x)
'(* 2 x)
> (deriv '(* x y (+ x 3)) 'x)
'(+ (* x y) (* y (+ x 3)))
> (deriv '(* (* x y) (+ x 3)) 'x)
'(+ (* x y) (* y (+ x 3)))
> (deriv '(+ x (* 2 x) (** x 5)) 'x)
'(+ 1 (+ 2 (* 5 (** x 4))))
> (deriv '(* (* 2 (** x 3)) (** x 5)) 'x)
'(+ (* (* 2 (** x 3)) (* 5 (** x 4))) (* (* 2 (* 3 (** x 2))) (** x 5)))
> (deriv '(** x x) 'x)
. . Exponent needs to be a number
> 
