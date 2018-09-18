#lang racket

; Allow standard algebraic notation, such as (x + 3 * (x + y + 2)), which does not use unnecessary parentheses and
; assumes that multiplication is done before addition.

; Note: I have added a new procedure: remove-superfluous-parens that helps simplify expressions. See below

; Let's take this example: (x * 3 + (x + y + 2) + (5 * x ** 2))
; We work our way from the left to the right assuming that any expression no matter how 
; complex it is, is a sum of other expressions. So the logic is: look for the first occurrence of the 
; + sign. Everything to its left will be the addend and everything to its right will be the augend. 
; Then we apply the sum rule of differentiation on the addend and the augend just like in the
; previous exercise. When we do this recursively, all the sum rules will get applied correctly regardless
; of how many terms are '+'ed together in the overall expression.
; If we do not find any + operators as we scan the list, we re-scan the list for * operators and apply
; product-rule. And if we do not find any * operators either, then we look for ** operators. 
; This order of evaluation ensures that multiplication is done before addition and exponentiation is done
; before multiplication
;
; This logic also allows us to treat +, * and ** as if these operators work on only two operands


(define (deriv expr var)
	(let ((exp (remove-superfluous-parens expr)))
		(cond
			((number? exp) 0)
			((variable? exp) (if (same-variable? exp var) 1 0))
			; Look for sum first
			((sum? exp)
				(make-sum
					(deriv (remove-superfluous-parens (addend exp)) var)
					(deriv (remove-superfluous-parens (augend exp)) var)
				)
			)
			; Look for product next
			((product? exp)
				(make-sum
					(make-product
						(remove-superfluous-parens (multiplier exp))
						(deriv (remove-superfluous-parens (multiplicand exp)) var)
					)
					(make-product
						(deriv (remove-superfluous-parens (multiplier exp)) var)
						(remove-superfluous-parens (multiplicand exp))
					)
				)
			)
			; Look for exponentiation last
			((and (exponentiation? exp) (number? (exponent exp)))
				(if (not (= (exponent exp) 0))
					; rule applies for real-number, non-zero exponents only
					(make-product
						(exponent exp)
						(make-product
							(make-exponentiation (remove-superfluous-parens (base exp)) (- (exponent exp) 1))
							(deriv (remove-superfluous-parens (base exp)) var)
						)
					)
					; if the exponent is zero, the expression will be 1 so the derivative 
					; will be zero
					0
				)
			)
			(else
				(error "unknown expression type -- DERIV" exp)
			)
		)
	)
)

; Reduces an expression of the form ((((a + b * (c + d))))) to (a + b * (c + d))
(define (remove-superfluous-parens exp)
	(cond
		((and (pair? exp) (null? (cdr exp))) (remove-superfluous-parens (car exp)))
		(else
			exp
		)
	)
)

; Infix notation will be of the form (base ** exponent)
; In this implementation, I am not supporting multiple levels of exponentiation.
; So something like x ** 5 ** 6 will not work. I expect the exponent to always be a number
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

; make-sum and make-product don't need to support more than 2 arguments because of the way
; in which expressions are broken down. See detailed comments at the top

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

; Infix notation will be of the form (a1 + a2 + a3 + ...)
(define (sum? exp)
	(operation? '+ exp)
)

(define (addend exp)
	(left-operand '+ exp)
)

(define (augend exp)
	(right-operand '+ exp)
)

; Infix notation will be of the form (a1 * a2 * a3 * ...)
(define (product? x)
	(operation? '* x)
)

(define (multiplier p)
	(left-operand '* p)
)

(define (multiplicand p)
	(right-operand '* p)
)

; Generic Procedures

; Allows us to check if a given expression is an operation in + or * or any other operator
(define (operation? oper exp)
	; Here 'oper' can be any operator like +, -, *, / etc.
	; Since this is standard algebraic notation, the 'oper' can appear anywhere
	; So we need to scan the expression and find the first one
	(define (operation-internal? op x)
		(cond
			((null? x) false)
			((eq? (car x) op) true)
			(else
				(operation-internal? op (cdr x))
			)
		)
	)

	(and (pair? exp) (operation-internal? oper exp))
)

; Given an operator, this procedure finds the left operand in the supplied expression
(define (left-operand oper exp)
	; Since this is standard algebraic notation, oper can appear anywhere
	; So we need to scan the expression and find the first one. Treat everything
	; before it as the left-operand

	; Assuming that the operator is '+', in an expression of the form (a1 * a2 ** a3 + a4 + a5),
	; the left-operand is (a1 * a2 ** a3)
	(define (left-operand-internal s)
		(cond
			; we are expecting to find oper before we reach the end
			; of the expression. So if we find a null, then it is an error
			((null? s) (error "unknown expression type -- left-operand" exp))
			; If we find oper to be the first element in the expression, then
			; what we have collected so far is the left-operand, so we can stop looking
			; Just return an empty list which will be used to terminate the list
			; that is being constructed recursively
			((eq? (car s) oper) (list))
			(else
				; continue building the left-operand expression
				(cons (car s) (left-operand-internal (cdr s)))
			)
		)
	)

	(cond
		; Since we are trying to find the left-operand, exp needs to be a pair
		((not (pair? exp)) (error "Argument needs to be a pair -- left-operand" exp))
		; Since this is an infix notation, the first element cannot be oper
		((eq? (car exp) oper) (error "First element cannot be a" oper " -- addend" exp))
		(else
			(left-operand-internal exp)
		)
	)
)

; Given an operator, this procedure finds the right operand in the supplied expression
(define (right-operand oper exp)
	; Since this is standard algebraic notation, oper can appear anywhere
	; So we need to scan the expression and find the first one. Treat everything
	; after it as the right-operand

	; Assuming that oper is +, in an expression of the form (a1 * a2 ** a3 + a4 + a5),
	; the right-operand is (a4 + a5)
	(define (right-operand-internal s)
		(cond
			; we are expecting to find oper before we reach the end
			; of the expression. So if we find a null, then it is an error
			((null? s) (error "unknown expression type -- right-operand" exp))
			; If we find oper to be the first element in the expression, then
			; we can stop looking. What follows oper will be the right-operand
			((eq? (car s) oper) (cdr s))
			(else
				; continue looking
				(right-operand-internal (cdr s))
			)
		)
	)

	(cond
		; Since we are trying to find the right-operand, exp needs to be a pair
		((not (pair? exp)) (error "Argument needs to be a pair -- right-operand" exp))
		; Since this is an infix notation, the first element cannot be oper
		((eq? (car exp) oper) (error "First element cannot be a " oper " -- augend" exp))
		(else
			(right-operand-internal exp)
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define exp '(a1 * a2 ** a3 + a4 + a5))
> exp
'(a1 * a2 ** a3 + a4 + a5)
> (addend exp)
'(a1 * a2 ** a3)
> (augend exp)
'(a4 + a5)
> (define exp2 '(a1 * (a2 + a6 + (a7 * a8 + a9)) ** a3 + a4 + a5))
> (define exp3 '((a1 * (a2 + a6 + (a7 * a8 + a9)) + a3) + a4 + a5))
> exp2
'(a1 * (a2 + a6 + (a7 * a8 + a9)) ** a3 + a4 + a5)
> exp3
'((a1 * (a2 + a6 + (a7 * a8 + a9)) + a3) + a4 + a5)
> (addend exp3)
'((a1 * (a2 + a6 + (a7 * a8 + a9)) + a3))
> (augend exp3)
'(a4 + a5)
> (addend (remove-superfluous-parens (addend exp3)))
'(a1 * (a2 + a6 + (a7 * a8 + a9)))
> (augend (remove-superfluous-parens (addend exp3)))
'(a3)
> (addend (remove-superfluous-parens (augend exp3)))
'(a4)
> (augend (remove-superfluous-parens (augend exp3)))
'(a5)
> (define exp4 '((a1 + (a2 + a6 + (a7 * a8 + a9)) + a3) + a4 + a5))
> (addend exp4)
'((a1 + (a2 + a6 + (a7 * a8 + a9)) + a3))
> (addend (remove-superfluous-parens (addend exp4)))
'(a1)
> (augend (remove-superfluous-parens (addend exp4)))
'((a2 + a6 + (a7 * a8 + a9)) + a3)
> (addend (augend (remove-superfluous-parens (addend exp4))))
'((a2 + a6 + (a7 * a8 + a9)))
> (augend (augend (remove-superfluous-parens (addend exp4))))
'(a3)
> (addend (remove-superfluous-parens (addend (augend (remove-superfluous-parens (addend exp4))))))
'(a2)
> (augend (remove-superfluous-parens (addend (augend (remove-superfluous-parens (addend exp4))))))
'(a6 + (a7 * a8 + a9))
> (addend (augend (remove-superfluous-parens (addend (augend (remove-superfluous-parens (addend exp4)))))))
'(a6)
> (augend (augend (remove-superfluous-parens (addend (augend (remove-superfluous-parens (addend exp4)))))))
'((a7 * a8 + a9))
> (addend (remove-superfluous-parens (augend (augend (remove-superfluous-parens (addend (augend (remove-superfluous-parens (addend exp4)))))))))
'(a7 * a8)
> (augend (remove-superfluous-parens (augend (augend (remove-superfluous-parens (addend (augend (remove-superfluous-parens (addend exp4)))))))))
'(a9)

> (deriv '(x + 4) 'x)
1
> (deriv '(x + x + 4) 'x)
2
> (deriv '((((x * y)))) 'x)
'y
> (deriv '(x * y * (x + 3)) 'x)
'((x * y) + (y * (x + 3)))
> (deriv '(x + x + x + x) 'x)
4

> (deriv '(((x + x) + x + x) + x * y) 'x)
'(4 + y)
> (deriv '(x * y + x + x + x + x) 'x)
'(y + 4)

> (deriv '(x ** -3) 'x)
> (deriv '(x ** 3) 'x)
> (deriv '(x ** 0) 'x)
> (deriv '(x ** 1) 'x)
> (deriv '(x ** 2) 'x)
'(-3 * (x ** -4))
'(3 * (x ** 2))
0
1
'(2 * x)

> (deriv '((x * y) * (x + 3)) 'x)
> (deriv '(((x * y) * (x + 3))) 'x)
> (deriv '(((x * y) * (((x + 3))))) 'x)
> (deriv '((((x * y)) * (((x + 3))))) 'x)
'((x * y) + (y * (x + 3)))
'((x * y) + (y * (x + 3)))
'((x * y) + (y * (x + 3)))
'((x * y) + (y * (x + 3)))

> (deriv '((x * y) * x + 3) 'x)
'((x * y) + (y * x))

> (deriv 'X 'x)
0
> (deriv '((((x * y)) * (((x + (x ** 7)))))) 'x)
'(((x * y) * (1 + (7 * (x ** 6)))) + (y * (x + (x ** 7))))
>

> (deriv '(x + (3 * (x + (y + 2)))) 'x)
4
> (deriv '(x + 3 * (x + y + 2)) 'x)
4
> (deriv '(x + 3 * x + y + 2) 'x)
4

> (deriv '(5 * x ** 8 + 8 * x ** 4) 'x)
'((5 * (8 * (x ** 7))) + (8 * (4 * (x ** 3))))
