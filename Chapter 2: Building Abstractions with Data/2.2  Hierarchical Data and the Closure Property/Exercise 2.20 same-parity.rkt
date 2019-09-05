#lang racket

; Exercise 2.20.  The procedures +, *, and list take arbitrary numbers of arguments. One way to
; define such procedures is to use define with dotted-tail notation. In a procedure definition,
; a parameter list that has a dot before the last parameter name indicates that, when the
; procedure is called, the initial parameters (if any) will have as values the initial
; arguments, as usual, but the final parameter's value will be a list of any remaining
; arguments. For instance, given the definition

; (define (f x y . z) <body>)

; the procedure f can be called with two or more arguments. If we evaluate

; (f 1 2 3 4 5 6)

; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4 5 6). Given the
; definition

; (define (g . w) <body>)

; the procedure g can be called with zero or more arguments. If we evaluate

; (g 1 2 3 4 5 6)

; then in the body of g, w will be the list (1 2 3 4 5 6).11

; Use this notation to write a procedure same-parity that takes one or more integers and
; returns a list of all the arguments that have the same even-odd parity as the first argument.
; For example,

; (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)

; (same-parity 2 3 4 5 6 7)
; (2 4 6)

; SOLUTION

(define (same-parity . list-of-integers)

	(define (pick-evens number-list)
		(cond
			((null? number-list) (list))
			((even? (car number-list)) (cons (car number-list) (pick-evens (cdr number-list))))
			(else
				; first element is odd, so continue looking
				(pick-evens (cdr number-list))
			)
		)
	)

	(define (pick-odds number-list)
		(cond
			((null? number-list) (list))
			((odd? (car number-list)) (cons (car number-list) (pick-odds (cdr number-list))))
			(else
				; first element is even, so continue looking
				(pick-odds (cdr number-list))
			)
		)
	)

	(cond
		((null? list-of-integers) (error "Supplied list is empty"))
		((even? (car list-of-integers)) (pick-evens list-of-integers))
		(else
			(pick-odds list-of-integers)
		)
	)

)

(define (even? n)
	(= (remainder n 2) 0)
)

(define (odd? n)
	(= (remainder n 2) 1)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (same-parity 1 2 3 4 5 6 7)
'(1 3 5 7)
> (same-parity 2 3 4 5 6 7)
'(2 4 6)
> (same-parity 3 4 5 6 7)
'(3 5 7)
> (same-parity 4 5 6 7)
'(4 6)
> (same-parity 5 6 7)
'(5 7)
> (same-parity 6 7)
'(6)
> (same-parity 7)
'(7)
> (same-parity 8)
'(8)
> (same-parity)
. . Supplied list is empty
> (same-parity 9 5 1 7 3 6 2 4 8 11 18 17 16 14 12 13 10)
'(9 5 1 7 3 11 17 13)
> (same-parity 50 9 5 1 7 3 6 2 4 8 11 18 17 16 14 12 13 10)
'(50 6 2 4 8 18 16 14 12 10)
> 
