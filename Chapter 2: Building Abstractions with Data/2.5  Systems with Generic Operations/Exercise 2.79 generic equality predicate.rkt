#lang racket

; Exercise 2.79.  Define a generic equality predicate equ? that tests the equality of two
; numbers, and install it in the generic arithmetic package. This operation should work
; for ordinary numbers, rational numbers, and complex numbers.

; Generic Mathematical operations on numbers that may be ordinary numbers, rational
; numbers or complex numbers

(define (equ? x y) (apply-generic 'equal? x y))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; Constructions of specific types of numbers (ordinary, rational or complex) using generic operations
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))

; Generic procedure implementation
(define (apply-generic op . args)
	; (display "Entered apply-generic with ")
	; (display op)
	; (display ", ")
	; (display args)
	; (newline)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			; (newline)
			; (display "Calling proc with ")
			; (display (map contents args))
			; (newline)
			(if proc
				(apply proc (map contents args))
				(error
					"No method for these types -- APPLY-GENERIC"
					(list op type-tags)
				)
			)
		)
	)
)

(define (type-tag datum)
	(cond
		((pair? datum) (car datum))
		((number? datum) 'scheme-number)
		(else
			(error "Bad tagged datum -- TYPE-TAG" datum)
		)
	)
)

(define (contents datum)
	(cond
		((pair? datum) (cdr datum))
		((number? datum) datum)
		(else
			(error "Bad tagged datum -- CONTENTS" datum)
		)
	)
)

(define (square x)
	(* x x)
)

; Rational Number procedures
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
	(let ((g (gcd n d)))
		(cond
			((= 0 d) (error "Denominator in a rational number cannot be zero"))
			((and (< n 0) (< d 0)) (cons (/ (abs n) g) (/ (abs d) g)))
			((and (< n 0) (> d 0)) (cons (/ n g) (/ d g)))
			((and (> n 0) (< d 0)) (cons (/ (* -1 n) g) (/ (abs d) g)))
			((and (> n 0) (> d 0)) (cons (/ n g) (/ d g)))
		)
	)
)

(define (add-rat x y) (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (sub-rat x y) (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (mul-rat x y) (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
(define (div-rat x y) (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
(define (equal-rat? x y)
	(display "Entered equal-rat?")
	(newline)
	(and (= (numer x) (numer y)) (= (denom x) (denom y)))
)

; Complex Number procedures
(define (magnitude z)
	; (display "Entered proc magnitude")
	; (newline)
	(apply-generic 'magnitude z)
)
(define (angle z) (apply-generic 'angle z))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))

(define (add-complex z1 z2)
	(make-from-real-imag
		(+ (real-part z1) (real-part z2))
		(+ (imag-part z1) (imag-part z2))
	)
)

(define (sub-complex z1 z2)
	(make-from-real-imag
		(- (real-part z1) (real-part z2))
		(- (imag-part z1) (imag-part z2))
	)
)

(define (mul-complex z1 z2)
	(make-from-mag-ang
		(* (magnitude z1) (magnitude z2))
		(+ (angle z1) (angle z2))
	)
)

(define (div-complex z1 z2)
	(make-from-mag-ang
		(/ (magnitude z1) (magnitude z2))
		(- (angle z1) (angle z2))
	)
)

(define (equal-complex? c1 c2)
	(display "Entered equal-complex?")
	(newline)
	(and (= (real-part c1) (real-part c2)) (= (imag-part c1) (imag-part c2)))
)

; Rectangular (Complex) Number procedures
(define (make-from-real-imag-rectangular x y) (cons x y))
(define (make-from-mag-ang-rectangular r a) (cons (* r (cos a)) (* r (sin a))))
(define (magnitude-rectangular z)
	; (display "Entered magnitude-rectangular")
	; (newline)
	(sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z))))
)
(define (angle-rectangular z) (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (real-part-rectangular z)
	; (display "Entered real-part-rectangular")
	; (newline)
	(car z)
)
(define (imag-part-rectangular z)
	; (display "Entered imag-part-rectangular")
	; (newline)
	(cdr z)
)

; Polar (Complex) Number procedures
(define (make-from-real-imag-polar x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
(define (make-from-mag-ang-polar r a) (cons r a))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))

; Implementation of get on the operation table.
; The table is hard-coded here so no 'put's are needed
(define (get operation type)

	(define (attach-tag type-tag contents)
		(if (eq? type-tag 'scheme-number)
			contents
			(cons type-tag contents)
		)
	)

	; (display "Searching op-table for ")
	; (display operation)
	; (display ", ")
	; (display type)
	; (newline)

	(define op-table
		(list
			(cons
				'make-from-real-imag
				(list
					(cons 'complex (lambda (x y) (attach-tag 'complex (make-from-real-imag x y))))
					(cons 'rectangular (lambda (x y) (attach-tag 'rectangular (make-from-real-imag-rectangular x y))))
					(cons 'polar (lambda (x y) (attach-tag 'polar (make-from-real-imag-polar x y))))
				)
			)
			(cons
				'make-from-mag-ang
				(list
					(cons 'complex (lambda (r a) (attach-tag 'complex (make-from-mag-ang r a))))
					(cons 'rectangular (lambda (r a) (attach-tag 'rectangular (make-from-mag-ang-rectangular r a))))
					(cons 'polar (lambda (r a) (attach-tag 'polar (make-from-mag-ang-polar r a))))
				)
			)
			(cons
				'make
				(list
					(cons 'scheme-number (lambda (x) (attach-tag 'scheme-number x)))
					(cons 'rational (lambda (n d) (attach-tag 'rational (make-rat n d))))
				)
			)
			(cons
				'add
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (+ x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (add-rat x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (add-complex z1 z2))))
				)
			)
			(cons
				'sub
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (- x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (sub-rat x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (sub-complex z1 z2))))
				)
			)
			(cons
				'mul
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (* x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (mul-rat x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (mul-complex z1 z2))))
				)
			)
			(cons
				'div
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (/ x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (div-rat x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (div-complex z1 z2))))
				)
			)
			(cons
				'equal?
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (= x y)))
					(cons '(rational rational) (lambda (x y) (equal-rat? x y)))
					(cons '(complex complex) (lambda (z1 z2) (equal-complex? z1 z2)))
				)
			)
			(cons
				'real-part
				(list
					(cons '(complex) real-part)
					(cons '(rectangular) real-part-rectangular)
					(cons '(polar) real-part-polar)
				)
			)
			(cons
				'imag-part
				(list
					(cons '(complex) imag-part)
					(cons '(rectangular) imag-part-rectangular)
					(cons '(polar) imag-part-polar)
				)
			)
			(cons
				'magnitude
				(list
					(cons '(complex) magnitude)
					(cons '(rectangular) magnitude-rectangular)
					(cons '(polar) magnitude-polar)
				)
			)
			(cons
				'angle
				(list
					(cons '(complex) angle)
					(cons '(rectangular) angle-rectangular)
					(cons '(polar) angle-polar)
				)
			)
		)
	)

	(define (find-operation-row op table)
		(cond
			((not (pair? table)) (error "op-table Not a pair!"))
			((null? table) (error "Operation not found for " op))
			(else
				(if (equal? op (car (car table)))
					(car table)
					(find-operation-row op (cdr table))
				)
			)
		)
	)

	(define (find-type-in-op-row type-list t)
		(cond
			((not (pair? type-list)) (error "type-list Not a pair!"))
			((null? type-list) (error "type not found: " t))
			(else
				(if (equal? t (car (car type-list)))
					(cdr (car type-list))
					(find-type-in-op-row (cdr type-list) t)
				)
			)
		)
	)

	(find-type-in-op-row (cdr (find-operation-row operation op-table)) type)
)

; Tests

(define n1 (make-scheme-number 7))
n1
(define n2 (make-scheme-number 11))
n2
(define n3 (make-scheme-number 7))
n3
(define r1 (make-rational 8 9))
r1
(define r2 (make-rational 2 3))
r2
(define r3 (make-rational -48 -54))
r3
(define c1 (make-complex-from-real-imag 3 4))
c1
(define c2 (make-complex-from-real-imag 5 6))
c2
(define c3 (make-complex-from-mag-ang 2 (/ pi 3)))
c3
(define c4 (make-complex-from-mag-ang 2 (/ pi 6)))
c4
(define c7 (make-complex-from-real-imag (/ 4 (sqrt 2)) (/ 4 (sqrt 2))))
c7
(define c8 (make-complex-from-real-imag (/ 4 (sqrt 2)) (/ 4 (sqrt 2))))
c8
(define c9 (make-complex-from-mag-ang 2 (/ pi 6)))
c9

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
7
11
7
'(rational 8 . 9)
'(rational 2 . 3)
'(rational 8 . 9)
'(complex rectangular 3 . 4)
'(complex rectangular 5 . 6)
'(complex polar 2 . 1.0471975511965976)
'(complex polar 2 . 0.5235987755982988)
'(complex rectangular 2.82842712474619 . 2.82842712474619)
'(complex rectangular 2.82842712474619 . 2.82842712474619)
'(complex polar 2 . 0.5235987755982988)
> (make-rational 13 15)
'(rational 13 . 15)
> (make-rational -13 15)
'(rational -13 . 15)
> (make-rational 13 -15)
'(rational -13 . 15)
> (make-rational -13 -15)
'(rational 13 . 15)
> r1
'(rational 8 . 9)
> r2
'(rational 2 . 3)
> r3
'(rational 8 . 9)
> (equ? r1 r2)
Entered equal-rat?
#f
> (equ? r2 r3)
Entered equal-rat?
#f
> (equ? r3 r1)
Entered equal-rat?
#t
> (equ? c1 c2)
Entered equal-complex?
#f
> (equ? c3 c4)
Entered equal-complex?
#f
> (equ? c7 c8)
Entered equal-complex?
#t
> (equ? c4 c9)
Entered equal-complex?
#t
> (equ? n1 n2)
#f
> (equ? n2 n3)
#f
> (equ? n1 n3)
#t
> (equ? r1 r2)
Entered equal-rat?
#f
> (equ? r2 r3)
Entered equal-rat?
#f
> (equ? r1 r3)
Entered equal-rat?
#t
> 
