#lang racket

; Exercise 2.80.  Define a generic predicate =zero? that tests if its argument is zero,
; and install it in the generic arithmetic package. This operation should work for ordinary
; numbers, rational numbers, and complex numbers.

; Generic Mathematical operations on numbers that may be ordinary numbers, rational numbers or complex numbers

(define (=zero? x) (apply-generic '=zero? x))
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
			((= d 0) (error "Denominator in a rational number cannot be zero"))
			((= n 0) (cons n d))
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

(define (=zero-rat? x)
	(= 0 (numer x))
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

(define (=zero-complex? c)
	(= 0 (magnitude c))
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
				'=zero?
				(list
					(cons '(scheme-number) (lambda (x) (= 0 x)))
					(cons '(rational) =zero-rat?)
					(cons '(complex) =zero-complex?)
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

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (=zero? 10)
#f
> (=zero? 0)
#t
> (=zero? -10)
#f
> (make-rational 23 67)
'(rational 23 . 67)
> (=zero? (make-rational 23 67))
#f
> (=zero? (make-rational 0 67))
#t
> (make-rational 0 67)
'(rational 0 . 67)
> 
> (=zero? (make-complex-from-real-imag 7 3))
#f
> (=zero? (make-complex-from-real-imag 0 3))
#f
> (=zero? (make-complex-from-real-imag 7 0))
#f
> (=zero? (make-complex-from-real-imag 0 0))
#t
> 
> (=zero? (make-complex-from-mag-ang 10 3))
#f
> (=zero? (make-complex-from-mag-ang 0 3))
#t
