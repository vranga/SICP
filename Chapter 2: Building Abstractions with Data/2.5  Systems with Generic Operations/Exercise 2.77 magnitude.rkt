#lang racket

; Exercise 2.77.  Louis Reasoner tries to evaluate the expression (magnitude z) where z
; is the object shown in figure 2.24. To his surprise, instead of the answer 5 he gets an
; error message from apply-generic, saying there is no method for the operation magnitude
; on the types (complex). He shows this interaction to Alyssa P. Hacker, who says "The
; problem is that the complex-number selectors were never defined for complex numbers,
; just for polar and rectangular numbers. All you have to do to make this work is add
; the following to the complex package:"

; SOLUTION

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

Describe in detail why this works. As an example, trace through all the procedures called in evaluating the expression (magnitude z) where z is the object shown in figure 2.24. In particular, how many times is apply-generic invoked? What procedure is dispatched to in each case?

; SOLUTION

; Generic Mathematical operations on numbers that may be ordinary numbers, rational numbers or complex numbers

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
	(display "Entered apply-generic with ")
	(display op)
	(display ", ")
	(display args)
	(newline)
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
	(if (pair? datum)
		(car datum)
		(error "Bad tagged datum -- TYPE-TAG" datum)
	)
)

(define (contents datum)
	(if (pair? datum)
		(cdr datum)
		(error "Bad tagged datum -- CONTENTS" datum)
	)
)

(define (square x)
	(* x x)
)

; Rational Number procedures
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (make-rat n d) (let ((g (gcd n d))) (cons (/ n g) (/ d g))))
(define (add-rat x y) (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (sub-rat x y) (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (mul-rat x y) (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
(define (div-rat x y) (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

; Complex Number procedures
(define (magnitude z)
	(display "Entered proc magnitude")
	(newline)
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

; Rectangular (Complex) Number procedures
(define (make-from-real-imag-rectangular x y) (cons x y))
(define (make-from-mag-ang-rectangular r a) (cons (* r (cos a)) (* r (sin a))))
(define (magnitude-rectangular z)
	(display "Entered magnitude-rectangular")
	(newline)
	(sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z))))
)
(define (angle-rectangular z) (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (real-part-rectangular z)
	(display "Entered real-part-rectangular")
	(newline)
	(car z)
)
(define (imag-part-rectangular z)
	(display "Entered imag-part-rectangular")
	(newline)
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

	(define (attach-tag type-tag contents) (cons type-tag contents))

	(display "Searching op-table for ")
	(display operation)
	(display ", ")
	(display type)
	(newline)

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
> (make-scheme-number 7)
'(scheme-number . 7)
> (define r1 (make-rational 8 9))
> r1
'(rational 8 . 9)
> (define r2 (make-rational 2 3))
> r2
'(rational 2 . 3)
> (define c1 (make-complex-from-real-imag 3 4))
> c1
'(complex rectangular 3 . 4)
> (define c2 (make-complex-from-real-imag 5 6))
> c2
'(complex rectangular 5 . 6)
> (define c3 (make-complex-from-mag-ang 2 (/ pi 3)))
> c3
'(complex polar 2 . 1.0471975511965976)
> (define c4 (make-complex-from-mag-ang 2 (/ pi 6)))
> c4
'(complex polar 2 . 0.5235987755982988)
> (define c5 (make-from-real-imag 8 9))
> c5
'(rectangular 8 . 9)
> (define c6 (make-from-mag-ang 4 (/ pi 4)))
> c6
'(polar 4 . 0.7853981633974483)

> (add r1 r2)
Entered apply-generic with add, ((rational 8 . 9) (rational 2 . 3))
'(rational 14 . 9)
> (sub r1 r2)
Entered apply-generic with sub, ((rational 8 . 9) (rational 2 . 3))
'(rational 2 . 9)
> (mul r1 r2)
Entered apply-generic with mul, ((rational 8 . 9) (rational 2 . 3))
'(rational 16 . 27)
> (div r1 r2)
Entered apply-generic with div, ((rational 8 . 9) (rational 2 . 3))
'(rational 4 . 3)
> 

> (add c1 c2)
Entered apply-generic with add, ((complex rectangular 3 . 4) (complex rectangular 5 . 6))
Entered apply-generic with real-part, ((rectangular 3 . 4))
Entered apply-generic with real-part, ((rectangular 5 . 6))
Entered apply-generic with imag-part, ((rectangular 3 . 4))
Entered apply-generic with imag-part, ((rectangular 5 . 6))
'(complex rectangular 8 . 10)
> (sub c1 c2)
Entered apply-generic with sub, ((complex rectangular 3 . 4) (complex rectangular 5 . 6))
Entered apply-generic with real-part, ((rectangular 3 . 4))
Entered apply-generic with real-part, ((rectangular 5 . 6))
Entered apply-generic with imag-part, ((rectangular 3 . 4))
Entered apply-generic with imag-part, ((rectangular 5 . 6))
'(complex rectangular -2 . -2)
> (mul c1 c2)
Entered apply-generic with mul, ((complex rectangular 3 . 4) (complex rectangular 5 . 6))
Entered apply-generic with magnitude, ((rectangular 3 . 4))
Entered apply-generic with magnitude, ((rectangular 5 . 6))
Entered apply-generic with angle, ((rectangular 3 . 4))
Entered apply-generic with angle, ((rectangular 5 . 6))
'(complex polar 39.05124837953327 . 1.8033532685998055)
> (make-complex-from-real-imag (real-part (mul c1 c2)) (imag-part (mul c1 c2)))
Entered apply-generic with mul, ((complex rectangular 3 . 4) (complex rectangular 5 . 6))
Entered apply-generic with magnitude, ((rectangular 3 . 4))
Entered apply-generic with magnitude, ((rectangular 5 . 6))
Entered apply-generic with angle, ((rectangular 3 . 4))
Entered apply-generic with angle, ((rectangular 5 . 6))
Entered apply-generic with real-part, ((complex polar 39.05124837953327 . 1.8033532685998055))
Entered apply-generic with real-part, ((polar 39.05124837953327 . 1.8033532685998055))
Entered apply-generic with mul, ((complex rectangular 3 . 4) (complex rectangular 5 . 6))
Entered apply-generic with magnitude, ((rectangular 3 . 4))
Entered apply-generic with magnitude, ((rectangular 5 . 6))
Entered apply-generic with angle, ((rectangular 3 . 4))
Entered apply-generic with angle, ((rectangular 5 . 6))
Entered apply-generic with imag-part, ((complex polar 39.05124837953327 . 1.8033532685998055))
Entered apply-generic with imag-part, ((polar 39.05124837953327 . 1.8033532685998055))
'(complex rectangular -8.999999999999993 . 38.0)
> (div c1 c2)
Entered apply-generic with div, ((complex rectangular 3 . 4) (complex rectangular 5 . 6))
Entered apply-generic with magnitude, ((rectangular 3 . 4))
Entered apply-generic with magnitude, ((rectangular 5 . 6))
Entered apply-generic with angle, ((rectangular 3 . 4))
Entered apply-generic with angle, ((rectangular 5 . 6))
'(complex polar 0.6401843996644799 . 0.05123716740341877)
> (make-complex-from-real-imag (real-part (div c1 c2)) (imag-part (div c1 c2)))
Entered apply-generic with div, ((complex rectangular 3 . 4) (complex rectangular 5 . 6))
Entered apply-generic with magnitude, ((rectangular 3 . 4))
Entered apply-generic with magnitude, ((rectangular 5 . 6))
Entered apply-generic with angle, ((rectangular 3 . 4))
Entered apply-generic with angle, ((rectangular 5 . 6))
Entered apply-generic with real-part, ((complex polar 0.6401843996644799 . 0.05123716740341877))
Entered apply-generic with real-part, ((polar 0.6401843996644799 . 0.05123716740341877))
Entered apply-generic with div, ((complex rectangular 3 . 4) (complex rectangular 5 . 6))
Entered apply-generic with magnitude, ((rectangular 3 . 4))
Entered apply-generic with magnitude, ((rectangular 5 . 6))
Entered apply-generic with angle, ((rectangular 3 . 4))
Entered apply-generic with angle, ((rectangular 5 . 6))
Entered apply-generic with imag-part, ((complex polar 0.6401843996644799 . 0.05123716740341877))
Entered apply-generic with imag-part, ((polar 0.6401843996644799 . 0.05123716740341877))
'(complex rectangular 0.639344262295082 . 0.03278688524590161)
> 

> (add c3 c4)
Entered apply-generic with add, ((complex polar 2 . 1.0471975511965976) (complex polar 2 . 0.5235987755982988))
Entered apply-generic with real-part, ((polar 2 . 1.0471975511965976))
Entered apply-generic with real-part, ((polar 2 . 0.5235987755982988))
Entered apply-generic with imag-part, ((polar 2 . 1.0471975511965976))
Entered apply-generic with imag-part, ((polar 2 . 0.5235987755982988))
'(complex rectangular 2.7320508075688776 . 2.732050807568877)
> (sub c3 c4)
Entered apply-generic with sub, ((complex polar 2 . 1.0471975511965976) (complex polar 2 . 0.5235987755982988))
Entered apply-generic with real-part, ((polar 2 . 1.0471975511965976))
Entered apply-generic with real-part, ((polar 2 . 0.5235987755982988))
Entered apply-generic with imag-part, ((polar 2 . 1.0471975511965976))
Entered apply-generic with imag-part, ((polar 2 . 0.5235987755982988))
'(complex rectangular -0.7320508075688772 . 0.7320508075688773)
> (mul c3 c4)
Entered apply-generic with mul, ((complex polar 2 . 1.0471975511965976) (complex polar 2 . 0.5235987755982988))
Entered apply-generic with magnitude, ((polar 2 . 1.0471975511965976))
Entered apply-generic with magnitude, ((polar 2 . 0.5235987755982988))
Entered apply-generic with angle, ((polar 2 . 1.0471975511965976))
Entered apply-generic with angle, ((polar 2 . 0.5235987755982988))
'(complex polar 4 . 1.5707963267948966)
> (div c3 c4)
Entered apply-generic with div, ((complex polar 2 . 1.0471975511965976) (complex polar 2 . 0.5235987755982988))
Entered apply-generic with magnitude, ((polar 2 . 1.0471975511965976))
Entered apply-generic with magnitude, ((polar 2 . 0.5235987755982988))
Entered apply-generic with angle, ((polar 2 . 1.0471975511965976))
Entered apply-generic with angle, ((polar 2 . 0.5235987755982988))
'(complex polar 1 . 0.5235987755982988)
> 

; The following was run after adding more logging statements so that we can trace
; the procedure calls


> (define c (make-complex-from-real-imag 6 8))
Searching op-table for make-from-real-imag, complex
Searching op-table for make-from-real-imag, rectangular
> (magnitude c)
Entered proc magnitude
Entered apply-generic with magnitude, ((complex rectangular 6 . 8))
Searching op-table for magnitude, (complex)
Entered proc magnitude
Entered apply-generic with magnitude, ((rectangular 6 . 8))
Searching op-table for magnitude, (rectangular)
Entered magnitude-rectangular
Entered real-part-rectangular
Entered imag-part-rectangular
10
> 

; We can see that Alyssa's suggestion works. To understand why, note that the procedure
; "magnitude" is a generic procedure. It only calls 'apply-generic' with op = 'magnitude and
; passes on the data object to it. "apply-generic" retrieves the first tag from the data object
; which happens to be 'complex and looks up the op-table using 'magnitude and '(complex).
; But the op-table points back to the same "magnitude" procedure. So "magnitude" is called
; again - only this time with the same data object with the first tag stripped off. 
; "magnitude" calls 'apply-generic' with op = 'magnitude and passes the (now) rectangular
; data object to it. This time around, "apply-generic" retrieves the first tag from the data
; object which is 'rectangular and looks up the op-table using 'magnitude and '(rectangular).
; The op-table points to 'magnitude-rectangular' which is the procedure that implements
; "magnitude" inside the rectangular package.

; To summarize, the call sequence is:

; magnitude --> apply-generic --> get --> magnitude --> apply-generic --> get --> magnitude-rectangular

; "apply-generic" is invoked twice
; The first time it dispatches back to "magnitude"
; The second time it dispatches to "magnitude-rectangular"
