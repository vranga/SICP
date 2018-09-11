#lang racket

; Exercise 2.82.  Show how to generalize apply-generic to handle coercion in the general case of
; multiple arguments. One strategy is to attempt to coerce all the arguments to the type of
; the first argument, then to the type of the second argument, and so on. Give an example of
; a situation where this strategy (and likewise the two-argument version given above) is not
; sufficiently general. (Hint: Consider the case where there are some suitable mixed-type
; operations present in the table that will not be tried.)

; S O L U T I O N

; Generic Mathematical operations on numbers that may be ordinary numbers, rational numbers
; or complex numbers

(define (mul-and-scale x y factor) (apply-generic 'mul-and-scale x y factor))

(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equal? x y))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))
(define (add-four-quantities w x y z) (apply-generic 'add-four-quantities w x y z))

; Constructions of specific types of numbers (ordinary, rational or complex) using generic operations
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))

(define (apply-generic op . args)
	
	(define (apply-generic-internal op position args)
		(let ((type-tags (map type-tag args)))
			(let ((proc (get op type-tags)))
				(if proc
					(apply proc (map contents args))
					; proc not found so we need to try coercion (provided the args are not all
					; of the same type)
					(if (dissimilar? type-tags)
						(if (<= position (length args))
							; Coerce all arguments to the type of the argument that is in 'position'
							; position in the list. If all the coercions obtained are valid,
							; then apply the operation on the coerced arguments.
							(let ((target-type (find-element type-tags position)))
								; type-tags will be something like:
								; ('complex 'rational 'scheme-number)
								; from this we want to generate a list of procedures like:
								; (complex->complex rational->complex 'scheme-number->complex)
								; If all the above procedures are valid, then we will apply them to
								; the respective arguments to do the coercion
								(let ((coercion-procs (build-coercion-proc-list type-tags target-type)))
									(if (allValid? coercion-procs)
										; coerce all the arguments
										(let ((coerced-args (coerce args coercion-procs)))
											(let ((coerced-type-tags (map type-tag coerced-args)))
												(let ((new-proc (get op coerced-type-tags)))
													(if new-proc
														; Found a valid procedure for the coerced args
														; So we are done
														(apply new-proc (map contents coerced-args))
														; Could not find a valid procedure for the coerced args so try the next coercion
														(apply-generic-internal op (+ position 1) args)
													)
												)
											)
										)
										; At least one coercion is not supported so try coercing
										; with the type of the next argument
										(apply-generic-internal op (+ position 1) args)
									)
								)
							)
							; tried all coercions so give up
							(error "Tried all coercions. Giving up" (list op type-tags))
						)
						(error "(All arguments are of the same type and) no procedure was found for these types" (list op type-tags))
					)
				)
			)
		)
	)

	(apply-generic-internal op 1 args)
)

(define (dissimilar? items)
	; evaluates to true if the list contains dissimilar items
	; false if all the items are the same
	(cond
		((not (pair? items)) false)
		((null? items) false)
		; reached the last item in the list
		((null? (cdr items)) false)
		((equal? (car items) (cadr items))
			; continue looking
			(dissimilar? (cdr items))
		)
		(else
			true
		)
	)
)

(define (find-element items index)
	; finds and returns the item in the 'index' position of the list
	(cond
		((and (> index 0) (<= index (length items)))
			(if (= index 1)
				(car items)
				(find-element (cdr items) (- index 1))
			)
		)
		(else
			null
		)
	)
)

(define (build-coercion-proc-list type-tags target-type)
	(cond
		((null? type-tags) (list))
		((not (pair? type-tags)) (error "type-tags not a pair: " type-tags))
		(else
			(cons
				(get-coercion (car type-tags) target-type)
				(build-coercion-proc-list (cdr type-tags) target-type)
			)
		)
	)
)

(define (allValid? procs)
	(cond
		((null? procs) true)
		((not (pair? procs)) (error "procs not a pair: " procs))
		((car procs) (allValid? (cdr procs)))
		(else
			false
		)
	)
)

(define (coerce args coercion-procs)
	(cond
		((or (null? args) (null? coercion-procs)) (list))
		((not (pair? args)) (error "args not a pair: " args))
		((not (pair? coercion-procs)) (error "coercion-procs not a pair: " coercion-procs))
		((not (= (length args) (length coercion-procs))) (error "The number of coercion procs needs to be equal to the number of arguments"))
		(else
			(cons ((car coercion-procs) (car args)) (coerce (cdr args) (cdr coercion-procs)))
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

(define (mul-and-scale-complex z1 z2 factor)
	(display "Entered proc mul-and-scale-complex")
	(newline)
	(let ((prod (mul-complex z1 z2)))
		(make-complex-from-real-imag (* (real-part prod) factor) (* (imag-part prod) factor))
	)
)

(define (add-four-complex-numbers z1 z2 z3 z4)
	(display "Entered proc add-four-complex-numbers")
	(newline)
	(make-from-real-imag
		(+ (real-part z1) (real-part z2) (real-part z3) (real-part z4))
		(+ (imag-part z1) (imag-part z2) (imag-part z3) (imag-part z4))
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
(define (make-from-real-imag-rectangular x y)
	; (display "Entered proc make-from-real-imag-rectangular")
	; (newline)
	; (display x)
	; (display " ")
	; (display y)
	; (newline)
	(cons x y)
)

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

; Coercion procedures

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(define (scheme-number->complex x)
	(make-complex-from-real-imag (contents x) 0)
)
(define (rational->complex x)
	(let ((rat (contents x)))
		(make-complex-from-real-imag (* 1.0 (/ (numer rat) (denom rat)))  0)
	)
)

(define (get-coercion type1 type2)
	(define coercion-table
		(list
			; Since I have re-written "apply-generic", there should be no harm in 
			; keeping coercion procedures that convert from one type to the same type. 
			; These should not cause infinite loops any more
			(cons
				'scheme-number
				(list
					(cons 'scheme-number scheme-number->scheme-number)
					(cons 'complex scheme-number->complex)
				)
			)
			(cons
				'complex
				(list
					(cons 'complex complex->complex)
				)
			)
			(cons
				'rational
				(list
					(cons 'complex rational->complex)
				)
			)
		)
	)

	(define (find-row type-name table)
		(cond
			((not (pair? table)) false)
			((null? table) (error "Type row not found for " type-name))
			(else
				(if (equal? type-name (car (car table)))
					(car table)
					(find-row type-name (cdr table))
				)
			)
		)
	)

	(define (find-type-in-row type-list type-name)
		(cond
			((not (pair? type-list)) false)
			((null? type-list) (error "type not found: " type-name))
			(else
				(if (equal? type-name (car (car type-list)))
					(cdr (car type-list))
					(find-type-in-row (cdr type-list) type-name)
				)
			)
		)
	)

	(if (find-row type1 coercion-table)
		(find-type-in-row (cdr (find-row type1 coercion-table)) type2)
		false
	)
)

; Implementation of get on the operation table.
; The table is hard-coded here so no 'put's are needed

(define (get operation type)

	; (display "Searching op-table for ")
	; (display operation)
	; (display ", ")
	; (display type)
	; (newline)

	(define (attach-tag type-tag contents)
		(if (eq? type-tag 'scheme-number)
			contents
			(cons type-tag contents)
		)
	)

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
				'add-four-quantities
				(list
					(cons '(complex complex complex complex) (lambda (z1 z2 z3 z4) (attach-tag 'complex (add-four-complex-numbers z1 z2 z3 z4))))
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
				'exp
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (expt x y))))
				)
			)
			(cons
				'mul-and-scale
				(list
					; (cons '(complex complex scheme-number) (lambda (x y factor) (attach-tag 'complex (mul-and-scale-complex x y factor))))
					(cons '(complex complex scheme-number) mul-and-scale-complex)
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
			((not (pair? type-list)) false)
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

(define z1 (make-complex-from-mag-ang 2 0.1))
(define z2 (make-complex-from-mag-ang 3 0.2))
(define z3 (make-complex-from-real-imag 2 3))
(define z4 (make-complex-from-real-imag 4 6))
(define z5 (make-complex-from-real-imag 1 5))
(define z6 (make-complex-from-real-imag 5 12))
(define r1 (make-rational 2 5))

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (mul z1 z2)
'(complex polar 6 . 0.30000000000000004)
> (add-four-quantities z3 z4 z5 z6)
Entered proc add-four-complex-numbers
'(complex rectangular 12 . 26)
> (add-four-quantities z3 z4 z5 r1)
Entered proc add-four-complex-numbers
'(complex rectangular 7.4 . 14)
> (add-four-quantities z3 z4 r1 z6)
Entered proc add-four-complex-numbers
'(complex rectangular 11.4 . 21)
> (add-four-quantities z3 r1 z5 z6)
Entered proc add-four-complex-numbers
'(complex rectangular 8.4 . 20)
> (add-four-quantities r1 z4 z5 z6)
Entered proc add-four-complex-numbers
'(complex rectangular 10.4 . 23)
> (add-four-quantities r1 r1 z5 z6)
Entered proc add-four-complex-numbers
'(complex rectangular 6.8 . 17)
> (add-four-quantities z3 r1 z5 r1)
Entered proc add-four-complex-numbers
'(complex rectangular 3.8 . 8)
> (add-four-quantities z3 z4 r1 r1)
Entered proc add-four-complex-numbers
'(complex rectangular 6.800000000000001 . 9)
> (add-four-quantities z3 r1 r1 r1)
Entered proc add-four-complex-numbers
'(complex rectangular 3.1999999999999997 . 3)
> (add-four-quantities r1 r1 r1 r1)
. . (All arguments are of the same type and) no procedure was found for these types (add-four-quantities (rational rational rational rational))
> (mul-and-scale z1 z2 10)
Entered proc mul-and-scale-complex
'(complex rectangular 57.32018934753636 . 17.731212399680377)
> (make-complex-from-mag-ang (magnitude (mul-and-scale z1 z2 10)) (angle (mul-and-scale z1 z2 10)))
Entered proc mul-and-scale-complex
Entered proc mul-and-scale-complex
'(complex polar 60.00000000000001 . 0.30000000000000004)

; Explanation of why this strategy is not sufficiently general:

; Let's assume that we need to support the following operation on three arguments. This
; function expects the first two arguments x and y to be of any types and the third one "factor"
; to be an ordinary number. It multiples c1 and c2 and scales the result to the value of factor

; Suppose the following implementations exist in the op-table for the operation "mul-and-scale":
; complex, complex, scheme-number
; rational, rational, scheme-number

; And suppose that there is no procedure for the following:
; complex, rational, scheme-number
; If apply-generic is called with the above permutation of arguments, it will first fail to
; find a procedure for it following which it will try to coerce it in the following ways:

; complex, complex, complex
; rational, rational, rational
; scheme-number, scheme-number, scheme-number

; None of these will work because there are no proecedures for these permutations

; But clearly, by coercing the 2nd argument only, this permutation can be coerced into the
; following permutation:

; complex, complex, scheme-number for which which a procedure exists

; Hence, the above is an example where this coercion strategy is not sufficiently general
