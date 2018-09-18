#lang racket

; Exercise 2.86.  Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes,
; and angles can be either ordinary numbers, rational numbers, or other numbers we might wish to add to
; the system. Describe and implement the changes to the system needed to accommodate this.
; You will have to define operations such as sine and cosine that are generic over ordinary numbers
; and rational numbers.

; S O L U T I O N

; 1. Here I am changing the generic procedures (real-part) and (imag-part) to (REAL-PART) and
; (IMAG-PART) in order to avoid name-conflicts with the Racket primitives (real-part) and (imag-part). I
; am using these primitives to understand more about Racket numbers (i.e. data that passes the number? test)

; I have tried to order the generics below in decreasing order of primitivity

(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))

(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equal? x y))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (exp x y) (apply-generic 'exp x y))

(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (tan-inverse x y) (apply-generic 'tan-inverse x y))

(define (add-four-quantities w x y z) (apply-generic 'add-four-quantities w x y z))
(define (mul-and-scale x y factor) (apply-generic 'mul-and-scale x y factor))
(define (mul-five-quantities v w x y z) (apply-generic 'mul-five-quantities v w x y z))

; Generic operation that 'projects' x one level lower in the tower
(define (project x) (apply-generic 'project x))

; Generic operation that raises x one level in the tower
(define (raise x) (apply-generic 'raise x))

; Constructions of specific types of numbers (ordinary, rational or complex) using generic operations
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-natural n) ((get 'make 'natural) n))
(define (make-integer n) ((get 'make 'integer) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-real r) ((get 'make 'real) r))
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))

(define (drop x)
	(with-handlers ([exn:fail? (lambda (exn)
									; (display "Could not drop beyond: ")
									; (display x)
									; (newline)
									x)])
		(if (not (pair? x))
			x
			(let ((projected-x (project x)))
				(cond
					((equ? x (raise projected-x)) (drop projected-x))
					(else
						; can't drop any further
						x
					)
				)
			)
		)
	)
)

; Generic procedure framework
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
								; ('complex 'rational 'real)
								; from this we want to generate a list of procedures like:
								; (complex->complex rational->complex 'real->complex)
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
							; tried all coercions so now try "raising", provided the op itself is not "raise"
							; Also, it does not make sense to raise up when we are trying to project down
							(if (and (not (equal? op 'raise)) (not (equal? op 'project)))
								(apply-raise op args)
								(error "Tried all coercions. No procedure was found for these types" (list op type-tags))
							)
						)
						; args are all of the same type so try raising, provided the op itself is not "raise"
						; Also, it does not make sense to raise up when we are trying to project down
						(if (and (not (equal? op 'raise)) (not (equal? op 'project)))
							(apply-raise op args)
							(error "(All arguments are of the same type and) no procedure was found for these types" (list op type-tags))
						)
					)
				)
			)
		)
	)

	; (display "Starting with args: ")
	; (display args)
	; (newline)
	(if (and (not (equal? op 'raise)) (not (equal? op 'project)))
		(drop (apply-generic-internal op 1 args))
		(apply-generic-internal op 1 args)
	)
)

(define (apply-raise op args)
	(let ((type-tags (map type-tag args)))
		(let ((new-args (raise-one-step args)))
			; (display "New args: ")
			; (display new-args)
			; (newline)
			(let ((new-type-tags (map type-tag new-args)))
				(if (not (equal? new-type-tags type-tags))
					; raise worked
					(let ((proc (get op new-type-tags)))
						(if proc
							; valid procedure found, so apply it
							(apply proc (map contents new-args))
							; valid procedure not found, so raise again
							(apply-raise op new-args)
						)
					)
					; we could not raise any more, so give up
					(error "Tried all raise options and failed to find a valid procedure for this operation" (list op args))
				)
			)
		)
	)
)

(define (raise-one-step args)
	; Logic of this procedure:
	; If the arguments are dissimilar, find the 'lowest' argument in the list and raise it one step
	; If the arguments are all similar (i.e. of the same type), then raise all of them one step
	; return the new arguments
	(let ((type-tags (map type-tag args)))
		(if (dissimilar? type-tags)
				(raise-lowest args)
				(raise-all args)
		)
	)
)

(define (raise-lowest args)

	(define (raise-lowest-internal arg-list position-of-lowest-type)
		(if (= position-of-lowest-type 1)
			(cons (try-raise (car arg-list)) (cdr arg-list))
			(cons
				(car arg-list)
				(raise-lowest-internal (cdr arg-list) (- position-of-lowest-type 1))
			)
		)
	)

	(let ((position-of-lowest-type (get-position-of-lowest-type args)))
		; (display "Raising the argument in position: ")
		; (display position-of-lowest-type)
		; (newline)
		(raise-lowest-internal args position-of-lowest-type)
	)
)

(define (get-position-of-lowest-type args)

	(define (get-position-of-lowest-type-internal
			arg-list
			current-candidate
			position-of-current-candidate
			current-position)
		(cond
			((null? arg-list) position-of-current-candidate)
			(else
				(if (lower-in-tower? (car arg-list) current-candidate)
					; we found a "lower" candidate
					(get-position-of-lowest-type-internal (cdr arg-list) (car arg-list) (+ current-position 1) (+ current-position 1))
					; we did not find a "lower" candidate, continue looking with the same current candidate
					(get-position-of-lowest-type-internal (cdr arg-list) current-candidate position-of-current-candidate (+ current-position 1))
				)
			)
		)
	)

	(cond
		((null? args) 0)
		((null? (cdr args)) 1)
		(else
			(get-position-of-lowest-type-internal (cdr args) (car args) 1 1)
		)
	)
)

(define (lower-in-tower? a b)
	; tests whether a is lower in the tower than b
	; returns true if a is lower, false if not
	; Logic: keep raising a till you find b or cannot raise any further
	(let ((new-a (try-raise a)))
		(if (same-type? new-a a)
			; raise failed
			false
			; raise worked and we got a new type
			(if (same-type? new-a b)
				; we successfully raised a to b
				true
				; a has not become b yet so keep looking
				(lower-in-tower? new-a b)
			)
		)
	)
)

(define (same-type? x y)
	(equal? (type-tag x) (type-tag y))
)

(define (raise-all arg-list)
	(cond
		((null? arg-list) (list))
		(else
			(cons
				(try-raise (car arg-list))
				(raise-all (cdr arg-list))
			)
		)
	)
)

(define (try-raise x)
	; Tries to raise the object. If successful, it returns the raised object
	; Otherwise it returns the same object
	(let ((proc (get 'raise (list (type-tag x)))))
		(if proc
			(proc (contents x))
			x
		)
	)
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
		; ((number? datum) 'scheme-number)
		((number? datum) (determine-number-type datum))
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

(define (determine-number-type x)
	(if (not (= (imag-part x) 0))
		; number has a non-zero imaginary part
		'complex
		; number is real
		'real
	)
)

; Natural number procedures
(define (make-natural-specific n)
	(if (and (>= n 0) (or (natural? n) (= n (exact-round n))))
		n
		(error "Cannot make natural with: " n)
	)
)

(define (raise-natural n)
	(make-integer n)
)

(define (square-root-natural n)
	(sqrt n)
)

(define (square-natural n)
	(* n n)
)

(define (cosine-natural n)
	(cos n)
)

(define (tan-inverse-natural n1 n2)
	(atan n1 n2)
)

(define (sine-natural n)
	(sin n)
)

; Integer procedures
(define (make-integer-specific n)
	(if (integer? n)
		n
		(error "Cannot make integer with: " n)
	)
)

(define (raise-int n)
	(make-rational n 1)
)

(define (project-int n)
	; tries to push this object down one step in the tower
	(make-natural n)
)

(define (square-root-integer i)
	(if (>= i 0)
		(sqrt i)
		(make-complex-from-real-imag 0 (sqrt (abs i)))
	)
)

(define (square-integer n)
	(* n n)
)

(define (cosine-integer i)
	(cos i)
)

(define (tan-inverse-integer i1 i2)
	(atan i1 i2)
)

(define (sine-integer i)
	(sin i)
)

; Rational Number procedures
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rational-specific n d)
	(define (construct-rational n d)
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

	(cond
		((and (integer? n) (integer? d))
			(construct-rational (exact-round n) (exact-round d))
		)
		; if both supplied arguments are not integers, try combining them and try to make a rational
		; Example: 4 / 0.5 gives us 8 which is rational
		((integer? (/ n d))
			(construct-rational (exact-round (/ n d)) 1)
		)
		(else
			(error "Rational number cannot be made with: " n d)
		)
	)
)

(define (add-rational x y) (make-rational-specific (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (sub-rational x y) (make-rational-specific (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (mul-rational x y) (make-rational-specific (* (numer x) (numer y)) (* (denom x) (denom y))))
(define (div-rational x y) (make-rational-specific (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equal-rational? x y)
	; (display "Entered equal-rational?")
	; (newline)
	(and (= (numer x) (numer y)) (= (denom x) (denom y)))
)

(define (=zero-rational? x)
	(= 0 (numer x))
)

(define (square-root-rational rat)
	(if (>= (numer rat) 0)
		(sqrt (/ (numer rat) (denom rat)))
		(make-complex-from-real-imag 0 (sqrt (abs (/ (numer rat) (denom rat)))))
	)
)

(define (square-rational rat)
	(cons (* (numer rat) (numer rat)) (* (denom rat) (denom rat)))
)

(define (cosine-rational rat)
	(cos (/ (numer rat) (denom rat)))
)

(define (tan-inverse-rational rat1 rat2)
	(atan (/ (numer rat1) (denom rat1)) (/ (numer rat2) (denom rat2)))
)

(define (sine-rational rat)
	(sin (/ (numer rat) (denom rat)))
)

(define (raise-rational r)
	(make-real (* 1.0 (/ (numer r) (denom r))))
)

(define (project-rational r)
	; tries to push this object down one step in the tower
	(make-integer (numer r))
)

(define (mul-five-rationals v w x y z)
	(mul-rational (mul-rational (mul-rational (mul-rational v w) x) y) z)
)

; Real Number procedures
(define (make-real-specific r)
	(if (real? r)
		r
		(error "Cannot make real with: " r)
	)
)

(define (=zero-real? x)
	(= 0 x)
)

(define (raise-real r)
	(make-complex-from-real-imag r 0)
)

(define (project-real r)
	(make-rational r 1)
)

(define (square-root-real r)
	(if (>= r 0)
		(sqrt r)
		(make-complex-from-real-imag 0 (sqrt (abs r)))
	)
)

(define (square-real r)
	(* r r)
)

(define (cosine-real r)
	(cos r)
)

(define (tan-inverse-real r1 r2)
	(atan r1 r2)
)

(define (sine-real r)
	(sin r)
)

; Complex Number procedures
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (REAL-PART z) (apply-generic 'REAL-PART z))
(define (IMAG-PART z) (apply-generic 'IMAG-PART z))

(define (add-complex z1 z2)
	(make-from-real-imag
		(add (REAL-PART z1) (REAL-PART z2))
		(add (IMAG-PART z1) (IMAG-PART z2))
	)
)

(define (sub-complex z1 z2)
	(make-from-real-imag
		(sub (REAL-PART z1) (REAL-PART z2))
		(sub (IMAG-PART z1) (IMAG-PART z2))
	)
)

(define (mul-complex z1 z2)
	(make-from-mag-ang
		(mul (magnitude z1) (magnitude z2))
		(add (angle z1) (angle z2))
	)
)

(define (div-complex z1 z2)
	(make-from-mag-ang
		(div (magnitude z1) (magnitude z2))
		(sub (angle z1) (angle z2))
	)
)

(define (mul-and-scale-complex z1 z2 factor)
	(display "Entered proc mul-and-scale-complex")
	(newline)
	(let ((prod (mul-complex z1 z2)))
		(make-complex-from-real-imag (mul (REAL-PART prod) factor) (mul (IMAG-PART prod) factor))
	)
)

(define (add-four-complex-numbers z1 z2 z3 z4)
	(display "Entered proc add-four-complex-numbers")
	(newline)
	(make-from-real-imag
		(add (REAL-PART z1) (REAL-PART z2) (REAL-PART z3) (REAL-PART z4))
		(add (IMAG-PART z1) (IMAG-PART z2) (IMAG-PART z3) (IMAG-PART z4))
	)
)

(define (equal-complex? c1 c2)
	(and (equ? (REAL-PART c1) (REAL-PART c2)) (equ? (IMAG-PART c1) (IMAG-PART c2)))
)

(define (=zero-complex? c) (equ? 0 (magnitude c)))

(define (project-complex c) (make-real (REAL-PART c)))

(define (square-complex c) 
	(mul-complex c c)
)

; Rectangular (Complex) Number procedures
(define (make-from-real-imag-rectangular x y) (cons x y))
(define (make-from-mag-ang-rectangular r a) (cons (mul r (cosine a)) (mul r (sine a))))

(define (magnitude-rectangular z)
	(square-root (add (square (real-part-rectangular z)) (square (imag-part-rectangular z))))
)

(define (angle-rectangular z) (tan-inverse (imag-part-rectangular z) (real-part-rectangular z)))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

; Polar (Complex) Number procedures
(define (make-from-real-imag-polar x y) (cons (square-root (add (square x) (square y))) (tan-inverse y x)))
(define (make-from-mag-ang-polar r a) (cons r a))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (real-part-polar z) (mul (magnitude-polar z) (cosine (angle-polar z))))
(define (imag-part-polar z) (mul (magnitude-polar z) (sine (angle-polar z))))

; Coercion procedures

; (define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
; (define (scheme-number->complex x)
; 	(make-complex-from-real-imag (contents x) 0)
; )
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
			; (cons
			; 	'scheme-number
			; 	(list
			; 		(cons 'scheme-number scheme-number->scheme-number)
			; 		(cons 'complex scheme-number->complex)
			; 	)
			; )
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
					(cons 'natural (lambda (n) (attach-tag 'natural (make-natural-specific n))))
					(cons 'integer (lambda (n) (attach-tag 'integer (make-integer-specific n))))
					(cons 'rational (lambda (n d) (attach-tag 'rational (make-rational-specific n d))))
					(cons 'real (lambda (r) (attach-tag 'real (make-real-specific r))))
				)
			)
			(cons
				'raise
				(list
					(cons '(natural) raise-natural)
					(cons '(integer) raise-int)
					(cons '(rational) raise-rational)
					(cons '(real) raise-real)
				)
			)
			(cons
				'project
				(list
					(cons '(complex) project-complex)
					(cons '(real) project-real)
					(cons '(rational) project-rational)
					(cons '(integer) project-int)
				)
			)
			(cons
				'add
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (+ x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (add-rational x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (+ x y))))
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
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (sub-rational x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (- x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (sub-complex z1 z2))))
				)
			)
			(cons
				'mul
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (* x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (mul-rational x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (* x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (mul-complex z1 z2))))
				)
			)
			(cons
				'div
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (/ x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (div-rational x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (/ x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (div-complex z1 z2))))
				)
			)
			(cons
				'exp
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (expt x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (expt x y))))
				)
			)
			(cons
				'square-root
				(list
					(cons '(real) square-root-real)
					(cons '(rational) square-root-rational)
					(cons '(integer) square-root-integer)
					(cons '(natural) square-root-natural)
				)
			)
			(cons
				'square
				(list
					(cons '(complex) (lambda (z) (attach-tag 'complex (square-complex z))))
					(cons '(real) (lambda (r) (attach-tag 'real (square-real r))))
					(cons '(rational) (lambda (r) (attach-tag 'rational (square-rational r))))
					(cons '(integer) (lambda (i) (attach-tag 'integer (square-integer i))))
					(cons '(natural) (lambda (n) (attach-tag 'natural (square-natural n))))
				)
			)
			(cons
				'sine
				(list
					(cons '(real) (lambda (x) (attach-tag 'real (sine-real x))))
					(cons '(rational) (lambda (x) (attach-tag 'real (sine-rational x))))
					(cons '(integer) (lambda (x) (attach-tag 'real (sine-integer x))))
					(cons '(natural) (lambda (x) (attach-tag 'real (sine-natural x))))
					(cons '(scheme-number) (lambda (x) (attach-tag 'real (sin x))))
				)
			)
			(cons
				'cosine
				(list
					(cons '(real) (lambda (x) (attach-tag 'real (cosine-real x))))
					(cons '(rational) (lambda (x) (attach-tag 'real (cosine-rational x))))
					(cons '(integer) (lambda (x) (attach-tag 'real (cosine-integer x))))
					(cons '(natural) (lambda (x) (attach-tag 'real (cosine-natural x))))
					(cons '(scheme-number) (lambda (x) (attach-tag 'real (cos x))))
				)
			)
			(cons
				'tan-inverse
				(list
					(cons '(real real) tan-inverse-real)
					(cons '(rational rational) tan-inverse-rational)
					(cons '(integer integer) tan-inverse-integer)
					(cons '(natural natural) tan-inverse-natural)
				)
			)
			(cons
				'mul-and-scale
				(list
					(cons '(complex complex scheme-number) mul-and-scale-complex)
				)
			)
			(cons
				'mul-five-quantities
				(list
					(cons '(rational rational rational rational rational) (lambda (r1 r2 r3 r4 r5) (attach-tag 'rational (mul-five-rationals r1 r2 r3 r4 r5))))
				)
			)
			(cons
				'equal?
				(list
					; (cons '(scheme-number scheme-number) (lambda (x y) (= x y)))
					; (cons '(natural natural) (lambda (x y) (= x y)))
					; (cons '(integer integer) (lambda (x y) (= x y)))
					; (cons '(rational rational) (lambda (x y) (equal-rational? x y)))
					; (cons '(real real) (lambda (x y) (= x y)))
					; (cons '(complex complex) (lambda (z1 z2) (equal-complex? z1 z2)))

					(cons '(scheme-number scheme-number) =)
					(cons '(natural natural) =)
					(cons '(integer integer) =)
					(cons '(rational rational) equal-rational?)
					(cons '(real real) =)
					(cons '(complex complex) equal-complex?)
				)
			)
			(cons
				'=zero?
				(list
					(cons '(scheme-number) (lambda (x) (= 0 x)))
					(cons '(rational) =zero-rational?)
					(cons '(real) =zero-real?)
					(cons '(complex) =zero-complex?)
				)
			)
			(cons
				'REAL-PART
				(list
					(cons '(complex) REAL-PART)
					(cons '(rectangular) real-part-rectangular)
					(cons '(polar) real-part-polar)
				)
			)
			(cons
				'IMAG-PART
				(list
					(cons '(complex) IMAG-PART)
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

; Test Driver

(define (run-test proc . args)
	(display "Running ") (display (cons proc args)) (display " ")
	(newline)
	(display (apply proc args))
	(newline)
)

; Tests

(define sn1 (make-scheme-number 25))
(display "Created ") (display sn1) (newline)
(define sn2 (make-scheme-number 26))
(display "Created ") (display sn2) (newline)
(define n1 (make-natural 30))
(display "Created ") (display n1) (newline)
(define n2 (make-natural 31))
(display "Created ") (display n2) (newline)
(define i1 (make-integer 35))
(display "Created ") (display i1) (newline)
(define i2 (make-integer 36))
(display "Created ") (display i2) (newline)
(define rat1 (make-rational 40 43))
(display "Created ") (display rat1) (newline)
(define rat2 (make-rational 44 47))
(display "Created ") (display rat2) (newline)
(define r1 (make-real 45.3))
(display "Created ") (display r1) (newline)
(define r2 (make-real (sqrt 45.3)))
(display "Created ") (display r2) (newline)

(define zri1 (make-complex-from-real-imag 50 55))
(display "Created ") (display zri1) (newline)
(define zri2 (make-complex-from-real-imag sn1 sn2))
(display "Created ") (display zri2) (newline)
(define zri3 (make-complex-from-real-imag n1 n2))
(display "Created ") (display zri3) (newline)
(define zri4 (make-complex-from-real-imag i1 i2))
(display "Created ") (display zri4) (newline)
(define zri5 (make-complex-from-real-imag rat1 rat2))
(display "Created ") (display zri5) (newline)
(define zri6 (make-complex-from-real-imag r1 r2))
(display "Created ") (display zri6) (newline)
(define zri7 (make-complex-from-real-imag rat1 r1))
(display "Created ") (display zri7) (newline)

(define zma1 (make-complex-from-mag-ang 50 55))
(display "Created ") (display zma1) (newline)
(define zma2 (make-complex-from-mag-ang sn1 sn2))
(display "Created ") (display zma2) (newline)
(define zma3 (make-complex-from-mag-ang n1 n2))
(display "Created ") (display zma3) (newline)
(define zma4 (make-complex-from-mag-ang i1 i2))
(display "Created ") (display zma4) (newline)
(define zma5 (make-complex-from-mag-ang rat1 rat2))
(display "Created ") (display zma5) (newline)
(define zma6 (make-complex-from-mag-ang r1 r2))
(display "Created ") (display zma6) (newline)
(define zma7 (make-complex-from-mag-ang r2 rat2))
(display "Created ") (display zma7) (newline)

(newline)

(run-test square 0.0)
(run-test square sn1)
(run-test square sn2)
(run-test square n1)
(run-test square n2)
(run-test square i1)
(run-test square i2)
(run-test square rat1)
(run-test square rat2)
(run-test square r1)
(run-test square r2)
(run-test square zri1)
(run-test square zri2)
(run-test square zri3)
(run-test square zri4)
(run-test square zri5)
(run-test square zri6)
(run-test square zri7)
(run-test square zma1)
(run-test square zma2)
(run-test square zma3)
(run-test square zma4)
(run-test square zma5)
(run-test square zma6)
(run-test square zma7)

(newline)

(run-test square-root 0.0)
(run-test square-root sn1)
(run-test square-root sn2)
(run-test square-root n1)
(run-test square-root n2)
(run-test square-root i1)
(run-test square-root i2)
(run-test square-root rat1)
(run-test square-root rat2)
(run-test square-root r1)
(run-test square-root r2)

(newline)

(run-test =zero? 0.0)
(run-test =zero? sn1)
(run-test =zero? sn2)
(run-test =zero? n1)
(run-test =zero? n2)
(run-test =zero? i1)
(run-test =zero? i2)
(run-test =zero? rat1)
(run-test =zero? rat2)
(run-test =zero? r1)
(run-test =zero? r2)
(run-test =zero? zri1)
(run-test =zero? zri2)
(run-test =zero? zri3)
(run-test =zero? zri4)
(run-test =zero? zri5)
(run-test =zero? zri6)
(run-test =zero? zri7)
(run-test =zero? zma1)
(run-test =zero? zma2)
(run-test =zero? zma3)
(run-test =zero? zma4)
(run-test =zero? zma5)
(run-test =zero? zma6)
(run-test =zero? zma7)

(newline)

(run-test equ? 0.0 9.0)
(run-test equ? 9 9.0)
(run-test equ? sn1 sn1)
(run-test equ? sn1 sn2)
(run-test equ? n1 n2)
(run-test equ? n1 i2)
(run-test equ? i1 i2)
(run-test equ? i1 n2)
(run-test equ? i1 i1)
(run-test equ? n2 n2)
(run-test equ? rat1 rat2)
(run-test equ? rat2 rat2)
(run-test equ? zri1 zri2)
(run-test equ? zma1 zma2)
(run-test equ? zri2 zri2)
(run-test equ? zma1 zma1)
(run-test equ? zma7 zri7)

(newline)

(run-test REAL-PART zri1)
(run-test IMAG-PART zri1)
(run-test REAL-PART zri2)
(run-test IMAG-PART zri2)
(run-test REAL-PART zri3)
(run-test IMAG-PART zri3)
(run-test REAL-PART zri4)
(run-test IMAG-PART zri4)
(run-test REAL-PART zri5)
(run-test IMAG-PART zri5)
(run-test REAL-PART zri6)
(run-test IMAG-PART zri6)
(run-test REAL-PART zri7)
(run-test IMAG-PART zri7)

(newline)

(run-test magnitude zma1)
(run-test angle zma1)
(run-test magnitude zma2)
(run-test angle zma2)
(run-test magnitude zma3)
(run-test angle zma3)
(run-test magnitude zma4)
(run-test angle zma4)
(run-test magnitude zma5)
(run-test angle zma5)
(run-test magnitude zma6)
(run-test angle zma6)
(run-test magnitude zma7)
(run-test angle zma7)
 
(newline)
 
(run-test REAL-PART zma1)
(run-test IMAG-PART zma1)
(run-test REAL-PART zma2)
(run-test IMAG-PART zma2)
(run-test REAL-PART zma3)
(run-test IMAG-PART zma3)
(run-test REAL-PART zma4)
(run-test IMAG-PART zma4)
(run-test REAL-PART zma5)
(run-test IMAG-PART zma5)
(run-test REAL-PART zma6)
(run-test IMAG-PART zma6)
(run-test REAL-PART zma7)
(run-test IMAG-PART zma7)

(newline)

(run-test magnitude zri1)
(run-test angle zri1)
(run-test magnitude zri2)
(run-test angle zri2)
(run-test magnitude zri3)
(run-test angle zri3)
(run-test magnitude zri4)
(run-test angle zri4)
(run-test magnitude zri5)
(run-test angle zri5)
(run-test magnitude zri6)
(run-test angle zri6)
(run-test magnitude zri7)
(run-test angle zri7)
 
(newline)

(run-test add 0.0 9.0)
(run-test add 9 9.0)
(run-test add sn1 sn1)
(run-test add sn1 sn2)
(run-test add n1 n2)
(run-test add n1 i2)
(run-test add i1 i2)
(run-test add i1 n2)
(run-test add i1 i1)
(run-test add n2 n2)
(run-test add rat1 n2)
(run-test add rat1 rat2)
(run-test add rat2 rat2)
(run-test add zri1 zri2)
(run-test add zma1 zma2)
(run-test add zri2 zri2)
(run-test add zma1 zma1)
(run-test add zma1 9.0)
(run-test add zma1 sn1)
(run-test add n1 zma1)
(run-test add i1 zma1)
(run-test add rat1 zma1)
(run-test add zri1 zma1)
(run-test add zri7 zma7)
(run-test add rat1 zri4)

(newline)

(run-test sub 0.0 9.0)
(run-test sub 9 9.0)
(run-test sub sn1 sn1)
(run-test sub sn1 sn2)
(run-test sub n1 n2)
(run-test sub n1 i2)
(run-test sub i1 i2)
(run-test sub i1 n2)
(run-test sub i1 i1)
(run-test sub n2 n2)
(run-test sub rat1 n2)
(run-test sub rat1 rat2)
(run-test sub rat2 rat2)
(run-test sub zri1 zri2)
(run-test sub zma1 zma2)
(run-test sub zri2 zri2)
(run-test sub zma1 zma1)
(run-test sub zma1 9.0)
(run-test sub zma1 sn1)
(run-test sub n1 zma1)
(run-test sub i1 zma1)
(run-test sub rat1 zma1)
(run-test sub zri1 zma1)
(run-test sub zri7 zma7)
(run-test sub rat1 zri4)

(newline)

(run-test mul 0.0 9.0)
(run-test mul 9 9.0)
(run-test mul sn1 sn1)
(run-test mul sn1 sn2)
(run-test mul n1 n2)
(run-test mul n1 i2)
(run-test mul i1 i2)
(run-test mul i1 n2)
(run-test mul i1 i1)
(run-test mul n2 n2)
(run-test mul rat1 n2)
(run-test mul rat1 rat2)
(run-test mul rat2 rat2)
(run-test mul zri1 zri2)
(run-test mul zma1 zma2)
(run-test mul zri2 zri2)
(run-test mul zma1 zma1)
(run-test mul zma1 9.0)
(run-test mul zma1 sn1)
(run-test mul n1 zma1)
(run-test mul i1 zma1)
(run-test mul rat1 zma1)
(run-test mul zri1 zma1)
(run-test mul zri7 zma7)
(run-test mul rat1 zri4)

(newline)

(run-test div 0.0 9.0)
(run-test div 9 9.0)
(run-test div sn1 sn1)
(run-test div sn1 sn2)
(run-test div n1 n2)
(run-test div n1 i2)
(run-test div i1 i2)
(run-test div i1 n2)
(run-test div i1 i1)
(run-test div n2 n2)
(run-test div rat1 n2)
(run-test div rat1 rat2)
(run-test div rat2 rat2)
(run-test div zri1 zri2)
(run-test div zma1 zma2)
(run-test div zri2 zri2)
(run-test div zma1 zma1)
(run-test div zma1 9.0)
(run-test div zma1 sn1)
(run-test div n1 zma1)
(run-test div i1 zma1)
(run-test div rat1 zma1)
(run-test div zri1 zma1)
(run-test div zri7 zma7)
(run-test div rat1 zri4)

(newline)

; Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Created 25
Created 26
Created (natural . 30)
Created (natural . 31)
Created (integer . 35)
Created (integer . 36)
Created (rational 40 . 43)
Created (rational 44 . 47)
Created (real . 45.3)
Created (real . 6.730527468185536)
Created (complex rectangular 50 . 55)
Created (complex rectangular 25 . 26)
Created (complex rectangular (natural . 30) natural . 31)
Created (complex rectangular (integer . 35) integer . 36)
Created (complex rectangular (rational 40 . 43) rational 44 . 47)
Created (complex rectangular (real . 45.3) real . 6.730527468185536)
Created (complex rectangular (rational 40 . 43) real . 45.3)
Created (complex polar 50 . 55)
Created (complex polar 25 . 26)
Created (complex polar (natural . 30) natural . 31)
Created (complex polar (integer . 35) integer . 36)
Created (complex polar (rational 40 . 43) rational 44 . 47)
Created (complex polar (real . 45.3) real . 6.730527468185536)
Created (complex polar (real . 6.730527468185536) rational 44 . 47)

Running (#<procedure:square> 0.0) 
(natural . 0)
Running (#<procedure:square> 25) 
(natural . 625)
Running (#<procedure:square> 26) 
(natural . 676)
Running (#<procedure:square> (natural . 30)) 
(natural . 900)
Running (#<procedure:square> (natural . 31)) 
(natural . 961)
Running (#<procedure:square> (integer . 35)) 
(natural . 1225)
Running (#<procedure:square> (integer . 36)) 
(natural . 1296)
Running (#<procedure:square> (rational 40 . 43)) 
(rational 1600 . 1849)
Running (#<procedure:square> (rational 44 . 47)) 
(rational 1936 . 2209)
Running (#<procedure:square> (real . 45.3)) 
(real . 2052.0899999999997)
Running (#<procedure:square> (real . 6.730527468185536)) 
(real . 45.300000000000004)
Running (#<procedure:square> (complex rectangular 50 . 55)) 
(complex polar (natural . 5525) real . 1.6659625333488635)
Running (#<procedure:square> (complex rectangular 25 . 26)) 
(complex polar (real . 1300.9999999999998) real . 1.610006988509306)
Running (#<procedure:square> (complex rectangular (natural . 30) natural . 31)) 
(complex polar (real . 1861.0000000000002) real . 1.6035802754109072)
Running (#<procedure:square> (complex rectangular (integer . 35) integer . 36)) 
(complex polar (real . 2520.9999999999995) real . 1.598963478440617)
Running (#<procedure:square> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
(complex polar (real . 1.7417472794930815) real . 1.5771589776520794)
Running (#<procedure:square> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
(complex polar (real . 2097.3899999999994) real . 0.2949954825171345)
Running (#<procedure:square> (complex rectangular (rational 40 . 43) real . 45.3)) 
(complex polar (real . 2052.9553326122227) real . 3.100528554842763)
Running (#<procedure:square> (complex polar 50 . 55)) 
(complex polar (natural . 2500) natural . 110)
Running (#<procedure:square> (complex polar 25 . 26)) 
(complex polar (natural . 625) natural . 52)
Running (#<procedure:square> (complex polar (natural . 30) natural . 31)) 
(complex polar (natural . 900) natural . 62)
Running (#<procedure:square> (complex polar (integer . 35) integer . 36)) 
(complex polar (natural . 1225) natural . 72)
Running (#<procedure:square> (complex polar (rational 40 . 43) rational 44 . 47)) 
(complex polar (rational 1600 . 1849) rational 88 . 47)
Running (#<procedure:square> (complex polar (real . 45.3) real . 6.730527468185536)) 
(complex polar (real . 2052.0899999999997) real . 13.461054936371072)
Running (#<procedure:square> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
(complex polar (real . 45.300000000000004) rational 88 . 47)

Running (#<procedure:square-root> 0.0) 
0.0
Running (#<procedure:square-root> 25) 
5
Running (#<procedure:square-root> 26) 
5.0990195135927845
Running (#<procedure:square-root> (natural . 30)) 
5.477225575051661
Running (#<procedure:square-root> (natural . 31)) 
5.5677643628300215
Running (#<procedure:square-root> (integer . 35)) 
5.916079783099616
Running (#<procedure:square-root> (integer . 36)) 
6
Running (#<procedure:square-root> (rational 40 . 43)) 
0.9644856443408242
Running (#<procedure:square-root> (rational 44 . 47)) 
0.9675588936937934
Running (#<procedure:square-root> (real . 45.3)) 
6.730527468185536
Running (#<procedure:square-root> (real . 6.730527468185536)) 
2.594326014244458

Running (#<procedure:=zero?> 0.0) 
#t
Running (#<procedure:=zero?> 25) 
#f
Running (#<procedure:=zero?> 26) 
#f
Running (#<procedure:=zero?> (natural . 30)) 
#f
Running (#<procedure:=zero?> (natural . 31)) 
#f
Running (#<procedure:=zero?> (integer . 35)) 
#f
Running (#<procedure:=zero?> (integer . 36)) 
#f
Running (#<procedure:=zero?> (rational 40 . 43)) 
#f
Running (#<procedure:=zero?> (rational 44 . 47)) 
#f
Running (#<procedure:=zero?> (real . 45.3)) 
#f
Running (#<procedure:=zero?> (real . 6.730527468185536)) 
#f
Running (#<procedure:=zero?> (complex rectangular 50 . 55)) 
#f
Running (#<procedure:=zero?> (complex rectangular 25 . 26)) 
#f
Running (#<procedure:=zero?> (complex rectangular (natural . 30) natural . 31)) 
#f
Running (#<procedure:=zero?> (complex rectangular (integer . 35) integer . 36)) 
#f
Running (#<procedure:=zero?> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
#f
Running (#<procedure:=zero?> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
#f
Running (#<procedure:=zero?> (complex rectangular (rational 40 . 43) real . 45.3)) 
#f
Running (#<procedure:=zero?> (complex polar 50 . 55)) 
#f
Running (#<procedure:=zero?> (complex polar 25 . 26)) 
#f
Running (#<procedure:=zero?> (complex polar (natural . 30) natural . 31)) 
#f
Running (#<procedure:=zero?> (complex polar (integer . 35) integer . 36)) 
#f
Running (#<procedure:=zero?> (complex polar (rational 40 . 43) rational 44 . 47)) 
#f
Running (#<procedure:=zero?> (complex polar (real . 45.3) real . 6.730527468185536)) 
#f
Running (#<procedure:=zero?> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
#f

Running (#<procedure:equ?> 0.0 9.0) 
#f
Running (#<procedure:equ?> 9 9.0) 
#t
Running (#<procedure:equ?> 25 25) 
#t
Running (#<procedure:equ?> 25 26) 
#f
Running (#<procedure:equ?> (natural . 30) (natural . 31)) 
#f
Running (#<procedure:equ?> (natural . 30) (integer . 36)) 
#f
Running (#<procedure:equ?> (integer . 35) (integer . 36)) 
#f
Running (#<procedure:equ?> (integer . 35) (natural . 31)) 
#f
Running (#<procedure:equ?> (integer . 35) (integer . 35)) 
#t
Running (#<procedure:equ?> (natural . 31) (natural . 31)) 
#t
Running (#<procedure:equ?> (rational 40 . 43) (rational 44 . 47)) 
#f
Running (#<procedure:equ?> (rational 44 . 47) (rational 44 . 47)) 
#t
Running (#<procedure:equ?> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
#f
Running (#<procedure:equ?> (complex polar 50 . 55) (complex polar 25 . 26)) 
#f
Running (#<procedure:equ?> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
#t
Running (#<procedure:equ?> (complex polar 50 . 55) (complex polar 50 . 55)) 
#t
Running (#<procedure:equ?> (complex polar (real . 6.730527468185536) rational 44 . 47) (complex rectangular (rational 40 . 43) real . 45.3)) 
#f

Running (#<procedure:REAL-PART> (complex rectangular 50 . 55)) 
50
Running (#<procedure:IMAG-PART> (complex rectangular 50 . 55)) 
55
Running (#<procedure:REAL-PART> (complex rectangular 25 . 26)) 
25
Running (#<procedure:IMAG-PART> (complex rectangular 25 . 26)) 
26
Running (#<procedure:REAL-PART> (complex rectangular (natural . 30) natural . 31)) 
(natural . 30)
Running (#<procedure:IMAG-PART> (complex rectangular (natural . 30) natural . 31)) 
(natural . 31)
Running (#<procedure:REAL-PART> (complex rectangular (integer . 35) integer . 36)) 
(natural . 35)
Running (#<procedure:IMAG-PART> (complex rectangular (integer . 35) integer . 36)) 
(natural . 36)
Running (#<procedure:REAL-PART> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
(rational 40 . 43)
Running (#<procedure:IMAG-PART> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
(rational 44 . 47)
Running (#<procedure:REAL-PART> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
(real . 45.3)
Running (#<procedure:IMAG-PART> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
(real . 6.730527468185536)
Running (#<procedure:REAL-PART> (complex rectangular (rational 40 . 43) real . 45.3)) 
(rational 40 . 43)
Running (#<procedure:IMAG-PART> (complex rectangular (rational 40 . 43) real . 45.3)) 
(real . 45.3)

Running (#<procedure:magnitude> (complex polar 50 . 55)) 
50
Running (#<procedure:angle> (complex polar 50 . 55)) 
55
Running (#<procedure:magnitude> (complex polar 25 . 26)) 
25
Running (#<procedure:angle> (complex polar 25 . 26)) 
26
Running (#<procedure:magnitude> (complex polar (natural . 30) natural . 31)) 
(natural . 30)
Running (#<procedure:angle> (complex polar (natural . 30) natural . 31)) 
(natural . 31)
Running (#<procedure:magnitude> (complex polar (integer . 35) integer . 36)) 
(natural . 35)
Running (#<procedure:angle> (complex polar (integer . 35) integer . 36)) 
(natural . 36)
Running (#<procedure:magnitude> (complex polar (rational 40 . 43) rational 44 . 47)) 
(rational 40 . 43)
Running (#<procedure:angle> (complex polar (rational 40 . 43) rational 44 . 47)) 
(rational 44 . 47)
Running (#<procedure:magnitude> (complex polar (real . 45.3) real . 6.730527468185536)) 
(real . 45.3)
Running (#<procedure:angle> (complex polar (real . 45.3) real . 6.730527468185536)) 
(real . 6.730527468185536)
Running (#<procedure:magnitude> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
(real . 6.730527468185536)
Running (#<procedure:angle> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
(rational 44 . 47)

Running (#<procedure:REAL-PART> (complex polar 50 . 55)) 
(real . 1.1063378130977866)
Running (#<procedure:IMAG-PART> (complex polar 50 . 55)) 
(real . -49.98775866793099)
Running (#<procedure:REAL-PART> (complex polar 25 . 26)) 
(real . 16.17298305821601)
Running (#<procedure:IMAG-PART> (complex polar 25 . 26)) 
(real . 19.06396126199007)
Running (#<procedure:REAL-PART> (complex polar (natural . 30) natural . 31)) 
(real . 27.44227073413594)
Running (#<procedure:IMAG-PART> (complex polar (natural . 30) natural . 31)) 
(real . -12.12112935969195)
Running (#<procedure:REAL-PART> (complex polar (integer . 35) integer . 36)) 
(real . -4.4787291369591635)
Running (#<procedure:IMAG-PART> (complex polar (integer . 35) integer . 36)) 
(real . -34.71225987050905)
Running (#<procedure:REAL-PART> (complex polar (rational 40 . 43) rational 44 . 47)) 
(real . 0.5515129933855453)
Running (#<procedure:IMAG-PART> (complex polar (rational 40 . 43) rational 44 . 47)) 
(real . 0.7491101590218483)
Running (#<procedure:REAL-PART> (complex polar (real . 45.3) real . 6.730527468185536)) 
(real . 40.84247949822263)
Running (#<procedure:IMAG-PART> (complex polar (real . 45.3) real . 6.730527468185536)) 
(real . 19.595455300586003)
Running (#<procedure:REAL-PART> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
(real . 3.990371352370838)
Running (#<procedure:IMAG-PART> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
(real . 5.420049489642889)

Running (#<procedure:magnitude> (complex rectangular 50 . 55)) 
74.33034373659252
Running (#<procedure:angle> (complex rectangular 50 . 55)) 
0.8329812666744317
Running (#<procedure:magnitude> (complex rectangular 25 . 26)) 
36.069377593742864
Running (#<procedure:angle> (complex rectangular 25 . 26)) 
0.805003494254653
Running (#<procedure:magnitude> (complex rectangular (natural . 30) natural . 31)) 
43.139309220245984
Running (#<procedure:angle> (complex rectangular (natural . 30) natural . 31)) 
0.8017901377054536
Running (#<procedure:magnitude> (complex rectangular (integer . 35) integer . 36)) 
50.20956084253277
Running (#<procedure:angle> (complex rectangular (integer . 35) integer . 36)) 
0.7994817392203085
Running (#<procedure:magnitude> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
1.3197527342245143
Running (#<procedure:angle> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
0.7885794888260397
Running (#<procedure:magnitude> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
45.797270661033934
Running (#<procedure:angle> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
0.14749774125856724
Running (#<procedure:magnitude> (complex rectangular (rational 40 . 43) real . 45.3)) 
45.309550125908586
Running (#<procedure:angle> (complex rectangular (rational 40 . 43) real . 45.3)) 
1.5502642774213815

Running (#<procedure:add> 0.0 9.0) 
(natural . 9)
Running (#<procedure:add> 9 9.0) 
(natural . 18)
Running (#<procedure:add> 25 25) 
(natural . 50)
Running (#<procedure:add> 25 26) 
(natural . 51)
Running (#<procedure:add> (natural . 30) (natural . 31)) 
(natural . 61)
Running (#<procedure:add> (natural . 30) (integer . 36)) 
(natural . 66)
Running (#<procedure:add> (integer . 35) (integer . 36)) 
(natural . 71)
Running (#<procedure:add> (integer . 35) (natural . 31)) 
(natural . 66)
Running (#<procedure:add> (integer . 35) (integer . 35)) 
(natural . 70)
Running (#<procedure:add> (natural . 31) (natural . 31)) 
(natural . 62)
Running (#<procedure:add> (rational 40 . 43) (natural . 31)) 
(rational 1373 . 43)
Running (#<procedure:add> (rational 40 . 43) (rational 44 . 47)) 
(rational 3772 . 2021)
Running (#<procedure:add> (rational 44 . 47) (rational 44 . 47)) 
(rational 88 . 47)
Running (#<procedure:add> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
(complex rectangular (natural . 75) natural . 81)
Running (#<procedure:add> (complex polar 50 . 55) (complex polar 25 . 26)) 
(complex rectangular (real . 17.279320871313796) real . -30.92379740594092)
Running (#<procedure:add> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
(complex rectangular (natural . 50) natural . 52)
Running (#<procedure:add> (complex polar 50 . 55) (complex polar 50 . 55)) 
(complex rectangular (real . 2.212675626195573) real . -99.97551733586198)
Running (#<procedure:add> (complex polar 50 . 55) 9.0) 
(complex rectangular (real . 10.106337813097786) real . -49.98775866793099)
Running (#<procedure:add> (complex polar 50 . 55) 25) 
(complex rectangular (real . 26.106337813097788) real . -49.98775866793099)
Running (#<procedure:add> (natural . 30) (complex polar 50 . 55)) 
(complex rectangular (real . 31.106337813097788) real . -49.98775866793099)
Running (#<procedure:add> (integer . 35) (complex polar 50 . 55)) 
(complex rectangular (real . 36.10633781309779) real . -49.98775866793099)
Running (#<procedure:add> (rational 40 . 43) (complex polar 50 . 55)) 
(complex rectangular (real . 2.0365703712373215) real . -49.98775866793099)
Running (#<procedure:add> (complex rectangular 50 . 55) (complex polar 50 . 55)) 
(complex rectangular (real . 51.10633781309779) real . 5.01224133206901)
Running (#<procedure:add> (complex rectangular (rational 40 . 43) real . 45.3) (complex polar (real . 6.730527468185536) rational 44 . 47)) 
(complex rectangular (real . 4.920603910510373) real . 50.72004948964289)
Running (#<procedure:add> (rational 40 . 43) (complex rectangular (integer . 35) integer . 36)) 
(complex rectangular (real . 35.93023255813954) natural . 36)

Running (#<procedure:sub> 0.0 9.0) 
(integer . -9)
Running (#<procedure:sub> 9 9.0) 
(natural . 0)
Running (#<procedure:sub> 25 25) 
(natural . 0)
Running (#<procedure:sub> 25 26) 
(integer . -1)
Running (#<procedure:sub> (natural . 30) (natural . 31)) 
(integer . -1)
Running (#<procedure:sub> (natural . 30) (integer . 36)) 
(integer . -6)
Running (#<procedure:sub> (integer . 35) (integer . 36)) 
(integer . -1)
Running (#<procedure:sub> (integer . 35) (natural . 31)) 
(natural . 4)
Running (#<procedure:sub> (integer . 35) (integer . 35)) 
(natural . 0)
Running (#<procedure:sub> (natural . 31) (natural . 31)) 
(natural . 0)
Running (#<procedure:sub> (rational 40 . 43) (natural . 31)) 
(rational -1293 . 43)
Running (#<procedure:sub> (rational 40 . 43) (rational 44 . 47)) 
(rational -12 . 2021)
Running (#<procedure:sub> (rational 44 . 47) (rational 44 . 47)) 
(rational 0 . 2209)
Running (#<procedure:sub> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
(complex rectangular (natural . 25) natural . 29)
Running (#<procedure:sub> (complex polar 50 . 55) (complex polar 25 . 26)) 
(complex rectangular (real . -15.066645245118222) real . -69.05171992992106)
Running (#<procedure:sub> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
(complex rectangular (natural . 0) natural . 0)
Running (#<procedure:sub> (complex polar 50 . 55) (complex polar 50 . 55)) 
(complex rectangular (natural . 0) natural . 0)
Running (#<procedure:sub> (complex polar 50 . 55) 9.0) 
(complex rectangular (real . -7.893662186902214) real . -49.98775866793099)
Running (#<procedure:sub> (complex polar 50 . 55) 25) 
(complex rectangular (real . -23.893662186902212) real . -49.98775866793099)
Running (#<procedure:sub> (natural . 30) (complex polar 50 . 55)) 
(complex rectangular (real . 28.893662186902212) real . 49.98775866793099)
Running (#<procedure:sub> (integer . 35) (complex polar 50 . 55)) 
(complex rectangular (real . 33.89366218690221) real . 49.98775866793099)
Running (#<procedure:sub> (rational 40 . 43) (complex polar 50 . 55)) 
(complex rectangular (real . -0.1761052549582517) real . 49.98775866793099)
Running (#<procedure:sub> (complex rectangular 50 . 55) (complex polar 50 . 55)) 
(complex rectangular (real . 48.89366218690221) real . 104.98775866793099)
Running (#<procedure:sub> (complex rectangular (rational 40 . 43) real . 45.3) (complex polar (real . 6.730527468185536) rational 44 . 47)) 
(complex rectangular (real . -3.060138794231303) real . 39.879950510357105)
Running (#<procedure:sub> (rational 40 . 43) (complex rectangular (integer . 35) integer . 36)) 
(complex rectangular (real . -34.06976744186046) integer . -36)

Running (#<procedure:mul> 0.0 9.0) 
(natural . 0)
Running (#<procedure:mul> 9 9.0) 
(natural . 81)
Running (#<procedure:mul> 25 25) 
(natural . 625)
Running (#<procedure:mul> 25 26) 
(natural . 650)
Running (#<procedure:mul> (natural . 30) (natural . 31)) 
(natural . 930)
Running (#<procedure:mul> (natural . 30) (integer . 36)) 
(natural . 1080)
Running (#<procedure:mul> (integer . 35) (integer . 36)) 
(natural . 1260)
Running (#<procedure:mul> (integer . 35) (natural . 31)) 
(natural . 1085)
Running (#<procedure:mul> (integer . 35) (integer . 35)) 
(natural . 1225)
Running (#<procedure:mul> (natural . 31) (natural . 31)) 
(natural . 961)
Running (#<procedure:mul> (rational 40 . 43) (natural . 31)) 
(rational 1240 . 43)
Running (#<procedure:mul> (rational 40 . 43) (rational 44 . 47)) 
(rational 1760 . 2021)
Running (#<procedure:mul> (rational 44 . 47) (rational 44 . 47)) 
(rational 1936 . 2209)
Running (#<procedure:mul> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
(complex polar (real . 2681.049234907856) real . 1.6379847609290847)
Running (#<procedure:mul> (complex polar 50 . 55) (complex polar 25 . 26)) 
(complex polar (natural . 1250) natural . 81)
Running (#<procedure:mul> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
(complex polar (real . 1300.9999999999998) real . 1.610006988509306)
Running (#<procedure:mul> (complex polar 50 . 55) (complex polar 50 . 55)) 
(complex polar (natural . 2500) natural . 110)
Running (#<procedure:mul> (complex polar 50 . 55) 9.0) 
(complex polar (natural . 450) natural . 55)
Running (#<procedure:mul> (complex polar 50 . 55) 25) 
(complex polar (natural . 1250) natural . 55)
Running (#<procedure:mul> (natural . 30) (complex polar 50 . 55)) 
(complex polar (natural . 1500) natural . 55)
Running (#<procedure:mul> (integer . 35) (complex polar 50 . 55)) 
(complex polar (natural . 1750) natural . 55)
Running (#<procedure:mul> (rational 40 . 43) (complex polar 50 . 55)) 
(complex polar (real . 46.51162790697674) natural . 55)
Running (#<procedure:mul> (complex rectangular 50 . 55) (complex polar 50 . 55)) 
(complex polar (real . 3716.517186829626) real . 55.83298126667443)
Running (#<procedure:mul> (complex rectangular (rational 40 . 43) real . 45.3) (complex polar (real . 6.730527468185536) rational 44 . 47)) 
(complex polar (real . 304.95717169355714) real . 2.4864344901873388)
Running (#<procedure:mul> (rational 40 . 43) (complex rectangular (integer . 35) integer . 36)) 
(complex polar (real . 46.706568225611875) real . 0.7994817392203085)

Running (#<procedure:div> 0.0 9.0) 
(natural . 0)
Running (#<procedure:div> 9 9.0) 
(natural . 1)
Running (#<procedure:div> 25 25) 
(natural . 1)
Running (#<procedure:div> 25 26) 
(real . 25/26)
Running (#<procedure:div> (natural . 30) (natural . 31)) 
(rational 30 . 31)
Running (#<procedure:div> (natural . 30) (integer . 36)) 
(rational 5 . 6)
Running (#<procedure:div> (integer . 35) (integer . 36)) 
(rational 35 . 36)
Running (#<procedure:div> (integer . 35) (natural . 31)) 
(rational 35 . 31)
Running (#<procedure:div> (integer . 35) (integer . 35)) 
(natural . 1)
Running (#<procedure:div> (natural . 31) (natural . 31)) 
(natural . 1)
Running (#<procedure:div> (rational 40 . 43) (natural . 31)) 
(rational 40 . 1333)
Running (#<procedure:div> (rational 40 . 43) (rational 44 . 47)) 
(rational 470 . 473)
Running (#<procedure:div> (rational 44 . 47) (rational 44 . 47)) 
(natural . 1)
Running (#<procedure:div> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
(complex polar (real . 2.060760365032941) real . 0.027977772419778724)
Running (#<procedure:div> (complex polar 50 . 55) (complex polar 25 . 26)) 
(complex polar (natural . 2) natural . 29)
Running (#<procedure:div> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
(complex polar (natural . 1) natural . 0)
Running (#<procedure:div> (complex polar 50 . 55) (complex polar 50 . 55)) 
(complex polar (natural . 1) natural . 0)
Running (#<procedure:div> (complex polar 50 . 55) 9.0) 
(complex polar (real . 50/9) natural . 55)
Running (#<procedure:div> (complex polar 50 . 55) 25) 
(complex polar (natural . 2) natural . 55)
Running (#<procedure:div> (natural . 30) (complex polar 50 . 55)) 
(complex polar (real . 3/5) integer . -55)
Running (#<procedure:div> (integer . 35) (complex polar 50 . 55)) 
(complex polar (real . 7/10) integer . -55)
Running (#<procedure:div> (rational 40 . 43) (complex polar 50 . 55)) 
(complex polar (real . 0.018604651162790697) integer . -55)
Running (#<procedure:div> (complex rectangular 50 . 55) (complex polar 50 . 55)) 
(complex polar (real . 1.4866068747318506) real . -54.16701873332557)
Running (#<procedure:div> (complex rectangular (rational 40 . 43) real . 45.3) (complex polar (real . 6.730527468185536) rational 44 . 47)) 
(complex polar (real . 6.731946395001262) real . 0.614094064655424)
Running (#<procedure:div> (rational 40 . 43) (complex rectangular (integer . 35) integer . 36)) 
(complex polar (real . 0.018527000486161) real . -0.7994817392203085)

> 
