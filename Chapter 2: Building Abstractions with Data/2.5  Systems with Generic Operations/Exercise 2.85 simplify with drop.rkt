#lang racket

; Exercise 2.85.  This section mentioned a method for "simplifying" a data object by lowering it
; in the tower of types as far as possible. Design a procedure drop that accomplishes this for the
; tower described in exercise 2.83. The key is to decide, in some general way, whether an object
; can be lowered. For example, the complex number 1.5 + 0i can be lowered as far as real, the
; complex number 1 + 0i can be lowered as far as integer, and the complex number 2 + 3i cannot be
; lowered at all. Here is a plan for determining whether an object can be lowered: Begin by
; defining a generic operation project that ``pushes'' an object down in the tower. For example,
; projecting a complex number would involve throwing away the imaginary part. Then a number can be
; dropped if, when we project it and raise the result back to the type we started with, we end up
; with something equal to what we started with. Show how to implement this idea in detail, by
; writing a drop procedure that drops an object as far as possible. You will need to design the
; various projection operations and install project as a generic operation in the system. You will
; also need to make use of a generic equality predicate, such as described in exercise 2.79.
; Finally, use drop to rewrite apply-generic from exercise 2.84 so that it "simplifies" its answers.

; S O L U T I O N

; Reference:
; (define (foo x)
;   (with-handlers ([exn:fail? (lambda (exn)
;                               (displayln (exn-message exn))
;                              #f)])
;  (/ 1 x)))

; Note: The tower is
; Natural->Integer->Rational->Real->Complex

; Generic Mathematical operations on numbers that may be ordinary numbers, rational numbers
; or complex numbers

(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equal? x y))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))
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
	(with-handlers ([exn:fail? (lambda (exn) (display "Could not drop beyond: ") (display x) (newline) x)])
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
			(display "New args: ")
			(display new-args)
			(newline)
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
					(error "Tried all raise options and failed to find a valid procedure for this operation")
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
		(display "Raising the argument in position: ")
		(display position-of-lowest-type)
		(newline)
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
		(if (same-type new-a a)
			; raise failed
			false
			; raise worked and we got a new type
			(if (same-type new-a b)
				; we successfully raised a to b
				true
				; a has not become b yet so keep looking
				(lower-in-tower? new-a b)
			)
		)
	)
)

(define (same-type x y)
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

; Natural number procedures
(define (make-natural-specific n)
	(if (or (natural? n) (= n (exact-round n)))
		n
		(error "Cannot make natural with: " n)
	)
)

(define (raise-natural n)
	(make-integer n)
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

(define (add-rat x y) (make-rational-specific (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (sub-rat x y) (make-rational-specific (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (mul-rat x y) (make-rational-specific (* (numer x) (numer y)) (* (denom x) (denom y))))
(define (div-rat x y) (make-rational-specific (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equal-rat? x y)
	; (display "Entered equal-rat?")
	; (newline)
	(and (= (numer x) (numer y)) (= (denom x) (denom y)))
)

(define (=zero-rat? x)
	(= 0 (numer x))
)

(define (raise-rat r)
	(make-real (* 1.0 (/ (numer r) (denom r))))
)

(define (project-rat r)
	; tries to push this object down one step in the tower
	(make-integer (numer r))
)

(define (mul-five-rats v w x y z)
	(mul-rat (mul-rat (mul-rat (mul-rat v w) x) y) z)
)

; Real Number procedures
(define (make-real-specific r)
	(if (real? r)
		r
		(error "Cannot make real with: " r)
	)
)

(define (raise-real r)
	(make-complex-from-real-imag r 0)
)

(define (project-real r)
	(make-rational r 1)
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
	; (display "Entered equal-complex?")
	; (newline)
	(and (= (real-part c1) (real-part c2)) (= (imag-part c1) (imag-part c2)))
)

(define (=zero-complex? c)
	(= 0 (magnitude c))
)

(define (project-complex c)
	(make-real (real-part c))
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
					(cons '(rational) raise-rat)
					(cons '(real) raise-real)
				)
			)
			(cons
				'project
				(list
					(cons '(complex) project-complex)
					(cons '(real) project-real)
					(cons '(rational) project-rat)
					(cons '(integer) project-int)
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
					(cons '(complex complex scheme-number) mul-and-scale-complex)
				)
			)
			(cons
				'mul-five-quantities
				(list
					(cons '(rational rational rational rational rational) (lambda (r1 r2 r3 r4 r5) (attach-tag 'rational (mul-five-rats r1 r2 r3 r4 r5))))
				)
			)
			(cons
				'equal?
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (= x y)))
					(cons '(natural natural) (lambda (x y) (= x y)))
					(cons '(integer integer) (lambda (x y) (= x y)))
					(cons '(rational rational) (lambda (x y) (equal-rat? x y)))
					(cons '(real real) (lambda (x y) (= x y)))
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

(define n1 (make-natural 46))

(define i1 (make-integer 35))
(define i2 (make-integer 17))
(define i3 (make-integer 236))
(define i4 (make-integer 29))

(define r1 (make-rational 100 25))
(define r2 (make-rational 2 5))
(define r3 (make-rational 6 5))
(define r4 (make-rational 7 -2))
(define r5 (make-rational 9 16))

(define real1 (make-real 65.0))
(define real2 (make-real (sqrt 10)))

(define z1 (make-complex-from-real-imag 23 12))
(define z2 (make-complex-from-real-imag 29.0 0))
(define z3 (make-complex-from-real-imag 29.5 0))
(define z4 (make-complex-from-real-imag 29.5 4.6))
(define z5 (make-complex-from-real-imag 89.3 348))
(define z6 (make-complex-from-real-imag 89.7 -348))

; Projection Tests
(display "Projection Tests")
(newline)

(project i1)
(project r1)
(project real1)
(project z1)

(newline)

z2
(project z2)
(project (project z2))
(project (project (project z2)))
(project (project (project (project z2))))

; Drop tests

(display "Drop Tests")
(newline)

(display "Dropping: ")
(display i1)
(display ": ")
(drop i1)
(display "Dropping: ")
(display r5)
(display ": ")
(drop r5)
(display "Dropping: ")
(display real2)
(display ": ")
(drop real2)

(display "Dropping: ")
(display z1)
(display ": ")
(drop z1)
(display "Dropping: ")
(display z2)
(display ": ")
(drop z2)
(display "Dropping: ")
(display z3)
(display ": ")
(drop z3)
(display "Dropping: ")
(display z4)
(display ": ")
(drop z4)

(display "Testing proc add-four-quantities")
(newline)
(add-four-quantities i1 i2 i3 i4)
(add-four-quantities i1 r4 z1 real1)
(display "Testing proc mul-five-quantities")
(newline)
(mul-five-quantities r1 r2 r3 r4 r5)
(mul-five-quantities i1 r2 r3 r4 r5)

(display "Testing proc mul-and-scale")
(newline)
(mul-and-scale z4 z5 200)

(display "z5: ")
z5
(newline)
(display "z6: ")
z6
(newline)
(display "z5 + z6: ")
(add z5 z6)
(newline)

; Error conditions
(project n1)

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Projection Tests
'(natural . 35)
'(integer . 4)
'(rational 65 . 1)
'(real . 23)

'(complex rectangular 29.0 . 0)
'(real . 29.0)
'(rational 29 . 1)
'(integer . 29)
'(natural . 29)
Drop Tests
Dropping: (integer . 35): Could not drop beyond: (natural . 35)
'(natural . 35)
Dropping: (rational 9 . 16): '(rational 9 . 16)
Dropping: (real . 3.1622776601683795): Could not drop beyond: (real . 3.1622776601683795)
'(real . 3.1622776601683795)
Dropping: (complex rectangular 23 . 12): '(complex rectangular 23 . 12)
Dropping: (complex rectangular 29.0 . 0): Could not drop beyond: (natural . 29)
'(natural . 29)
Dropping: (complex rectangular 29.5 . 0): Could not drop beyond: (real . 29.5)
'(real . 29.5)
Dropping: (complex rectangular 29.5 . 4.6): '(complex rectangular 29.5 . 4.6)
Testing proc add-four-quantities
New args: ((rational 35 . 1) (rational 17 . 1) (rational 236 . 1) (rational 29 . 1))
New args: ((real . 35.0) (real . 17.0) (real . 236.0) (real . 29.0))
New args: ((complex rectangular 35.0 . 0) (complex rectangular 17.0 . 0) (complex rectangular 236.0 . 0) (complex rectangular 29.0 . 0))
Entered proc add-four-complex-numbers
Could not drop beyond: (natural . 317)
'(natural . 317)
Raising the argument in position: 1
New args: ((rational 35 . 1) (rational -7 . 2) (complex rectangular 23 . 12) (real . 65.0))
Raising the argument in position: 1
New args: ((real . 35.0) (rational -7 . 2) (complex rectangular 23 . 12) (real . 65.0))
Raising the argument in position: 2
New args: ((real . 35.0) (real . -3.5) (complex rectangular 23 . 12) (real . 65.0))
Raising the argument in position: 1
New args: ((complex rectangular 35.0 . 0) (real . -3.5) (complex rectangular 23 . 12) (real . 65.0))
Raising the argument in position: 2
New args: ((complex rectangular 35.0 . 0) (complex rectangular -3.5 . 0) (complex rectangular 23 . 12) (real . 65.0))
Raising the argument in position: 4
New args: ((complex rectangular 35.0 . 0) (complex rectangular -3.5 . 0) (complex rectangular 23 . 12) (complex rectangular 65.0 . 0))
Entered proc add-four-complex-numbers
'(complex rectangular 119.5 . 12)
Testing proc mul-five-quantities
'(rational -189 . 50)
Raising the argument in position: 1
New args: ((rational 35 . 1) (rational 2 . 5) (rational 6 . 5) (rational -7 . 2) (rational 9 . 16))
'(rational -1323 . 40)
Testing proc mul-and-scale
Entered proc mul-and-scale-complex
'(complex rectangular 206709.9999999999 . 2135356.0)
z5: '(complex rectangular 89.3 . 348)

z6: '(complex rectangular 89.7 . -348)

z5 + z6: Could not drop beyond: (natural . 179)
'(natural . 179)

. . (All arguments are of the same type and) no procedure was found for these types (project (natural))
> 
