#lang racket

; Exercise 2.88. Extend the polynomial system to include subtraction of polynomials.
; (Hint: You may find it helpful to define a generic negation operation.)

; S O L U T I O N

(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))

(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equal? x y))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (NEGATE x) (apply-generic 'negate x))

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
(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))

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

; Polynomial procedures

(define (make-poly variable term-list) (cons variable term-list))
(define (variable p) (car p))
(define (term-list p) (cdr p))

(define (adjoin-term term term-list)
	(if (=zero? (coeff term))
		term-list
		(cons term term-list)
	)
)
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (add-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly
			(variable p1)
			(add-terms (term-list p1) (term-list p2))
		)
		(error "Polys not in same var -- ADD-POLY"
			(list p1 p2)
		)
	)
)

(define (sub-poly p1 p2)
	(add-poly p1 (negate-poly p2))
)

(define (mul-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly
			(variable p1)
			(mul-terms (term-list p1) (term-list p2))
		)
		(error "Polys not in same var -- MUL-POLY"
			(list p1 p2)
		)
	)
)

(define (add-terms L1 L2)
	(cond
		((empty-termlist? L1) L2)
		((empty-termlist? L2) L1)
		(else
			(let ((t1 (first-term L1)) (t2 (first-term L2)))
				(cond
					((> (order t1) (order t2))
						(adjoin-term t1 (add-terms (rest-terms L1) L2))
					)
					((< (order t1) (order t2))
						(adjoin-term t2 (add-terms L1 (rest-terms L2)))
					)
					(else
						(adjoin-term
							(make-term (order t1) (add (coeff t1) (coeff t2)))
							(add-terms (rest-terms L1) (rest-terms L2))
						)
					)
				)
			)
		)
	)
)

(define (mul-terms L1 L2)
	(if (empty-termlist? L1)
		(the-empty-termlist)
		(add-terms
			(mul-term-by-all-terms (first-term L1) L2)
			(mul-terms (rest-terms L1) L2)
		)
	)
)

(define (mul-term-by-all-terms t1 L)
	(if (empty-termlist? L)
		(the-empty-termlist)
		(let ((t2 (first-term L)))
			(adjoin-term
				(make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
				(mul-term-by-all-terms t1 (rest-terms L))
			)
		)
	)
)

(define (=zero-polynomial? p)
	; A polynomial is zero if its term-list is empty
	(empty-termlist? (term-list p))
)

(define (negate-poly p)
	(make-poly
		(variable p)
		(negate-term-list (term-list p))
	)
)

(define (negate-term-list t)
	(if (null? t)
		(list)
		(cons (negate-term (first-term t)) (negate-term-list (rest-terms t)))
	)
)

(define (negate-term t)
	(list (order t) (* -1 (coeff t)))
)

(define (empty-termlist? t)
	(null? t)
)

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (variable? x) (symbol? x))

; Coercion procedures

(define (complex->complex z) z)
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
					(cons 'polynomial (lambda (var terms) (attach-tag 'polynomial (make-poly var terms))))
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
					(cons '(polynomial polynomial) (lambda (p1 p2) (attach-tag 'polynomial (add-poly p1 p2))))
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
					(cons '(polynomial polynomial) (lambda (p1 p2) (attach-tag 'polynomial (sub-poly p1 p2))))
				)
			)
			(cons
				'mul
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (* x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (mul-rational x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (* x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (mul-complex z1 z2))))
					(cons '(polynomial polynomial) (lambda (p1 p2) (attach-tag 'polynomial (mul-poly p1 p2))))
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
				'negate
				(list
					(cons '(polynomial) (lambda (p) (attach-tag 'polynomial (negate-poly p))))
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
					(cons '(polynomial) =zero-polynomial?)
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

(define p1 (make-polynomial 'x (list (list 5 1) (list 4 2) (list 2 3) (list 1 -2) (list 0 -5))))
(define p2 (make-polynomial 'x (list (list 5 2) (list 4 4) (list 2 5) (list 1 -7) (list 0 -15))))

(run-test =zero? p1)
(run-test =zero? p2)
(run-test add p1 p2)
(run-test mul p1 p2)
(run-test NEGATE p1)
(run-test NEGATE p2)
(run-test sub p1 p2)
(run-test sub p1 p1)
(run-test =zero? (sub p1 p1))

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Running (#<procedure:=zero?> (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
#f
Running (#<procedure:=zero?> (polynomial x (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
#f
Running (#<procedure:add> (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x (5 (natural . 3)) (4 (natural . 6)) (2 (natural . 8)) (1 (integer . -9)) (0 (integer . -20)))
Running (#<procedure:mul> (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x (10 (natural . 2)) (9 (natural . 8)) (8 (natural . 8)) (7 (natural . 11)) (6 (natural . 11)) (5 (integer . -47)) (4 (integer . -35)) (3 (integer . -31)) (2 (integer . -56)) (1 (natural . 65)) (0 (natural . 75)))
Running (#<procedure:NEGATE> (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
(polynomial x (5 -1) (4 -2) (2 -3) (1 2) (0 5))
Running (#<procedure:NEGATE> (polynomial x (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x (5 -2) (4 -4) (2 -5) (1 7) (0 15))
Running (#<procedure:sub> (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -2)) (1 (natural . 5)) (0 (natural . 10)))
Running (#<procedure:sub> (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
(polynomial x)
Running (#<procedure:=zero?> (polynomial x)) 
#t
> 
