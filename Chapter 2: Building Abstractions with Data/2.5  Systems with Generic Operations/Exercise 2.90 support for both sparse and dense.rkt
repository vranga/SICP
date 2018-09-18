#lang racket

; Exercise 2.90.  Suppose we want to have a polynomial system that is efficient for both sparse and dense
; polynomials. One way to do this is to allow both kinds of term-list representations in our system.
; The situation is analogous to the complex-number example of section 2.4, where we allowed both rectangular
; and polar representations. To do this we must distinguish different types of term lists and make the
; operations on term lists generic.
; Redesign the polynomial system to implement this generalization. This is a major effort, not a local change.

; S O L U T I O N

; GENERIC PROCEDURES

; Generic Polynomial procedures
; Note: I have designed this with the assumption that the procedures adjoin-term, first-term and rest-terms
; though generic, will still be used only internally by the polynomial procedures.
; These three procedures are generic but not exposed to the outside world.
(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (order term) (apply-generic 'order term))
(define (coeff term) (apply-generic 'coeff term))
(define (negate-term term) (apply-generic 'negate-term term))

; Generic Logical procedures
(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equal? x y))

(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))
(define (NEGATE x) (apply-generic 'negate x))

; Generic Arithmetic procedures with two arguments
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))

; Generic Trigonometric procedures
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (tan-inverse x y) (apply-generic 'tan-inverse x y))

; Generic Contrived procedures
(define (add-four-quantities w x y z) (apply-generic 'add-four-quantities w x y z))
(define (mul-and-scale x y factor) (apply-generic 'mul-and-scale x y factor))
(define (mul-five-quantities v w x y z) (apply-generic 'mul-five-quantities v w x y z))

; Generic operation that 'projects' x one level lower in the tower
(define (project x) (apply-generic 'project x))

; Generic operation that raises x one level in the tower
(define (raise x) (apply-generic 'raise x))

; Generic Constructions of specific types of entities (ordinary, rational, complex etc.)
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-natural n) ((get 'make 'natural) n))
(define (make-integer n) ((get 'make 'integer) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-real r) ((get 'make 'real) r))
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))
(define (make-polynomial-dense-terms terms) ((get 'make 'polynomial-dense-terms) terms))
(define (make-polynomial-sparse-terms terms) ((get 'make 'polynomial-sparse-terms) terms))
(define (make-term order coeff) ((get 'make 'polynomial-term) order coeff))

(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))

; POLYNOMIAL PROCEDURES

(define (make-poly variable term-list) (cons variable term-list))
(define (make-dense-terms terms) terms)
(define (make-sparse-terms terms) terms)

(define (variable p) (car p))
(define (term-list p) (cdr p))

(define (the-empty-poly-termlist) '())
; (define (the-empty-poly-termlist) (list 'polynomial-sparse-terms (list 0 0)))

; Term-list manipulation for dense polynomials
(define (adjoin-term-dense term term-list)
	; Structure of term-list
	; [OrderOfPolynomial, (list of coefficients)]
	; Maintaining the order of the polynomial as the first element allows us to avoid repeated
	; expensive 'length' calls on the coefficient list
	; Reminder: We assume that term lists are represented as lists of terms,
	; arranged from highest-order to lowest-order term.
	(if (=zero? (coeff-poly-term term))
		term-list
		(cond
			((= (order-poly-term term) (+ (polynomial-order-dense term-list) 1))
				; no need to insert a zero in the coefficient list
				(cons (+ 1 (polynomial-order-dense term-list)) (cons (coeff-poly-term term) (coefficients-dense term-list)))
			)
			((and (empty-termlist? term-list) (= 0 (order-poly-term term)))
				(list 0 (coeff-poly-term term))
			)
			((and (empty-termlist? term-list) (> (order-poly-term term) 0))
				(adjoin-term-dense term (list 0 0))
			)
			(else
				; we need to supply zero(s) if there are gaps
				(adjoin-term-dense term (cons (+ 1 (polynomial-order-dense term-list)) (cons 0 (coefficients-dense term-list))))
			)
		)
	)
)

(define (first-term-dense term-list)
	; Since we are using the term-list representation that is appropriate for
	; dense polynomials (see SICP text), we need to do some extra processing
	; to retrieve both the order and coefficient
	(if (pair? term-list)
		(if (pair? (coefficients-dense term-list))
			(list (polynomial-order-dense term-list) (car (coefficients-dense term-list)))	
			null
		)
		null
	)
)

(define (rest-terms-dense term-list)
	; Since we are using the term-list representation that is appropriate for
	; dense polynomials (see SICP text), we need to do some extra processing
	; to retrieve both the order and coefficient
	(if (> (polynomial-order-dense term-list) 0)
		(cons (- (polynomial-order-dense term-list) 1) (cdr (coefficients-dense term-list)))
		(the-empty-poly-termlist)
	)
)

(define (polynomial-order-dense term-list)
	(if (pair? term-list)
		(car term-list)
		0
	)
)

(define (coefficients-dense term-list)
	(if (pair? term-list)
		(cdr term-list)
		(list 0)
	)
)

; Term-list manipulation for sparse polynomials
(define (adjoin-term-sparse term term-list)
	(if (=zero? (coeff-poly-term term))
		term-list
		(cons term term-list)
	)
)

(define (first-term-sparse term-list)
	(cond
		((null? term-list) null)
		((not (pair? term-list)) (error "Procedure first-term-sparse: term-list is not a pair!"))
		(else
			(car term-list)
		)
	)
)

(define (rest-terms-sparse term-list)
	(cond
		((null? term-list) (the-empty-poly-termlist))
		((not (pair? term-list)) (error "Procedure rest-terms-sparse: term-list is not a pair!"))
		(else
			(cdr term-list)
		)
	)
)

; Term manipulation (used in both dense and sparse polynomial representations)
(define (make-poly-term order coeff) (list order coeff))

(define (order-poly-term term)
	(if (pair? term)
		(car term)
		0
	)
)

(define (coeff-poly-term term)
	(if (pair? term)
		(cadr term)
		0
	)
)

; Polynomial Operations
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
			(variable p2)
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
	(if (or (empty-termlist? L1) (empty-termlist? L2))
		(the-empty-poly-termlist)
		(add-terms
			(mul-term-by-all-terms (first-term L1) L2)
			(mul-terms (rest-terms L1) L2)
		)
	)
)

(define (mul-term-by-all-terms t1 L)
	(if (or (empty-termlist? L) (empty-term? t1))
		; (the-empty-poly-termlist)
		; Returning an empty list with an explicit tag so that the generic procedure
		; 'adjoin-term' below does not fail. Generic procedures expect a data tag for each of their arguments
		(list 'polynomial-sparse-terms (list 0 0))
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
	(if (empty-termlist? t)
		t
		(adjoin-term (negate-term (first-term t)) (negate-term-list (rest-terms t)))
	)
)

(define (negate-poly-term t)
	(list (order-poly-term t) (* -1 (coeff-poly-term t)))
)

(define (empty-termlist? t)
	; consider both with tag and without tag
	; This procedure will return true for the following data:
	; '()
	; ('polynomial-sparse-terms)
	; ('polynomial-dense-terms)
	; ('polynomial-sparse-terms (0 0))
	; ('polynomial-dense-terms 0 0)
	(or
		(null? t)
		(and (symbol? (car t)) (null? (cdr t)))
		(and (eq? (car t) 'polynomial-sparse-terms) (eq? (caadr t) 0) (eq? (cadadr t) 0))
		(and (eq? (car t) 'polynomial-dense-terms) (eq? (cadr t) 0) (eq? (caddr t) 0))
	)
)

(define (empty-term? t)
	(cond
		((null? t) true)
		((not (pair? t)) (error "Procedure empty-term: t not a pair!"))
		(else
			(eq? (cadr t) 0)
		)
	)
)

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (variable? x) (symbol? x))

; COMPLEX NUMBER PROCEDURES
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

; REAL NUMBER PROCEDURES
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

; RATIONAL NUMBER PROCEDURES
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

; INTEGER PROCEDURES
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

; NATURAL NUMBER PROCEDURES
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

; GENERIC PROCEDURE FRAMEWORK

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
					(cons 'polynomial-dense-terms (lambda (terms) (attach-tag 'polynomial-dense-terms (make-dense-terms terms))))
					(cons 'polynomial-sparse-terms (lambda (terms) (attach-tag 'polynomial-sparse-terms (make-sparse-terms terms))))
					(cons 'polynomial-term (lambda (order coeff) (attach-tag 'polynomial-term (make-poly-term order coeff))))
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
			(cons
				'first-term
				(list
					(cons '(polynomial-dense-terms) (lambda (x) (attach-tag 'polynomial-term (first-term-dense x))))
					(cons '(polynomial-sparse-terms) (lambda (x) (attach-tag 'polynomial-term (first-term-sparse x))))
				)
			)
			(cons
				'rest-terms
				(list
					(cons '(polynomial-dense-terms) (lambda (x) (attach-tag 'polynomial-dense-terms (rest-terms-dense x))))
					(cons '(polynomial-sparse-terms) (lambda (x) (attach-tag 'polynomial-sparse-terms (rest-terms-sparse x))))
				)
			)
			(cons
				'adjoin-term
				(list
					(cons '(polynomial-term polynomial-dense-terms) (lambda (term terms) (attach-tag 'polynomial-dense-terms (adjoin-term-dense term terms))))
					(cons '(polynomial-term polynomial-sparse-terms) (lambda (term terms) (attach-tag 'polynomial-sparse-terms (adjoin-term-sparse term terms))))
				)
			)
			(cons
				'order
				(list
					(cons '(polynomial-term) order-poly-term)
				)
			)
			(cons
				'coeff
				(list
					(cons '(polynomial-term) coeff-poly-term)
				)
			)
			(cons
				'negate-term
				(list
					(cons '(polynomial-term) (lambda (term) (attach-tag 'polynomial-term (negate-poly-term term))))
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

(define t1-sparse (make-polynomial-sparse-terms (list (list 0 0))))
(define t2-sparse (make-polynomial-sparse-terms (list (list 0 5))))
(define t3-sparse (make-polynomial-sparse-terms (list (list 1 7) (list 0 5))))
(define t4-sparse (make-polynomial-sparse-terms (list (list 1 9) (list 0 -25))))
(define t5-sparse (make-polynomial-sparse-terms (list (list 5 1) (list 4 2) (list 2 3) (list 1 -2) (list 0 -5))))
(define t6-sparse (make-polynomial-sparse-terms (list (list 5 2) (list 4 4) (list 2 5) (list 1 -7) (list 0 -15))))
(define t7-sparse (make-polynomial-sparse-terms (list (list 10 3) (list 7 4) (list 5 -11) (list 1 -7) (list 0 -15))))
(define t8-sparse (make-polynomial-sparse-terms (list (list 12 4) (list 6 13) (list 5 -12) (list 1 -7) (list 0 -15))))

(define t1-dense (make-polynomial-dense-terms (list 0 0)))
(define t2-dense (make-polynomial-dense-terms (list 0 5)))
(define t3-dense (make-polynomial-dense-terms (list 1 3 5)))
(define t4-dense (make-polynomial-dense-terms (list 1 4 9)))
(define t5-dense (make-polynomial-dense-terms (list 2 3 5 23)))
(define t6-dense (make-polynomial-dense-terms (list 2 4 9 19)))
(define t7-dense (make-polynomial-dense-terms (list 5 1 2 0 3 -2 -5)))
(define t8-dense (make-polynomial-dense-terms (list 5 2 4 0 5 -7 -15)))

(define sp1 (make-polynomial 'x t1-sparse))
(define sp2 (make-polynomial 'x t2-sparse))
(define sp3 (make-polynomial 'x t3-sparse))
(define sp4 (make-polynomial 'x t4-sparse))
(define sp5 (make-polynomial 'x t5-sparse))
(define sp6 (make-polynomial 'x t6-sparse))
(define sp7 (make-polynomial 'x t7-sparse))
(define sp8 (make-polynomial 'x t8-sparse))

(define dp1 (make-polynomial 'x t1-dense))
(define dp2 (make-polynomial 'x t2-dense))
(define dp3 (make-polynomial 'x t3-dense))
(define dp4 (make-polynomial 'x t4-dense))
(define dp5 (make-polynomial 'x t5-dense))
(define dp6 (make-polynomial 'x t6-dense))
(define dp7 (make-polynomial 'x t7-dense))
(define dp8 (make-polynomial 'x t8-dense))

(run-test =zero? sp1)
(run-test =zero? sp2)
(run-test =zero? sp3)
(run-test =zero? sp4)

(run-test =zero? dp1)
(run-test =zero? dp2)
(run-test =zero? dp3)
(run-test =zero? dp4)
(run-test =zero? dp5)
(run-test =zero? dp6)

sp1
(define term1 (make-term 6 20))
term1
(adjoin-term term1 (term-list (cdr sp1)))

sp2
(define term2 (make-term 7 20))
term2
(adjoin-term term2 (term-list (cdr sp2)))

sp3
(define term3 (make-term 9 20))
term3
(adjoin-term term3 (term-list (cdr sp3)))

sp4
(define term4 (make-term 9 40))
term4
(adjoin-term term4 (term-list (cdr sp4)))

dp1
(adjoin-term term2 (term-list (cdr dp1)))
dp2
(adjoin-term term2 (term-list (cdr dp2)))
dp3
(adjoin-term term2 (term-list (cdr dp3)))
dp4
(adjoin-term term2 (term-list (cdr dp4)))
dp5
(adjoin-term term2 (term-list (cdr dp5)))
dp6
(adjoin-term term2 (term-list (cdr dp6)))

dp5
(define term5 (make-term 8 20))
term5
(adjoin-term term5 (term-list (cdr dp5)))

dp6
(define term6 (make-term 30 20))
term6
(adjoin-term term6 (term-list (cdr dp6)))

(run-test add sp1 sp1)
(run-test add sp1 sp2)
(run-test add sp2 sp3)
(run-test add sp3 sp4)
(run-test add sp4 sp5)
(run-test add sp5 sp6)
(run-test add sp6 sp7)
(run-test add sp7 sp8)

(newline)

(run-test add dp1 dp1)
(run-test add dp1 dp2)
(run-test add dp2 dp3)
(run-test add dp3 dp4)
(run-test add dp4 dp5)
(run-test add dp5 dp6)
(run-test add dp6 dp7)
(run-test add dp7 dp8)

(newline)

(run-test add sp1 dp2)
(run-test add dp2 sp1)
(run-test add sp2 dp3)
(run-test add sp3 dp4)
(run-test add sp4 dp5)
(run-test add sp5 dp6)
(run-test add sp6 dp7)
(run-test add sp7 dp8)

(newline)

(run-test add dp1 sp2)
(run-test add sp2 dp1)
(run-test add dp2 sp3)
(run-test add dp3 sp4)
(run-test add dp4 sp5)
(run-test add dp5 sp6)
(run-test add dp6 sp7)
(run-test add dp7 sp8)

(newline)

(run-test mul sp1 sp1)
(run-test mul sp1 sp2)
(run-test mul sp2 sp2)
(run-test mul sp2 sp3)
(run-test mul sp3 sp4)
(run-test mul sp4 sp5)
(run-test mul sp5 sp6)
(run-test mul sp6 sp7)
(run-test mul sp7 sp8)

(newline)

(run-test mul dp1 dp1)
(run-test mul dp1 dp2)
(run-test mul dp2 dp2)
(run-test mul dp2 dp3)
(run-test mul dp3 dp4)
(run-test mul dp4 dp5)
(run-test mul dp5 dp6)
(run-test mul dp6 dp7)
(run-test mul dp7 dp8)

(newline)

(run-test mul sp1 dp2)
(run-test mul dp2 sp1)
(run-test mul sp2 dp3)
(run-test mul sp3 dp4)
(run-test mul sp4 dp5)
(run-test mul sp5 dp6)
(run-test mul sp6 dp7)
(run-test mul sp7 dp8)

(newline)

(run-test mul dp1 sp2)
(run-test mul sp2 dp1)
(run-test mul dp2 sp3)
(run-test mul dp3 sp4)
(run-test mul dp4 sp5)
(run-test mul dp5 sp6)
(run-test mul dp6 sp7)
(run-test mul dp7 sp8)

(newline)

(run-test NEGATE sp1)
(run-test NEGATE sp2)
(run-test NEGATE sp2)
(run-test NEGATE sp3)
(run-test NEGATE sp4)
(run-test NEGATE sp5)
(run-test NEGATE sp6)
(run-test NEGATE sp7)
(run-test NEGATE sp8)

(newline)

(run-test NEGATE dp1)
(run-test NEGATE dp2)
(run-test NEGATE dp2)
(run-test NEGATE dp3)
(run-test NEGATE dp4)
(run-test NEGATE dp5)
(run-test NEGATE dp6)
(run-test NEGATE dp7)
(run-test NEGATE dp8)

(run-test sub sp1 sp1)
(run-test sub sp1 sp2)
(run-test sub sp2 sp2)
(run-test sub sp2 sp3)
(run-test sub sp3 sp4)
(run-test sub sp4 sp5)
(run-test sub sp5 sp6)
(run-test sub sp6 sp7)
(run-test sub sp7 sp8)

(newline)

(run-test sub dp1 dp1)
(run-test sub dp1 dp2)
(run-test sub dp2 dp2)
(run-test sub dp2 dp3)
(run-test sub dp3 dp4)
(run-test sub dp4 dp5)
(run-test sub dp5 dp6)
(run-test sub dp6 dp7)
(run-test sub dp7 dp8)

(newline)

(run-test sub sp1 dp2)
(run-test sub dp2 sp1)
(run-test sub sp2 dp3)
(run-test sub sp3 dp4)
(run-test sub sp4 dp5)
(run-test sub sp5 dp6)
(run-test sub sp6 dp7)
(run-test sub sp7 dp8)

(newline)

(run-test sub dp1 sp2)
(run-test sub sp2 dp1)
(run-test sub dp2 sp3)
(run-test sub dp3 sp4)
(run-test sub dp4 sp5)
(run-test sub dp5 sp6)
(run-test sub dp6 sp7)
(run-test sub dp7 sp8)

(newline)

(run-test =zero? (sub dp6 dp6))
(run-test =zero? (sub sp4 sp4))

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Running (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (0 0))) 
#t
Running (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (0 5))) 
#f
Running (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
#f
Running (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
#f
Running (#<procedure:=zero?> (polynomial x polynomial-dense-terms 0 0)) 
#t
Running (#<procedure:=zero?> (polynomial x polynomial-dense-terms 0 5)) 
#f
Running (#<procedure:=zero?> (polynomial x polynomial-dense-terms 1 3 5)) 
#f
Running (#<procedure:=zero?> (polynomial x polynomial-dense-terms 1 4 9)) 
#f
Running (#<procedure:=zero?> (polynomial x polynomial-dense-terms 2 3 5 23)) 
#f
Running (#<procedure:=zero?> (polynomial x polynomial-dense-terms 2 4 9 19)) 
#f
'(polynomial x polynomial-sparse-terms (0 0))
'(polynomial-term 6 20)
'(polynomial-sparse-terms (6 20) (0 0))
'(polynomial x polynomial-sparse-terms (0 5))
'(polynomial-term 7 20)
'(polynomial-sparse-terms (7 20) (0 5))
'(polynomial x polynomial-sparse-terms (1 7) (0 5))
'(polynomial-term 9 20)
'(polynomial-sparse-terms (9 20) (1 7) (0 5))
'(polynomial x polynomial-sparse-terms (1 9) (0 -25))
'(polynomial-term 9 40)
'(polynomial-sparse-terms (9 40) (1 9) (0 -25))
'(polynomial x polynomial-dense-terms 0 0)
'(polynomial-dense-terms 7 20 0 0 0 0 0 0 0)
'(polynomial x polynomial-dense-terms 0 5)
'(polynomial-dense-terms 7 20 0 0 0 0 0 0 5)
'(polynomial x polynomial-dense-terms 1 3 5)
'(polynomial-dense-terms 7 20 0 0 0 0 0 3 5)
'(polynomial x polynomial-dense-terms 1 4 9)
'(polynomial-dense-terms 7 20 0 0 0 0 0 4 9)
'(polynomial x polynomial-dense-terms 2 3 5 23)
'(polynomial-dense-terms 7 20 0 0 0 0 3 5 23)
'(polynomial x polynomial-dense-terms 2 4 9 19)
'(polynomial-dense-terms 7 20 0 0 0 0 4 9 19)
'(polynomial x polynomial-dense-terms 2 3 5 23)
'(polynomial-term 8 20)
'(polynomial-dense-terms 8 20 0 0 0 0 0 3 5 23)
'(polynomial x polynomial-dense-terms 2 4 9 19)
'(polynomial-term 30 20)
'(polynomial-dense-terms 30 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 9 19)
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 0))) 
(polynomial x polynomial-sparse-terms (0 0))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x polynomial-sparse-terms (0 5))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
(polynomial x polynomial-sparse-terms (1 7) (0 (natural . 10)))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
(polynomial x polynomial-sparse-terms (1 (natural . 16)) (0 (integer . -20)))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
(polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 (natural . 7)) (0 (integer . -30)))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (5 (natural . 3)) (4 (natural . 6)) (2 (natural . 8)) (1 (integer . -9)) (0 (integer . -20)))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (10 3) (7 4) (5 (integer . -9)) (4 4) (2 5) (1 (integer . -14)) (0 (integer . -30)))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (12 4) (10 3) (7 4) (6 13) (5 (integer . -23)) (1 (integer . -14)) (0 (integer . -30)))

Running (#<procedure:add> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 0)) 
(polynomial x polynomial-dense-terms 0 0)
Running (#<procedure:add> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x polynomial-dense-terms 0 5)
Running (#<procedure:add> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 1 3 5)) 
(polynomial x polynomial-dense-terms 1 3 (natural . 10))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-dense-terms 1 4 9)) 
(polynomial x polynomial-dense-terms 1 (natural . 7) (natural . 14))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-dense-terms 2 3 5 23)) 
(polynomial x polynomial-dense-terms 2 3 (natural . 9) (natural . 32))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 4 9 19)) 
(polynomial x polynomial-dense-terms 2 (natural . 7) (natural . 14) (natural . 42))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
(polynomial x polynomial-dense-terms 5 1 2 0 (natural . 7) (natural . 7) (natural . 14))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
(polynomial x polynomial-dense-terms 5 (natural . 3) (natural . 6) 0 (natural . 8) (integer . -9) (integer . -20))

Running (#<procedure:add> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x polynomial-dense-terms 0 5)
Running (#<procedure:add> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (0 0))) 
(polynomial x polynomial-dense-terms 0 5)
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 1 3 5)) 
(polynomial x polynomial-dense-terms 1 3 (natural . 10))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-dense-terms 1 4 9)) 
(polynomial x polynomial-dense-terms 1 (natural . 11) (natural . 14))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-dense-terms 2 3 5 23)) 
(polynomial x polynomial-dense-terms 2 3 (natural . 14) (integer . -2))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-dense-terms 2 4 9 19)) 
(polynomial x polynomial-dense-terms 5 1 2 0 (natural . 7) (natural . 7) (natural . 14))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
(polynomial x polynomial-dense-terms 5 (natural . 3) (natural . 6) 0 (natural . 8) (integer . -9) (integer . -20))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
(polynomial x polynomial-dense-terms 10 3 0 0 4 0 (integer . -9) 4 0 5 (integer . -14) (integer . -30))

Running (#<procedure:add> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x polynomial-sparse-terms (0 5))
Running (#<procedure:add> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 0 0)) 
(polynomial x polynomial-sparse-terms (0 5))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
(polynomial x polynomial-sparse-terms (1 7) (0 (natural . 10)))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
(polynomial x polynomial-sparse-terms (1 (natural . 12)) (0 (integer . -20)))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
(polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 (natural . 2)) (0 (natural . 4)))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (5 2) (4 4) (2 (natural . 8)) (1 (integer . -2)) (0 (natural . 8)))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (2 4) (1 (natural . 2)) (0 (natural . 4)))
Running (#<procedure:add> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (12 4) (6 13) (5 (integer . -11)) (4 2) (2 3) (1 (integer . -9)) (0 (integer . -20)))

Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 0))) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
(polynomial x polynomial-sparse-terms (2 (natural . 63)) (1 (integer . -175)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
(polynomial x polynomial-sparse-terms (6 (natural . 9)) (5 (natural . 18)) (3 (natural . 27)) (2 (integer . -18)) (1 (integer . -45)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (10 (natural . 2)) (9 (natural . 8)) (8 (natural . 8)) (7 (natural . 11)) (6 (natural . 11)) (5 (integer . -37)) (4 (integer . -15)) (3 (integer . -31)) (2 (integer . -31)) (1 (natural . 30)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (15 (natural . 6)) (14 (natural . 12)) (12 (natural . 23)) (11 (integer . -5)) (10 (integer . -22)) (9 (integer . -24)) (8 (integer . -28)) (7 (integer . -55)) (6 (natural . 63)) (5 (integer . -58)) (4 (integer . -60)) (3 (integer . -35)) (2 (integer . -26)) (1 (natural . 105)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (22 (natural . 12)) (19 (natural . 16)) (17 (integer . -44)) (16 (natural . 39)) (15 (integer . -36)) (13 (natural . 24)) (12 (integer . -48)) (11 (integer . -164)) (10 (natural . 87)) (8 (integer . -28)) (7 (integer . -151)) (6 (natural . 161)) (5 (natural . 165)) (2 (natural . 49)) (1 (natural . 105)) (0 0))

Running (#<procedure:mul> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 0)) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 1 3 5)) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-dense-terms 1 4 9)) 
(polynomial x polynomial-sparse-terms (2 (natural . 12)) (1 (natural . 27)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-dense-terms 2 3 5 23)) 
(polynomial x polynomial-sparse-terms (3 (natural . 12)) (2 (natural . 20)) (1 (natural . 92)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 4 9 19)) 
(polynomial x polynomial-sparse-terms (4 (natural . 12)) (3 (natural . 47)) (2 (natural . 102)) (1 (natural . 95)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
(polynomial x polynomial-sparse-terms (7 (natural . 4)) (6 (natural . 17)) (5 (natural . 18)) (4 (natural . 12)) (3 (natural . 19)) (2 (integer . -38)) (1 (integer . -45)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
(polynomial x polynomial-sparse-terms (10 (natural . 2)) (9 (natural . 8)) (8 (natural . 8)) (7 (natural . 11)) (6 (natural . 11)) (5 (integer . -37)) (4 (integer . -15)) (3 (integer . -31)) (2 (integer . -31)) (1 (natural . 30)) (0 0))

Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (0 0))) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 1 3 5)) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-dense-terms 1 4 9)) 
(polynomial x polynomial-sparse-terms (2 (natural . 28)) (1 (natural . 63)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-dense-terms 2 3 5 23)) 
(polynomial x polynomial-sparse-terms (3 (natural . 27)) (2 (natural . 45)) (1 (natural . 207)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-dense-terms 2 4 9 19)) 
(polynomial x polynomial-sparse-terms (7 (natural . 4)) (6 (natural . 17)) (5 (natural . 37)) (4 (natural . 50)) (3 (natural . 19)) (2 (natural . 39)) (1 (integer . -38)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
(polynomial x polynomial-sparse-terms (10 (natural . 2)) (9 (natural . 8)) (8 (natural . 8)) (7 (natural . 11)) (6 (natural . 11)) (5 (integer . -32)) (4 (integer . -5)) (3 (integer . -31)) (2 (integer . -11)) (1 (natural . 35)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
(polynomial x polynomial-sparse-terms (15 (natural . 6)) (14 (natural . 12)) (12 (natural . 23)) (11 (integer . -5)) (10 (integer . -67)) (9 (integer . -24)) (8 (integer . -28)) (7 (integer . -115)) (6 (natural . 63)) (5 (natural . 137)) (3 (integer . -35)) (2 (natural . 49)) (1 (natural . 105)) (0 0))

Running (#<procedure:mul> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 0 0)) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
(polynomial x)
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
(polynomial x polynomial-sparse-terms (2 (natural . 27)) (1 (integer . -75)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
(polynomial x polynomial-sparse-terms (6 (natural . 4)) (5 (natural . 8)) (3 (natural . 12)) (2 (integer . -8)) (1 (integer . -20)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (7 (natural . 6)) (6 (natural . 22)) (5 (natural . 20)) (4 (natural . 15)) (3 (natural . 4)) (2 (integer . -80)) (1 (integer . -75)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (12 (natural . 12)) (11 (natural . 27)) (9 (natural . 16)) (8 (natural . 36)) (7 (integer . -44)) (6 (integer . -99)) (3 (integer . -28)) (2 (integer . -123)) (1 (integer . -135)) (0 0))
Running (#<procedure:mul> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (17 (natural . 4)) (16 (natural . 8)) (14 (natural . 12)) (13 (integer . -8)) (11 (natural . 13)) (10 (natural . 14)) (9 (integer . -24)) (8 (natural . 39)) (7 (integer . -62)) (6 (natural . 17)) (5 (integer . -29)) (4 (integer . -30)) (3 (integer . -21)) (2 (integer . -31)) (1 (natural . 30)) (0 0))

Running (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (0 0))) 
(polynomial x polynomial-sparse-terms (0 0))
Running (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x polynomial-sparse-terms (0 -5))
Running (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x polynomial-sparse-terms (0 -5))
Running (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
(polynomial x polynomial-sparse-terms (1 -7) (0 -5))
Running (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
(polynomial x polynomial-sparse-terms (1 -9) (0 25))
Running (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
(polynomial x polynomial-sparse-terms (5 -1) (4 -2) (2 -3) (1 2) (0 5))
Running (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (5 -2) (4 -4) (2 -5) (1 7) (0 15))
Running (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (10 -3) (7 -4) (5 11) (1 7) (0 15))
Running (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (12 -4) (6 -13) (5 12) (1 7) (0 15))

Running (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 0 0)) 
(polynomial x polynomial-dense-terms 0 0)
Running (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x polynomial-dense-terms 0 -5)
Running (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x polynomial-dense-terms 0 -5)
Running (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 1 3 5)) 
(polynomial x polynomial-dense-terms 1 -3 -5)
Running (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 1 4 9)) 
(polynomial x polynomial-dense-terms 1 -4 -9)
Running (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 2 3 5 23)) 
(polynomial x polynomial-dense-terms 2 -3 -5 -23)
Running (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 2 4 9 19)) 
(polynomial x polynomial-dense-terms 2 -4 -9 -19)
Running (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
(polynomial x polynomial-dense-terms 5 -1 -2 0 -3 2 5)
Running (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
(polynomial x polynomial-dense-terms 5 -2 -4 0 -5 7 15)
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 0))) 
(polynomial x polynomial-sparse-terms (0 0))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x polynomial-sparse-terms (0 -5))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x polynomial-sparse-terms)
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
(polynomial x polynomial-sparse-terms (1 -7))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
(polynomial x polynomial-sparse-terms (1 (integer . -2)) (0 (natural . 30)))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
(polynomial x polynomial-sparse-terms (5 -1) (4 -2) (2 -3) (1 (natural . 11)) (0 (integer . -20)))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -2)) (1 (natural . 5)) (0 (natural . 10)))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (10 -3) (7 -4) (5 (natural . 13)) (4 4) (2 5))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (12 -4) (10 3) (7 4) (6 -13) (5 (natural . 1)))

Running (#<procedure:sub> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 0)) 
(polynomial x polynomial-dense-terms 0 0)
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x polynomial-dense-terms 0 -5)
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x polynomial-dense-terms)
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 1 3 5)) 
(polynomial x polynomial-dense-terms 1 -3 0)
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-dense-terms 1 4 9)) 
(polynomial x polynomial-dense-terms 1 (integer . -1) (integer . -4))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-dense-terms 2 3 5 23)) 
(polynomial x polynomial-dense-terms 2 -3 (integer . -1) (integer . -14))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 4 9 19)) 
(polynomial x polynomial-dense-terms 2 (integer . -1) (integer . -4) (natural . 4))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
(polynomial x polynomial-dense-terms 5 -1 -2 0 (natural . 1) (natural . 11) (natural . 24))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
(polynomial x polynomial-dense-terms 5 (integer . -1) (integer . -2) 0 (integer . -2) (natural . 5) (natural . 10))

Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-dense-terms 0 5)) 
(polynomial x polynomial-dense-terms 0 -5)
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (0 0))) 
(polynomial x polynomial-dense-terms 0 5)
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 1 3 5)) 
(polynomial x polynomial-dense-terms 1 -3 0)
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-dense-terms 1 4 9)) 
(polynomial x polynomial-dense-terms 1 (natural . 3) (integer . -4))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-dense-terms 2 3 5 23)) 
(polynomial x polynomial-dense-terms 2 -3 (natural . 4) (integer . -48))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-dense-terms 2 4 9 19)) 
(polynomial x polynomial-dense-terms 5 1 2 0 (integer . -1) (integer . -11) (integer . -24))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
(polynomial x polynomial-dense-terms 5 (natural . 1) (natural . 2) 0 (natural . 2) (integer . -5) (integer . -10))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
(polynomial x polynomial-dense-terms 10 3 0 0 4 0 (integer . -13) -4 0 -5 0 0)

Running (#<procedure:sub> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-sparse-terms (0 5))) 
(polynomial x polynomial-sparse-terms (0 -5))
Running (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 0 0)) 
(polynomial x polynomial-sparse-terms (0 5))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
(polynomial x polynomial-sparse-terms (1 -7))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
(polynomial x polynomial-sparse-terms (1 (integer . -6)) (0 (natural . 30)))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
(polynomial x polynomial-sparse-terms (5 -1) (4 -2) (2 -3) (1 (natural . 6)) (0 (natural . 14)))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (5 -2) (4 -4) (2 (integer . -2)) (1 (natural . 12)) (0 (natural . 38)))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (10 -3) (7 -4) (5 11) (2 4) (1 (natural . 16)) (0 (natural . 34)))
Running (#<procedure:sub> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
(polynomial x polynomial-sparse-terms (12 -4) (6 -13) (5 (natural . 13)) (4 2) (2 3) (1 (natural . 5)) (0 (natural . 10)))

Running (#<procedure:=zero?> (polynomial x polynomial-dense-terms)) 
#t
Running (#<procedure:=zero?> (polynomial x polynomial-sparse-terms)) 
#t
> 
