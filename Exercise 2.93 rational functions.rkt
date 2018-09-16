#lang racket

; Exercise 2.92.  By imposing an ordering on variables, extend the polynomial package so that addition and
; multiplication of polynomials works for polynomials in different variables. (This is not easy!)

; S O L U T I O N

; Assumed ordering of variables is (in increasing order of priority):
; p, q, r, s, t, u, v, w, x, y, z
; I have implemented a generic print procedure so that polynomials and other types can be printed
; in an easy to read manner.
; The main work in this exercise lies in the 'convert-polynomial' procedure that transforms a polynomial
; from one variable to another

; GENERIC PROCEDURES

; Generic Polynomial procedures
; Note: I have designed this with the assumption that the procedures adjoin-term, first-term and rest-terms
; though generic, will still be used only internally by the polynomial procedures.
; These three procedures are generic but not exposed to the outside world.
(define (variable p) (apply-generic 'variable p))
(define (term-list p) (apply-generic 'term-list p))
(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (order term) (apply-generic 'order term))
(define (coeff term) (apply-generic 'coeff term))
(define (negate-term term) (apply-generic 'negate-term term))

; Generic Print procedure
(define (print x) (apply-generic 'print x))

; Generic Logical procedures
(define (ABS x) (apply-generic 'abs x))
(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equal? x y))
(define (greater? x y) (apply-generic 'greater? x y))
(define (lesser? x y) (apply-generic 'lesser? x y))
(define (empty-term? t) (apply-generic 'empty-term? t))

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

(define (variable-poly p) (car p))
(define (term-list-poly p) (cdr p))

; Returns the polynomial constructed with the first-term of the supplied polynomial
(define (first-term-part-poly p)
	(make-polynomial (variable p) (make-polynomial-sparse-terms (list (contents (first-term (term-list p))))))
)

; Returns the polynomial constructed with the rest-terms of the supplied polynomial
(define (rest-terms-part-poly p)
	(let ((rt (rest-terms (term-list p))))
		(if (empty-termlist? rt)
			(make-polynomial (variable p) (the-empty-poly-termlist))
			(make-polynomial (variable p) (rest-terms (term-list p)))
		)
	)
)

(define (print-poly p)

	(define (print-poly-internal var terms first-time?)
		(cond
			((empty-termlist? terms) (display "}"))
			(else
				(let ((coeff-first-term (coeff (first-term terms))) (order-first-term (order (first-term terms))))
					; Print + or -
					(cond
						((=zero? coeff-first-term) (display ""))
						((and (pair? coeff-first-term) (lesser? coeff-first-term 0)) (display " ") (display '-) (display " "))
						((and (pair? coeff-first-term) (not first-time?)) (display " ") (display '+) (display " "))
						((and (not (pair? coeff-first-term)) (> coeff-first-term 0) (not first-time?)) (display " ") (display '+) (display " "))
						((and (not (pair? coeff-first-term)) (< coeff-first-term 0)) (display " ") (display '-) (display " "))
					)
					; Print the coefficient
					(cond
						((=zero? coeff-first-term) (display ""))
						((and (pair? coeff-first-term) (not (or (equ? coeff-first-term 1) (equ? coeff-first-term -1))))
							; Expecting this to be a polynomial
							(print (ABS coeff-first-term))
						)
						((not (equ? coeff-first-term 1))
							(display (ABS coeff-first-term))
						)
					)
					; Print the variable and order
					(cond
						((=zero? coeff-first-term) (display ""))
						((equal? order-first-term 1)
							(display var)
						)
						((not (=zero? order-first-term))
							(display var)
							(display '^)
							(print order-first-term)
						)
					)
					(if (and (=zero? coeff-first-term) first-time?)
						(print-poly-internal var (rest-terms terms) true)
						(print-poly-internal var (rest-terms terms) false)
					)
				)
			)
		)
	)

	; (display "Printing: ")
	; (display p)
	; (newline)
	(display "{")
	(print-poly-internal (variable-poly p) (term-list-poly p) true)
)

(define (is-poly? p)
	(if (pair? p)
		(equal? (type-tag p) 'polynomial)
		false
	)
)

(define (the-empty-poly-termlist) '())
; (define (the-empty-poly-termlist) (list 'polynomial-sparse-terms (list 0 0)))

(define (convert-polynomial p new-var)
	; Converts the polynomial by expanding and rearranging the terms so that 'new-var' becomes the variable
	; of the polynomial
	; Assumptions made in this implementation:
	; 1. The supplied object p should be tagged with the tag 'polynomial
	; 2. We expect that a polynomial in a variable say z, will not have any coefficient that itself contains z
	(cond
		; If the polynomial is empty, then return a new empty polynomial in the new-variable
		((empty-termlist? (term-list p)) (make-polynomial new-var (the-empty-poly-termlist)))
		; If the polynomial is empty, then return zero
		; ((empty-termlist? (term-list p)) 0)
		; if new-var is not really new, return the polynomial as it is
		((equal? (variable p) new-var) p)
		; If the polynomial has only one term
		((empty-termlist? (rest-terms (term-list p)))
			(let ((the-term (first-term (term-list p))))
				(cond
					; Coefficient is a non-empty polynomial
					((and (is-poly? (coeff the-term)) (not (empty-termlist? (term-list (coeff the-term)))))
	 					; coeff in the term is a polynomial by itself
						; First, convert the coefficient to be in the target variable.
						; Then do something like this (In this example, the converted coefficient is Cx^2):
						; Cx^2y4 needs to become Cy^4x^2
						; i.e.
						; ('polynomial y (4 (polynomial x (2 C)))) needs to become
						; ('polynomial x (2 (polynomial y (4 C))))
						(let ((converted-inner-poly (convert-polynomial (coeff the-term) new-var)))
							(let (
								  ; (cip-first-term-poly (first-term-part-poly converted-inner-poly))
								  (cip-rest-terms-poly (rest-terms-part-poly converted-inner-poly))
								  (cip-first-term-order (order (first-term (term-list converted-inner-poly))))
								  (cip-first-term-coeff (coeff (first-term (term-list converted-inner-poly))))
								 )
								(add
									(make-polynomial
										new-var
										(make-polynomial-sparse-terms
											(list
												(list
													; ORDER
													cip-first-term-order
													; COEFFICIENT
													(make-polynomial
														(variable p)
														(make-polynomial-sparse-terms
															(list
																(list
																	(order the-term)
																	cip-first-term-coeff
																)
															)
														)
													)
												)
											)
										)
									)
									; LOGIC PROBLEM HERE
									(convert-polynomial
										; cip-rest-terms-poly
										(make-polynomial
											(variable p)
											(make-polynomial-sparse-terms
												(list
													(list
														; ORDER
														(order the-term)
														; COEFFICIENT
														cip-rest-terms-poly
													)
												)
											)
										)
										new-var
									)
								)
							)
						)
	 					; coeff in the term is not a polynomial
	 					; Example: 7y^4 becomes (7y^4)x^0
	 				)
					; Coefficient is an empty polynomial, so return an empty polynomial in the new variable
					((and (is-poly? (coeff the-term)) (empty-termlist? (term-list (coeff the-term))))
						(make-polynomial new-var (the-empty-poly-termlist))
					)
					(else
	 					(make-polynomial new-var (make-polynomial-sparse-terms (list (list 0 p))))
					)
				)
			)
		)
		; If the polynomial has more than one term
		; This is the high level driving logic
		((not (equal? (variable p) new-var))
			(add
				(convert-polynomial
					(make-polynomial (variable p) (make-polynomial-sparse-terms (list (contents (first-term (term-list p))))))
					new-var
				)
				(convert-polynomial
					(make-polynomial (variable p) (rest-terms (term-list p)))
					new-var
				)
			)
		)
		(else
			(error "Unable to convert " (list p new-var))
		)
	)
)

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

(define (higher-in-hierarchy? v1 v2)
	; returns true if v1 is higher than v2, otherwise false
	; Items in the below list are arranged from low to high
	(define var-hierarchy (list 'p 'q 'r 's 't 'u 'v 'w 'z 'y 'x))

	(define (find-position item items current-position)
		(cond
			((pair? items)
				(if (equal? item (car items))
					current-position
					(find-position item (cdr items) (+ current-position 1))
				)
			)
			(else
				-1
			)
		)
	)

	(let ((v1-pos (find-position v1 var-hierarchy 1)) (v2-pos (find-position v2 var-hierarchy 1)))
		(cond
			((< v1-pos v2-pos) false)
			((> v1-pos v2-pos) true)
			(else
				; default treatment
				false
			)
		)
	)
)

; Polynomial Operations
(define (add-poly p1 p2)
	(if (same-variable? (variable-poly p1) (variable-poly p2))
		(make-poly
			(variable-poly p1)
			(add-terms (term-list-poly p1) (term-list-poly p2))
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
	(if (same-variable? (variable-poly p1) (variable-poly p2))
		(make-poly
			(variable-poly p2)
			(mul-terms (term-list-poly p1) (term-list-poly p2))
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
					((greater? (order t1) (order t2))
						(adjoin-term t1 (add-terms (rest-terms L1) L2))
					)
					((lesser? (order t1) (order t2))
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
		; Returning an empty list with an explicit tag so that the generic procedure
		; 'adjoin-term' below does not fail. Generic procedures expect a data tag for each of their arguments
		(list 'polynomial-sparse-terms (list 0 0))
		(let ((t2 (first-term L)))
			(adjoin-term
				(make-term (add (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
				(mul-term-by-all-terms t1 (rest-terms L))
			)
		)
	)
)

(define (div-poly p-dividend p-divisor)
	(if (same-variable? (variable-poly p-dividend) (variable-poly p-divisor))
		(let ((result (div-terms (term-list-poly p-dividend) (term-list-poly p-divisor))))
			(list
				(make-poly (variable-poly p-dividend) (car result))
				(make-poly (variable-poly p-dividend) (cadr result))
			)
		)
		(error "Polys not in same var -- DIV-POLY"
			(list p-dividend p-divisor)
		)
	)
)

(define (div-terms L-dividend L-divisor)
	(if (empty-termlist? L-dividend)
		(list (the-empty-poly-termlist) (the-empty-poly-termlist))
		(let ((ft-dividend (first-term L-dividend)) (ft-divisor (first-term L-divisor)))
			(if (greater? (order ft-divisor) (order ft-dividend))
				(list (the-empty-poly-termlist) L-dividend)
				(let ((new-c (div (coeff ft-dividend) (coeff ft-divisor)))
					  (new-o (sub (order ft-dividend) (order ft-divisor))))
					(let ((rest-of-result
							(div-terms
								(term-list-poly
									(sub-poly
										(make-poly 'x L-dividend)
										(mul-poly
											(make-poly 'x L-divisor)
											(make-poly 'x (make-polynomial-sparse-terms (list (list new-o new-c))))
										)
									)
								)
								L-divisor
							)
						 ))
						; <form complete result>
						(list
							(add-terms
								(make-polynomial-sparse-terms (list (list new-o new-c)))
								(car rest-of-result)
							)
							(cadr rest-of-result)
						)
					)
				)
			)
		)
	)
)

(define (=zero-polynomial? p)
	; A polynomial is zero if its term-list is empty
	(empty-termlist? (term-list-poly p))
)

(define (lesser-polynomial-real? p r)
	; returns true if the polynomial is just a constant which is less than r
	(and (=zero? (order (first-term (term-list-poly p)))) (lesser? (coeff (first-term (term-list-poly p))) r))
)

(define (negate-poly p)
	(make-poly
		(variable-poly p)
		(negate-term-list (term-list-poly p))
	)
)

(define (negate-term-list t)
	(if (empty-termlist? t)
		t
		(adjoin-term (negate-term (first-term t)) (negate-term-list (rest-terms t)))
	)
)

(define (negate-poly-term t)
	(list (order-poly-term t) (mul -1 (coeff-poly-term t)))
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

(define (equal-polynomial-real? p r)
	; returns true if the polynomial is just a constant which is less than r
	(and (=zero? (order (first-term (term-list-poly p)))) (equ? (coeff (first-term (term-list-poly p))) r))
)

(define (empty-poly-term? t)
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

(define (print-complex c)
	(if (=zero? (IMAG-PART c))
		(print (REAL-PART c))
		(begin
			(print (REAL-PART c))
			(display '+)
			(print (IMAG-PART c))
			(display 'i)
		)
	)
)

; (define (project-complex c) (make-real (REAL-PART c)))
(define (project-complex c) (REAL-PART c))

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

(define (raise-rectangular r)
	(make-complex-from-real-imag (real-part-rectangular r) (imag-part-rectangular r))
)

; Polar (Complex) Number procedures
(define (make-from-real-imag-polar x y) (cons (square-root (add (square x) (square y))) (tan-inverse y x)))
(define (make-from-mag-ang-polar r a) (cons r a))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (real-part-polar z) (mul (magnitude-polar z) (cosine (angle-polar z))))
(define (imag-part-polar z) (mul (magnitude-polar z) (sine (angle-polar z))))

(define (raise-polar p)
	(make-complex-from-mag-ang (magnitude-polar p) (angle-polar p))
)

; REAL NUMBER PROCEDURES
(define (make-real-specific r)
	(if (real? r)
		r
		(error "Cannot make real with: " r)
	)
)

(define (abs-real r)
	(abs r)
)

(define (=zero-real? r)
	(= 0 r)
)

(define (print-real r)
	(display r)
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

(define (greater-rational? x y)
	(> (* (numer x) (denom y)) (* (denom x) (numer y)))
)

(define (lesser-rational? x y)
	(< (* (numer x) (denom y)) (* (denom x) (numer y)))
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
(define (abs-integer i)
	(abs i)
)

(define (make-integer-specific n)
	(if (integer? n)
		n
		(error "Cannot make integer with: " n)
	)
)

(define (print-int i)
	(display i)
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

(define (abs-natural n)
	(abs n)
)

(define (print-natural n)
	(display n)
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

(define (project-natural n)
	; Natural numbers are the lowest in the tower, we will simply return n itself (in effect getting rid of
	; the type-tag)
	n
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
		((boolean? datum) 'boolean)
		(else
			(error "Bad tagged datum -- TYPE-TAG" datum)
		)
	)
)

(define (contents datum)
	(cond
		((pair? datum) (cdr datum))
		((number? datum) datum)
		((boolean? datum) datum)
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
					(cons '(rectangular) raise-rectangular)
					(cons '(polar) raise-polar)
				)
			)
			(cons
				'project
				(list
					(cons '(complex) project-complex)
					(cons '(real) project-real)
					(cons '(rational) project-rational)
					(cons '(integer) project-int)
					(cons '(natural) project-natural)
				)
			)
			(cons
				'add
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (+ x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (add-rational x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (+ x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (add-complex z1 z2))))
					(cons
						'(polynomial polynomial)
						(lambda (p1 p2)
							(attach-tag 'polynomial
								(if (same-variable? (variable-poly p1) (variable-poly p2))
									(add-poly p1 p2)
									; Note: The procedure convert-polynomial expects the type-tag in the object
									; we supply to it
									(let ((pg1 (make-polynomial (variable-poly p1) (term-list-poly p1)))
										  (pg2 (make-polynomial (variable-poly p2) (term-list-poly p2))))
										(if (higher-in-hierarchy? (variable-poly p1) (variable-poly p2))
											(let ((cpg2 (convert-polynomial pg2 (variable-poly p1))))
												(add-poly p1 (contents cpg2))
											)
											(let ((cpg1 (convert-polynomial pg1 (variable-poly p2))))
												(add-poly p2 (contents cpg1))
											)
										)
									)
								)
							)
						)
					)
					(cons
						'(polynomial complex)
						(lambda (p c)
							(attach-tag
								'polynomial
								; 'Raise' the complex object to a polynomial and then do ordinary addition
								(let ((pg (make-polynomial (variable-poly p) (make-polynomial-sparse-terms (list (list 0 c))))))
									(add-poly p (contents pg))
								)
							)
						)
					)
					(cons
						'(complex polynomial)
						(lambda (c p)
							(attach-tag
								'polynomial
								; 'Raise' the complex object to a polynomial and then do ordinary addition
								(let ((pg (make-polynomial (variable-poly p) (make-polynomial-sparse-terms (list (list 0 c))))))
									(add-poly p (contents pg))
								)
							)
						)
					)
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
					(cons
						'(polynomial polynomial)
						(lambda (p1 p2)
							(let ((pg1 (make-polynomial (variable-poly p1) (term-list-poly p1)))
							  	  (pg2 (make-polynomial (variable-poly p2) (term-list-poly p2))))
								  (add pg1 (NEGATE pg2))
							)
						)
						; (lambda (p1 p2)
; 							; (attach-tag 'polynomial (sub-poly p1 p2))
; 							(attach-tag 'polynomial
; 								(if (same-variable? (variable-poly p1) (variable-poly p2))
; 									(sub-poly p1 p2)
; 									; Note: The procedure convert-polynomial expects the type-tag in the object
; 									; we supply to it
; 									(let ((pg1 (make-polynomial (variable-poly p1) (term-list-poly p1)))
; 										  (pg2 (make-polynomial (variable-poly p2) (term-list-poly p2))))
; 										(if (higher-in-hierarchy? (variable-poly p1) (variable-poly p2))
; 											(let ((cpg2 (convert-polynomial pg2 (variable-poly p1))))
; 												(sub-poly p1 (contents cpg2))
; 											)
; 											(let ((cpg1 (convert-polynomial pg1 (variable-poly p2))))
; 												(sub-poly (contents cpg1) p2)
; 											)
; 										)
; 									)
; 								)
;							)
;						)
					)
				)
			)
			(cons
				'mul
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (* x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (mul-rational x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (* x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (mul-complex z1 z2))))
					(cons
						'(polynomial polynomial)
						; (lambda (p1 p2) (attach-tag 'polynomial (mul-poly p1 p2)))
						(lambda (p1 p2)
							(attach-tag 'polynomial
								(if (same-variable? (variable-poly p1) (variable-poly p2))
									(mul-poly p1 p2)
									; Note: The procedure convert-polynomial expects the type-tag in the object
									; we supply to it
									(let ((pg1 (make-polynomial (variable-poly p1) (term-list-poly p1)))
										  (pg2 (make-polynomial (variable-poly p2) (term-list-poly p2))))
										(if (higher-in-hierarchy? (variable-poly p1) (variable-poly p2))
											(let ((cpg2 (convert-polynomial pg2 (variable-poly p1))))
												(mul-poly p1 (contents cpg2))
											)
											(let ((cpg1 (convert-polynomial pg1 (variable-poly p2))))
												(mul-poly p2 (contents cpg1))
											)
										)
									)
								)
							)
						)
					)
					(cons
						'(polynomial real)
						(lambda (p r)
							(attach-tag
								'polynomial
								; 'Raise' the real object to a polynomial and then do ordinary multiplication
								(let ((pg (make-polynomial (variable-poly p) (make-polynomial-sparse-terms (list (list 0 r))))))
									(mul-poly p (contents pg))
								)
							)
						)
					)
					(cons
						'(real polynomial)
						(lambda (r p)
							(attach-tag
								'polynomial
								; 'Raise' the real object to a polynomial and then do ordinary multiplication
								(let ((pg (make-polynomial (variable-poly p) (make-polynomial-sparse-terms (list (list 0 r))))))
									(mul-poly p (contents pg))
								)
							)
						)
					)
					(cons
						'(polynomial complex)
						(lambda (p c)
							(attach-tag
								'polynomial
								; 'Raise' the complex object to a polynomial and then do ordinary multiplication
								(let ((pg (make-polynomial (variable-poly p) (make-polynomial-sparse-terms (list (list 0 c))))))
									(mul-poly p (contents pg))
								)
							)
						)
					)
					(cons
						'(complex polynomial)
						(lambda (c p)
							(attach-tag
								'polynomial
								; 'Raise' the complex object to a polynomial and then do ordinary multiplication
								(let ((pg (make-polynomial (variable-poly p) (make-polynomial-sparse-terms (list (list 0 c))))))
									(mul-poly p (contents pg))
								)
							)
						)
					)
				)
			)
			(cons
				'div
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (/ x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (div-rational x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (/ x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (div-complex z1 z2))))
					(cons '(polynomial polynomial) (lambda (p1 p2) (attach-tag 'polynomial-list (div-poly p1 p2))))
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
					(cons '(polynomial real) equal-polynomial-real?)
				)
			)
			(cons
				'greater?
				(list
					(cons '(scheme-number scheme-number) >)
					(cons '(natural natural) >)
					(cons '(integer integer) >)
					(cons '(rational rational) greater-rational?)
					(cons '(real real) >)
				)
			)
			(cons
				'lesser?
				(list
					(cons '(scheme-number scheme-number) <)
					(cons '(natural natural) <)
					(cons '(integer integer) <)
					(cons '(rational rational) lesser-rational?)
					(cons '(real real) <)
					(cons '(polynomial real) lesser-polynomial-real?)
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
				'empty-term?
				(list
					(cons '(polynomial-term) empty-poly-term?)
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
			(cons
				'variable
				(list
					(cons '(polynomial) variable-poly)
				)
			)
			(cons
				'term-list
				(list
					(cons '(polynomial) term-list-poly)
				)
			)
			(cons
				'print
				(list
					(cons '(polynomial) print-poly)
					(cons '(complex) print-complex)
					(cons '(real) print-real)
					(cons '(integer) print-int)
					(cons '(natural) print-natural)
					(cons '(boolean) (lambda (x) (if x (display 'True) (display 'False))))
				)
			)
			(cons
				'abs
				(list
					(cons '(polynomial) (lambda (x) (attach-tag 'polynomial x)))
					(cons '(complex) (lambda (x) (attach-tag 'complex x)))
					(cons '(real) (lambda (x) (attach-tag 'real (abs-real x))))
					(cons '(integer) (lambda (x) (attach-tag 'integer (abs-integer x))))
					(cons '(natural) (lambda (x) (attach-tag 'natural (abs-natural x))))
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

	(define (print-item-list items first-time?)
		(cond
			((not (pair? items)) (void))
			(else
				(if (not first-time?)
					(display ", ")
					(void)
				)
				(print (car items))
				(print-item-list (cdr items) false)
			)
		)
	)

	(display "Running Test: ") (display (cons proc args)) (display " ")
	(newline)
	(display "Applying ")
	(display proc)
	(display " on: ")
	(print-item-list args true)
	(newline)
	(let ((result (apply proc args)))
		(display "Result: ")
		(display result)
		(newline)
		(print result)
		(newline)
	)
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
(define t9-sparse (make-polynomial-sparse-terms (list (list 12 -4) (list 6 13) (list 5 -12) (list 1 -7) (list 0 -15))))
(define t10-sparse (make-polynomial-sparse-terms (list (list 12 0) (list 6 -13) (list 5 -12) (list 1 -7) (list 0 -15))))

(define t1-dense (make-polynomial-dense-terms (list 0 0)))
(define t2-dense (make-polynomial-dense-terms (list 0 5)))
(define t3-dense (make-polynomial-dense-terms (list 1 3 5)))
(define t4-dense (make-polynomial-dense-terms (list 1 4 9)))
(define t5-dense (make-polynomial-dense-terms (list 2 3 5 23)))
(define t6-dense (make-polynomial-dense-terms (list 2 4 9 19)))
(define t7-dense (make-polynomial-dense-terms (list 5 1 2 0 3 -2 -5)))
(define t8-dense (make-polynomial-dense-terms (list 5 2 4 0 5 -7 -15)))
(define t9-dense (make-polynomial-dense-terms (list 5 -2 4 0 5 -7 -15)))
(define t10-dense (make-polynomial-dense-terms (list 5 0 -4 0 5 -7 -15)))

(define sp1 (make-polynomial 'p t1-sparse))
(define sp2 (make-polynomial 'q t2-sparse))
(define sp3 (make-polynomial 'r t3-sparse))
(define sp4 (make-polynomial 's t4-sparse))
(define sp5 (make-polynomial 't t5-sparse))
(define sp6 (make-polynomial 'u t6-sparse))
(define sp7 (make-polynomial 'v t7-sparse))
(define sp8 (make-polynomial 'z t8-sparse))
(define sp9 (make-polynomial 'y t9-sparse))
(define sp10 (make-polynomial 'x t10-sparse))

(define dp1 (make-polynomial 'q t1-dense))
(define dp2 (make-polynomial 'r t2-dense))
(define dp3 (make-polynomial 's t3-dense))
(define dp4 (make-polynomial 't t4-dense))
(define dp5 (make-polynomial 'u t5-dense))
(define dp6 (make-polynomial 'v t6-dense))
(define dp7 (make-polynomial 'w t7-dense))
(define dp8 (make-polynomial 'z t8-dense))
(define dp9 (make-polynomial 'y t9-dense))
(define dp10 (make-polynomial 'x t10-dense))

(define p-1-divisor-terms (make-polynomial-sparse-terms (list (list 1 1))))
(define p-1-dividend-terms (make-polynomial-sparse-terms (list (list 1 1))))

(define p-1-divisor (make-polynomial 'y p-1-divisor-terms))
(define p-1-dividend (make-polynomial 'y p-1-dividend-terms))

(define p0-divisor-terms (make-polynomial-sparse-terms (list (list 4 1))))
(define p0-dividend-terms (make-polynomial-sparse-terms (list (list 6 1))))

(define p0-divisor (make-polynomial 'w p0-divisor-terms))
(define p0-dividend (make-polynomial 'w p0-dividend-terms))

(define p1-divisor-terms (make-polynomial-sparse-terms (list (list 4 1) (list 0 -1))))
(define p1-dividend-terms (make-polynomial-sparse-terms (list (list 5 1) (list 0 -1))))

(define p1-divisor (make-polynomial 'p p1-divisor-terms))
(define p1-dividend (make-polynomial 'p p1-dividend-terms))

(define p2-divisor-terms (make-polynomial-sparse-terms (list (list 2 1) (list 0 -1))))
(define p2-dividend-terms (make-polynomial-sparse-terms (list (list 5 1) (list 0 -1))))

(define p2-divisor (make-polynomial 'x p2-divisor-terms))
(define p2-dividend (make-polynomial 'x p2-dividend-terms))

(define p3-divisor-terms (make-polynomial-sparse-terms (list (list 2 2) (list 1 3) (list 0 4))))
(define p3-dividend-terms (make-polynomial-sparse-terms (list (list 5 6) (list 4 9) (list 3 8) (list 2 -15) (list 1 -16) (list 0 5))))

(define p3-divisor (make-polynomial 'x p3-divisor-terms))
(define p3-dividend (make-polynomial 'x p3-dividend-terms))

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

(newline)

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
(run-test mul sp4 sp4)
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

(newline)

(div p-1-dividend p-1-divisor)
(div p0-dividend p0-divisor)
(div p1-dividend p1-divisor)
(div p2-dividend p2-divisor)
(div p3-dividend p3-divisor)

(newline)

(define t11-sparse (make-polynomial-sparse-terms (list (list 1 3) (list 0 -2))))
(define t11-dense (make-polynomial-dense-terms (list 1 7 3)))
(define P1x (make-polynomial 'x t11-sparse))
(define P2x (make-polynomial 'x t11-dense))

(define t12-sparse (make-polynomial-sparse-terms (list (list 1 5) (list 0 -4))))
(define t12-dense (make-polynomial-dense-terms (list 1 9 -6)))
(define P3x (make-polynomial 'x t12-sparse))
(define P4x (make-polynomial 'x t12-dense))

(define t13-sparse (make-polynomial-sparse-terms (list (list 1 P1x) (list 0 P2x))))
(define t13-dense (make-polynomial-dense-terms (list 1 P3x P4x)))

(define P1y (make-polynomial 'y t13-sparse))
(define P2y (make-polynomial 'y t13-dense))

(define t14-sparse (make-polynomial-sparse-terms (list (list 1 P1y) (list 0 P2y))))
(define P1z (make-polynomial 'z t14-sparse))

; P(y) = y in terms of x
(define P3y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 1 1)))))
(display "P3y: ")
P3y
(display "P3y: ")
(print P3y)
(newline)
(display "P3y in terms of x: ")
(convert-polynomial P3y 'x)
(display "P3y in terms of x: ")
(print (convert-polynomial P3y 'x))
(newline)

(newline)

; P(y) = xy in terms of x should become yx
(define X (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 1)))))
(define P4y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 1 X)))))
(display "X: ")
X
(display "X: ")
(print X)
(newline)
(display "P4y: ")
P4y
(display "P4y: ")
(print P4y)
(newline)
(display "P4y in terms of x: ")
(convert-polynomial P4y 'x)
(display "P4y in terms of x: ")
(print (convert-polynomial P4y 'x))
(newline)

(newline)

; P(y) = x^2y in terms of x should become yx^2
(define Xsq (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 1)))))
(define P5y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 1 Xsq)))))
(display "Xsq: ")
Xsq
(display "Xsq: ")
(print Xsq)
(newline)
(display "P5y: ")
P5y
(display "P5y: ")
(print P5y)
(newline)
(display "P5y in terms of x: ")
(convert-polynomial P5y 'x)
(display "P5y in terms of x: ")
(print (convert-polynomial P5y 'x))
(newline)

(newline)

; P(y) = 5x^2y in terms of x should become 5yx^2
(define 5Xsq (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 5)))))
(define P6y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 1 5Xsq)))))
(display "5Xsq: ")
5Xsq
(display "5Xsq: ")
(print 5Xsq)
(newline)
(display "P6y: ")
P6y
(display "P6y: ")
(print P6y)
(newline)
(display "P6y in terms of x: ")
(convert-polynomial P6y 'x)
(display "P6y in terms of x: ")
(print (convert-polynomial P6y 'x))
(newline)

(newline)

; P(y) = 5x^3y^7 in terms of x should become 5y^7x^3
(define 5Xcb (make-polynomial 'x (make-polynomial-sparse-terms (list (list 3 5)))))
(define P7y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 7 5Xcb)))))
(display "5Xcb: ")
5Xcb
(display "5Xcb: ")
(print 5Xcb)
(newline)
(display "P7y: ")
P7y
(display "P7y: ")
(print P7y)
(newline)
(display "P7y in terms of x: ")
(convert-polynomial P7y 'x)
(display "P7y in terms of x: ")
(print (convert-polynomial P7y 'x))
(newline)

(newline)

; Two terms
(define P8y (add P6y P7y))
(display "P8y: ")
P8y
(display "P8y: ")
(print P8y)
(newline)
(display "P8y in terms of x: ")
(convert-polynomial P8y 'x)
(display "P8y in terms of x: ")
(print (convert-polynomial P8y 'x))
(newline)

(newline)

; Two terms with same order of x but different orders of y
(define P9y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 4 Xsq)))))
(display "P9y: ")
(print P9y)
(newline)
(display "P6y: ")
(print P6y)
(newline)
(define P10y (add P9y P6y))
(display "P10y = P9y + P6y: ")
P10y
(display "P10y = P9y + P6y: ")
(print P10y)
(newline)
(display "P10y in terms of x: ")
(convert-polynomial P10y 'x)
(display "P10y in terms of x: ")
(print (convert-polynomial P10y 'x))
(newline)

(newline)

; Two terms with same order of x and same orders of y
(define P61y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 4 5Xsq)))))
(display "P9y: ")
(print P9y)
(newline)
(display "P61y: ")
(print P61y)
(newline)
(define P11y (add P9y P61y))
(display "P11y = P9y + P61y: ")
P11y
(display "P11y = P9y + P61y: ")
(print P11y)
(newline)
(display "P11y in terms of x: ")
(convert-polynomial P11y 'x)
(display "P11y in terms of x: ")
(print (convert-polynomial P11y 'x))
(newline)

(newline)
(display "P1x: ")
P1x
(display "P1x: ")
(print P1x)
(newline)
(display "P1x in terms of z: ")
(convert-polynomial P1x 'z)
(display "P1x in terms of z: ")
(print (convert-polynomial P1x 'z))
(newline)

(newline)
(display "P1y: ")
P1y
(display "P1y: ")
(print P1y)
(newline)
(display "P1y in terms of x: ")
(convert-polynomial P1y 'x)
(display "P1y in terms of x: ")
(print (convert-polynomial P1y 'x))
(newline)

(newline)
(display "P1z: ")
P1z
(display "P1z: ")
(print P1z)
(newline)
(display "P1z in terms of y: ")
(convert-polynomial P1z 'y)
(display "P1z in terms of y: ")
(print (convert-polynomial P1z 'y))
(newline)

(newline)
(display "P1z in terms of x: ")
(convert-polynomial P1z 'x)
(display "P1z in terms of x: ")
(print (convert-polynomial P1z 'x))
(newline)

; Addition of two similar polynomials in different variables
(newline)
(define P5x (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 11)))))
(define P12y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 1 15)))))
(display "P5x: ")
P5x
(display "P5x: ")
(print P5x)
(newline)
(display "P12y: ")
P12y
(display "P12y: ")
(print P12y)
(newline)
(display "P5x + P12y: ")
(newline)
(run-test add P5x P12y)
(display "P5x + P12y: ")
(newline)
(print (add P5x P12y))
(newline)

; Addition of two similar polynomials in different variables
(newline)
(define P6x (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 11) (list 0 12)))))
(define P13y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 1 15) (list 0 17)))))
(display "P6x: ")
P6x
(display "P6x: ")
(print P6x)
(newline)
(display "P13y: ")
P13y
(display "P13y: ")
(print P13y)
(newline)
(display "P6x + P13y: ")
(newline)
(run-test add P6x P13y)
(display "P6x + P13y: ")
(newline)
(print (add P6x P13y))
(newline)

; Addition of two similar polynomials in different variables
(newline)
(define P7x (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 10) (list 1 11) (list 0 12)))))
(define P14y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 2 13) (list 1 15) (list 0 17)))))
(display "P7x: ")
P7x
(display "P7x: ")
(print P7x)
(newline)
(display "P14y: ")
P14y
(display "P14y: ")
(print P14y)
(newline)
(display "P7x + P14y: ")
(newline)
(run-test add P7x P14y)
(display "P7x + P14y: ")
(newline)
(print (add P7x P14y))
(newline)

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Running Test: (#<procedure:=zero?> (polynomial p polynomial-sparse-terms (0 0))) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

Running Test: (#<procedure:=zero?> (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:=zero?> on: {5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial r polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:=zero?> on: {7r + 5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial s polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:=zero?> on: {9s - 25}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial q polynomial-dense-terms 0 0)) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

Running Test: (#<procedure:=zero?> (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:=zero?> on: {5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial s polynomial-dense-terms 1 3 5)) 
Applying #<procedure:=zero?> on: {3s + 5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial t polynomial-dense-terms 1 4 9)) 
Applying #<procedure:=zero?> on: {4t + 9}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial u polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:=zero?> on: {3u^2 + 5u + 23}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial v polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:=zero?> on: {4v^2 + 9v + 19}
Result: #f
False


Running Test: (#<procedure:add> (polynomial p polynomial-sparse-terms (0 0)) (polynomial p polynomial-sparse-terms (0 0))) 
Applying #<procedure:add> on: {}, {}
Result: (polynomial p polynomial-sparse-terms (0 0))
{}

Running Test: (#<procedure:add> (polynomial p polynomial-sparse-terms (0 0)) (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:add> on: {}, {5}
Result: (polynomial q polynomial-sparse-terms (0 5))
{5}

Running Test: (#<procedure:add> (polynomial q polynomial-sparse-terms (0 5)) (polynomial r polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:add> on: {5}, {7r + 5}
Result: (polynomial r polynomial-sparse-terms (1 7) (0 (polynomial q polynomial-sparse-terms (0 10))))
{7r + {10}}

Running Test: (#<procedure:add> (polynomial r polynomial-sparse-terms (1 7) (0 5)) (polynomial s polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:add> on: {7r + 5}, {9s - 25}
Result: (polynomial s polynomial-sparse-terms (1 9) (0 (polynomial r polynomial-sparse-terms (1 7) (0 (integer . -20)))))
{9s + {7r - 20}}

Running Test: (#<procedure:add> (polynomial s polynomial-sparse-terms (1 9) (0 -25)) (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:add> on: {9s - 25}, {t^5 + 2t^4 + 3t^2 - 2t - 5}
Result: (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 (polynomial s polynomial-sparse-terms (1 9) (0 (integer . -30)))))
{t^5 + 2t^4 + 3t^2 - 2t + {9s - 30}}

Running Test: (#<procedure:add> (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {t^5 + 2t^4 + 3t^2 - 2t - 5}, {2u^5 + 4u^4 + 5u^2 - 7u - 15}
Result: (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 (integer . -20)))))
{2u^5 + 4u^4 + 5u^2 - 7u + {t^5 + 2t^4 + 3t^2 - 2t - 20}}

Running Test: (#<procedure:add> (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {2u^5 + 4u^4 + 5u^2 - 7u - 15}, {3v^10 + 4v^7 - 11v^5 - 7v - 15}
Result: (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 (integer . -30)))))
{3v^10 + 4v^7 - 11v^5 - 7v + {2u^5 + 4u^4 + 5u^2 - 7u - 30}}

Running Test: (#<procedure:add> (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial z polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {3v^10 + 4v^7 - 11v^5 - 7v - 15}, {4z^12 + 13z^6 - 12z^5 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 (integer . -30)))))
{4z^12 + 13z^6 - 12z^5 - 7z + {3v^10 + 4v^7 - 11v^5 - 7v - 30}}


Running Test: (#<procedure:add> (polynomial q polynomial-dense-terms 0 0) (polynomial q polynomial-dense-terms 0 0)) 
Applying #<procedure:add> on: {}, {}
Result: (polynomial q polynomial-dense-terms 0 0)
{}

Running Test: (#<procedure:add> (polynomial q polynomial-dense-terms 0 0) (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:add> on: {}, {5}
Result: (polynomial r polynomial-dense-terms 0 5)
{5}

Running Test: (#<procedure:add> (polynomial r polynomial-dense-terms 0 5) (polynomial s polynomial-dense-terms 1 3 5)) 
Applying #<procedure:add> on: {5}, {3s + 5}
Result: (polynomial s polynomial-sparse-terms (1 3) (0 (polynomial r polynomial-sparse-terms (0 10))))
{3s + {10}}

Running Test: (#<procedure:add> (polynomial s polynomial-dense-terms 1 3 5) (polynomial t polynomial-dense-terms 1 4 9)) 
Applying #<procedure:add> on: {3s + 5}, {4t + 9}
Result: (polynomial t polynomial-sparse-terms (1 4) (0 (polynomial s polynomial-sparse-terms (1 3) (0 14))))
{4t + {3s + 14}}

Running Test: (#<procedure:add> (polynomial t polynomial-dense-terms 1 4 9) (polynomial u polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:add> on: {4t + 9}, {3u^2 + 5u + 23}
Result: (polynomial u polynomial-sparse-terms (2 3) (1 5) (0 (polynomial t polynomial-sparse-terms (1 4) (0 32))))
{3u^2 + 5u + {4t + 32}}

Running Test: (#<procedure:add> (polynomial u polynomial-dense-terms 2 3 5 23) (polynomial v polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:add> on: {3u^2 + 5u + 23}, {4v^2 + 9v + 19}
Result: (polynomial v polynomial-sparse-terms (2 4) (1 9) (0 (polynomial u polynomial-sparse-terms (2 3) (1 5) (0 42))))
{4v^2 + 9v + {3u^2 + 5u + 42}}

Running Test: (#<procedure:add> (polynomial v polynomial-dense-terms 2 4 9 19) (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:add> on: {4v^2 + 9v + 19}, {w^5 + 2w^4 + 3w^2 - 2w - 5}
Result: (polynomial w polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 (polynomial v polynomial-sparse-terms (2 4) (1 9) (0 14))))
{w^5 + 2w^4 + 3w^2 - 2w + {4v^2 + 9v + 14}}

Running Test: (#<procedure:add> (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial z polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:add> on: {w^5 + 2w^4 + 3w^2 - 2w - 5}, {2z^5 + 4z^4 + 5z^2 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 (polynomial w polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 (integer . -20)))))
{2z^5 + 4z^4 + 5z^2 - 7z + {w^5 + 2w^4 + 3w^2 - 2w - 20}}


Running Test: (#<procedure:add> (polynomial p polynomial-sparse-terms (0 0)) (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:add> on: {}, {5}
Result: (polynomial r polynomial-dense-terms 0 5)
{5}

Running Test: (#<procedure:add> (polynomial r polynomial-dense-terms 0 5) (polynomial p polynomial-sparse-terms (0 0))) 
Applying #<procedure:add> on: {5}, {}
Result: (polynomial r polynomial-dense-terms 0 5)
{5}

Running Test: (#<procedure:add> (polynomial q polynomial-sparse-terms (0 5)) (polynomial s polynomial-dense-terms 1 3 5)) 
Applying #<procedure:add> on: {5}, {3s + 5}
Result: (polynomial s polynomial-sparse-terms (1 3) (0 (polynomial q polynomial-sparse-terms (0 10))))
{3s + {10}}

Running Test: (#<procedure:add> (polynomial r polynomial-sparse-terms (1 7) (0 5)) (polynomial t polynomial-dense-terms 1 4 9)) 
Applying #<procedure:add> on: {7r + 5}, {4t + 9}
Result: (polynomial t polynomial-sparse-terms (1 4) (0 (polynomial r polynomial-sparse-terms (1 7) (0 14))))
{4t + {7r + 14}}

Running Test: (#<procedure:add> (polynomial s polynomial-sparse-terms (1 9) (0 -25)) (polynomial u polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:add> on: {9s - 25}, {3u^2 + 5u + 23}
Result: (polynomial u polynomial-sparse-terms (2 3) (1 5) (0 (polynomial s polynomial-sparse-terms (1 9) (0 (integer . -2)))))
{3u^2 + 5u + {9s - 2}}

Running Test: (#<procedure:add> (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial v polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:add> on: {t^5 + 2t^4 + 3t^2 - 2t - 5}, {4v^2 + 9v + 19}
Result: (polynomial v polynomial-sparse-terms (2 4) (1 9) (0 (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 14))))
{4v^2 + 9v + {t^5 + 2t^4 + 3t^2 - 2t + 14}}

Running Test: (#<procedure:add> (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:add> on: {2u^5 + 4u^4 + 5u^2 - 7u - 15}, {w^5 + 2w^4 + 3w^2 - 2w - 5}
Result: (polynomial w polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 (integer . -20)))))
{w^5 + 2w^4 + 3w^2 - 2w + {2u^5 + 4u^4 + 5u^2 - 7u - 20}}

Running Test: (#<procedure:add> (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial z polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:add> on: {3v^10 + 4v^7 - 11v^5 - 7v - 15}, {2z^5 + 4z^4 + 5z^2 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 (integer . -30)))))
{2z^5 + 4z^4 + 5z^2 - 7z + {3v^10 + 4v^7 - 11v^5 - 7v - 30}}


Running Test: (#<procedure:add> (polynomial q polynomial-dense-terms 0 0) (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:add> on: {}, {5}
Result: (polynomial q polynomial-sparse-terms (0 5))
{5}

Running Test: (#<procedure:add> (polynomial q polynomial-sparse-terms (0 5)) (polynomial q polynomial-dense-terms 0 0)) 
Applying #<procedure:add> on: {5}, {}
Result: (polynomial q polynomial-sparse-terms (0 5))
{5}

Running Test: (#<procedure:add> (polynomial r polynomial-dense-terms 0 5) (polynomial r polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:add> on: {5}, {7r + 5}
Result: (polynomial r polynomial-sparse-terms (1 7) (0 10))
{7r + 10}

Running Test: (#<procedure:add> (polynomial s polynomial-dense-terms 1 3 5) (polynomial s polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:add> on: {3s + 5}, {9s - 25}
Result: (polynomial s polynomial-sparse-terms (1 12) (0 (integer . -20)))
{12s - 20}

Running Test: (#<procedure:add> (polynomial t polynomial-dense-terms 1 4 9) (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:add> on: {4t + 9}, {t^5 + 2t^4 + 3t^2 - 2t - 5}
Result: (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 2) (0 4))
{t^5 + 2t^4 + 3t^2 + 2t + 4}

Running Test: (#<procedure:add> (polynomial u polynomial-dense-terms 2 3 5 23) (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {3u^2 + 5u + 23}, {2u^5 + 4u^4 + 5u^2 - 7u - 15}
Result: (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 8) (1 (integer . -2)) (0 8))
{2u^5 + 4u^4 + 8u^2 - 2u + 8}

Running Test: (#<procedure:add> (polynomial v polynomial-dense-terms 2 4 9 19) (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {4v^2 + 9v + 19}, {3v^10 + 4v^7 - 11v^5 - 7v - 15}
Result: (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (2 4) (1 2) (0 4))
{3v^10 + 4v^7 - 11v^5 + 4v^2 + 2v + 4}

Running Test: (#<procedure:add> (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial z polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {w^5 + 2w^4 + 3w^2 - 2w - 5}, {4z^12 + 13z^6 - 12z^5 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 (polynomial w polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 (integer . -20)))))
{4z^12 + 13z^6 - 12z^5 - 7z + {w^5 + 2w^4 + 3w^2 - 2w - 20}}


Running Test: (#<procedure:mul> (polynomial p polynomial-sparse-terms (0 0)) (polynomial p polynomial-sparse-terms (0 0))) 
Applying #<procedure:mul> on: {}, {}
Result: (polynomial p)
{}

Running Test: (#<procedure:mul> (polynomial p polynomial-sparse-terms (0 0)) (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:mul> on: {}, {5}
Result: (polynomial q)
{}

Running Test: (#<procedure:mul> (polynomial q polynomial-sparse-terms (0 5)) (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:mul> on: {5}, {5}
Result: (polynomial q polynomial-sparse-terms (0 25) (0 0))
{25}

Running Test: (#<procedure:mul> (polynomial q polynomial-sparse-terms (0 5)) (polynomial r polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:mul> on: {5}, {7r + 5}
Result: (polynomial r polynomial-sparse-terms (1 (polynomial q polynomial-sparse-terms (0 35) (0 0))) (0 (polynomial q polynomial-sparse-terms (0 25) (0 0))) (0 0))
{{35}r + {25}}

Running Test: (#<procedure:mul> (polynomial r polynomial-sparse-terms (1 7) (0 5)) (polynomial s polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:mul> on: {7r + 5}, {9s - 25}
Result: (polynomial s polynomial-sparse-terms (1 (polynomial r polynomial-sparse-terms (1 63) (0 45) (0 0))) (0 (polynomial r polynomial-sparse-terms (1 (integer . -175)) (0 (integer . -125)) (0 0))) (0 0))
{{63r + 45}s + { - 175r - 125}}

Running Test: (#<procedure:mul> (polynomial s polynomial-sparse-terms (1 9) (0 -25)) (polynomial s polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:mul> on: {9s - 25}, {9s - 25}
Result: (polynomial s polynomial-sparse-terms (2 81) (1 (integer . -450)) (0 625) (0 0))
{81s^2 - 450s + 625}

Running Test: (#<procedure:mul> (polynomial s polynomial-sparse-terms (1 9) (0 -25)) (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:mul> on: {9s - 25}, {t^5 + 2t^4 + 3t^2 - 2t - 5}
Result: (polynomial t polynomial-sparse-terms (5 (polynomial s polynomial-sparse-terms (1 9) (0 (integer . -25)) (0 0))) (4 (polynomial s polynomial-sparse-terms (1 18) (0 (integer . -50)) (0 0))) (2 (polynomial s polynomial-sparse-terms (1 27) (0 (integer . -75)) (0 0))) (1 (polynomial s polynomial-sparse-terms (1 (integer . -18)) (0 50) (0 0))) (0 (polynomial s polynomial-sparse-terms (1 (integer . -45)) (0 125) (0 0))) (0 0))
{{9s - 25}t^5 + {18s - 50}t^4 + {27s - 75}t^2 + { - 18s + 50}t + { - 45s + 125}}

Running Test: (#<procedure:mul> (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {t^5 + 2t^4 + 3t^2 - 2t - 5}, {2u^5 + 4u^4 + 5u^2 - 7u - 15}
Result: (polynomial u polynomial-sparse-terms (5 (polynomial t polynomial-sparse-terms (5 2) (4 4) (2 6) (1 (integer . -4)) (0 (integer . -10)) (0 0))) (4 (polynomial t polynomial-sparse-terms (5 4) (4 8) (2 12) (1 (integer . -8)) (0 (integer . -20)) (0 0))) (2 (polynomial t polynomial-sparse-terms (5 5) (4 10) (2 15) (1 (integer . -10)) (0 (integer . -25)) (0 0))) (1 (polynomial t polynomial-sparse-terms (5 (integer . -7)) (4 (integer . -14)) (2 (integer . -21)) (1 14) (0 35) (0 0))) (0 (polynomial t polynomial-sparse-terms (5 (integer . -15)) (4 (integer . -30)) (2 (integer . -45)) (1 30) (0 75) (0 0))) (0 0))
{{2t^5 + 4t^4 + 6t^2 - 4t - 10}u^5 + {4t^5 + 8t^4 + 12t^2 - 8t - 20}u^4 + {5t^5 + 10t^4 + 15t^2 - 10t - 25}u^2 + { - 7t^5 - 14t^4 - 21t^2 + 14t + 35}u + { - 15t^5 - 30t^4 - 45t^2 + 30t + 75}}

Running Test: (#<procedure:mul> (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {2u^5 + 4u^4 + 5u^2 - 7u - 15}, {3v^10 + 4v^7 - 11v^5 - 7v - 15}
Result: (polynomial v polynomial-sparse-terms (10 (polynomial u polynomial-sparse-terms (5 6) (4 12) (2 15) (1 (integer . -21)) (0 (integer . -45)) (0 0))) (7 (polynomial u polynomial-sparse-terms (5 8) (4 16) (2 20) (1 (integer . -28)) (0 (integer . -60)) (0 0))) (5 (polynomial u polynomial-sparse-terms (5 (integer . -22)) (4 (integer . -44)) (2 (integer . -55)) (1 77) (0 165) (0 0))) (1 (polynomial u polynomial-sparse-terms (5 (integer . -14)) (4 (integer . -28)) (2 (integer . -35)) (1 49) (0 105) (0 0))) (0 (polynomial u polynomial-sparse-terms (5 (integer . -30)) (4 (integer . -60)) (2 (integer . -75)) (1 105) (0 225) (0 0))) (0 0))
{{6u^5 + 12u^4 + 15u^2 - 21u - 45}v^10 + {8u^5 + 16u^4 + 20u^2 - 28u - 60}v^7 + { - 22u^5 - 44u^4 - 55u^2 + 77u + 165}v^5 + { - 14u^5 - 28u^4 - 35u^2 + 49u + 105}v + { - 30u^5 - 60u^4 - 75u^2 + 105u + 225}}

Running Test: (#<procedure:mul> (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial z polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {3v^10 + 4v^7 - 11v^5 - 7v - 15}, {4z^12 + 13z^6 - 12z^5 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (12 (polynomial v polynomial-sparse-terms (10 12) (7 16) (5 (integer . -44)) (1 (integer . -28)) (0 (integer . -60)) (0 0))) (6 (polynomial v polynomial-sparse-terms (10 39) (7 52) (5 (integer . -143)) (1 (integer . -91)) (0 (integer . -195)) (0 0))) (5 (polynomial v polynomial-sparse-terms (10 (integer . -36)) (7 (integer . -48)) (5 132) (1 84) (0 180) (0 0))) (1 (polynomial v polynomial-sparse-terms (10 (integer . -21)) (7 (integer . -28)) (5 77) (1 49) (0 105) (0 0))) (0 (polynomial v polynomial-sparse-terms (10 (integer . -45)) (7 (integer . -60)) (5 165) (1 105) (0 225) (0 0))) (0 0))
{{12v^10 + 16v^7 - 44v^5 - 28v - 60}z^12 + {39v^10 + 52v^7 - 143v^5 - 91v - 195}z^6 + { - 36v^10 - 48v^7 + 132v^5 + 84v + 180}z^5 + { - 21v^10 - 28v^7 + 77v^5 + 49v + 105}z + { - 45v^10 - 60v^7 + 165v^5 + 105v + 225}}


Running Test: (#<procedure:mul> (polynomial q polynomial-dense-terms 0 0) (polynomial q polynomial-dense-terms 0 0)) 
Applying #<procedure:mul> on: {}, {}
Result: (polynomial q)
{}

Running Test: (#<procedure:mul> (polynomial q polynomial-dense-terms 0 0) (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:mul> on: {}, {5}
Result: (polynomial r)
{}

Running Test: (#<procedure:mul> (polynomial r polynomial-dense-terms 0 5) (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:mul> on: {5}, {5}
Result: (polynomial r polynomial-sparse-terms (0 25) (0 0))
{25}

Running Test: (#<procedure:mul> (polynomial r polynomial-dense-terms 0 5) (polynomial s polynomial-dense-terms 1 3 5)) 
Applying #<procedure:mul> on: {5}, {3s + 5}
Result: (polynomial s polynomial-sparse-terms (1 (polynomial r polynomial-sparse-terms (0 15) (0 0))) (0 (polynomial r polynomial-sparse-terms (0 25) (0 0))) (0 0))
{{15}s + {25}}

Running Test: (#<procedure:mul> (polynomial s polynomial-dense-terms 1 3 5) (polynomial t polynomial-dense-terms 1 4 9)) 
Applying #<procedure:mul> on: {3s + 5}, {4t + 9}
Result: (polynomial t polynomial-sparse-terms (1 (polynomial s polynomial-sparse-terms (1 12) (0 20) (0 0))) (0 (polynomial s polynomial-sparse-terms (1 27) (0 45) (0 0))) (0 0))
{{12s + 20}t + {27s + 45}}

Running Test: (#<procedure:mul> (polynomial t polynomial-dense-terms 1 4 9) (polynomial u polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:mul> on: {4t + 9}, {3u^2 + 5u + 23}
Result: (polynomial u polynomial-sparse-terms (2 (polynomial t polynomial-sparse-terms (1 12) (0 27) (0 0))) (1 (polynomial t polynomial-sparse-terms (1 20) (0 45) (0 0))) (0 (polynomial t polynomial-sparse-terms (1 92) (0 207) (0 0))) (0 0))
{{12t + 27}u^2 + {20t + 45}u + {92t + 207}}

Running Test: (#<procedure:mul> (polynomial u polynomial-dense-terms 2 3 5 23) (polynomial v polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:mul> on: {3u^2 + 5u + 23}, {4v^2 + 9v + 19}
Result: (polynomial v polynomial-sparse-terms (2 (polynomial u polynomial-sparse-terms (2 12) (1 20) (0 92) (0 0))) (1 (polynomial u polynomial-sparse-terms (2 27) (1 45) (0 207) (0 0))) (0 (polynomial u polynomial-sparse-terms (2 57) (1 95) (0 437) (0 0))) (0 0))
{{12u^2 + 20u + 92}v^2 + {27u^2 + 45u + 207}v + {57u^2 + 95u + 437}}

Running Test: (#<procedure:mul> (polynomial v polynomial-dense-terms 2 4 9 19) (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:mul> on: {4v^2 + 9v + 19}, {w^5 + 2w^4 + 3w^2 - 2w - 5}
Result: (polynomial w polynomial-sparse-terms (5 (polynomial v polynomial-sparse-terms (2 4) (1 9) (0 19) (0 0))) (4 (polynomial v polynomial-sparse-terms (2 8) (1 18) (0 38) (0 0))) (2 (polynomial v polynomial-sparse-terms (2 12) (1 27) (0 57) (0 0))) (1 (polynomial v polynomial-sparse-terms (2 (integer . -8)) (1 (integer . -18)) (0 (integer . -38)) (0 0))) (0 (polynomial v polynomial-sparse-terms (2 (integer . -20)) (1 (integer . -45)) (0 (integer . -95)) (0 0))) (0 0))
{{4v^2 + 9v + 19}w^5 + {8v^2 + 18v + 38}w^4 + {12v^2 + 27v + 57}w^2 + { - 8v^2 - 18v - 38}w + { - 20v^2 - 45v - 95}}

Running Test: (#<procedure:mul> (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial z polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:mul> on: {w^5 + 2w^4 + 3w^2 - 2w - 5}, {2z^5 + 4z^4 + 5z^2 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (5 (polynomial w polynomial-sparse-terms (5 2) (4 4) (2 6) (1 (integer . -4)) (0 (integer . -10)) (0 0))) (4 (polynomial w polynomial-sparse-terms (5 4) (4 8) (2 12) (1 (integer . -8)) (0 (integer . -20)) (0 0))) (2 (polynomial w polynomial-sparse-terms (5 5) (4 10) (2 15) (1 (integer . -10)) (0 (integer . -25)) (0 0))) (1 (polynomial w polynomial-sparse-terms (5 (integer . -7)) (4 (integer . -14)) (2 (integer . -21)) (1 14) (0 35) (0 0))) (0 (polynomial w polynomial-sparse-terms (5 (integer . -15)) (4 (integer . -30)) (2 (integer . -45)) (1 30) (0 75) (0 0))) (0 0))
{{2w^5 + 4w^4 + 6w^2 - 4w - 10}z^5 + {4w^5 + 8w^4 + 12w^2 - 8w - 20}z^4 + {5w^5 + 10w^4 + 15w^2 - 10w - 25}z^2 + { - 7w^5 - 14w^4 - 21w^2 + 14w + 35}z + { - 15w^5 - 30w^4 - 45w^2 + 30w + 75}}


Running Test: (#<procedure:mul> (polynomial p polynomial-sparse-terms (0 0)) (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:mul> on: {}, {5}
Result: (polynomial r)
{}

Running Test: (#<procedure:mul> (polynomial r polynomial-dense-terms 0 5) (polynomial p polynomial-sparse-terms (0 0))) 
Applying #<procedure:mul> on: {5}, {}
Result: (polynomial r)
{}

Running Test: (#<procedure:mul> (polynomial q polynomial-sparse-terms (0 5)) (polynomial s polynomial-dense-terms 1 3 5)) 
Applying #<procedure:mul> on: {5}, {3s + 5}
Result: (polynomial s polynomial-sparse-terms (1 (polynomial q polynomial-sparse-terms (0 15) (0 0))) (0 (polynomial q polynomial-sparse-terms (0 25) (0 0))) (0 0))
{{15}s + {25}}

Running Test: (#<procedure:mul> (polynomial r polynomial-sparse-terms (1 7) (0 5)) (polynomial t polynomial-dense-terms 1 4 9)) 
Applying #<procedure:mul> on: {7r + 5}, {4t + 9}
Result: (polynomial t polynomial-sparse-terms (1 (polynomial r polynomial-sparse-terms (1 28) (0 20) (0 0))) (0 (polynomial r polynomial-sparse-terms (1 63) (0 45) (0 0))) (0 0))
{{28r + 20}t + {63r + 45}}

Running Test: (#<procedure:mul> (polynomial s polynomial-sparse-terms (1 9) (0 -25)) (polynomial u polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:mul> on: {9s - 25}, {3u^2 + 5u + 23}
Result: (polynomial u polynomial-sparse-terms (2 (polynomial s polynomial-sparse-terms (1 27) (0 (integer . -75)) (0 0))) (1 (polynomial s polynomial-sparse-terms (1 45) (0 (integer . -125)) (0 0))) (0 (polynomial s polynomial-sparse-terms (1 207) (0 (integer . -575)) (0 0))) (0 0))
{{27s - 75}u^2 + {45s - 125}u + {207s - 575}}

Running Test: (#<procedure:mul> (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial v polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:mul> on: {t^5 + 2t^4 + 3t^2 - 2t - 5}, {4v^2 + 9v + 19}
Result: (polynomial v polynomial-sparse-terms (2 (polynomial t polynomial-sparse-terms (5 4) (4 8) (2 12) (1 (integer . -8)) (0 (integer . -20)) (0 0))) (1 (polynomial t polynomial-sparse-terms (5 9) (4 18) (2 27) (1 (integer . -18)) (0 (integer . -45)) (0 0))) (0 (polynomial t polynomial-sparse-terms (5 19) (4 38) (2 57) (1 (integer . -38)) (0 (integer . -95)) (0 0))) (0 0))
{{4t^5 + 8t^4 + 12t^2 - 8t - 20}v^2 + {9t^5 + 18t^4 + 27t^2 - 18t - 45}v + {19t^5 + 38t^4 + 57t^2 - 38t - 95}}

Running Test: (#<procedure:mul> (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:mul> on: {2u^5 + 4u^4 + 5u^2 - 7u - 15}, {w^5 + 2w^4 + 3w^2 - 2w - 5}
Result: (polynomial w polynomial-sparse-terms (5 (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 (integer . -7)) (0 (integer . -15)) (0 0))) (4 (polynomial u polynomial-sparse-terms (5 4) (4 8) (2 10) (1 (integer . -14)) (0 (integer . -30)) (0 0))) (2 (polynomial u polynomial-sparse-terms (5 6) (4 12) (2 15) (1 (integer . -21)) (0 (integer . -45)) (0 0))) (1 (polynomial u polynomial-sparse-terms (5 (integer . -4)) (4 (integer . -8)) (2 (integer . -10)) (1 14) (0 30) (0 0))) (0 (polynomial u polynomial-sparse-terms (5 (integer . -10)) (4 (integer . -20)) (2 (integer . -25)) (1 35) (0 75) (0 0))) (0 0))
{{2u^5 + 4u^4 + 5u^2 - 7u - 15}w^5 + {4u^5 + 8u^4 + 10u^2 - 14u - 30}w^4 + {6u^5 + 12u^4 + 15u^2 - 21u - 45}w^2 + { - 4u^5 - 8u^4 - 10u^2 + 14u + 30}w + { - 10u^5 - 20u^4 - 25u^2 + 35u + 75}}

Running Test: (#<procedure:mul> (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial z polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:mul> on: {3v^10 + 4v^7 - 11v^5 - 7v - 15}, {2z^5 + 4z^4 + 5z^2 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (5 (polynomial v polynomial-sparse-terms (10 6) (7 8) (5 (integer . -22)) (1 (integer . -14)) (0 (integer . -30)) (0 0))) (4 (polynomial v polynomial-sparse-terms (10 12) (7 16) (5 (integer . -44)) (1 (integer . -28)) (0 (integer . -60)) (0 0))) (2 (polynomial v polynomial-sparse-terms (10 15) (7 20) (5 (integer . -55)) (1 (integer . -35)) (0 (integer . -75)) (0 0))) (1 (polynomial v polynomial-sparse-terms (10 (integer . -21)) (7 (integer . -28)) (5 77) (1 49) (0 105) (0 0))) (0 (polynomial v polynomial-sparse-terms (10 (integer . -45)) (7 (integer . -60)) (5 165) (1 105) (0 225) (0 0))) (0 0))
{{6v^10 + 8v^7 - 22v^5 - 14v - 30}z^5 + {12v^10 + 16v^7 - 44v^5 - 28v - 60}z^4 + {15v^10 + 20v^7 - 55v^5 - 35v - 75}z^2 + { - 21v^10 - 28v^7 + 77v^5 + 49v + 105}z + { - 45v^10 - 60v^7 + 165v^5 + 105v + 225}}


Running Test: (#<procedure:mul> (polynomial q polynomial-dense-terms 0 0) (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:mul> on: {}, {5}
Result: (polynomial q)
{}

Running Test: (#<procedure:mul> (polynomial q polynomial-sparse-terms (0 5)) (polynomial q polynomial-dense-terms 0 0)) 
Applying #<procedure:mul> on: {5}, {}
Result: (polynomial q)
{}

Running Test: (#<procedure:mul> (polynomial r polynomial-dense-terms 0 5) (polynomial r polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:mul> on: {5}, {7r + 5}
Result: (polynomial r polynomial-sparse-terms (1 35) (0 25) (0 0))
{35r + 25}

Running Test: (#<procedure:mul> (polynomial s polynomial-dense-terms 1 3 5) (polynomial s polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:mul> on: {3s + 5}, {9s - 25}
Result: (polynomial s polynomial-sparse-terms (2 27) (1 (integer . -30)) (0 (integer . -125)) (0 0))
{27s^2 - 30s - 125}

Running Test: (#<procedure:mul> (polynomial t polynomial-dense-terms 1 4 9) (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:mul> on: {4t + 9}, {t^5 + 2t^4 + 3t^2 - 2t - 5}
Result: (polynomial t polynomial-sparse-terms (6 4) (5 17) (4 18) (3 12) (2 19) (1 (integer . -38)) (0 (integer . -45)) (0 0))
{4t^6 + 17t^5 + 18t^4 + 12t^3 + 19t^2 - 38t - 45}

Running Test: (#<procedure:mul> (polynomial u polynomial-dense-terms 2 3 5 23) (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {3u^2 + 5u + 23}, {2u^5 + 4u^4 + 5u^2 - 7u - 15}
Result: (polynomial u polynomial-sparse-terms (7 6) (6 22) (5 66) (4 107) (3 4) (2 35) (1 (integer . -236)) (0 (integer . -345)) (0 0))
{6u^7 + 22u^6 + 66u^5 + 107u^4 + 4u^3 + 35u^2 - 236u - 345}

Running Test: (#<procedure:mul> (polynomial v polynomial-dense-terms 2 4 9 19) (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {4v^2 + 9v + 19}, {3v^10 + 4v^7 - 11v^5 - 7v - 15}
Result: (polynomial v polynomial-sparse-terms (12 12) (11 27) (10 57) (9 16) (8 36) (7 32) (6 (integer . -99)) (5 (integer . -209)) (3 (integer . -28)) (2 (integer . -123)) (1 (integer . -268)) (0 (integer . -285)) (0 0))
{12v^12 + 27v^11 + 57v^10 + 16v^9 + 36v^8 + 32v^7 - 99v^6 - 209v^5 - 28v^3 - 123v^2 - 268v - 285}

Running Test: (#<procedure:mul> (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial z polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {w^5 + 2w^4 + 3w^2 - 2w - 5}, {4z^12 + 13z^6 - 12z^5 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (12 (polynomial w polynomial-sparse-terms (5 4) (4 8) (2 12) (1 (integer . -8)) (0 (integer . -20)) (0 0))) (6 (polynomial w polynomial-sparse-terms (5 13) (4 26) (2 39) (1 (integer . -26)) (0 (integer . -65)) (0 0))) (5 (polynomial w polynomial-sparse-terms (5 (integer . -12)) (4 (integer . -24)) (2 (integer . -36)) (1 24) (0 60) (0 0))) (1 (polynomial w polynomial-sparse-terms (5 (integer . -7)) (4 (integer . -14)) (2 (integer . -21)) (1 14) (0 35) (0 0))) (0 (polynomial w polynomial-sparse-terms (5 (integer . -15)) (4 (integer . -30)) (2 (integer . -45)) (1 30) (0 75) (0 0))) (0 0))
{{4w^5 + 8w^4 + 12w^2 - 8w - 20}z^12 + {13w^5 + 26w^4 + 39w^2 - 26w - 65}z^6 + { - 12w^5 - 24w^4 - 36w^2 + 24w + 60}z^5 + { - 7w^5 - 14w^4 - 21w^2 + 14w + 35}z + { - 15w^5 - 30w^4 - 45w^2 + 30w + 75}}


Running Test: (#<procedure:NEGATE> (polynomial p polynomial-sparse-terms (0 0))) 
Applying #<procedure:NEGATE> on: {}
Result: (polynomial p polynomial-sparse-terms (0 0))
{}

Running Test: (#<procedure:NEGATE> (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:NEGATE> on: {5}
Result: (polynomial q polynomial-sparse-terms (0 (integer . -5)))
{ - 5}

Running Test: (#<procedure:NEGATE> (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:NEGATE> on: {5}
Result: (polynomial q polynomial-sparse-terms (0 (integer . -5)))
{ - 5}

Running Test: (#<procedure:NEGATE> (polynomial r polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:NEGATE> on: {7r + 5}
Result: (polynomial r polynomial-sparse-terms (1 (integer . -7)) (0 (integer . -5)))
{ - 7r - 5}

Running Test: (#<procedure:NEGATE> (polynomial s polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:NEGATE> on: {9s - 25}
Result: (polynomial s polynomial-sparse-terms (1 (integer . -9)) (0 25))
{ - 9s + 25}

Running Test: (#<procedure:NEGATE> (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:NEGATE> on: {t^5 + 2t^4 + 3t^2 - 2t - 5}
Result: (polynomial t polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -3)) (1 2) (0 5))
{ - 1t^5 - 2t^4 - 3t^2 + 2t + 5}

Running Test: (#<procedure:NEGATE> (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:NEGATE> on: {2u^5 + 4u^4 + 5u^2 - 7u - 15}
Result: (polynomial u polynomial-sparse-terms (5 (integer . -2)) (4 (integer . -4)) (2 (integer . -5)) (1 7) (0 15))
{ - 2u^5 - 4u^4 - 5u^2 + 7u + 15}

Running Test: (#<procedure:NEGATE> (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:NEGATE> on: {3v^10 + 4v^7 - 11v^5 - 7v - 15}
Result: (polynomial v polynomial-sparse-terms (10 (integer . -3)) (7 (integer . -4)) (5 11) (1 7) (0 15))
{ - 3v^10 - 4v^7 + 11v^5 + 7v + 15}

Running Test: (#<procedure:NEGATE> (polynomial z polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:NEGATE> on: {4z^12 + 13z^6 - 12z^5 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (12 (integer . -4)) (6 (integer . -13)) (5 12) (1 7) (0 15))
{ - 4z^12 - 13z^6 + 12z^5 + 7z + 15}


Running Test: (#<procedure:NEGATE> (polynomial q polynomial-dense-terms 0 0)) 
Applying #<procedure:NEGATE> on: {}
Result: (polynomial q polynomial-dense-terms 0 0)
{}

Running Test: (#<procedure:NEGATE> (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:NEGATE> on: {5}
Result: (polynomial r polynomial-dense-terms 0 (integer . -5))
{ - 5}

Running Test: (#<procedure:NEGATE> (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:NEGATE> on: {5}
Result: (polynomial r polynomial-dense-terms 0 (integer . -5))
{ - 5}

Running Test: (#<procedure:NEGATE> (polynomial s polynomial-dense-terms 1 3 5)) 
Applying #<procedure:NEGATE> on: {3s + 5}
Result: (polynomial s polynomial-dense-terms 1 (integer . -3) (integer . -5))
{ - 3s - 5}

Running Test: (#<procedure:NEGATE> (polynomial t polynomial-dense-terms 1 4 9)) 
Applying #<procedure:NEGATE> on: {4t + 9}
Result: (polynomial t polynomial-dense-terms 1 (integer . -4) (integer . -9))
{ - 4t - 9}

Running Test: (#<procedure:NEGATE> (polynomial u polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:NEGATE> on: {3u^2 + 5u + 23}
Result: (polynomial u polynomial-dense-terms 2 (integer . -3) (integer . -5) (integer . -23))
{ - 3u^2 - 5u - 23}

Running Test: (#<procedure:NEGATE> (polynomial v polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:NEGATE> on: {4v^2 + 9v + 19}
Result: (polynomial v polynomial-dense-terms 2 (integer . -4) (integer . -9) (integer . -19))
{ - 4v^2 - 9v - 19}

Running Test: (#<procedure:NEGATE> (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:NEGATE> on: {w^5 + 2w^4 + 3w^2 - 2w - 5}
Result: (polynomial w polynomial-dense-terms 5 (integer . -1) (integer . -2) 0 (integer . -3) 2 5)
{ - 1w^5 - 2w^4 - 3w^2 + 2w + 5}

Running Test: (#<procedure:NEGATE> (polynomial z polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:NEGATE> on: {2z^5 + 4z^4 + 5z^2 - 7z - 15}
Result: (polynomial z polynomial-dense-terms 5 (integer . -2) (integer . -4) 0 (integer . -5) 7 15)
{ - 2z^5 - 4z^4 - 5z^2 + 7z + 15}

Running Test: (#<procedure:sub> (polynomial p polynomial-sparse-terms (0 0)) (polynomial p polynomial-sparse-terms (0 0))) 
Applying #<procedure:sub> on: {}, {}
Result: (polynomial p polynomial-sparse-terms (0 0))
{}

Running Test: (#<procedure:sub> (polynomial p polynomial-sparse-terms (0 0)) (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:sub> on: {}, {5}
Result: (polynomial q polynomial-sparse-terms (0 (integer . -5)))
{ - 5}

Running Test: (#<procedure:sub> (polynomial q polynomial-sparse-terms (0 5)) (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:sub> on: {5}, {5}
Result: (polynomial q polynomial-sparse-terms)
{}

Running Test: (#<procedure:sub> (polynomial q polynomial-sparse-terms (0 5)) (polynomial r polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:sub> on: {5}, {7r + 5}
Result: (polynomial r polynomial-sparse-terms (1 (integer . -7)))
{ - 7r}

Running Test: (#<procedure:sub> (polynomial r polynomial-sparse-terms (1 7) (0 5)) (polynomial s polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:sub> on: {7r + 5}, {9s - 25}
Result: (polynomial s polynomial-sparse-terms (1 (integer . -9)) (0 (polynomial r polynomial-sparse-terms (1 7) (0 30))))
{ - 9s + {7r + 30}}

Running Test: (#<procedure:sub> (polynomial s polynomial-sparse-terms (1 9) (0 -25)) (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:sub> on: {9s - 25}, {t^5 + 2t^4 + 3t^2 - 2t - 5}
Result: (polynomial t polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -3)) (1 2) (0 (polynomial s polynomial-sparse-terms (1 9) (0 (integer . -20)))))
{ - 1t^5 - 2t^4 - 3t^2 + 2t + {9s - 20}}

Running Test: (#<procedure:sub> (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {t^5 + 2t^4 + 3t^2 - 2t - 5}, {2u^5 + 4u^4 + 5u^2 - 7u - 15}
Result: (polynomial u polynomial-sparse-terms (5 (integer . -2)) (4 (integer . -4)) (2 (integer . -5)) (1 7) (0 (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 10))))
{ - 2u^5 - 4u^4 - 5u^2 + 7u + {t^5 + 2t^4 + 3t^2 - 2t + 10}}

Running Test: (#<procedure:sub> (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {2u^5 + 4u^4 + 5u^2 - 7u - 15}, {3v^10 + 4v^7 - 11v^5 - 7v - 15}
Result: (polynomial v polynomial-sparse-terms (10 (integer . -3)) (7 (integer . -4)) (5 11) (1 7) (0 (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7))))
{ - 3v^10 - 4v^7 + 11v^5 + 7v + {2u^5 + 4u^4 + 5u^2 - 7u}}

Running Test: (#<procedure:sub> (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial z polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {3v^10 + 4v^7 - 11v^5 - 7v - 15}, {4z^12 + 13z^6 - 12z^5 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (12 (integer . -4)) (6 (integer . -13)) (5 12) (1 7) (0 (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7))))
{ - 4z^12 - 13z^6 + 12z^5 + 7z + {3v^10 + 4v^7 - 11v^5 - 7v}}


Running Test: (#<procedure:sub> (polynomial q polynomial-dense-terms 0 0) (polynomial q polynomial-dense-terms 0 0)) 
Applying #<procedure:sub> on: {}, {}
Result: (polynomial q polynomial-dense-terms 0 0)
{}

Running Test: (#<procedure:sub> (polynomial q polynomial-dense-terms 0 0) (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:sub> on: {}, {5}
Result: (polynomial r polynomial-dense-terms 0 (integer . -5))
{ - 5}

Running Test: (#<procedure:sub> (polynomial r polynomial-dense-terms 0 5) (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:sub> on: {5}, {5}
Result: (polynomial r polynomial-dense-terms)
{}

Running Test: (#<procedure:sub> (polynomial r polynomial-dense-terms 0 5) (polynomial s polynomial-dense-terms 1 3 5)) 
Applying #<procedure:sub> on: {5}, {3s + 5}
Result: (polynomial s polynomial-sparse-terms (1 (integer . -3)))
{ - 3s}

Running Test: (#<procedure:sub> (polynomial s polynomial-dense-terms 1 3 5) (polynomial t polynomial-dense-terms 1 4 9)) 
Applying #<procedure:sub> on: {3s + 5}, {4t + 9}
Result: (polynomial t polynomial-sparse-terms (1 (integer . -4)) (0 (polynomial s polynomial-sparse-terms (1 3) (0 (integer . -4)))))
{ - 4t + {3s - 4}}

Running Test: (#<procedure:sub> (polynomial t polynomial-dense-terms 1 4 9) (polynomial u polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:sub> on: {4t + 9}, {3u^2 + 5u + 23}
Result: (polynomial u polynomial-sparse-terms (2 (integer . -3)) (1 (integer . -5)) (0 (polynomial t polynomial-sparse-terms (1 4) (0 (integer . -14)))))
{ - 3u^2 - 5u + {4t - 14}}

Running Test: (#<procedure:sub> (polynomial u polynomial-dense-terms 2 3 5 23) (polynomial v polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:sub> on: {3u^2 + 5u + 23}, {4v^2 + 9v + 19}
Result: (polynomial v polynomial-sparse-terms (2 (integer . -4)) (1 (integer . -9)) (0 (polynomial u polynomial-sparse-terms (2 3) (1 5) (0 4))))
{ - 4v^2 - 9v + {3u^2 + 5u + 4}}

Running Test: (#<procedure:sub> (polynomial v polynomial-dense-terms 2 4 9 19) (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:sub> on: {4v^2 + 9v + 19}, {w^5 + 2w^4 + 3w^2 - 2w - 5}
Result: (polynomial w polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -3)) (1 2) (0 (polynomial v polynomial-sparse-terms (2 4) (1 9) (0 24))))
{ - 1w^5 - 2w^4 - 3w^2 + 2w + {4v^2 + 9v + 24}}

Running Test: (#<procedure:sub> (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial z polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:sub> on: {w^5 + 2w^4 + 3w^2 - 2w - 5}, {2z^5 + 4z^4 + 5z^2 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (5 (integer . -2)) (4 (integer . -4)) (2 (integer . -5)) (1 7) (0 (polynomial w polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 10))))
{ - 2z^5 - 4z^4 - 5z^2 + 7z + {w^5 + 2w^4 + 3w^2 - 2w + 10}}


Running Test: (#<procedure:sub> (polynomial p polynomial-sparse-terms (0 0)) (polynomial r polynomial-dense-terms 0 5)) 
Applying #<procedure:sub> on: {}, {5}
Result: (polynomial r polynomial-dense-terms 0 (integer . -5))
{ - 5}

Running Test: (#<procedure:sub> (polynomial r polynomial-dense-terms 0 5) (polynomial p polynomial-sparse-terms (0 0))) 
Applying #<procedure:sub> on: {5}, {}
Result: (polynomial r polynomial-dense-terms 0 5)
{5}

Running Test: (#<procedure:sub> (polynomial q polynomial-sparse-terms (0 5)) (polynomial s polynomial-dense-terms 1 3 5)) 
Applying #<procedure:sub> on: {5}, {3s + 5}
Result: (polynomial s polynomial-sparse-terms (1 (integer . -3)))
{ - 3s}

Running Test: (#<procedure:sub> (polynomial r polynomial-sparse-terms (1 7) (0 5)) (polynomial t polynomial-dense-terms 1 4 9)) 
Applying #<procedure:sub> on: {7r + 5}, {4t + 9}
Result: (polynomial t polynomial-sparse-terms (1 (integer . -4)) (0 (polynomial r polynomial-sparse-terms (1 7) (0 (integer . -4)))))
{ - 4t + {7r - 4}}

Running Test: (#<procedure:sub> (polynomial s polynomial-sparse-terms (1 9) (0 -25)) (polynomial u polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:sub> on: {9s - 25}, {3u^2 + 5u + 23}
Result: (polynomial u polynomial-sparse-terms (2 (integer . -3)) (1 (integer . -5)) (0 (polynomial s polynomial-sparse-terms (1 9) (0 (integer . -48)))))
{ - 3u^2 - 5u + {9s - 48}}

Running Test: (#<procedure:sub> (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial v polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:sub> on: {t^5 + 2t^4 + 3t^2 - 2t - 5}, {4v^2 + 9v + 19}
Result: (polynomial v polynomial-sparse-terms (2 (integer . -4)) (1 (integer . -9)) (0 (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 (integer . -24)))))
{ - 4v^2 - 9v + {t^5 + 2t^4 + 3t^2 - 2t - 24}}

Running Test: (#<procedure:sub> (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:sub> on: {2u^5 + 4u^4 + 5u^2 - 7u - 15}, {w^5 + 2w^4 + 3w^2 - 2w - 5}
Result: (polynomial w polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -3)) (1 2) (0 (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 (integer . -10)))))
{ - 1w^5 - 2w^4 - 3w^2 + 2w + {2u^5 + 4u^4 + 5u^2 - 7u - 10}}

Running Test: (#<procedure:sub> (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial z polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:sub> on: {3v^10 + 4v^7 - 11v^5 - 7v - 15}, {2z^5 + 4z^4 + 5z^2 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (5 (integer . -2)) (4 (integer . -4)) (2 (integer . -5)) (1 7) (0 (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7))))
{ - 2z^5 - 4z^4 - 5z^2 + 7z + {3v^10 + 4v^7 - 11v^5 - 7v}}


Running Test: (#<procedure:sub> (polynomial q polynomial-dense-terms 0 0) (polynomial q polynomial-sparse-terms (0 5))) 
Applying #<procedure:sub> on: {}, {5}
Result: (polynomial q polynomial-sparse-terms (0 (integer . -5)))
{ - 5}

Running Test: (#<procedure:sub> (polynomial q polynomial-sparse-terms (0 5)) (polynomial q polynomial-dense-terms 0 0)) 
Applying #<procedure:sub> on: {5}, {}
Result: (polynomial q polynomial-sparse-terms (0 5))
{5}

Running Test: (#<procedure:sub> (polynomial r polynomial-dense-terms 0 5) (polynomial r polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:sub> on: {5}, {7r + 5}
Result: (polynomial r polynomial-sparse-terms (1 (integer . -7)))
{ - 7r}

Running Test: (#<procedure:sub> (polynomial s polynomial-dense-terms 1 3 5) (polynomial s polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:sub> on: {3s + 5}, {9s - 25}
Result: (polynomial s polynomial-sparse-terms (1 (integer . -6)) (0 30))
{ - 6s + 30}

Running Test: (#<procedure:sub> (polynomial t polynomial-dense-terms 1 4 9) (polynomial t polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:sub> on: {4t + 9}, {t^5 + 2t^4 + 3t^2 - 2t - 5}
Result: (polynomial t polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -3)) (1 6) (0 14))
{ - 1t^5 - 2t^4 - 3t^2 + 6t + 14}

Running Test: (#<procedure:sub> (polynomial u polynomial-dense-terms 2 3 5 23) (polynomial u polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {3u^2 + 5u + 23}, {2u^5 + 4u^4 + 5u^2 - 7u - 15}
Result: (polynomial u polynomial-sparse-terms (5 (integer . -2)) (4 (integer . -4)) (2 (integer . -2)) (1 12) (0 38))
{ - 2u^5 - 4u^4 - 2u^2 + 12u + 38}

Running Test: (#<procedure:sub> (polynomial v polynomial-dense-terms 2 4 9 19) (polynomial v polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {4v^2 + 9v + 19}, {3v^10 + 4v^7 - 11v^5 - 7v - 15}
Result: (polynomial v polynomial-sparse-terms (10 (integer . -3)) (7 (integer . -4)) (5 11) (2 4) (1 16) (0 34))
{ - 3v^10 - 4v^7 + 11v^5 + 4v^2 + 16v + 34}

Running Test: (#<procedure:sub> (polynomial w polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial z polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {w^5 + 2w^4 + 3w^2 - 2w - 5}, {4z^12 + 13z^6 - 12z^5 - 7z - 15}
Result: (polynomial z polynomial-sparse-terms (12 (integer . -4)) (6 (integer . -13)) (5 12) (1 7) (0 (polynomial w polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 10))))
{ - 4z^12 - 13z^6 + 12z^5 + 7z + {w^5 + 2w^4 + 3w^2 - 2w + 10}}


Running Test: (#<procedure:=zero?> (polynomial v polynomial-dense-terms)) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

Running Test: (#<procedure:=zero?> (polynomial s polynomial-sparse-terms)) 
Applying #<procedure:=zero?> on: {}
Result: #t
True


'(polynomial-list (y polynomial-sparse-terms (0 1)) (y))
'(polynomial-list (w polynomial-sparse-terms (2 1)) (w))
'(polynomial-list (p polynomial-sparse-terms (1 1)) (p polynomial-sparse-terms (1 1) (0 -1)))
'(polynomial-list (x polynomial-sparse-terms (3 1) (1 1)) (x polynomial-sparse-terms (1 1) (0 -1)))
'(polynomial-list (x polynomial-sparse-terms (3 3) (1 (integer . -2)) (0 (real . -4.5))) (x polynomial-sparse-terms (1 (real . 5.5)) (0 23) (0 0)))

P3y: '(polynomial y polynomial-sparse-terms (1 1))
P3y: {y}
P3y in terms of x: '(polynomial x polynomial-sparse-terms (0 (polynomial y polynomial-sparse-terms (1 1))))
P3y in terms of x: {{y}}

X: '(polynomial x polynomial-sparse-terms (1 1))
X: {x}
P4y: '(polynomial y polynomial-sparse-terms (1 (polynomial x polynomial-sparse-terms (1 1))))
P4y: {{x}y}
P4y in terms of x: '(polynomial x polynomial-sparse-terms (1 (polynomial y polynomial-sparse-terms (1 1))))
P4y in terms of x: {{y}x}

Xsq: '(polynomial x polynomial-sparse-terms (2 1))
Xsq: {x^2}
P5y: '(polynomial y polynomial-sparse-terms (1 (polynomial x polynomial-sparse-terms (2 1))))
P5y: {{x^2}y}
P5y in terms of x: '(polynomial x polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (1 1))))
P5y in terms of x: {{y}x^2}

5Xsq: '(polynomial x polynomial-sparse-terms (2 5))
5Xsq: {5x^2}
P6y: '(polynomial y polynomial-sparse-terms (1 (polynomial x polynomial-sparse-terms (2 5))))
P6y: {{5x^2}y}
P6y in terms of x: '(polynomial x polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (1 5))))
P6y in terms of x: {{5y}x^2}

5Xcb: '(polynomial x polynomial-sparse-terms (3 5))
5Xcb: {5x^3}
P7y: '(polynomial y polynomial-sparse-terms (7 (polynomial x polynomial-sparse-terms (3 5))))
P7y: {{5x^3}y^7}
P7y in terms of x: '(polynomial x polynomial-sparse-terms (3 (polynomial y polynomial-sparse-terms (7 5))))
P7y in terms of x: {{5y^7}x^3}

P8y: '(polynomial y polynomial-sparse-terms (7 (polynomial x polynomial-sparse-terms (3 5))) (1 (polynomial x polynomial-sparse-terms (2 5))))
P8y: {{5x^3}y^7 + {5x^2}y}
P8y in terms of x: '(polynomial x polynomial-sparse-terms (3 (polynomial y polynomial-sparse-terms (7 5))) (2 (polynomial y polynomial-sparse-terms (1 5))))
P8y in terms of x: {{5y^7}x^3 + {5y}x^2}

P9y: {{x^2}y^4}
P6y: {{5x^2}y}
P10y = P9y + P6y: '(polynomial y polynomial-sparse-terms (4 (polynomial x polynomial-sparse-terms (2 1))) (1 (polynomial x polynomial-sparse-terms (2 5))))
P10y = P9y + P6y: {{x^2}y^4 + {5x^2}y}
P10y in terms of x: '(polynomial x polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (4 1) (1 5))))
P10y in terms of x: {{y^4 + 5y}x^2}

P9y: {{x^2}y^4}
P61y: {{5x^2}y^4}
P11y = P9y + P61y: '(polynomial y polynomial-sparse-terms (4 (polynomial x polynomial-sparse-terms (2 6))))
P11y = P9y + P61y: {{6x^2}y^4}
P11y in terms of x: '(polynomial x polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (4 6))))
P11y in terms of x: {{6y^4}x^2}

P1x: '(polynomial x polynomial-sparse-terms (1 3) (0 -2))
P1x: {3x - 2}
P1x in terms of z: '(polynomial z polynomial-sparse-terms (0 (polynomial x polynomial-sparse-terms (1 3) (0 -2))))
P1x in terms of z: {{3x - 2}}

P1y: '(polynomial y polynomial-sparse-terms (1 (polynomial x polynomial-sparse-terms (1 3) (0 -2))) (0 (polynomial x polynomial-dense-terms 1 7 3)))
P1y: {{3x - 2}y + {7x + 3}}
P1y in terms of x: '(polynomial x polynomial-sparse-terms (1 (polynomial y polynomial-sparse-terms (1 3) (0 7))) (0 (polynomial y polynomial-sparse-terms (1 -2) (0 3))))
P1y in terms of x: {{3y + 7}x + { - 2y + 3}}

P1z: '(polynomial
  z
  polynomial-sparse-terms
  (1 (polynomial y polynomial-sparse-terms (1 (polynomial x polynomial-sparse-terms (1 3) (0 -2))) (0 (polynomial x polynomial-dense-terms 1 7 3))))
  (0 (polynomial y polynomial-dense-terms 1 (polynomial x polynomial-sparse-terms (1 5) (0 -4)) (polynomial x polynomial-dense-terms 1 9 -6))))
P1z: {{{3x - 2}y + {7x + 3}}z + {{5x - 4}y + {9x - 6}}}
P1z in terms of y: '(polynomial
  y
  polynomial-sparse-terms
  (1 (polynomial z polynomial-sparse-terms (1 (polynomial x polynomial-sparse-terms (1 3) (0 -2))) (0 (polynomial x polynomial-sparse-terms (1 5) (0 -4)))))
  (0 (polynomial z polynomial-sparse-terms (1 (polynomial x polynomial-dense-terms 1 7 3)) (0 (polynomial x polynomial-dense-terms 1 9 -6)))))
P1z in terms of y: {{{3x - 2}z + {5x - 4}}y + {{7x + 3}z + {9x - 6}}}

P1z in terms of x: '(polynomial
  x
  polynomial-sparse-terms
  (1 (polynomial z polynomial-sparse-terms (1 (polynomial y polynomial-sparse-terms (1 3) (0 7))) (0 (polynomial y polynomial-sparse-terms (1 5) (0 9)))))
  (0 (polynomial z polynomial-sparse-terms (1 (polynomial y polynomial-sparse-terms (1 -2) (0 3))) (0 (polynomial y polynomial-sparse-terms (1 -4) (0 -6))))))
P1z in terms of x: {{{3y + 7}z + {5y + 9}}x + {{ - 2y + 3}z + { - 4y - 6}}}

P5x: '(polynomial x polynomial-sparse-terms (1 11))
P5x: {11x}
P12y: '(polynomial y polynomial-sparse-terms (1 15))
P12y: {15y}
P5x + P12y: 
Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (1 11)) (polynomial y polynomial-sparse-terms (1 15))) 
Applying #<procedure:add> on: {11x}, {15y}
Result: (polynomial x polynomial-sparse-terms (1 11) (0 (polynomial y polynomial-sparse-terms (1 15))))
{11x + {15y}}

P5x + P12y: 
{11x + {15y}}

P6x: '(polynomial x polynomial-sparse-terms (1 11) (0 12))
P6x: {11x + 12}
P13y: '(polynomial y polynomial-sparse-terms (1 15) (0 17))
P13y: {15y + 17}
P6x + P13y: 
Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (1 11) (0 12)) (polynomial y polynomial-sparse-terms (1 15) (0 17))) 
Applying #<procedure:add> on: {11x + 12}, {15y + 17}
Result: (polynomial x polynomial-sparse-terms (1 11) (0 (polynomial y polynomial-sparse-terms (1 15) (0 29))))
{11x + {15y + 29}}

P6x + P13y: 
{11x + {15y + 29}}

P7x: '(polynomial x polynomial-sparse-terms (2 10) (1 11) (0 12))
P7x: {10x^2 + 11x + 12}
P14y: '(polynomial y polynomial-sparse-terms (2 13) (1 15) (0 17))
P14y: {13y^2 + 15y + 17}
P7x + P14y: 
Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (2 10) (1 11) (0 12)) (polynomial y polynomial-sparse-terms (2 13) (1 15) (0 17))) 
Applying #<procedure:add> on: {10x^2 + 11x + 12}, {13y^2 + 15y + 17}
Result: (polynomial x polynomial-sparse-terms (2 10) (1 11) (0 (polynomial y polynomial-sparse-terms (2 13) (1 15) (0 29))))
{10x^2 + 11x + {13y^2 + 15y + 29}}

P7x + P14y: 
{10x^2 + 11x + {13y^2 + 15y + 29}}
> 
