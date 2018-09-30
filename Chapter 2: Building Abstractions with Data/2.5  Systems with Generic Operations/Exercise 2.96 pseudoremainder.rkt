#lang racket

; Exercise 2.96.  a. Implement the procedure pseudoremainder-terms, which is just like remainder-terms
; except that it multiplies the dividend by the integerizing factor described above before calling
; div-terms. Modify gcd-terms to use pseudoremainder-terms, and verify that greatest-common-divisor
; now produces an answer with integer coefficients on the example in exercise 2.95.

; b. The GCD now has integer coefficients, but they are larger than those of P1. Modify gcd-terms
; so that it removes common factors from the coefficients of the answer by dividing all the
; coefficients by their (integer) greatest common divisor.

; S O L U T I O N

; GENERIC PROCEDURES

; Generic Polynomial procedures

; Assumed ordering of variables is (in increasing order of priority):
; p, q, r, s, t, u, v, w, x, y, z
; I have implemented a generic print procedure so that polynomials and other types can be printed
; in an easy to read manner.
; The main work in this exercise lies in the 'convert-polynomial' procedure that transforms a polynomial
; from one variable to another

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
(define (scale-down-term term factor) (apply-generic 'scale-down-term term factor))

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
(define (greatest-common-divisor x y) (apply-generic 'gcd x y))

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
						((and (pair? coeff-first-term) (or (not (or (equ? coeff-first-term 1) (equ? coeff-first-term -1))) (=zero? order-first-term)))
							; Expecting this to be a polynomial
							(print (ABS coeff-first-term))
						)
						((or (and (not (equ? coeff-first-term 1)) (not (equ? coeff-first-term -1))) (=zero? order-first-term))
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
	 				)
					; Coefficient is an empty polynomial, so return an empty polynomial in the new variable
					((and (is-poly? (coeff the-term)) (empty-termlist? (term-list (coeff the-term))))
						(make-polynomial new-var (the-empty-poly-termlist))
					)
	 				; coeff in the term is not a polynomial
	 				; Example: 7y^4 becomes (7y^4)x^0
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

(define (gcd-poly p1 p2)
	(if (same-variable? (variable-poly p1) (variable-poly p2))
		(make-poly
			(variable-poly p2)
			(gcd-terms (term-list-poly p1) (term-list-poly p2))
		)
		(error "Polys not in same var -- GCD-POLY"
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

(define (pseudoremainder-terms L-dividend L-divisor)
	(let ((integerizing-factor (exp (coeff (first-term L-divisor)) (+ 1 (sub (order (first-term L-dividend)) (order (first-term L-divisor)))))))
		(display "Integerizing factor is: ")
		(display integerizing-factor)
		(newline)
		(let ((pseudodividend
				(term-list (mul (make-polynomial 'x L-dividend) integerizing-factor))
			  )
			 )
			(cadr (div-terms pseudodividend L-divisor))
		)
	)
)

(define (remainder-terms L-dividend L-divisor)
	(cadr (div-terms L-dividend L-divisor))
)

(define (gcd-terms a b)

	(define (gcd-terms-internal m n)
		(newline)
		(display "Computing gcd of: ")
		(print (make-polynomial 'x m))
		(display " and ")
		(print (make-polynomial 'x n))
		(newline)
		(if (empty-termlist? n)
			m
			(gcd-terms-internal n (pseudoremainder-terms m n))
		)
	)

	(reduce-coefficients (gcd-terms-internal a b))
)

(define (reduce-coefficients term-list)
	(divide-terms-by-integer term-list (gcd-coefficients term-list))
)

(define (gcd-coefficients term-list)
	; Computes the gcd of all the coefficients of 'term-list'. Assumes that 
	; all the coefficients are integers
	; (display "Entered proc gcd-coefficients with term-list: ")
	; (display term-list)
	; (newline)
	(cond
		; ((empty-termlist? term-list) 1)
		; there is only one term, then the gcd is the coefficient of that term
		((empty-termlist? (rest-terms term-list)) (coeff (first-term term-list)))
		(else
			(greatest-common-divisor (coeff (first-term term-list)) (gcd-coefficients (rest-terms term-list)))
		)
	)
)

(define (divide-terms-by-integer terms factor)
	; (display "Entered proc divide-terms-by-integer with terms and factor: ")
	; (display terms)
	; (display ", ")
	; (display factor)
	; (newline)
	(cond
		((empty-termlist? terms) terms)
		(else
			(adjoin-term
				(scale-down-term (first-term terms) factor)
				(divide-terms-by-integer (rest-terms terms) factor)
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

(define (scale-down-poly-term t factor)
	(list (order-poly-term t) (div (coeff-poly-term t) factor))
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

; RATIONAL ARITHMETIC PROCEDURES
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rational-specific n d)
	(define (construct-rational n d)
		(cond
			((and (pair? n) (pair? d)) (cons n d))
			(else
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
		)
	)

	(cond
		; This will be the case when we are constructing a rational function in which the numerator
		; and denominator are polynomials
		((and (pair? n) (pair? d)) 
			(construct-rational n d)
		)
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

(define (add-rational x y)
	(make-rational-specific
		(add (mul (numer x) (denom y)) (mul (numer y) (denom x)))
		(mul (denom x) (denom y))
	)
)

(define (sub-rational x y)
	(make-rational-specific
		(sub (mul (numer x) (denom y)) (mul (numer y) (denom x)))
		(mul (denom x) (denom y))
	)
)

(define (mul-rational x y) (make-rational-specific (mul (numer x) (numer y)) (mul (denom x) (denom y))))

(define (div-rational x y) (make-rational-specific (mul (numer x) (denom y)) (mul (denom x) (numer y))))

(define (equal-rational? x y)
	; (display "Entered equal-rational?")
	; (newline)
	(and (equ? (numer x) (numer y)) (equ? (denom x) (denom y)))
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

(define (print-rational x)
	(print (numer x))
	(display " ")
	(display '/)
	(display " ")
	(print (denom x))
)

(define (square-root-rational rat)
	(if (>= (numer rat) 0)
		(sqrt (/ (numer rat) (denom rat)))
		(make-complex-from-real-imag 0 (sqrt (abs (/ (numer rat) (denom rat)))))
	)
)

(define (square-rational rat)
	(cons (mul (numer rat) (numer rat)) (mul (denom rat) (denom rat)))
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
					(cons '(integer integer) (lambda (x y) (attach-tag 'integer (+ x y))))
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
				'gcd
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (gcd x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (gcd x y))))
					(cons '(polynomial polynomial) (lambda (x y) (attach-tag 'polynomial (gcd-poly x y))))
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
				'scale-down-term
				(list
					(cons '(polynomial-term scheme-number) (lambda (term factor) (attach-tag 'polynomial-term (scale-down-poly-term term factor))))
					(cons '(polynomial-term real) (lambda (term factor) (attach-tag 'polynomial-term (scale-down-poly-term term factor))))
					(cons '(polynomial-term integer) (lambda (term factor) (attach-tag 'polynomial-term (scale-down-poly-term term factor))))
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
					(cons '(rational) print-rational)
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

(newline)

(define P1 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 1) (list 1 -2) (list 0 1)))))
(define P2 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 11) (list 0 7)))))
(define P3 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 13) (list 0 5)))))

(display "P1 is: ")
(print P1)
(newline)
(display "P2 is: ")
(print P2)
(newline)
(display "P3 is: ")
(print P3)
(newline)

(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))

(display "Q1 is: ")
(print Q1)
(newline)
(display "Q2 is: ")
(print Q2)
(newline)

(run-test greatest-common-divisor Q1 Q2)
(display "P1 is: ")
(print P1)
(newline)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.

P1 is: {x^2 - 2x + 1}
P2 is: {11x^2 + 7}
P3 is: {13x + 5}
Q1 is: {11x^4 - 22x^3 + 18x^2 - 14x + 7}
Q2 is: {13x^3 - 21x^2 + 3x + 5}
Running Test: (#<procedure:greatest-common-divisor> (polynomial x polynomial-sparse-terms (4 11) (3 (integer . -22)) (2 18) (1 (integer . -14)) (0 7) (0 0)) (polynomial x polynomial-sparse-terms (3 13) (2 (integer . -21)) (1 3) (0 5) (0 0))) 
Applying #<procedure:greatest-common-divisor> on: {11x^4 - 22x^3 + 18x^2 - 14x + 7}, {13x^3 - 21x^2 + 3x + 5}

Computing gcd of: {11x^4 - 22x^3 + 18x^2 - 14x + 7} and {13x^3 - 21x^2 + 3x + 5}
Integerizing factor is: 169

Computing gcd of: {13x^3 - 21x^2 + 3x + 5} and {1458x^2 - 2916x + 1458}
Integerizing factor is: 2125764

Computing gcd of: {1458x^2 - 2916x + 1458} and {}
Result: (polynomial x polynomial-sparse-terms (2 1) (1 (integer . -2)) (0 1) (0 0))
{x^2 - 2x + 1}

P1 is: {x^2 - 2x + 1}
> 
