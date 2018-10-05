#lang racket

; In this program I show that the final version of the generic framework and operations including
; polynomial, complex, real, rational and other math types works for all the tests from
; exercise 2.77 to exercise 2.97

; S O L U T I O N

; GENERIC PROCEDURES

; Generic Polynomial procedures

; Assumed ordering of variables is (in increasing order of priority):
; p, q, r, s, t, u, v, w, z, y, x
; I have implemented a generic print procedure so that polynomials and other types can be printed
; in an easy to read manner.
; The main work in this exercise lies in the 'convert-polynomial' procedure that transforms a
; polynomial from one variable to another

; Note: I have designed this with the assumption that the procedures adjoin-term, first-term
; and rest-terms though generic, will still be used only internally by the polynomial procedures.
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
(define (polynomial-order p) (order (first-term (term-list p))))
(define (coefficients term-list) (apply-generic 'coefficients term-list))

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

; Generic operation that reduces x
(define (reduce x y) (apply-generic 'reduce x y))

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
						((and (pair? coeff-first-term) (not (eq? (type-tag coeff-first-term) 'polynomial)) (lesser? coeff-first-term 0) first-time?) (display '-))
						((and (pair? coeff-first-term) (not (eq? (type-tag coeff-first-term) 'polynomial)) (lesser? coeff-first-term 0) (not first-time?)) (display " ") (display '-) (display " "))
						((and (pair? coeff-first-term) (not first-time?)) (display " ") (display '+) (display " "))
						((and (not (pair? coeff-first-term)) (> coeff-first-term 0) (not first-time?)) (display " ") (display '+) (display " "))
						((and (not (pair? coeff-first-term)) (< coeff-first-term 0)) (display " ") (display '-) (display " "))
						; (else
						; 	(display " ") (display '+) (display " ")
						; )
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

(define (print-polynomial-dense-terms terms)
	(display terms)
)

(define (is-poly? p)
	(if (pair? p)
		(equal? (type-tag p) 'polynomial)
		false
	)
)

(define (the-empty-poly-termlist) '())

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
							(let ((cip-rest-terms-poly (rest-terms-part-poly converted-inner-poly))
								  (cip-first-term-order (order (first-term (term-list converted-inner-poly))))
								  (cip-first-term-coeff (coeff (first-term (term-list converted-inner-poly)))))
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
									(convert-polynomial
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
			((= (order-poly-term term) (+ (polynomial-dense-terms-order term-list) 1))
				; no need to insert a zero in the coefficient list
				(cons (+ 1 (polynomial-dense-terms-order term-list)) (cons (coeff-poly-term term) (coefficients-dense term-list)))
			)
			((and (empty-termlist? term-list) (= 0 (order-poly-term term)))
				(list 0 (coeff-poly-term term))
			)
			((and (empty-termlist? term-list) (> (order-poly-term term) 0))
				(adjoin-term-dense term (list 0 0))
			)
			(else
				; we need to supply zero(s) if there are gaps
				(adjoin-term-dense term (cons (+ 1 (polynomial-dense-terms-order term-list)) (cons 0 (coefficients-dense term-list))))
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
			(list (polynomial-dense-terms-order term-list) (car (coefficients-dense term-list)))	
			null
		)
		null
	)
)

(define (rest-terms-dense term-list)
	; Since we are using the term-list representation that is appropriate for
	; dense polynomials (see SICP text), we need to do some extra processing
	; to retrieve both the order and coefficient
	(if (> (polynomial-dense-terms-order term-list) 0)
		(cons (- (polynomial-dense-terms-order term-list) 1) (cdr (coefficients-dense term-list)))
		(the-empty-poly-termlist)
	)
)

(define (polynomial-dense-terms-order term-list)
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

(define (reduce-poly p1 p2)
	(if (same-variable? (variable-poly p1) (variable-poly p2))
		(let ((reduced-terms (reduce-terms (term-list-poly p1) (term-list-poly p2))))
			(cons
				(make-poly (variable-poly p1) (car reduced-terms))
				(make-poly (variable-poly p1) (cdr reduced-terms))
			)
		)
		(error "Polys not in same var -- REDUCE-POLY"
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

(define (reduce-terms n d)
	(let ((gcd-of-n-and-d (gcd-terms n d))
		  (order-of-n (order (first-term n)))
		  (order-of-d (order (first-term d))))
		(let ((max-order (if (> order-of-n order-of-d) order-of-n order-of-d)))
			(let ((integerizing-factor (exp (coeff (first-term gcd-of-n-and-d)) (+ 1 (sub max-order (order (first-term gcd-of-n-and-d)))))))
				(let ((scaled-up-n (term-list (mul (make-polynomial 'x n) integerizing-factor)))
					  (scaled-up-d (term-list (mul (make-polynomial 'x d) integerizing-factor))))
					(let ((reduced-n (car (div-terms scaled-up-n gcd-of-n-and-d)))
						  (reduced-d (car (div-terms scaled-up-d gcd-of-n-and-d))))
						(let ((combined-integer-gcd (greatest-common-divisor (gcd-coefficients reduced-n) (gcd-coefficients reduced-d))))
							(cons
								(divide-terms-by-integer reduced-n combined-integer-gcd)
								(divide-terms-by-integer reduced-d combined-integer-gcd)
							)
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
	; Returns true if the polynomial is just a constant which is less than r
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
	; Consider both with tag and without tag
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
	; Returns true if the polynomial is just a constant which is less than r
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
		(add (REAL-PART z1) (add (REAL-PART z2) (add (REAL-PART z3) (REAL-PART z4))))
		(add (IMAG-PART z1) (add (IMAG-PART z2) (add (IMAG-PART z3) (IMAG-PART z4))))
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
			(display "[")
			(print (REAL-PART c))
			(display '+)
			(print (IMAG-PART c))
			(display 'i)
			(display "]")
		)
	)
)

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
		(reduce n d)
	)

	(cond
		; This will be the case when we are constructing a rational function in which the numerator
		; and/or denominator are polynomials or tagged types
		((or (pair? n) (pair? d)) 
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

(define (reduce-rat-integers n d)
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
	(and (equ? (numer x) (numer y)) (equ? (denom x) (denom y)))
)

(define (greater-rational? x y)
	(greater? (mul (numer x) (denom y)) (mul (denom x) (numer y)))
)

(define (lesser-rational? x y)
	(lesser? (mul (numer x) (denom y)) (mul (denom x) (numer y)))
)

(define (=zero-rational? x)
	(=zero? (numer x))
)

(define (print-rational x)
	(display "[")
	(print (numer x))
	(display " ")
	(display '/)
	(display " ")
	(print (denom x))
	(display "]")
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
	(make-real (div (numer r) (denom r)))
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

(define (=zero-integer? i)
	(= 0 i)
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
		(if (integer? x)
			; number is an integer
			(if (> x 0)
				; number is natural
				'natural
				'integer
			)
			'real
		)
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
					(cons '(natural) project-natural)
					(cons '(integer) project-int)
					(cons '(rational) project-rational)
					(cons '(real) project-real)
					(cons '(complex) project-complex)
				)
			)
			(cons
				'reduce
				(list
					(cons '(integer integer)
						(lambda (i1 i2)
							(let ((reduced-integers (reduce-rat-integers i1 i2)))
								(cons
									(car reduced-integers)
									(cdr reduced-integers)
								)
							)
						)
					)
					(cons '(polynomial polynomial)
						(lambda (p1 p2)
							(let ((reduced-polys (reduce-poly p1 p2)))
								(cons
									(attach-tag 'polynomial (car reduced-polys))
									(attach-tag 'polynomial (cdr reduced-polys))
								)
							)
						)
					)
				)
			)
			(cons
				'add
				(list
					(cons '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (+ x y))))
					(cons '(natural natural) (lambda (x y) (attach-tag 'natural (+ x y))))
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
					(cons '(natural natural) (lambda (x y) (attach-tag 'natural (- x y))))
					(cons '(integer integer) (lambda (x y) (attach-tag 'integer (- x y))))
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
					(cons '(natural natural) (lambda (x y) (attach-tag 'natural (* x y))))
					(cons '(integer integer) (lambda (x y) (attach-tag 'integer (* x y))))
					(cons '(rational rational) (lambda (x y) (attach-tag 'rational (mul-rational x y))))
					(cons '(real real) (lambda (x y) (attach-tag 'real (* x y))))
					(cons '(complex complex) (lambda (z1 z2) (attach-tag 'complex (mul-complex z1 z2))))
					(cons
						'(polynomial polynomial)
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
						'(polynomial natural)
						(lambda (p n)
							(attach-tag
								'polynomial
								; 'Raise' the natural object to a polynomial and then do ordinary multiplication
								(let ((pg (make-polynomial (variable-poly p) (make-polynomial-sparse-terms (list (list 0 n))))))
									(mul-poly p (contents pg))
								)
							)
						)
					)
					(cons
						'(polynomial integer)
						(lambda (p i)
							(attach-tag
								'polynomial
								; 'Raise' the integer object to a polynomial and then do ordinary multiplication
								(let ((pg (make-polynomial (variable-poly p) (make-polynomial-sparse-terms (list (list 0 i))))))
									(mul-poly p (contents pg))
								)
							)
						)
					)
					(cons
						'(polynomial rational)
						(lambda (p r)
							(attach-tag
								'polynomial
								; 'Raise' the rational object to a polynomial and then do ordinary multiplication
								(let ((pg (make-polynomial (variable-poly p) (make-polynomial-sparse-terms (list (list 0 (attach-tag 'rational r)))))))
									(mul-poly p (contents pg))
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
					(cons '(natural natural) (lambda (x y) (attach-tag 'scheme-number (/ x y))))
					(cons '(integer integer) (lambda (x y) (attach-tag 'scheme-number (/ x y))))
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
					(cons '(natural natural) (lambda (x y) (attach-tag 'natural (gcd x y))))
					(cons '(integer integer) (lambda (x y) (attach-tag 'integer (gcd x y))))
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
					(cons '(natural) (lambda (n) (attach-tag 'natural (square-natural n))))
					(cons '(integer) (lambda (i) (attach-tag 'integer (square-integer i))))
					(cons '(rational) (lambda (r) (attach-tag 'rational (square-rational r))))
					(cons '(real) (lambda (r) (attach-tag 'real (square-real r))))
					(cons '(complex) (lambda (z) (attach-tag 'complex (square-complex z))))
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
					(cons '(scheme-number) (lambda (x) (attach-tag 'real (sin x))))
					(cons '(natural) (lambda (x) (attach-tag 'real (sine-natural x))))
					(cons '(integer) (lambda (x) (attach-tag 'real (sine-integer x))))
					(cons '(rational) (lambda (x) (attach-tag 'real (sine-rational x))))
					(cons '(real) (lambda (x) (attach-tag 'real (sine-real x))))
				)
			)
			(cons
				'cosine
				(list
					(cons '(scheme-number) (lambda (x) (attach-tag 'real (cos x))))
					(cons '(natural) (lambda (x) (attach-tag 'real (cosine-natural x))))
					(cons '(integer) (lambda (x) (attach-tag 'real (cosine-integer x))))
					(cons '(rational) (lambda (x) (attach-tag 'real (cosine-rational x))))
					(cons '(real) (lambda (x) (attach-tag 'real (cosine-real x))))
				)
			)
			(cons
				'tan-inverse
				(list
					(cons '(natural natural) tan-inverse-natural)
					(cons '(integer integer) tan-inverse-integer)
					(cons '(rational rational) tan-inverse-rational)
					(cons '(real real) tan-inverse-real)
				)
			)
			(cons
				'mul-and-scale
				(list
					(cons '(complex complex scheme-number) mul-and-scale-complex)
					(cons '(complex complex integer) mul-and-scale-complex)
					(cons '(complex complex complex) mul-and-scale-complex)
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
					(cons '(polynomial natural) equal-polynomial-real?)
					(cons '(polynomial integer) equal-polynomial-real?)
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
					(cons '(integer) =zero-integer?)
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
					(cons '(polynomial-term natural) (lambda (term factor) (attach-tag 'polynomial-term (scale-down-poly-term term factor))))
					(cons '(polynomial-term integer) (lambda (term factor) (attach-tag 'polynomial-term (scale-down-poly-term term factor))))
					(cons '(polynomial-term rational) (lambda (term factor) (attach-tag 'polynomial-term (scale-down-poly-term term (attach-tag 'rational factor)))))
					(cons '(polynomial-term real) (lambda (term factor) (attach-tag 'polynomial-term (scale-down-poly-term term factor))))
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
				'coefficients
				(list
					(cons '(polynomial-dense-terms) coefficients-dense)
				)
			)
			(cons
				'print
				(list
					(cons '(boolean) (lambda (x) (if x (display 'True) (display 'False))))
					(cons '(natural) print-natural)
					(cons '(integer) print-int)
					(cons '(rational) print-rational)
					(cons '(real) print-real)
					(cons '(complex) print-complex)
					(cons '(polynomial) print-poly)
					(cons '(polynomial-dense-terms) print-polynomial-dense-terms)
				)
			)
			(cons
				'abs
				(list
					(cons '(natural) (lambda (x) (attach-tag 'natural (abs-natural x))))
					(cons '(integer) (lambda (x) (attach-tag 'integer (abs-integer x))))
					(cons '(real) (lambda (x) (attach-tag 'real (abs-real x))))
					(cons '(complex) (lambda (x) (attach-tag 'complex x)))
					(cons '(polynomial) (lambda (x) (attach-tag 'polynomial x)))
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
(display "TESTS FROM EXERCISE 2.77")
(newline)
(newline)

(make-scheme-number 7)
(define r1 (make-rational 8 9))
r1
(define r2 (make-rational 2 3))
r2
(define c1 (make-complex-from-real-imag 3 4))
c1
(define c2 (make-complex-from-real-imag 5 6))
c2
(define c3 (make-complex-from-mag-ang 2 (/ pi 3)))
c3
(define c4 (make-complex-from-mag-ang 2 (/ pi 6)))
c4
(define c5 (make-from-real-imag 8 9))
c5
(define c6 (make-from-mag-ang 4 (/ pi 4)))
c6

(run-test add r1 r2)
(run-test sub r1 r2)
(run-test mul r1 r2)
(run-test div r1 r2)
(run-test add c1 c2)
(run-test sub c1 c2)
(run-test mul c1 c2)
(run-test make-complex-from-real-imag (REAL-PART (mul c1 c2)) (IMAG-PART (mul c1 c2)))
(run-test div c1 c2)
(run-test make-complex-from-real-imag (REAL-PART (div c1 c2)) (IMAG-PART (div c1 c2)))
(run-test add c3 c4)
(run-test sub c3 c4)
(run-test mul c3 c4)
(run-test div c3 c4)

(define c (make-complex-from-real-imag 6 8))
(run-test magnitude c)

(newline)
(display "TESTS FROM EXERCISE 2.78. (Tests on scheme numbers)")
(newline)
(newline)

(run-test add 8 9)
(run-test make-scheme-number 45)
(run-test sub -54 34)
(run-test mul -32 3)
(run-test div -34 17)
(run-test make-scheme-number -439)
(run-test div 35 3)

(display "TESTS FROM EXERCISE 2.79")
(newline)
(newline)

(define n1 (make-scheme-number 7))
n1
(define n2 (make-scheme-number 11))
n2
(define n3 (make-scheme-number 7))
n3
(define r3 (make-rational -48 -54))
r3
(define c7 (make-complex-from-real-imag (/ 4 (sqrt 2)) (/ 4 (sqrt 2))))
c7
(define c8 (make-complex-from-real-imag (/ 4 (sqrt 2)) (/ 4 (sqrt 2))))
c8
(define c9 (make-complex-from-mag-ang 2 (/ pi 6)))
c9

(run-test make-rational 13 15)
(run-test make-rational -13 15)
(run-test make-rational 13 -15)
(run-test make-rational -13 -15)
(run-test equ? r1 r2)
(run-test equ? r2 r3)
(run-test equ? r3 r1)
(run-test equ? c1 c2)
(run-test equ? c3 c4)
(run-test equ? c7 c8)
(run-test equ? c4 c9)
(run-test equ? n1 n2)
(run-test equ? n2 n3)
(run-test equ? n1 n3)
(run-test equ? r1 r2)
(run-test equ? r2 r3)
(run-test equ? r1 r3)

(display "TESTS FROM EXERCISE 2.80")
(newline)
(newline)

(run-test =zero? 10)
(run-test =zero? 0)
(run-test =zero? -10)
(run-test make-rational 23 67)
(run-test =zero? (make-rational 23 67))
(run-test =zero? (make-rational 0 67))
(run-test make-rational 0 67)
(run-test =zero? (make-complex-from-real-imag 7 3))
(run-test =zero? (make-complex-from-real-imag 0 3))
(run-test =zero? (make-complex-from-real-imag 7 0))
(run-test =zero? (make-complex-from-real-imag 0 0))
(run-test =zero? (make-complex-from-mag-ang 10 3))
(run-test =zero? (make-complex-from-mag-ang 0 3))

(display "TESTS FROM EXERCISE 2.81")
(newline)
(newline)

(define c10 (make-complex-from-real-imag 11 12))
(define c11 (make-complex-from-real-imag 17 19))
(define r4 (make-rational 15 19))
(run-test exp 4 3)
; The following will fail, so commented them out
; (run-test exp c10 c11)
; (run-test exp c10 4)

(display "TESTS FROM EXERCISE 2.82")
(newline)
(newline)

(define z1 (make-complex-from-mag-ang 2 0.1))
(define z2 (make-complex-from-mag-ang 3 0.2))
(define z3 (make-complex-from-real-imag 2 3))
(define z4 (make-complex-from-real-imag 4 6))
(define z5 (make-complex-from-real-imag 1 5))
(define z6 (make-complex-from-real-imag 5 12))

(run-test mul z1 z2)
(run-test add-four-quantities z3 z4 z5 z6)
(run-test add-four-quantities z3 z4 z5 r1)
(run-test add-four-quantities z3 z4 r1 z6)
(run-test add-four-quantities z3 r1 z5 z6)
(run-test add-four-quantities r1 z4 z5 z6)
(run-test add-four-quantities r1 r1 z5 z6)
(run-test add-four-quantities z3 r1 z5 r1)
(run-test add-four-quantities z3 z4 r1 r1)
(run-test add-four-quantities z3 r1 r1 r1)
(run-test add-four-quantities r1 r1 r1 r1)
(run-test mul-and-scale z1 z2 10)
(run-test make-complex-from-mag-ang (magnitude (mul-and-scale z1 z2 10)) (angle (mul-and-scale z1 z2 10)))

(display "TESTS FROM EXERCISE 2.83")
(newline)
(newline)

(define s (make-scheme-number 25))
(define i (make-integer 26))
(define rat (make-rational 27 4))
(define real1 (make-real 28))
(define real2 (make-real 28.6))
(define real3 (make-real (sqrt 28.6)))

(run-test raise i)
(run-test raise rat)
(run-test raise real1)
; (make-integer 26.1)
; (run-test make-rational 27 0)
make-real (sqrt -28.6)
(run-test raise (raise (raise i)))

(display "TESTS FROM EXERCISE 2.84")
(newline)
(newline)

(define r5 (make-rational 9 16))

(define i1 (make-integer 35))
(define i2 (make-integer 17))
(define i3 (make-integer 236))
(define i4 (make-integer 29))

(define real4 (make-real (sqrt 10)))

(run-test mul-five-quantities r1 r2 r3 r4 r5)
(run-test mul-five-quantities i1 r2 r3 r4 r5)

(run-test add-four-quantities i1 i2 i3 i4)

(run-test add-four-quantities i1 r4 z1 real4)

(display "TESTS FROM EXERCISE 2.85")
(newline)
(newline)

(define n1-2.85 (make-natural 46))

(define i1-2.85 (make-integer 35))
(define i2-2.85 (make-integer 17))
(define i3-2.85 (make-integer 236))
(define i4-2.85 (make-integer 29))

(define r1-2.85 (make-rational 100 25))
(define r2-2.85 (make-rational 2 5))
(define r3-2.85 (make-rational 6 5))
(define r4-2.85 (make-rational 7 -2))
(define r5-2.85 (make-rational 9 16))

(define real1-2.85 (make-real 65.0))
(define real2-2.85 (make-real (sqrt 10)))

(define z1-2.85 (make-complex-from-real-imag 23 12))
(define z2-2.85 (make-complex-from-real-imag 29.0 0))
(define z3-2.85 (make-complex-from-real-imag 29.5 0))
(define z4-2.85 (make-complex-from-real-imag 29.5 4.6))
(define z5-2.85 (make-complex-from-real-imag 89.3 348))
(define z6-2.85 (make-complex-from-real-imag 89.7 -348))

; Projection Tests
(display "Projection Tests")
(newline)

(run-test project i1-2.85)
(run-test project r1-2.85)
(run-test project real1-2.85)
(run-test project z1-2.85)

(newline)

z2-2.85
(run-test project z2-2.85)
(run-test project (project z2-2.85))
(run-test project (project (project z2-2.85)))
(run-test project (project (project (project z2-2.85))))

; Drop tests

(display "Drop Tests")
(newline)

(display "Dropping: ")
(display i1-2.85)
(display ": ")
(run-test drop i1-2.85)
(display "Dropping: ")
(display r5-2.85)
(display ": ")
(run-test drop r5-2.85)
(display "Dropping: ")
(display real2-2.85)
(display ": ")
(run-test drop real2-2.85)

(display "Dropping: ")
(display z1-2.85)
(display ": ")
(run-test drop z1-2.85)
(display "Dropping: ")
(display z2-2.85)
(display ": ")
(run-test drop z2-2.85)
(display "Dropping: ")
(display z3-2.85)
(display ": ")
(run-test drop z3-2.85)
(display "Dropping: ")
(display z4-2.85)
(display ": ")
(run-test drop z4-2.85)

(display "Testing proc add-four-quantities")
(newline)
(run-test add-four-quantities i1-2.85 i2-2.85 i3-2.85 i4-2.85)
(run-test add-four-quantities i1-2.85 r4-2.85 z1-2.85 real1-2.85)
(display "Testing proc mul-five-quantities")
(newline)
(run-test mul-five-quantities r1-2.85 r2-2.85 r3-2.85 r4-2.85 r5-2.85)
(run-test mul-five-quantities i1-2.85 r2-2.85 r3-2.85 r4-2.85 r5-2.85)

(display "Testing proc mul-and-scale")
(newline)
(run-test mul-and-scale z4-2.85 z5-2.85 200)

(display "z5-2.85: ")
z5-2.85
(newline)
(display "z6-2.85: ")
z6-2.85
(newline)
(display "z5-2.85 + z6-2.85: ")
(run-test add z5-2.85 z6-2.85)
(newline)
(run-test project n1-2.85)

(display "TESTS FROM EXERCISE 2.86")
(newline)
(newline)

(define sn1-2.86 (make-scheme-number 25))
(display "Created ") (display sn1-2.86) (newline)
(define sn2-2.86 (make-scheme-number 26))
(display "Created ") (display sn2-2.86) (newline)
(define n1-2.86 (make-natural 30))
(display "Created ") (display n1-2.86) (newline)
(define n2-2.86 (make-natural 31))
(display "Created ") (display n2-2.86) (newline)
(define i1-2.86 (make-integer 35))
(display "Created ") (display i1-2.86) (newline)
(define i2-2.86 (make-integer 36))
(display "Created ") (display i2-2.86) (newline)
(define rat1-2.86 (make-rational 40 43))
(display "Created ") (display rat1-2.86) (newline)
(define rat2-2.86 (make-rational 44 47))
(display "Created ") (display rat2-2.86) (newline)
(define r1-2.86 (make-real 45.3))
(display "Created ") (display r1-2.86) (newline)
(define r2-2.86 (make-real (sqrt 45.3)))
(display "Created ") (display r2-2.86) (newline)

(define zri1-2.86 (make-complex-from-real-imag 50 55))
(display "Created ") (display zri1-2.86) (newline)
(define zri2-2.86 (make-complex-from-real-imag sn1-2.86 sn2-2.86))
(display "Created ") (display zri2-2.86) (newline)
(define zri3-2.86 (make-complex-from-real-imag n1-2.86 n2-2.86))
(display "Created ") (display zri3-2.86) (newline)
(define zri4-2.86 (make-complex-from-real-imag i1-2.86 i2-2.86))
(display "Created ") (display zri4-2.86) (newline)
(define zri5-2.86 (make-complex-from-real-imag rat1-2.86 rat2-2.86))
(display "Created ") (display zri5-2.86) (newline)
(define zri6-2.86 (make-complex-from-real-imag r1-2.86 r2-2.86))
(display "Created ") (display zri6-2.86) (newline)
(define zri7-2.86 (make-complex-from-real-imag rat1-2.86 r1-2.86))
(display "Created ") (display zri7-2.86) (newline)

(define zma1-2.86 (make-complex-from-mag-ang 50 55))
(display "Created ") (display zma1-2.86) (newline)
(define zma2-2.86 (make-complex-from-mag-ang sn1-2.86 sn2-2.86))
(display "Created ") (display zma2-2.86) (newline)
(define zma3-2.86 (make-complex-from-mag-ang n1-2.86 n2-2.86))
(display "Created ") (display zma3-2.86) (newline)
(define zma4-2.86 (make-complex-from-mag-ang i1-2.86 i2-2.86))
(display "Created ") (display zma4-2.86) (newline)
(define zma5-2.86 (make-complex-from-mag-ang rat1-2.86 rat2-2.86))
(display "Created ") (display zma5-2.86) (newline)
(define zma6-2.86 (make-complex-from-mag-ang r1-2.86 r2-2.86))
(display "Created ") (display zma6-2.86) (newline)
(define zma7-2.86 (make-complex-from-mag-ang r2-2.86 rat2-2.86))
(display "Created ") (display zma7-2.86) (newline)

(newline)

(run-test square 0.0)
(run-test square sn1-2.86)
(run-test square sn2-2.86)
(run-test square n1-2.86)
(run-test square n2-2.86)
(run-test square i1-2.86)
(run-test square i2-2.86)
(run-test square rat1-2.86)
(run-test square rat2-2.86)
(run-test square r1-2.86)
(run-test square r2-2.86)
(run-test square zri1-2.86)
(run-test square zri2-2.86)
(run-test square zri3-2.86)
(run-test square zri4-2.86)
(run-test square zri5-2.86)
(run-test square zri6-2.86)
(run-test square zri7-2.86)
(run-test square zma1-2.86)
(run-test square zma2-2.86)
(run-test square zma3-2.86)
(run-test square zma4-2.86)
(run-test square zma5-2.86)
(run-test square zma6-2.86)
(run-test square zma7-2.86)

(newline)

(run-test square-root 0.0)
(run-test square-root sn1-2.86)
(run-test square-root sn2-2.86)
(run-test square-root n1-2.86)
(run-test square-root n2-2.86)
(run-test square-root i1-2.86)
(run-test square-root i2-2.86)
(run-test square-root rat1-2.86)
(run-test square-root rat2-2.86)
(run-test square-root r1-2.86)
(run-test square-root r2-2.86)

(newline)

(run-test =zero? 0.0)
(run-test =zero? sn1-2.86)
(run-test =zero? sn2-2.86)
(run-test =zero? n1-2.86)
(run-test =zero? n2-2.86)
(run-test =zero? i1-2.86)
(run-test =zero? i2-2.86)
(run-test =zero? rat1-2.86)
(run-test =zero? rat2-2.86)
(run-test =zero? r1-2.86)
(run-test =zero? r2-2.86)
(run-test =zero? zri1-2.86)
(run-test =zero? zri2-2.86)
(run-test =zero? zri3-2.86)
(run-test =zero? zri4-2.86)
(run-test =zero? zri5-2.86)
(run-test =zero? zri6-2.86)
(run-test =zero? zri7-2.86)
(run-test =zero? zma1-2.86)
(run-test =zero? zma2-2.86)
(run-test =zero? zma3-2.86)
(run-test =zero? zma4-2.86)
(run-test =zero? zma5-2.86)
(run-test =zero? zma6-2.86)
(run-test =zero? zma7-2.86)

(newline)

(run-test equ? 0.0 9.0)
(run-test equ? 9 9.0)
(run-test equ? sn1-2.86 sn1-2.86)
(run-test equ? sn1-2.86 sn2-2.86)
(run-test equ? n1-2.86 n2-2.86)
(run-test equ? n1-2.86 i2-2.86)
(run-test equ? i1-2.86 i2-2.86)
(run-test equ? i1-2.86 n2-2.86)
(run-test equ? i1-2.86 i1-2.86)
(run-test equ? n2-2.86 n2-2.86)
(run-test equ? rat1-2.86 rat2-2.86)
(run-test equ? rat2-2.86 rat2-2.86)
(run-test equ? zri1-2.86 zri2-2.86)
(run-test equ? zma1-2.86 zma2-2.86)
(run-test equ? zri2-2.86 zri2-2.86)
(run-test equ? zma1-2.86 zma1-2.86)
(run-test equ? zma7-2.86 zri7-2.86)

(newline)

(run-test REAL-PART zri1-2.86)
(run-test IMAG-PART zri1-2.86)
(run-test REAL-PART zri2-2.86)
(run-test IMAG-PART zri2-2.86)
(run-test REAL-PART zri3-2.86)
(run-test IMAG-PART zri3-2.86)
(run-test REAL-PART zri4-2.86)
(run-test IMAG-PART zri4-2.86)
(run-test REAL-PART zri5-2.86)
(run-test IMAG-PART zri5-2.86)
(run-test REAL-PART zri6-2.86)
(run-test IMAG-PART zri6-2.86)
(run-test REAL-PART zri7-2.86)
(run-test IMAG-PART zri7-2.86)

(newline)

(run-test magnitude zma1-2.86)
(run-test angle zma1-2.86)
(run-test magnitude zma2-2.86)
(run-test angle zma2-2.86)
(run-test magnitude zma3-2.86)
(run-test angle zma3-2.86)
(run-test magnitude zma4-2.86)
(run-test angle zma4-2.86)
(run-test magnitude zma5-2.86)
(run-test angle zma5-2.86)
(run-test magnitude zma6-2.86)
(run-test angle zma6-2.86)
(run-test magnitude zma7-2.86)
(run-test angle zma7-2.86)
 
(newline)
 
(run-test REAL-PART zma1-2.86)
(run-test IMAG-PART zma1-2.86)
(run-test REAL-PART zma2-2.86)
(run-test IMAG-PART zma2-2.86)
(run-test REAL-PART zma3-2.86)
(run-test IMAG-PART zma3-2.86)
(run-test REAL-PART zma4-2.86)
(run-test IMAG-PART zma4-2.86)
(run-test REAL-PART zma5-2.86)
(run-test IMAG-PART zma5-2.86)
(run-test REAL-PART zma6-2.86)
(run-test IMAG-PART zma6-2.86)
(run-test REAL-PART zma7-2.86)
(run-test IMAG-PART zma7-2.86)

(newline)

(run-test magnitude zri1-2.86)
(run-test angle zri1-2.86)
(run-test magnitude zri2-2.86)
(run-test angle zri2-2.86)
(run-test magnitude zri3-2.86)
(run-test angle zri3-2.86)
(run-test magnitude zri4-2.86)
(run-test angle zri4-2.86)
(run-test magnitude zri5-2.86)
(run-test angle zri5-2.86)
(run-test magnitude zri6-2.86)
(run-test angle zri6-2.86)
(run-test magnitude zri7-2.86)
(run-test angle zri7-2.86)
 
(newline)

(run-test add 0.0 9.0)
(run-test add 9 9.0)
(run-test add sn1-2.86 sn1-2.86)
(run-test add sn1-2.86 sn2-2.86)
(run-test add n1-2.86 n2-2.86)
(run-test add n1-2.86 i2-2.86)
(run-test add i1-2.86 i2-2.86)
(run-test add i1-2.86 n2-2.86)
(run-test add i1-2.86 i1-2.86)
(run-test add n2-2.86 n2-2.86)
(run-test add rat1-2.86 n2-2.86)
(run-test add rat1-2.86 rat2-2.86)
(run-test add rat2-2.86 rat2-2.86)
(run-test add zri1-2.86 zri2-2.86)
(run-test add zma1-2.86 zma2-2.86)
(run-test add zri2-2.86 zri2-2.86)
(run-test add zma1-2.86 zma1-2.86)
(run-test add zma1-2.86 9.0)
(run-test add zma1-2.86 sn1-2.86)
(run-test add n1-2.86 zma1-2.86)
(run-test add i1-2.86 zma1-2.86)
(run-test add rat1-2.86 zma1-2.86)
(run-test add zri1-2.86 zma1-2.86)
(run-test add zri7-2.86 zma7-2.86)
(run-test add rat1-2.86 zri4-2.86)

(newline)

(run-test sub 0.0 9.0)
(run-test sub 9 9.0)
(run-test sub sn1-2.86 sn1-2.86)
(run-test sub sn1-2.86 sn2-2.86)
(run-test sub n1-2.86 n2-2.86)
(run-test sub n1-2.86 i2-2.86)
(run-test sub i1-2.86 i2-2.86)
(run-test sub i1-2.86 n2-2.86)
(run-test sub i1-2.86 i1-2.86)
(run-test sub n2-2.86 n2-2.86)
(run-test sub rat1-2.86 n2-2.86)
(run-test sub rat1-2.86 rat2-2.86)
(run-test sub rat2-2.86 rat2-2.86)
(run-test sub zri1-2.86 zri2-2.86)
(run-test sub zma1-2.86 zma2-2.86)
(run-test sub zri2-2.86 zri2-2.86)
(run-test sub zma1-2.86 zma1-2.86)
(run-test sub zma1-2.86 9.0)
(run-test sub zma1-2.86 sn1-2.86)
(run-test sub n1-2.86 zma1-2.86)
(run-test sub i1-2.86 zma1-2.86)
(run-test sub rat1-2.86 zma1-2.86)
(run-test sub zri1-2.86 zma1-2.86)
(run-test sub zri7-2.86 zma7-2.86)
(run-test sub rat1-2.86 zri4-2.86)

(newline)

(run-test mul 0.0 9.0)
(run-test mul 9 9.0)
(run-test mul sn1-2.86 sn1-2.86)
(run-test mul sn1-2.86 sn2-2.86)
(run-test mul n1-2.86 n2-2.86)
(run-test mul n1-2.86 i2-2.86)
(run-test mul i1-2.86 i2-2.86)
(run-test mul i1-2.86 n2-2.86)
(run-test mul i1-2.86 i1-2.86)
(run-test mul n2-2.86 n2-2.86)
(run-test mul rat1-2.86 n2-2.86)
(run-test mul rat1-2.86 rat2-2.86)
(run-test mul rat2-2.86 rat2-2.86)
(run-test mul zri1-2.86 zri2-2.86)
(run-test mul zma1-2.86 zma2-2.86)
(run-test mul zri2-2.86 zri2-2.86)
(run-test mul zma1-2.86 zma1-2.86)
(run-test mul zma1-2.86 9.0)
(run-test mul zma1-2.86 sn1-2.86)
(run-test mul n1-2.86 zma1-2.86)
(run-test mul i1-2.86 zma1-2.86)
(run-test mul rat1-2.86 zma1-2.86)
(run-test mul zri1-2.86 zma1-2.86)
(run-test mul zri7-2.86 zma7-2.86)
(run-test mul rat1-2.86 zri4-2.86)

(newline)

(run-test div 0.0 9.0)
(run-test div 9 9.0)
(run-test div sn1-2.86 sn1-2.86)
(run-test div sn1-2.86 sn2-2.86)
(run-test div n1-2.86 n2-2.86)
(run-test div n1-2.86 i2-2.86)
(run-test div i1-2.86 i2-2.86)
(run-test div i1-2.86 n2-2.86)
(run-test div i1-2.86 i1-2.86)
(run-test div n2-2.86 n2-2.86)
(run-test div rat1-2.86 n2-2.86)
(run-test div rat1-2.86 rat2-2.86)
(run-test div rat2-2.86 rat2-2.86)
(run-test div zri1-2.86 zri2-2.86)
(run-test div zma1-2.86 zma2-2.86)
(run-test div zri2-2.86 zri2-2.86)
(run-test div zma1-2.86 zma1-2.86)
(run-test div zma1-2.86 9.0)
(run-test div zma1-2.86 sn1-2.86)
(run-test div n1-2.86 zma1-2.86)
(run-test div i1-2.86 zma1-2.86)
(run-test div rat1-2.86 zma1-2.86)
(run-test div zri1-2.86 zma1-2.86)
(run-test div zri7-2.86 zma7-2.86)
(run-test div rat1-2.86 zri4-2.86)

(newline)

(display "TESTS FROM EXERCISE 2.87 and 2.88")
(newline)
(newline)

(define p1 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 5 1) (list 4 2) (list 2 3) (list 1 -2) (list 0 -5)))))
(define p2 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 5 2) (list 4 4) (list 2 5) (list 1 -7) (list 0 -15)))))

(run-test =zero? p1)
(run-test =zero? p2)
(run-test add p1 p2)
(run-test mul p1 p2)
(run-test NEGATE p1)
(run-test NEGATE p2)
(run-test sub p1 p2)
(run-test sub p1 p1)
(run-test =zero? (sub p1 p1))

(display "TESTS FROM EXERCISE 2.89")
(newline)
(newline)

(define sp1-2.89 (make-polynomial 'x (make-polynomial-dense-terms (list 1 3 5))))
(define sp2-2.89 (make-polynomial 'x (make-polynomial-dense-terms (list 1 4 9))))
(define p1-2.89 (make-polynomial 'x (make-polynomial-dense-terms (list 2 3 5 23))))
(define p2-2.89 (make-polynomial 'x (make-polynomial-dense-terms (list 2 4 9 19))))
(define p3-2.89 (make-polynomial 'x (make-polynomial-dense-terms (list 5 1 2 0 3 -2 -5))))
(define p4-2.89 (make-polynomial 'x (make-polynomial-dense-terms (list 5 2 4 0 5 -7 -15))))

(run-test term-list sp1-2.89)
(variable sp1-2.89)
(run-test term-list sp2-2.89)
(variable sp2-2.89)
(run-test term-list p1-2.89)
(variable p1-2.89)
(run-test term-list p2-2.89)
(variable p2-2.89)

(run-test =zero? sp1-2.89)
(run-test =zero? sp2-2.89)
(run-test =zero? p1-2.89)
(run-test =zero? p2-2.89)
(run-test =zero? p3-2.89)
(run-test =zero? p4-2.89)

(run-test add sp1-2.89 sp2-2.89)
(run-test add p1-2.89 p2-2.89)
(run-test add p3-2.89 p4-2.89)
(run-test add p1-2.89 p4-2.89)
(run-test add p2-2.89 p3-2.89)

(run-test mul sp1-2.89 sp2-2.89)
(run-test mul p1-2.89 p2-2.89)
(run-test mul p3-2.89 p4-2.89)
(run-test mul p1-2.89 p3-2.89)
(run-test mul p2-2.89 p4-2.89)

(run-test NEGATE p1-2.89)
(run-test NEGATE p2-2.89)
(run-test sub p1-2.89 p2-2.89)
(run-test sub p1-2.89 p1-2.89)
(run-test sub p1-2.89 p4-2.89)
(run-test sub p3-2.89 p2-2.89)
(run-test =zero? (sub p1-2.89 p1-2.89))
(run-test =zero? (sub p4-2.89 p4-2.89))

(run-test rest-terms (term-list p1-2.89))
(run-test rest-terms (term-list p2-2.89))

(run-test polynomial-order p1-2.89)
(run-test polynomial-order p2-2.89)
(run-test polynomial-order p3-2.89)
(run-test polynomial-order p4-2.89)

(coefficients (term-list p1-2.89))
(coefficients (term-list p2-2.89))
(coefficients (term-list p3-2.89))
(coefficients (term-list p4-2.89))

(display "TESTS FROM EXERCISE 2.90")
(newline)
(newline)

(define t1-2.90-sparse (make-polynomial-sparse-terms (list (list 0 0))))
(define t2-2.90-sparse (make-polynomial-sparse-terms (list (list 0 5))))
(define t3-2.90-sparse (make-polynomial-sparse-terms (list (list 1 7) (list 0 5))))
(define t4-2.90-sparse (make-polynomial-sparse-terms (list (list 1 9) (list 0 -25))))
(define t5-2.90-sparse (make-polynomial-sparse-terms (list (list 5 1) (list 4 2) (list 2 3) (list 1 -2) (list 0 -5))))
(define t6-2.90-sparse (make-polynomial-sparse-terms (list (list 5 2) (list 4 4) (list 2 5) (list 1 -7) (list 0 -15))))
(define t7-2.90-sparse (make-polynomial-sparse-terms (list (list 10 3) (list 7 4) (list 5 -11) (list 1 -7) (list 0 -15))))
(define t8-2.90-sparse (make-polynomial-sparse-terms (list (list 12 4) (list 6 13) (list 5 -12) (list 1 -7) (list 0 -15))))

(define t1-2.90-dense (make-polynomial-dense-terms (list 0 0)))
(define t2-2.90-dense (make-polynomial-dense-terms (list 0 5)))
(define t3-2.90-dense (make-polynomial-dense-terms (list 1 3 5)))
(define t4-2.90-dense (make-polynomial-dense-terms (list 1 4 9)))
(define t5-2.90-dense (make-polynomial-dense-terms (list 2 3 5 23)))
(define t6-2.90-dense (make-polynomial-dense-terms (list 2 4 9 19)))
(define t7-2.90-dense (make-polynomial-dense-terms (list 5 1 2 0 3 -2 -5)))
(define t8-2.90-dense (make-polynomial-dense-terms (list 5 2 4 0 5 -7 -15)))

(define sp1-2.90 (make-polynomial 'x t1-2.90-sparse))
(define sp2-2.90 (make-polynomial 'x t2-2.90-sparse))
(define sp3-2.90 (make-polynomial 'x t3-2.90-sparse))
(define sp4-2.90 (make-polynomial 'x t4-2.90-sparse))
(define sp5-2.90 (make-polynomial 'x t5-2.90-sparse))
(define sp6-2.90 (make-polynomial 'x t6-2.90-sparse))
(define sp7-2.90 (make-polynomial 'x t7-2.90-sparse))
(define sp8-2.90 (make-polynomial 'x t8-2.90-sparse))

(define dp1-2.90 (make-polynomial 'x t1-2.90-dense))
(define dp2-2.90 (make-polynomial 'x t2-2.90-dense))
(define dp3-2.90 (make-polynomial 'x t3-2.90-dense))
(define dp4-2.90 (make-polynomial 'x t4-2.90-dense))
(define dp5-2.90 (make-polynomial 'x t5-2.90-dense))
(define dp6-2.90 (make-polynomial 'x t6-2.90-dense))
(define dp7-2.90 (make-polynomial 'x t7-2.90-dense))
(define dp8-2.90 (make-polynomial 'x t8-2.90-dense))

(run-test =zero? sp1-2.90)
(run-test =zero? sp2-2.90)
(run-test =zero? sp3-2.90)
(run-test =zero? sp4-2.90)

(run-test =zero? dp1-2.90)
(run-test =zero? dp2-2.90)
(run-test =zero? dp3-2.90)
(run-test =zero? dp4-2.90)
(run-test =zero? dp5-2.90)
(run-test =zero? dp6-2.90)

sp1-2.90
(define term1-2.90 (make-term 6 20))
term1-2.90

sp2-2.90
(define term2-2.90 (make-term 7 20))
term2-2.90

sp3-2.90
(define term3-2.90 (make-term 9 20))
term3-2.90

sp4-2.90
(define term4-2.90 (make-term 9 40))
term4-2.90

dp1-2.90
dp2-2.90
dp3-2.90
dp4-2.90
dp5-2.90
dp6-2.90

dp5-2.90
(define term5-2.90 (make-term 8 20))
term5-2.90

dp6-2.90
(define term6-2.90 (make-term 30 20))
term6-2.90

(run-test add sp1-2.90 sp1-2.90)
(run-test add sp1-2.90 sp2-2.90)
(run-test add sp2-2.90 sp3-2.90)
(run-test add sp3-2.90 sp4-2.90)
(run-test add sp4-2.90 sp5-2.90)
(run-test add sp5-2.90 sp6-2.90)
(run-test add sp6-2.90 sp7-2.90)
(run-test add sp7-2.90 sp8-2.90)

(newline)

(run-test add dp1-2.90 dp1-2.90)
(run-test add dp1-2.90 dp2-2.90)
(run-test add dp2-2.90 dp3-2.90)
(run-test add dp3-2.90 dp4-2.90)
(run-test add dp4-2.90 dp5-2.90)
(run-test add dp5-2.90 dp6-2.90)
(run-test add dp6-2.90 dp7-2.90)
(run-test add dp7-2.90 dp8-2.90)

(newline)

(run-test add sp1-2.90 dp2-2.90)
(run-test add dp2-2.90 sp1-2.90)
(run-test add sp2-2.90 dp3-2.90)
(run-test add sp3-2.90 dp4-2.90)
(run-test add sp4-2.90 dp5-2.90)
(run-test add sp5-2.90 dp6-2.90)
(run-test add sp6-2.90 dp7-2.90)
(run-test add sp7-2.90 dp8-2.90)

(newline)

(run-test add dp1-2.90 sp2-2.90)
(run-test add sp2-2.90 dp1-2.90)
(run-test add dp2-2.90 sp3-2.90)
(run-test add dp3-2.90 sp4-2.90)
(run-test add dp4-2.90 sp5-2.90)
(run-test add dp5-2.90 sp6-2.90)
(run-test add dp6-2.90 sp7-2.90)
(run-test add dp7-2.90 sp8-2.90)

(newline)

(run-test mul sp1-2.90 sp1-2.90)
(run-test mul sp1-2.90 sp2-2.90)
(run-test mul sp2-2.90 sp2-2.90)
(run-test mul sp2-2.90 sp3-2.90)
(run-test mul sp3-2.90 sp4-2.90)
(run-test mul sp4-2.90 sp5-2.90)
(run-test mul sp5-2.90 sp6-2.90)
(run-test mul sp6-2.90 sp7-2.90)
(run-test mul sp7-2.90 sp8-2.90)

(newline)

(run-test mul dp1-2.90 dp1-2.90)
(run-test mul dp1-2.90 dp2-2.90)
(run-test mul dp2-2.90 dp2-2.90)
(run-test mul dp2-2.90 dp3-2.90)
(run-test mul dp3-2.90 dp4-2.90)
(run-test mul dp4-2.90 dp5-2.90)
(run-test mul dp5-2.90 dp6-2.90)
(run-test mul dp6-2.90 dp7-2.90)
(run-test mul dp7-2.90 dp8-2.90)

(newline)

(run-test mul sp1-2.90 dp2-2.90)
(run-test mul dp2-2.90 sp1-2.90)
(run-test mul sp2-2.90 dp3-2.90)
(run-test mul sp3-2.90 dp4-2.90)
(run-test mul sp4-2.90 dp5-2.90)
(run-test mul sp5-2.90 dp6-2.90)
(run-test mul sp6-2.90 dp7-2.90)
(run-test mul sp7-2.90 dp8-2.90)

(newline)

(run-test mul dp1-2.90 sp2-2.90)
(run-test mul sp2-2.90 dp1-2.90)
(run-test mul dp2-2.90 sp3-2.90)
(run-test mul dp3-2.90 sp4-2.90)
(run-test mul dp4-2.90 sp5-2.90)
(run-test mul dp5-2.90 sp6-2.90)
(run-test mul dp6-2.90 sp7-2.90)
(run-test mul dp7-2.90 sp8-2.90)

(newline)

(run-test NEGATE sp1-2.90)
(run-test NEGATE sp2-2.90)
(run-test NEGATE sp2-2.90)
(run-test NEGATE sp3-2.90)
(run-test NEGATE sp4-2.90)
(run-test NEGATE sp5-2.90)
(run-test NEGATE sp6-2.90)
(run-test NEGATE sp7-2.90)
(run-test NEGATE sp8-2.90)

(newline)

(run-test NEGATE dp1-2.90)
(run-test NEGATE dp2-2.90)
(run-test NEGATE dp2-2.90)
(run-test NEGATE dp3-2.90)
(run-test NEGATE dp4-2.90)
(run-test NEGATE dp5-2.90)
(run-test NEGATE dp6-2.90)
(run-test NEGATE dp7-2.90)
(run-test NEGATE dp8-2.90)

(run-test sub sp1-2.90 sp1-2.90)
(run-test sub sp1-2.90 sp2-2.90)
(run-test sub sp2-2.90 sp2-2.90)
(run-test sub sp2-2.90 sp3-2.90)
(run-test sub sp3-2.90 sp4-2.90)
(run-test sub sp4-2.90 sp5-2.90)
(run-test sub sp5-2.90 sp6-2.90)
(run-test sub sp6-2.90 sp7-2.90)
(run-test sub sp7-2.90 sp8-2.90)

(newline)

(run-test sub dp1-2.90 dp1-2.90)
(run-test sub dp1-2.90 dp2-2.90)
(run-test sub dp2-2.90 dp2-2.90)
(run-test sub dp2-2.90 dp3-2.90)
(run-test sub dp3-2.90 dp4-2.90)
(run-test sub dp4-2.90 dp5-2.90)
(run-test sub dp5-2.90 dp6-2.90)
(run-test sub dp6-2.90 dp7-2.90)
(run-test sub dp7-2.90 dp8-2.90)

(newline)

(run-test sub sp1-2.90 dp2-2.90)
(run-test sub dp2-2.90 sp1-2.90)
(run-test sub sp2-2.90 dp3-2.90)
(run-test sub sp3-2.90 dp4-2.90)
(run-test sub sp4-2.90 dp5-2.90)
(run-test sub sp5-2.90 dp6-2.90)
(run-test sub sp6-2.90 dp7-2.90)
(run-test sub sp7-2.90 dp8-2.90)

(newline)

(run-test sub dp1-2.90 sp2-2.90)
(run-test sub sp2-2.90 dp1-2.90)
(run-test sub dp2-2.90 sp3-2.90)
(run-test sub dp3-2.90 sp4-2.90)
(run-test sub dp4-2.90 sp5-2.90)
(run-test sub dp5-2.90 sp6-2.90)
(run-test sub dp6-2.90 sp7-2.90)
(run-test sub dp7-2.90 sp8-2.90)

(newline)

(run-test =zero? (sub dp6-2.90 dp6-2.90))
(run-test =zero? (sub sp4-2.90 sp4-2.90))

(display "TESTS FROM EXERCISE 2.91")
(newline)
(newline)

(define p-1-divisor-terms (make-polynomial-sparse-terms (list (list 1 1))))
(define p-1-dividend-terms (make-polynomial-sparse-terms (list (list 1 1))))

(define p-1-divisor (make-polynomial 'x p-1-divisor-terms))
(define p-1-dividend (make-polynomial 'x p-1-dividend-terms))

(define p0-divisor-terms (make-polynomial-sparse-terms (list (list 4 1))))
(define p0-dividend-terms (make-polynomial-sparse-terms (list (list 6 1))))

(define p0-divisor (make-polynomial 'x p0-divisor-terms))
(define p0-dividend (make-polynomial 'x p0-dividend-terms))

(define p1-divisor-terms (make-polynomial-sparse-terms (list (list 4 1) (list 0 -1))))
(define p1-dividend-terms (make-polynomial-sparse-terms (list (list 5 1) (list 0 -1))))

(define p1-divisor (make-polynomial 'x p1-divisor-terms))
(define p1-dividend (make-polynomial 'x p1-dividend-terms))

(define p2-divisor-terms (make-polynomial-sparse-terms (list (list 2 1) (list 0 -1))))
(define p2-dividend-terms (make-polynomial-sparse-terms (list (list 5 1) (list 0 -1))))

(define p2-divisor (make-polynomial 'x p2-divisor-terms))
(define p2-dividend (make-polynomial 'x p2-dividend-terms))

(define p3-divisor-terms (make-polynomial-sparse-terms (list (list 2 2) (list 1 3) (list 0 4))))
(define p3-dividend-terms (make-polynomial-sparse-terms (list (list 5 6) (list 4 9) (list 3 8) (list 2 -15) (list 1 -16) (list 0 5))))

(define p3-divisor (make-polynomial 'x p3-divisor-terms))
(define p3-dividend (make-polynomial 'x p3-dividend-terms))

(div p-1-dividend p-1-divisor)
(div p0-dividend p0-divisor)
(div p1-dividend p1-divisor)
(div p2-dividend p2-divisor)
(div p3-dividend p3-divisor)

(display "TESTS FROM EXERCISE 2.92")
(newline)
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

; testing deeply embedded polynomials

(define Px (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 1)))))
(define Py (make-polynomial 'y (make-polynomial-sparse-terms (list (list 1 Px)))))
(define Pz (make-polynomial 'z (make-polynomial-sparse-terms (list (list 1 Py)))))
(define Pw (make-polynomial 'w (make-polynomial-sparse-terms (list (list 1 Pz)))))
(define Pv (make-polynomial 'v (make-polynomial-sparse-terms (list (list 1 Pw)))))
(define Pu (make-polynomial 'u (make-polynomial-sparse-terms (list (list 1 Pv)))))
(define Pt (make-polynomial 't (make-polynomial-sparse-terms (list (list 1 Pu)))))
(define Ps (make-polynomial 's (make-polynomial-sparse-terms (list (list 1 Pt)))))
(define Pr (make-polynomial 'r (make-polynomial-sparse-terms (list (list 1 Ps)))))
(define Pq (make-polynomial 'q (make-polynomial-sparse-terms (list (list 1 Pr)))))
(define Pp (make-polynomial 'p (make-polynomial-sparse-terms (list (list 1 Pq)))))

(print Px)
(newline)
(print Py)
(newline)
(print Pz)
(newline)
(print Pw)
(newline)
(print Pv)
(newline)
(print Pu)
(newline)
(print Pt)
(newline)
(print Ps)
(newline)
(print Pr)
(newline)
(print Pq)
(newline)
(print Pp)
(newline)

(display "Py in terms of x: ")
(print (convert-polynomial Py 'x))
(newline)
(display "Pz in terms of x: ")
(print (convert-polynomial Pz 'x))
(newline)
(display "Pw in terms of x: ")
(print (convert-polynomial Pw 'x))
(newline)
(display "Pv in terms of x: ")
(print (convert-polynomial Pv 'x))
(newline)
(display "Pu in terms of x: ")
(print (convert-polynomial Pu 'x))
(newline)
(display "Pt in terms of x: ")
(print (convert-polynomial Pt 'x))
(newline)
(display "Ps in terms of x: ")
(print (convert-polynomial Ps 'x))
(newline)
(display "Pr in terms of x: ")
(print (convert-polynomial Pr 'x))
(newline)
(display "Pq in terms of x: ")
(print (convert-polynomial Pq 'x))
(newline)
(display "Pp in terms of x: ")
(print (convert-polynomial Pp 'x))
(newline)

(display "Pp in terms of u: ")
(print (convert-polynomial Pp 'u))
(newline)

(display "Pt in terms of s: ")
(print (convert-polynomial Pt 's))
(newline)

(define PP1x (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 5) (list 0 -9)))))
(define PP1y (make-polynomial 'y (make-polynomial-sparse-terms (list (list 1 PP1x) (list 0 4)))))
(define PP1z (make-polynomial 'z (make-polynomial-sparse-terms (list (list 1 PP1y) (list 0 PP1x)))))
(define PP1w (make-polynomial 'w (make-polynomial-sparse-terms (list (list 1 PP1z) (list 0 PP1y)))))
(define PP1v (make-polynomial 'v (make-polynomial-sparse-terms (list (list 1 PP1w) (list 0 PP1z)))))
(define PP1u (make-polynomial 'u (make-polynomial-sparse-terms (list (list 1 PP1v) (list 0 PP1w)))))
(define PP1t (make-polynomial 't (make-polynomial-sparse-terms (list (list 1 PP1u) (list 0 PP1v)))))
(define PP1s (make-polynomial 's (make-polynomial-sparse-terms (list (list 1 PP1t) (list 0 PP1u)))))
(define PP1r (make-polynomial 'r (make-polynomial-sparse-terms (list (list 1 PP1s) (list 0 PP1t)))))
(define PP1q (make-polynomial 'q (make-polynomial-sparse-terms (list (list 1 PP1r) (list 0 PP1s)))))
(define PP1p (make-polynomial 'p (make-polynomial-sparse-terms (list (list 1 PP1q) (list 0 PP1r)))))

(print PP1x)
(newline)
(newline)

(print PP1y)
(newline)
(display "PP1y in terms of x: ")
(print (convert-polynomial PP1y 'x))
(newline)
(newline)

(print PP1z)
(newline)
(display "PP1z in terms of x: ")
(print (convert-polynomial PP1z 'x))
(newline)
(newline)

(print PP1w)
(newline)
(display "PP1w in terms of x: ")
(print (convert-polynomial PP1w 'x))
(newline)
(newline)

(print PP1v)
(newline)
(display "PP1v in terms of x: ")
(print (convert-polynomial PP1v 'x))
(newline)
(newline)

(print PP1u)
(newline)
(display "PP1u in terms of x: ")
(print (convert-polynomial PP1u 'x))
(newline)
(newline)

(print PP1t)
(newline)
(display "PP1t in terms of x: ")
(print (convert-polynomial PP1t 'x))
(newline)
(newline)

(print PP1s)
(newline)
(display "PP1s in terms of x: ")
(print (convert-polynomial PP1s 'x))
(newline)
(newline)

(print PP1r)
(newline)
(display "PP1r in terms of x: ")
(print (convert-polynomial PP1r 'x))
(newline)
(newline)

(print PP1q)
(newline)
(display "PP1q in terms of x: ")
(print (convert-polynomial PP1q 'x))
(newline)
(newline)

(print PP1p)
(newline)
(display "PP1p in terms of x: ")
(print (convert-polynomial PP1p 'x))
(newline)
(newline)

(display "PP1z is: ")
(print PP1z)
(run-test mul PP1z PP1z)
(print (convert-polynomial PP1z 'y))
(print (convert-polynomial (convert-polynomial PP1z 'y) 'z))
(print (convert-polynomial PP1z 'x))
(run-test mul PP1z (convert-polynomial PP1z 'x))
(run-test mul (mul PP1z (convert-polynomial PP1z 'x)) (mul PP1z (convert-polynomial PP1z 'y)))

(display "TESTS FROM EXERCISE 2.93")
(newline)
(newline)

(define p1-2.93 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 1) (list 0 1)))))
(define p2-2.93 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 3 1) (list 0 1)))))
(define rf (make-rational p2-2.93 p1-2.93))

(run-test add rf rf)

(display "TESTS FROM EXERCISE 2.94")
(newline)
(newline)

(define p1-2.94 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 4 1) (list 3 -1) (list 2 -2) (list 1 2)))))
(define p2-2.94 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 3 1) (list 1 -1)))))

(run-test greatest-common-divisor p1-2.94 p2-2.94)

(display "TESTS FROM EXERCISE 2.95")
(newline)
(newline)

(define P1-2.95 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 1) (list 1 -2) (list 0 1)))))
(define P2-2.95 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 11) (list 0 7)))))
(define P3-2.95 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 13) (list 0 5)))))

(define Q1-2.95 (mul P1-2.95 P2-2.95))
(define Q2-2.95 (mul P1-2.95 P3-2.95))

(run-test greatest-common-divisor Q1-2.95 Q2-2.95)

(display "TESTS FROM EXERCISE 2.96")
(newline)
(newline)

(define P1-2.96 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 1) (list 1 -2) (list 0 1)))))
(define P2-2.96 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 11) (list 0 7)))))
(define P3-2.96 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 13) (list 0 5)))))

(display "P1-2.96 is: ")
(print P1-2.96)
(newline)
(display "P2-2.96 is: ")
(print P2-2.96)
(newline)
(display "P3-2.96 is: ")
(print P3-2.96)
(newline)

(define Q1-2.96 (mul P1-2.96 P2-2.96))
(define Q2-2.96 (mul P1-2.96 P3-2.96))

(display "Q1-2.96 is: ")
(print Q1-2.96)
(newline)
(display "Q2-2.96 is: ")
(print Q2-2.96)
(newline)

(run-test greatest-common-divisor Q1-2.96 Q2-2.96)
(display "P1-2.96 is: ")
(print P1-2.96)

(display "TESTS FROM EXERCISE 2.97")
(newline)
(newline)

(define P1-2.97 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 1) (list 1 -2) (list 0 1)))))
(define P2-2.97 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 11) (list 0 7)))))
(define P3-2.97 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 13) (list 0 5)))))

(display "P1-2.97 is: ")
(print P1-2.97)
(newline)
(display "P2-2.97 is: ")
(print P2-2.97)
(newline)
(display "P3-2.97 is: ")
(print P3-2.97)
(newline)

(define Q1-2.97 (mul P1-2.97 P2-2.97))
(define Q2-2.97 (mul P1-2.97 P3-2.97))

(display "Q1-2.97 is: ")
(print Q1-2.97)
(newline)
(display "Q2-2.97 is: ")
(print Q2-2.97)
(newline)

(run-test greatest-common-divisor Q1-2.97 Q2-2.97)
(display "P1-2.97 is: ")
(print P1-2.97)
(newline)

(newline)
(define p1-2.97 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 1) (list 0 1)))))
(display "p1-2.97 is: ")
(print p1-2.97)
(newline)
(define p2-2.97 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 3 1) (list 0 -1)))))
(display "p2-2.97 is: ")
(print p2-2.97)
(newline)
(define p3-2.97 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 1 1)))))
(display "p3-2.97 is: ")
(print p3-2.97)
(newline)
(define p4-2.97 (make-polynomial 'x (make-polynomial-sparse-terms (list (list 2 1) (list 0 -1)))))
(display "p4-2.97 is: ")
(print p4-2.97)
(newline)

(define rf1-2.97 (make-rational p1-2.97 p2-2.97))
(define rf2-2.97 (make-rational p3-2.97 p4-2.97))

(display "rf1-2.97 is: ")
(print rf1-2.97)
(newline)
(display "rf2-2.97 is: ")
(print rf2-2.97)
(newline)

(run-test add rf1-2.97 rf2-2.97)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.

TESTS FROM EXERCISE 2.77

7
'(rational 8 . 9)
'(rational 2 . 3)
'(complex rectangular 3 . 4)
'(complex rectangular 5 . 6)
'(complex polar 2 . 1.0471975511965976)
'(complex polar 2 . 0.5235987755982988)
'(rectangular 8 . 9)
'(polar 4 . 0.7853981633974483)
Running Test: (#<procedure:add> (rational 8 . 9) (rational 2 . 3)) 
Applying #<procedure:add> on: [8 / 9], [2 / 3]
Result: (rational 14 . 9)
[14 / 9]

Running Test: (#<procedure:sub> (rational 8 . 9) (rational 2 . 3)) 
Applying #<procedure:sub> on: [8 / 9], [2 / 3]
Result: (rational 2 . 9)
[2 / 9]

Running Test: (#<procedure:mul> (rational 8 . 9) (rational 2 . 3)) 
Applying #<procedure:mul> on: [8 / 9], [2 / 3]
Result: (rational 16 . 27)
[16 / 27]

Running Test: (#<procedure:div> (rational 8 . 9) (rational 2 . 3)) 
Applying #<procedure:div> on: [8 / 9], [2 / 3]
Result: (rational 4 . 3)
[4 / 3]

Running Test: (#<procedure:add> (complex rectangular 3 . 4) (complex rectangular 5 . 6)) 
Applying #<procedure:add> on: [3+4i], [5+6i]
Result: (complex rectangular 8 . 10)
[8+10i]

Running Test: (#<procedure:sub> (complex rectangular 3 . 4) (complex rectangular 5 . 6)) 
Applying #<procedure:sub> on: [3+4i], [5+6i]
Result: (complex rectangular -2 . -2)
[-2+-2i]

Running Test: (#<procedure:mul> (complex rectangular 3 . 4) (complex rectangular 5 . 6)) 
Applying #<procedure:mul> on: [3+4i], [5+6i]
Result: (complex polar (real . 39.05124837953327) real . 1.8033532685998055)
[-8.999999999999993+38i]

Running Test: (#<procedure:make-complex-from-real-imag> (real . -8.999999999999993) 38) 
Applying #<procedure:make-complex-from-real-imag> on: -8.999999999999993, 38
Result: (complex rectangular (real . -8.999999999999993) . 38)
[-8.999999999999993+38i]

Running Test: (#<procedure:div> (complex rectangular 3 . 4) (complex rectangular 5 . 6)) 
Applying #<procedure:div> on: [3+4i], [5+6i]
Result: (complex polar (real . 0.6401843996644799) real . 0.05123716740341877)
[0.639344262295082+0.03278688524590161i]

Running Test: (#<procedure:make-complex-from-real-imag> (real . 0.639344262295082) (real . 0.03278688524590161)) 
Applying #<procedure:make-complex-from-real-imag> on: 0.639344262295082, 0.03278688524590161
Result: (complex rectangular (real . 0.639344262295082) real . 0.03278688524590161)
[0.639344262295082+0.03278688524590161i]

Running Test: (#<procedure:add> (complex polar 2 . 1.0471975511965976) (complex polar 2 . 0.5235987755982988)) 
Applying #<procedure:add> on: [1.0000000000000002+1.7320508075688772i], [1.7320508075688774+0.9999999999999999i]
Result: (complex rectangular (real . 2.7320508075688776) real . 2.732050807568877)
[2.7320508075688776+2.732050807568877i]

Running Test: (#<procedure:sub> (complex polar 2 . 1.0471975511965976) (complex polar 2 . 0.5235987755982988)) 
Applying #<procedure:sub> on: [1.0000000000000002+1.7320508075688772i], [1.7320508075688774+0.9999999999999999i]
Result: (complex rectangular (real . -0.7320508075688772) real . 0.7320508075688773)
[-0.7320508075688772+0.7320508075688773i]

Running Test: (#<procedure:mul> (complex polar 2 . 1.0471975511965976) (complex polar 2 . 0.5235987755982988)) 
Applying #<procedure:mul> on: [1.0000000000000002+1.7320508075688772i], [1.7320508075688774+0.9999999999999999i]
Result: (complex polar 4 real . 1.5707963267948966)
[2.4492935982947064e-16+4i]

Running Test: (#<procedure:div> (complex polar 2 . 1.0471975511965976) (complex polar 2 . 0.5235987755982988)) 
Applying #<procedure:div> on: [1.0000000000000002+1.7320508075688772i], [1.7320508075688774+0.9999999999999999i]
Result: (complex polar 1 real . 0.5235987755982988)
[0.8660254037844387+0.49999999999999994i]

Running Test: (#<procedure:magnitude> (complex rectangular 6 . 8)) 
Applying #<procedure:magnitude> on: [6+8i]
Result: 10
10


TESTS FROM EXERCISE 2.78. (Tests on scheme numbers)

Running Test: (#<procedure:add> 8 9) 
Applying #<procedure:add> on: 8, 9
Result: 17
17

Running Test: (#<procedure:make-scheme-number> 45) 
Applying #<procedure:make-scheme-number> on: 45
Result: 45
45

Running Test: (#<procedure:sub> -54 34) 
Applying #<procedure:sub> on: -54, 34
Result: (integer . -88)
-88

Running Test: (#<procedure:mul> -32 3) 
Applying #<procedure:mul> on: -32, 3
Result: (integer . -96)
-96

Running Test: (#<procedure:div> -34 17) 
Applying #<procedure:div> on: -34, 17
Result: -2
-2

Running Test: (#<procedure:make-scheme-number> -439) 
Applying #<procedure:make-scheme-number> on: -439
Result: -439
-439

Running Test: (#<procedure:div> 35 3) 
Applying #<procedure:div> on: 35, 3
Result: 35/3
35/3

TESTS FROM EXERCISE 2.79

7
11
7
'(rational 8 . 9)
'(complex rectangular 2.82842712474619 . 2.82842712474619)
'(complex rectangular 2.82842712474619 . 2.82842712474619)
'(complex polar 2 . 0.5235987755982988)
Running Test: (#<procedure:make-rational> 13 15) 
Applying #<procedure:make-rational> on: 13, 15
Result: (rational 13 . 15)
[13 / 15]

Running Test: (#<procedure:make-rational> -13 15) 
Applying #<procedure:make-rational> on: -13, 15
Result: (rational -13 . 15)
[-13 / 15]

Running Test: (#<procedure:make-rational> 13 -15) 
Applying #<procedure:make-rational> on: 13, -15
Result: (rational -13 . 15)
[-13 / 15]

Running Test: (#<procedure:make-rational> -13 -15) 
Applying #<procedure:make-rational> on: -13, -15
Result: (rational 13 . 15)
[13 / 15]

Running Test: (#<procedure:equ?> (rational 8 . 9) (rational 2 . 3)) 
Applying #<procedure:equ?> on: [8 / 9], [2 / 3]
Result: #f
False

Running Test: (#<procedure:equ?> (rational 2 . 3) (rational 8 . 9)) 
Applying #<procedure:equ?> on: [2 / 3], [8 / 9]
Result: #f
False

Running Test: (#<procedure:equ?> (rational 8 . 9) (rational 8 . 9)) 
Applying #<procedure:equ?> on: [8 / 9], [8 / 9]
Result: #t
True

Running Test: (#<procedure:equ?> (complex rectangular 3 . 4) (complex rectangular 5 . 6)) 
Applying #<procedure:equ?> on: [3+4i], [5+6i]
Result: #f
False

Running Test: (#<procedure:equ?> (complex polar 2 . 1.0471975511965976) (complex polar 2 . 0.5235987755982988)) 
Applying #<procedure:equ?> on: [1.0000000000000002+1.7320508075688772i], [1.7320508075688774+0.9999999999999999i]
Result: #f
False

Running Test: (#<procedure:equ?> (complex rectangular 2.82842712474619 . 2.82842712474619) (complex rectangular 2.82842712474619 . 2.82842712474619)) 
Applying #<procedure:equ?> on: [2.82842712474619+2.82842712474619i], [2.82842712474619+2.82842712474619i]
Result: #t
True

Running Test: (#<procedure:equ?> (complex polar 2 . 0.5235987755982988) (complex polar 2 . 0.5235987755982988)) 
Applying #<procedure:equ?> on: [1.7320508075688774+0.9999999999999999i], [1.7320508075688774+0.9999999999999999i]
Result: #t
True

Running Test: (#<procedure:equ?> 7 11) 
Applying #<procedure:equ?> on: 7, 11
Result: #f
False

Running Test: (#<procedure:equ?> 11 7) 
Applying #<procedure:equ?> on: 11, 7
Result: #f
False

Running Test: (#<procedure:equ?> 7 7) 
Applying #<procedure:equ?> on: 7, 7
Result: #t
True

Running Test: (#<procedure:equ?> (rational 8 . 9) (rational 2 . 3)) 
Applying #<procedure:equ?> on: [8 / 9], [2 / 3]
Result: #f
False

Running Test: (#<procedure:equ?> (rational 2 . 3) (rational 8 . 9)) 
Applying #<procedure:equ?> on: [2 / 3], [8 / 9]
Result: #f
False

Running Test: (#<procedure:equ?> (rational 8 . 9) (rational 8 . 9)) 
Applying #<procedure:equ?> on: [8 / 9], [8 / 9]
Result: #t
True

TESTS FROM EXERCISE 2.80

Running Test: (#<procedure:=zero?> 10) 
Applying #<procedure:=zero?> on: 10
Result: #f
False

Running Test: (#<procedure:=zero?> 0) 
Applying #<procedure:=zero?> on: 0
Result: #t
True

Running Test: (#<procedure:=zero?> -10) 
Applying #<procedure:=zero?> on: -10
Result: #f
False

Running Test: (#<procedure:make-rational> 23 67) 
Applying #<procedure:make-rational> on: 23, 67
Result: (rational 23 . 67)
[23 / 67]

Running Test: (#<procedure:=zero?> (rational 23 . 67)) 
Applying #<procedure:=zero?> on: [23 / 67]
Result: #f
False

Running Test: (#<procedure:=zero?> (rational 0 . 67)) 
Applying #<procedure:=zero?> on: [0 / 67]
Result: #t
True

Running Test: (#<procedure:make-rational> 0 67) 
Applying #<procedure:make-rational> on: 0, 67
Result: (rational 0 . 67)
[0 / 67]

Running Test: (#<procedure:=zero?> (complex rectangular 7 . 3)) 
Applying #<procedure:=zero?> on: [7+3i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular 0 . 3)) 
Applying #<procedure:=zero?> on: [0+3i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular 7 . 0)) 
Applying #<procedure:=zero?> on: 7
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular 0 . 0)) 
Applying #<procedure:=zero?> on: 0
Result: #t
True

Running Test: (#<procedure:=zero?> (complex polar 10 . 3)) 
Applying #<procedure:=zero?> on: [-9.899924966004454+1.4112000805986722i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex polar 0 . 3)) 
Applying #<procedure:=zero?> on: 0
Result: #t
True

TESTS FROM EXERCISE 2.81

Running Test: (#<procedure:exp> 4 3) 
Applying #<procedure:exp> on: 4, 3
Result: 64
64

TESTS FROM EXERCISE 2.82

Running Test: (#<procedure:mul> (complex polar 2 . 0.1) (complex polar 3 . 0.2)) 
Applying #<procedure:mul> on: [1.9900083305560514+0.1996668332936563i], [2.940199733523725+0.5960079923851836i]
Result: (complex polar 6 real . 0.30000000000000004)
[5.732018934753636+1.7731212399680376i]

Running Test: (#<procedure:add-four-quantities> (complex rectangular 2 . 3) (complex rectangular 4 . 6) (complex rectangular 1 . 5) (complex rectangular 5 . 12)) 
Applying #<procedure:add-four-quantities> on: [2+3i], [4+6i], [1+5i], [5+12i]
Entered proc add-four-complex-numbers
Result: (complex rectangular 12 . 26)
[12+26i]

Running Test: (#<procedure:add-four-quantities> (complex rectangular 2 . 3) (complex rectangular 4 . 6) (complex rectangular 1 . 5) (rational 8 . 9)) 
Applying #<procedure:add-four-quantities> on: [2+3i], [4+6i], [1+5i], [8 / 9]
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 7.888888888888889) . 14)
[7.888888888888889+14i]

Running Test: (#<procedure:add-four-quantities> (complex rectangular 2 . 3) (complex rectangular 4 . 6) (rational 8 . 9) (complex rectangular 5 . 12)) 
Applying #<procedure:add-four-quantities> on: [2+3i], [4+6i], [8 / 9], [5+12i]
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 11.88888888888889) . 21)
[11.88888888888889+21i]

Running Test: (#<procedure:add-four-quantities> (complex rectangular 2 . 3) (rational 8 . 9) (complex rectangular 1 . 5) (complex rectangular 5 . 12)) 
Applying #<procedure:add-four-quantities> on: [2+3i], [8 / 9], [1+5i], [5+12i]
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 8.88888888888889) . 20)
[8.88888888888889+20i]

Running Test: (#<procedure:add-four-quantities> (rational 8 . 9) (complex rectangular 4 . 6) (complex rectangular 1 . 5) (complex rectangular 5 . 12)) 
Applying #<procedure:add-four-quantities> on: [8 / 9], [4+6i], [1+5i], [5+12i]
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 10.88888888888889) . 23)
[10.88888888888889+23i]

Running Test: (#<procedure:add-four-quantities> (rational 8 . 9) (rational 8 . 9) (complex rectangular 1 . 5) (complex rectangular 5 . 12)) 
Applying #<procedure:add-four-quantities> on: [8 / 9], [8 / 9], [1+5i], [5+12i]
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 7.777777777777779) . 17)
[7.777777777777779+17i]

Running Test: (#<procedure:add-four-quantities> (complex rectangular 2 . 3) (rational 8 . 9) (complex rectangular 1 . 5) (rational 8 . 9)) 
Applying #<procedure:add-four-quantities> on: [2+3i], [8 / 9], [1+5i], [8 / 9]
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 4.777777777777778) . 8)
[4.777777777777778+8i]

Running Test: (#<procedure:add-four-quantities> (complex rectangular 2 . 3) (complex rectangular 4 . 6) (rational 8 . 9) (rational 8 . 9)) 
Applying #<procedure:add-four-quantities> on: [2+3i], [4+6i], [8 / 9], [8 / 9]
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 7.777777777777778) . 9)
[7.777777777777778+9i]

Running Test: (#<procedure:add-four-quantities> (complex rectangular 2 . 3) (rational 8 . 9) (rational 8 . 9) (rational 8 . 9)) 
Applying #<procedure:add-four-quantities> on: [2+3i], [8 / 9], [8 / 9], [8 / 9]
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 4.666666666666666) . 3)
[4.666666666666666+3i]

Running Test: (#<procedure:add-four-quantities> (rational 8 . 9) (rational 8 . 9) (rational 8 . 9) (rational 8 . 9)) 
Applying #<procedure:add-four-quantities> on: [8 / 9], [8 / 9], [8 / 9], [8 / 9]
Entered proc add-four-complex-numbers
Result: (real . 32/9)
32/9

Running Test: (#<procedure:mul-and-scale> (complex polar 2 . 0.1) (complex polar 3 . 0.2) 10) 
Applying #<procedure:mul-and-scale> on: [1.9900083305560514+0.1996668332936563i], [2.940199733523725+0.5960079923851836i], 10
Entered proc mul-and-scale-complex
Result: (complex rectangular (real . 57.32018934753636) real . 17.731212399680377)
[57.32018934753636+17.731212399680377i]

Entered proc mul-and-scale-complex
Entered proc mul-and-scale-complex
Running Test: (#<procedure:make-complex-from-mag-ang> 60.00000000000001 0.30000000000000004) 
Applying #<procedure:make-complex-from-mag-ang> on: 60.00000000000001, 0.30000000000000004
Result: (complex polar 60.00000000000001 . 0.30000000000000004)
[57.32018934753636+17.731212399680377i]

TESTS FROM EXERCISE 2.83

Running Test: (#<procedure:raise> (integer . 26)) 
Applying #<procedure:raise> on: 26
Result: (rational 26 . 1)
[26 / 1]

Running Test: (#<procedure:raise> (rational 27 . 4)) 
Applying #<procedure:raise> on: [27 / 4]
Result: (real . 27/4)
27/4

Running Test: (#<procedure:raise> (real . 28)) 
Applying #<procedure:raise> on: 28
Result: (complex rectangular 28 . 0)
28

#<procedure:make-real>
0+5.347896782848375i
Running Test: (#<procedure:raise> (real . 26)) 
Applying #<procedure:raise> on: 26
Result: (complex rectangular 26 . 0)
26

TESTS FROM EXERCISE 2.84

Running Test: (#<procedure:mul-five-quantities> (rational 8 . 9) (rational 2 . 3) (rational 8 . 9) (rational 15 . 19) (rational 9 . 16)) 
Applying #<procedure:mul-five-quantities> on: [8 / 9], [2 / 3], [8 / 9], [15 / 19], [9 / 16]
Result: (rational 40 . 171)
[40 / 171]

Running Test: (#<procedure:mul-five-quantities> (integer . 35) (rational 2 . 3) (rational 8 . 9) (rational 15 . 19) (rational 9 . 16)) 
Applying #<procedure:mul-five-quantities> on: 35, [2 / 3], [8 / 9], [15 / 19], [9 / 16]
Result: (rational 175 . 19)
[175 / 19]

Running Test: (#<procedure:add-four-quantities> (integer . 35) (integer . 17) (integer . 236) (integer . 29)) 
Applying #<procedure:add-four-quantities> on: 35, 17, 236, 29
Entered proc add-four-complex-numbers
Result: 317
317

Running Test: (#<procedure:add-four-quantities> (integer . 35) (rational 15 . 19) (complex polar 2 . 0.1) (real . 3.1622776601683795)) 
Applying #<procedure:add-four-quantities> on: 35, [15 / 19], [1.9900083305560514+0.1996668332936563i], 3.1622776601683795
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 40.94175967493496) real . 0.1996668332936563)
[40.94175967493496+0.1996668332936563i]

TESTS FROM EXERCISE 2.85

Projection Tests
Running Test: (#<procedure:project> (integer . 35)) 
Applying #<procedure:project> on: 35
Result: (natural . 35)
35

Running Test: (#<procedure:project> (rational 4 . 1)) 
Applying #<procedure:project> on: [4 / 1]
Result: (integer . 4)
4

Running Test: (#<procedure:project> (real . 65.0)) 
Applying #<procedure:project> on: 65.0
Result: (rational 65 . 1)
[65 / 1]

Running Test: (#<procedure:project> (complex rectangular 23 . 12)) 
Applying #<procedure:project> on: [23+12i]
Result: 23
23


'(complex rectangular 29.0 . 0)
Running Test: (#<procedure:project> (complex rectangular 29.0 . 0)) 
Applying #<procedure:project> on: 29.0
Result: 29.0
29.0

Running Test: (#<procedure:project> 29.0) 
Applying #<procedure:project> on: 29.0
Result: 29.0
29.0

Running Test: (#<procedure:project> 29.0) 
Applying #<procedure:project> on: 29.0
Result: 29.0
29.0

Running Test: (#<procedure:project> 29.0) 
Applying #<procedure:project> on: 29.0
Result: 29.0
29.0

Drop Tests
Dropping: (integer . 35): Running Test: (#<procedure:drop> (integer . 35)) 
Applying #<procedure:drop> on: 35
Result: 35
35

Dropping: (rational 9 . 16): Running Test: (#<procedure:drop> (rational 9 . 16)) 
Applying #<procedure:drop> on: [9 / 16]
Result: (rational 9 . 16)
[9 / 16]

Dropping: (real . 3.1622776601683795): Running Test: (#<procedure:drop> (real . 3.1622776601683795)) 
Applying #<procedure:drop> on: 3.1622776601683795
Result: (real . 3.1622776601683795)
3.1622776601683795

Dropping: (complex rectangular 23 . 12): Running Test: (#<procedure:drop> (complex rectangular 23 . 12)) 
Applying #<procedure:drop> on: [23+12i]
Result: (complex rectangular 23 . 12)
[23+12i]

Dropping: (complex rectangular 29.0 . 0): Running Test: (#<procedure:drop> (complex rectangular 29.0 . 0)) 
Applying #<procedure:drop> on: 29.0
Result: 29.0
29.0

Dropping: (complex rectangular 29.5 . 0): Running Test: (#<procedure:drop> (complex rectangular 29.5 . 0)) 
Applying #<procedure:drop> on: 29.5
Result: 29.5
29.5

Dropping: (complex rectangular 29.5 . 4.6): Running Test: (#<procedure:drop> (complex rectangular 29.5 . 4.6)) 
Applying #<procedure:drop> on: [29.5+4.6i]
Result: (complex rectangular 29.5 . 4.6)
[29.5+4.6i]

Testing proc add-four-quantities
Running Test: (#<procedure:add-four-quantities> (integer . 35) (integer . 17) (integer . 236) (integer . 29)) 
Applying #<procedure:add-four-quantities> on: 35, 17, 236, 29
Entered proc add-four-complex-numbers
Result: 317
317

Running Test: (#<procedure:add-four-quantities> (integer . 35) (rational -7 . 2) (complex rectangular 23 . 12) (real . 65.0)) 
Applying #<procedure:add-four-quantities> on: 35, [-7 / 2], [23+12i], 65.0
Entered proc add-four-complex-numbers
Result: (complex rectangular (real . 239/2) . 12)
[239/2+12i]

Testing proc mul-five-quantities
Running Test: (#<procedure:mul-five-quantities> (rational 4 . 1) (rational 2 . 5) (rational 6 . 5) (rational -7 . 2) (rational 9 . 16)) 
Applying #<procedure:mul-five-quantities> on: [4 / 1], [2 / 5], [6 / 5], [-7 / 2], [9 / 16]
Result: (rational -189 . 50)
[-189 / 50]

Running Test: (#<procedure:mul-five-quantities> (integer . 35) (rational 2 . 5) (rational 6 . 5) (rational -7 . 2) (rational 9 . 16)) 
Applying #<procedure:mul-five-quantities> on: 35, [2 / 5], [6 / 5], [-7 / 2], [9 / 16]
Result: (rational -1323 . 40)
[-1323 / 40]

Testing proc mul-and-scale
Running Test: (#<procedure:mul-and-scale> (complex rectangular 29.5 . 4.6) (complex rectangular 89.3 . 348) 200) 
Applying #<procedure:mul-and-scale> on: [29.5+4.6i], [89.3+348i], 200
Entered proc mul-and-scale-complex
Result: (complex rectangular (real . 206709.9999999999) . 2135356)
[206709.9999999999+2135356i]

z5-2.85: '(complex rectangular 89.3 . 348)

z6-2.85: '(complex rectangular 89.7 . -348)

z5-2.85 + z6-2.85: Running Test: (#<procedure:add> (complex rectangular 89.3 . 348) (complex rectangular 89.7 . -348)) 
Applying #<procedure:add> on: [89.3+348i], [89.7+-348i]
Result: 179
179


Running Test: (#<procedure:project> (natural . 46)) 
Applying #<procedure:project> on: 46
Result: 46
46

TESTS FROM EXERCISE 2.86

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

Running Test: (#<procedure:square> 0.0) 
Applying #<procedure:square> on: 0.0
Result: 0.0
0.0

Running Test: (#<procedure:square> 25) 
Applying #<procedure:square> on: 25
Result: 625
625

Running Test: (#<procedure:square> 26) 
Applying #<procedure:square> on: 26
Result: 676
676

Running Test: (#<procedure:square> (natural . 30)) 
Applying #<procedure:square> on: 30
Result: 900
900

Running Test: (#<procedure:square> (natural . 31)) 
Applying #<procedure:square> on: 31
Result: 961
961

Running Test: (#<procedure:square> (integer . 35)) 
Applying #<procedure:square> on: 35
Result: 1225
1225

Running Test: (#<procedure:square> (integer . 36)) 
Applying #<procedure:square> on: 36
Result: 1296
1296

Running Test: (#<procedure:square> (rational 40 . 43)) 
Applying #<procedure:square> on: [40 / 43]
Result: (rational 1600 . 1849)
[1600 / 1849]

Running Test: (#<procedure:square> (rational 44 . 47)) 
Applying #<procedure:square> on: [44 / 47]
Result: (rational 1936 . 2209)
[1936 / 2209]

Running Test: (#<procedure:square> (real . 45.3)) 
Applying #<procedure:square> on: 45.3
Result: (real . 2052.0899999999997)
2052.0899999999997

Running Test: (#<procedure:square> (real . 6.730527468185536)) 
Applying #<procedure:square> on: 6.730527468185536
Result: (real . 45.300000000000004)
45.300000000000004

Running Test: (#<procedure:square> (complex rectangular 50 . 55)) 
Applying #<procedure:square> on: [50+55i]
Result: (complex polar 5525 real . 1.6659625333488635)
[-525.0000000000002+5500i]

Running Test: (#<procedure:square> (complex rectangular 25 . 26)) 
Applying #<procedure:square> on: [25+26i]
Result: (complex polar (real . 1300.9999999999998) real . 1.610006988509306)
[-51.000000000000014+1299.9999999999998i]

Running Test: (#<procedure:square> (complex rectangular (natural . 30) natural . 31)) 
Applying #<procedure:square> on: [30+31i]
Result: (complex polar (real . 1861.0000000000002) real . 1.6035802754109072)
[-61.000000000000085+1860.0000000000002i]

Running Test: (#<procedure:square> (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:square> on: [35+36i]
Result: (complex polar (real . 2520.9999999999995) real . 1.598963478440617)
[-71.00000000000007+2519.9999999999995i]

Running Test: (#<procedure:square> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:square> on: [[40 / 43]+[44 / 47]i]
Result: (complex polar (real . 1.7417472794930815) real . 1.5771589776520794)
[-0.011082055047434779+1.7417120237506187i]

Running Test: (#<procedure:square> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:square> on: [45.3+6.730527468185536i]
Result: (complex polar (real . 2097.3899999999994) real . 0.2949954825171345)
[2006.7899999999993+609.7857886176093i]

Running Test: (#<procedure:square> (complex rectangular (rational 40 . 43) real . 45.3)) 
Applying #<procedure:square> on: [[40 / 43]+45.3i]
Result: (complex polar (real . 2052.9553326122227) real . 3.100528554842763)
[-2051.224667387777+84.27906976744227i]

Running Test: (#<procedure:square> (complex polar 50 . 55)) 
Applying #<procedure:square> on: [1.1063378130977866+-49.98775866793099i]
Result: (complex polar 2500 . 110)
[-2497.55203328662+-110.60669521267741i]

Running Test: (#<procedure:square> (complex polar 25 . 26)) 
Applying #<procedure:square> on: [16.17298305821601+19.06396126199007i]
Result: (complex polar 625 . 52)
[-101.86923799731592+616.6422450253033i]

Running Test: (#<procedure:square> (complex polar (natural . 30) natural . 31)) 
Applying #<procedure:square> on: [27.44227073413594+-12.12112935969195i]
Result: (complex polar 900 . 62)
[606.1564460912276+-665.2626269843006i]

Running Test: (#<procedure:square> (complex polar (integer . 35) integer . 36)) 
Applying #<procedure:square> on: [-4.4787291369591635+-34.71225987050905i]
Result: (complex polar 1225 . 72)
[-1184.881970635506+310.93361938349443i]

Running Test: (#<procedure:square> (complex polar (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:square> on: [0.5515129933855453+0.7491101590218483i]
Result: (complex polar (rational 1600 . 1849) rational 88 . 47)
[-0.2569994484766544+0.8262879723553225i]

Running Test: (#<procedure:square> (complex polar (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:square> on: [40.84247949822263+19.595455300586003i]
Result: (complex polar (real . 2052.0899999999997) real . 13.461054936371072)
[1284.1262631254717+1600.6539627450434i]

Running Test: (#<procedure:square> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:square> on: [3.990371352370838+5.420049489642889i]
Result: (complex polar (real . 45.300000000000004) rational 88 . 47)
[-13.453872940356272+43.25602042380632i]


Running Test: (#<procedure:square-root> 0.0) 
Applying #<procedure:square-root> on: 0.0
Result: 0.0
0.0

Running Test: (#<procedure:square-root> 25) 
Applying #<procedure:square-root> on: 25
Result: 5
5

Running Test: (#<procedure:square-root> 26) 
Applying #<procedure:square-root> on: 26
Result: 5.0990195135927845
5.0990195135927845

Running Test: (#<procedure:square-root> (natural . 30)) 
Applying #<procedure:square-root> on: 30
Result: 5.477225575051661
5.477225575051661

Running Test: (#<procedure:square-root> (natural . 31)) 
Applying #<procedure:square-root> on: 31
Result: 5.5677643628300215
5.5677643628300215

Running Test: (#<procedure:square-root> (integer . 35)) 
Applying #<procedure:square-root> on: 35
Result: 5.916079783099616
5.916079783099616

Running Test: (#<procedure:square-root> (integer . 36)) 
Applying #<procedure:square-root> on: 36
Result: 6
6

Running Test: (#<procedure:square-root> (rational 40 . 43)) 
Applying #<procedure:square-root> on: [40 / 43]
Result: 0.9644856443408242
0.9644856443408242

Running Test: (#<procedure:square-root> (rational 44 . 47)) 
Applying #<procedure:square-root> on: [44 / 47]
Result: 0.9675588936937934
0.9675588936937934

Running Test: (#<procedure:square-root> (real . 45.3)) 
Applying #<procedure:square-root> on: 45.3
Result: 6.730527468185536
6.730527468185536

Running Test: (#<procedure:square-root> (real . 6.730527468185536)) 
Applying #<procedure:square-root> on: 6.730527468185536
Result: 2.594326014244458
2.594326014244458


Running Test: (#<procedure:=zero?> 0.0) 
Applying #<procedure:=zero?> on: 0.0
Result: #t
True

Running Test: (#<procedure:=zero?> 25) 
Applying #<procedure:=zero?> on: 25
Result: #f
False

Running Test: (#<procedure:=zero?> 26) 
Applying #<procedure:=zero?> on: 26
Result: #f
False

Running Test: (#<procedure:=zero?> (natural . 30)) 
Applying #<procedure:=zero?> on: 30
Result: #f
False

Running Test: (#<procedure:=zero?> (natural . 31)) 
Applying #<procedure:=zero?> on: 31
Result: #f
False

Running Test: (#<procedure:=zero?> (integer . 35)) 
Applying #<procedure:=zero?> on: 35
Result: #f
False

Running Test: (#<procedure:=zero?> (integer . 36)) 
Applying #<procedure:=zero?> on: 36
Result: #f
False

Running Test: (#<procedure:=zero?> (rational 40 . 43)) 
Applying #<procedure:=zero?> on: [40 / 43]
Result: #f
False

Running Test: (#<procedure:=zero?> (rational 44 . 47)) 
Applying #<procedure:=zero?> on: [44 / 47]
Result: #f
False

Running Test: (#<procedure:=zero?> (real . 45.3)) 
Applying #<procedure:=zero?> on: 45.3
Result: #f
False

Running Test: (#<procedure:=zero?> (real . 6.730527468185536)) 
Applying #<procedure:=zero?> on: 6.730527468185536
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular 50 . 55)) 
Applying #<procedure:=zero?> on: [50+55i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular 25 . 26)) 
Applying #<procedure:=zero?> on: [25+26i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular (natural . 30) natural . 31)) 
Applying #<procedure:=zero?> on: [30+31i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:=zero?> on: [35+36i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:=zero?> on: [[40 / 43]+[44 / 47]i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:=zero?> on: [45.3+6.730527468185536i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex rectangular (rational 40 . 43) real . 45.3)) 
Applying #<procedure:=zero?> on: [[40 / 43]+45.3i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex polar 50 . 55)) 
Applying #<procedure:=zero?> on: [1.1063378130977866+-49.98775866793099i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex polar 25 . 26)) 
Applying #<procedure:=zero?> on: [16.17298305821601+19.06396126199007i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex polar (natural . 30) natural . 31)) 
Applying #<procedure:=zero?> on: [27.44227073413594+-12.12112935969195i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex polar (integer . 35) integer . 36)) 
Applying #<procedure:=zero?> on: [-4.4787291369591635+-34.71225987050905i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex polar (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:=zero?> on: [0.5515129933855453+0.7491101590218483i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex polar (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:=zero?> on: [40.84247949822263+19.595455300586003i]
Result: #f
False

Running Test: (#<procedure:=zero?> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:=zero?> on: [3.990371352370838+5.420049489642889i]
Result: #f
False


Running Test: (#<procedure:equ?> 0.0 9.0) 
Applying #<procedure:equ?> on: 0.0, 9.0
Result: #f
False

Running Test: (#<procedure:equ?> 9 9.0) 
Applying #<procedure:equ?> on: 9, 9.0
Result: #t
True

Running Test: (#<procedure:equ?> 25 25) 
Applying #<procedure:equ?> on: 25, 25
Result: #t
True

Running Test: (#<procedure:equ?> 25 26) 
Applying #<procedure:equ?> on: 25, 26
Result: #f
False

Running Test: (#<procedure:equ?> (natural . 30) (natural . 31)) 
Applying #<procedure:equ?> on: 30, 31
Result: #f
False

Running Test: (#<procedure:equ?> (natural . 30) (integer . 36)) 
Applying #<procedure:equ?> on: 30, 36
Result: #f
False

Running Test: (#<procedure:equ?> (integer . 35) (integer . 36)) 
Applying #<procedure:equ?> on: 35, 36
Result: #f
False

Running Test: (#<procedure:equ?> (integer . 35) (natural . 31)) 
Applying #<procedure:equ?> on: 35, 31
Result: #f
False

Running Test: (#<procedure:equ?> (integer . 35) (integer . 35)) 
Applying #<procedure:equ?> on: 35, 35
Result: #t
True

Running Test: (#<procedure:equ?> (natural . 31) (natural . 31)) 
Applying #<procedure:equ?> on: 31, 31
Result: #t
True

Running Test: (#<procedure:equ?> (rational 40 . 43) (rational 44 . 47)) 
Applying #<procedure:equ?> on: [40 / 43], [44 / 47]
Result: #f
False

Running Test: (#<procedure:equ?> (rational 44 . 47) (rational 44 . 47)) 
Applying #<procedure:equ?> on: [44 / 47], [44 / 47]
Result: #t
True

Running Test: (#<procedure:equ?> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
Applying #<procedure:equ?> on: [50+55i], [25+26i]
Result: #f
False

Running Test: (#<procedure:equ?> (complex polar 50 . 55) (complex polar 25 . 26)) 
Applying #<procedure:equ?> on: [1.1063378130977866+-49.98775866793099i], [16.17298305821601+19.06396126199007i]
Result: #f
False

Running Test: (#<procedure:equ?> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
Applying #<procedure:equ?> on: [25+26i], [25+26i]
Result: #t
True

Running Test: (#<procedure:equ?> (complex polar 50 . 55) (complex polar 50 . 55)) 
Applying #<procedure:equ?> on: [1.1063378130977866+-49.98775866793099i], [1.1063378130977866+-49.98775866793099i]
Result: #t
True

Running Test: (#<procedure:equ?> (complex polar (real . 6.730527468185536) rational 44 . 47) (complex rectangular (rational 40 . 43) real . 45.3)) 
Applying #<procedure:equ?> on: [3.990371352370838+5.420049489642889i], [[40 / 43]+45.3i]
Result: #f
False


Running Test: (#<procedure:REAL-PART> (complex rectangular 50 . 55)) 
Applying #<procedure:REAL-PART> on: [50+55i]
Result: 50
50

Running Test: (#<procedure:IMAG-PART> (complex rectangular 50 . 55)) 
Applying #<procedure:IMAG-PART> on: [50+55i]
Result: 55
55

Running Test: (#<procedure:REAL-PART> (complex rectangular 25 . 26)) 
Applying #<procedure:REAL-PART> on: [25+26i]
Result: 25
25

Running Test: (#<procedure:IMAG-PART> (complex rectangular 25 . 26)) 
Applying #<procedure:IMAG-PART> on: [25+26i]
Result: 26
26

Running Test: (#<procedure:REAL-PART> (complex rectangular (natural . 30) natural . 31)) 
Applying #<procedure:REAL-PART> on: [30+31i]
Result: 30
30

Running Test: (#<procedure:IMAG-PART> (complex rectangular (natural . 30) natural . 31)) 
Applying #<procedure:IMAG-PART> on: [30+31i]
Result: 31
31

Running Test: (#<procedure:REAL-PART> (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:REAL-PART> on: [35+36i]
Result: 35
35

Running Test: (#<procedure:IMAG-PART> (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:IMAG-PART> on: [35+36i]
Result: 36
36

Running Test: (#<procedure:REAL-PART> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:REAL-PART> on: [[40 / 43]+[44 / 47]i]
Result: (rational 40 . 43)
[40 / 43]

Running Test: (#<procedure:IMAG-PART> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:IMAG-PART> on: [[40 / 43]+[44 / 47]i]
Result: (rational 44 . 47)
[44 / 47]

Running Test: (#<procedure:REAL-PART> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:REAL-PART> on: [45.3+6.730527468185536i]
Result: (real . 45.3)
45.3

Running Test: (#<procedure:IMAG-PART> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:IMAG-PART> on: [45.3+6.730527468185536i]
Result: (real . 6.730527468185536)
6.730527468185536

Running Test: (#<procedure:REAL-PART> (complex rectangular (rational 40 . 43) real . 45.3)) 
Applying #<procedure:REAL-PART> on: [[40 / 43]+45.3i]
Result: (rational 40 . 43)
[40 / 43]

Running Test: (#<procedure:IMAG-PART> (complex rectangular (rational 40 . 43) real . 45.3)) 
Applying #<procedure:IMAG-PART> on: [[40 / 43]+45.3i]
Result: (real . 45.3)
45.3


Running Test: (#<procedure:magnitude> (complex polar 50 . 55)) 
Applying #<procedure:magnitude> on: [1.1063378130977866+-49.98775866793099i]
Result: 50
50

Running Test: (#<procedure:angle> (complex polar 50 . 55)) 
Applying #<procedure:angle> on: [1.1063378130977866+-49.98775866793099i]
Result: 55
55

Running Test: (#<procedure:magnitude> (complex polar 25 . 26)) 
Applying #<procedure:magnitude> on: [16.17298305821601+19.06396126199007i]
Result: 25
25

Running Test: (#<procedure:angle> (complex polar 25 . 26)) 
Applying #<procedure:angle> on: [16.17298305821601+19.06396126199007i]
Result: 26
26

Running Test: (#<procedure:magnitude> (complex polar (natural . 30) natural . 31)) 
Applying #<procedure:magnitude> on: [27.44227073413594+-12.12112935969195i]
Result: 30
30

Running Test: (#<procedure:angle> (complex polar (natural . 30) natural . 31)) 
Applying #<procedure:angle> on: [27.44227073413594+-12.12112935969195i]
Result: 31
31

Running Test: (#<procedure:magnitude> (complex polar (integer . 35) integer . 36)) 
Applying #<procedure:magnitude> on: [-4.4787291369591635+-34.71225987050905i]
Result: 35
35

Running Test: (#<procedure:angle> (complex polar (integer . 35) integer . 36)) 
Applying #<procedure:angle> on: [-4.4787291369591635+-34.71225987050905i]
Result: 36
36

Running Test: (#<procedure:magnitude> (complex polar (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:magnitude> on: [0.5515129933855453+0.7491101590218483i]
Result: (rational 40 . 43)
[40 / 43]

Running Test: (#<procedure:angle> (complex polar (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:angle> on: [0.5515129933855453+0.7491101590218483i]
Result: (rational 44 . 47)
[44 / 47]

Running Test: (#<procedure:magnitude> (complex polar (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:magnitude> on: [40.84247949822263+19.595455300586003i]
Result: (real . 45.3)
45.3

Running Test: (#<procedure:angle> (complex polar (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:angle> on: [40.84247949822263+19.595455300586003i]
Result: (real . 6.730527468185536)
6.730527468185536

Running Test: (#<procedure:magnitude> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:magnitude> on: [3.990371352370838+5.420049489642889i]
Result: (real . 6.730527468185536)
6.730527468185536

Running Test: (#<procedure:angle> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:angle> on: [3.990371352370838+5.420049489642889i]
Result: (rational 44 . 47)
[44 / 47]


Running Test: (#<procedure:REAL-PART> (complex polar 50 . 55)) 
Applying #<procedure:REAL-PART> on: [1.1063378130977866+-49.98775866793099i]
Result: (real . 1.1063378130977866)
1.1063378130977866

Running Test: (#<procedure:IMAG-PART> (complex polar 50 . 55)) 
Applying #<procedure:IMAG-PART> on: [1.1063378130977866+-49.98775866793099i]
Result: (real . -49.98775866793099)
-49.98775866793099

Running Test: (#<procedure:REAL-PART> (complex polar 25 . 26)) 
Applying #<procedure:REAL-PART> on: [16.17298305821601+19.06396126199007i]
Result: (real . 16.17298305821601)
16.17298305821601

Running Test: (#<procedure:IMAG-PART> (complex polar 25 . 26)) 
Applying #<procedure:IMAG-PART> on: [16.17298305821601+19.06396126199007i]
Result: (real . 19.06396126199007)
19.06396126199007

Running Test: (#<procedure:REAL-PART> (complex polar (natural . 30) natural . 31)) 
Applying #<procedure:REAL-PART> on: [27.44227073413594+-12.12112935969195i]
Result: (real . 27.44227073413594)
27.44227073413594

Running Test: (#<procedure:IMAG-PART> (complex polar (natural . 30) natural . 31)) 
Applying #<procedure:IMAG-PART> on: [27.44227073413594+-12.12112935969195i]
Result: (real . -12.12112935969195)
-12.12112935969195

Running Test: (#<procedure:REAL-PART> (complex polar (integer . 35) integer . 36)) 
Applying #<procedure:REAL-PART> on: [-4.4787291369591635+-34.71225987050905i]
Result: (real . -4.4787291369591635)
-4.4787291369591635

Running Test: (#<procedure:IMAG-PART> (complex polar (integer . 35) integer . 36)) 
Applying #<procedure:IMAG-PART> on: [-4.4787291369591635+-34.71225987050905i]
Result: (real . -34.71225987050905)
-34.71225987050905

Running Test: (#<procedure:REAL-PART> (complex polar (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:REAL-PART> on: [0.5515129933855453+0.7491101590218483i]
Result: (real . 0.5515129933855453)
0.5515129933855453

Running Test: (#<procedure:IMAG-PART> (complex polar (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:IMAG-PART> on: [0.5515129933855453+0.7491101590218483i]
Result: (real . 0.7491101590218483)
0.7491101590218483

Running Test: (#<procedure:REAL-PART> (complex polar (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:REAL-PART> on: [40.84247949822263+19.595455300586003i]
Result: (real . 40.84247949822263)
40.84247949822263

Running Test: (#<procedure:IMAG-PART> (complex polar (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:IMAG-PART> on: [40.84247949822263+19.595455300586003i]
Result: (real . 19.595455300586003)
19.595455300586003

Running Test: (#<procedure:REAL-PART> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:REAL-PART> on: [3.990371352370838+5.420049489642889i]
Result: (real . 3.990371352370838)
3.990371352370838

Running Test: (#<procedure:IMAG-PART> (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:IMAG-PART> on: [3.990371352370838+5.420049489642889i]
Result: (real . 5.420049489642889)
5.420049489642889


Running Test: (#<procedure:magnitude> (complex rectangular 50 . 55)) 
Applying #<procedure:magnitude> on: [50+55i]
Result: 74.33034373659252
74.33034373659252

Running Test: (#<procedure:angle> (complex rectangular 50 . 55)) 
Applying #<procedure:angle> on: [50+55i]
Result: 0.8329812666744317
0.8329812666744317

Running Test: (#<procedure:magnitude> (complex rectangular 25 . 26)) 
Applying #<procedure:magnitude> on: [25+26i]
Result: 36.069377593742864
36.069377593742864

Running Test: (#<procedure:angle> (complex rectangular 25 . 26)) 
Applying #<procedure:angle> on: [25+26i]
Result: 0.805003494254653
0.805003494254653

Running Test: (#<procedure:magnitude> (complex rectangular (natural . 30) natural . 31)) 
Applying #<procedure:magnitude> on: [30+31i]
Result: 43.139309220245984
43.139309220245984

Running Test: (#<procedure:angle> (complex rectangular (natural . 30) natural . 31)) 
Applying #<procedure:angle> on: [30+31i]
Result: 0.8017901377054536
0.8017901377054536

Running Test: (#<procedure:magnitude> (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:magnitude> on: [35+36i]
Result: 50.20956084253277
50.20956084253277

Running Test: (#<procedure:angle> (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:angle> on: [35+36i]
Result: 0.7994817392203085
0.7994817392203085

Running Test: (#<procedure:magnitude> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:magnitude> on: [[40 / 43]+[44 / 47]i]
Result: 1.3197527342245143
1.3197527342245143

Running Test: (#<procedure:angle> (complex rectangular (rational 40 . 43) rational 44 . 47)) 
Applying #<procedure:angle> on: [[40 / 43]+[44 / 47]i]
Result: 0.7885794888260397
0.7885794888260397

Running Test: (#<procedure:magnitude> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:magnitude> on: [45.3+6.730527468185536i]
Result: 45.797270661033934
45.797270661033934

Running Test: (#<procedure:angle> (complex rectangular (real . 45.3) real . 6.730527468185536)) 
Applying #<procedure:angle> on: [45.3+6.730527468185536i]
Result: 0.14749774125856724
0.14749774125856724

Running Test: (#<procedure:magnitude> (complex rectangular (rational 40 . 43) real . 45.3)) 
Applying #<procedure:magnitude> on: [[40 / 43]+45.3i]
Result: 45.309550125908586
45.309550125908586

Running Test: (#<procedure:angle> (complex rectangular (rational 40 . 43) real . 45.3)) 
Applying #<procedure:angle> on: [[40 / 43]+45.3i]
Result: 1.5502642774213815
1.5502642774213815


Running Test: (#<procedure:add> 0.0 9.0) 
Applying #<procedure:add> on: 0.0, 9.0
Result: 9.0
9.0

Running Test: (#<procedure:add> 9 9.0) 
Applying #<procedure:add> on: 9, 9.0
Result: 18.0
18.0

Running Test: (#<procedure:add> 25 25) 
Applying #<procedure:add> on: 25, 25
Result: 50
50

Running Test: (#<procedure:add> 25 26) 
Applying #<procedure:add> on: 25, 26
Result: 51
51

Running Test: (#<procedure:add> (natural . 30) (natural . 31)) 
Applying #<procedure:add> on: 30, 31
Result: 61
61

Running Test: (#<procedure:add> (natural . 30) (integer . 36)) 
Applying #<procedure:add> on: 30, 36
Result: 66
66

Running Test: (#<procedure:add> (integer . 35) (integer . 36)) 
Applying #<procedure:add> on: 35, 36
Result: 71
71

Running Test: (#<procedure:add> (integer . 35) (natural . 31)) 
Applying #<procedure:add> on: 35, 31
Result: 66
66

Running Test: (#<procedure:add> (integer . 35) (integer . 35)) 
Applying #<procedure:add> on: 35, 35
Result: 70
70

Running Test: (#<procedure:add> (natural . 31) (natural . 31)) 
Applying #<procedure:add> on: 31, 31
Result: 62
62

Running Test: (#<procedure:add> (rational 40 . 43) (natural . 31)) 
Applying #<procedure:add> on: [40 / 43], 31
Result: (rational 1373 . 43)
[1373 / 43]

Running Test: (#<procedure:add> (rational 40 . 43) (rational 44 . 47)) 
Applying #<procedure:add> on: [40 / 43], [44 / 47]
Result: (rational 3772 . 2021)
[3772 / 2021]

Running Test: (#<procedure:add> (rational 44 . 47) (rational 44 . 47)) 
Applying #<procedure:add> on: [44 / 47], [44 / 47]
Result: (rational 88 . 47)
[88 / 47]

Running Test: (#<procedure:add> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
Applying #<procedure:add> on: [50+55i], [25+26i]
Result: (complex rectangular 75 . 81)
[75+81i]

Running Test: (#<procedure:add> (complex polar 50 . 55) (complex polar 25 . 26)) 
Applying #<procedure:add> on: [1.1063378130977866+-49.98775866793099i], [16.17298305821601+19.06396126199007i]
Result: (complex rectangular (real . 17.279320871313796) real . -30.92379740594092)
[17.279320871313796+-30.92379740594092i]

Running Test: (#<procedure:add> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
Applying #<procedure:add> on: [25+26i], [25+26i]
Result: (complex rectangular 50 . 52)
[50+52i]

Running Test: (#<procedure:add> (complex polar 50 . 55) (complex polar 50 . 55)) 
Applying #<procedure:add> on: [1.1063378130977866+-49.98775866793099i], [1.1063378130977866+-49.98775866793099i]
Result: (complex rectangular (real . 2.212675626195573) real . -99.97551733586198)
[2.212675626195573+-99.97551733586198i]

Running Test: (#<procedure:add> (complex polar 50 . 55) 9.0) 
Applying #<procedure:add> on: [1.1063378130977866+-49.98775866793099i], 9.0
Result: (complex rectangular (real . 10.106337813097786) real . -49.98775866793099)
[10.106337813097786+-49.98775866793099i]

Running Test: (#<procedure:add> (complex polar 50 . 55) 25) 
Applying #<procedure:add> on: [1.1063378130977866+-49.98775866793099i], 25
Result: (complex rectangular (real . 26.106337813097788) real . -49.98775866793099)
[26.106337813097788+-49.98775866793099i]

Running Test: (#<procedure:add> (natural . 30) (complex polar 50 . 55)) 
Applying #<procedure:add> on: 30, [1.1063378130977866+-49.98775866793099i]
Result: (complex rectangular (real . 31.106337813097788) real . -49.98775866793099)
[31.106337813097788+-49.98775866793099i]

Running Test: (#<procedure:add> (integer . 35) (complex polar 50 . 55)) 
Applying #<procedure:add> on: 35, [1.1063378130977866+-49.98775866793099i]
Result: (complex rectangular (real . 36.10633781309779) real . -49.98775866793099)
[36.10633781309779+-49.98775866793099i]

Running Test: (#<procedure:add> (rational 40 . 43) (complex polar 50 . 55)) 
Applying #<procedure:add> on: [40 / 43], [1.1063378130977866+-49.98775866793099i]
Result: (complex rectangular (real . 2.0365703712373215) real . -49.98775866793099)
[2.0365703712373215+-49.98775866793099i]

Running Test: (#<procedure:add> (complex rectangular 50 . 55) (complex polar 50 . 55)) 
Applying #<procedure:add> on: [50+55i], [1.1063378130977866+-49.98775866793099i]
Result: (complex rectangular (real . 51.10633781309779) real . 5.01224133206901)
[51.10633781309779+5.01224133206901i]

Running Test: (#<procedure:add> (complex rectangular (rational 40 . 43) real . 45.3) (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:add> on: [[40 / 43]+45.3i], [3.990371352370838+5.420049489642889i]
Result: (complex rectangular (real . 4.920603910510373) real . 50.72004948964289)
[4.920603910510373+50.72004948964289i]

Running Test: (#<procedure:add> (rational 40 . 43) (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:add> on: [40 / 43], [35+36i]
Result: (complex rectangular (real . 35.93023255813954) . 36)
[35.93023255813954+36i]


Running Test: (#<procedure:sub> 0.0 9.0) 
Applying #<procedure:sub> on: 0.0, 9.0
Result: (integer . -9.0)
-9.0

Running Test: (#<procedure:sub> 9 9.0) 
Applying #<procedure:sub> on: 9, 9.0
Result: 0.0
0.0

Running Test: (#<procedure:sub> 25 25) 
Applying #<procedure:sub> on: 25, 25
Result: 0
0

Running Test: (#<procedure:sub> 25 26) 
Applying #<procedure:sub> on: 25, 26
Result: -1
-1

Running Test: (#<procedure:sub> (natural . 30) (natural . 31)) 
Applying #<procedure:sub> on: 30, 31
Result: -1
-1

Running Test: (#<procedure:sub> (natural . 30) (integer . 36)) 
Applying #<procedure:sub> on: 30, 36
Result: (integer . -6)
-6

Running Test: (#<procedure:sub> (integer . 35) (integer . 36)) 
Applying #<procedure:sub> on: 35, 36
Result: (integer . -1)
-1

Running Test: (#<procedure:sub> (integer . 35) (natural . 31)) 
Applying #<procedure:sub> on: 35, 31
Result: 4
4

Running Test: (#<procedure:sub> (integer . 35) (integer . 35)) 
Applying #<procedure:sub> on: 35, 35
Result: 0
0

Running Test: (#<procedure:sub> (natural . 31) (natural . 31)) 
Applying #<procedure:sub> on: 31, 31
Result: 0
0

Running Test: (#<procedure:sub> (rational 40 . 43) (natural . 31)) 
Applying #<procedure:sub> on: [40 / 43], 31
Result: (rational -1293 . 43)
[-1293 / 43]

Running Test: (#<procedure:sub> (rational 40 . 43) (rational 44 . 47)) 
Applying #<procedure:sub> on: [40 / 43], [44 / 47]
Result: (rational -12 . 2021)
[-12 / 2021]

Running Test: (#<procedure:sub> (rational 44 . 47) (rational 44 . 47)) 
Applying #<procedure:sub> on: [44 / 47], [44 / 47]
Result: (rational 0 . 2209)
[0 / 2209]

Running Test: (#<procedure:sub> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
Applying #<procedure:sub> on: [50+55i], [25+26i]
Result: (complex rectangular 25 . 29)
[25+29i]

Running Test: (#<procedure:sub> (complex polar 50 . 55) (complex polar 25 . 26)) 
Applying #<procedure:sub> on: [1.1063378130977866+-49.98775866793099i], [16.17298305821601+19.06396126199007i]
Result: (complex rectangular (real . -15.066645245118222) real . -69.05171992992106)
[-15.066645245118222+-69.05171992992106i]

Running Test: (#<procedure:sub> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
Applying #<procedure:sub> on: [25+26i], [25+26i]
Result: 0
0

Running Test: (#<procedure:sub> (complex polar 50 . 55) (complex polar 50 . 55)) 
Applying #<procedure:sub> on: [1.1063378130977866+-49.98775866793099i], [1.1063378130977866+-49.98775866793099i]
Result: 0
0

Running Test: (#<procedure:sub> (complex polar 50 . 55) 9.0) 
Applying #<procedure:sub> on: [1.1063378130977866+-49.98775866793099i], 9.0
Result: (complex rectangular (real . -7.893662186902214) real . -49.98775866793099)
[-7.893662186902214+-49.98775866793099i]

Running Test: (#<procedure:sub> (complex polar 50 . 55) 25) 
Applying #<procedure:sub> on: [1.1063378130977866+-49.98775866793099i], 25
Result: (complex rectangular (real . -23.893662186902212) real . -49.98775866793099)
[-23.893662186902212+-49.98775866793099i]

Running Test: (#<procedure:sub> (natural . 30) (complex polar 50 . 55)) 
Applying #<procedure:sub> on: 30, [1.1063378130977866+-49.98775866793099i]
Result: (complex rectangular (real . 28.893662186902212) real . 49.98775866793099)
[28.893662186902212+49.98775866793099i]

Running Test: (#<procedure:sub> (integer . 35) (complex polar 50 . 55)) 
Applying #<procedure:sub> on: 35, [1.1063378130977866+-49.98775866793099i]
Result: (complex rectangular (real . 33.89366218690221) real . 49.98775866793099)
[33.89366218690221+49.98775866793099i]

Running Test: (#<procedure:sub> (rational 40 . 43) (complex polar 50 . 55)) 
Applying #<procedure:sub> on: [40 / 43], [1.1063378130977866+-49.98775866793099i]
Result: (complex rectangular (real . -0.1761052549582517) real . 49.98775866793099)
[-0.1761052549582517+49.98775866793099i]

Running Test: (#<procedure:sub> (complex rectangular 50 . 55) (complex polar 50 . 55)) 
Applying #<procedure:sub> on: [50+55i], [1.1063378130977866+-49.98775866793099i]
Result: (complex rectangular (real . 48.89366218690221) real . 104.98775866793099)
[48.89366218690221+104.98775866793099i]

Running Test: (#<procedure:sub> (complex rectangular (rational 40 . 43) real . 45.3) (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:sub> on: [[40 / 43]+45.3i], [3.990371352370838+5.420049489642889i]
Result: (complex rectangular (real . -3.060138794231303) real . 39.879950510357105)
[-3.060138794231303+39.879950510357105i]

Running Test: (#<procedure:sub> (rational 40 . 43) (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:sub> on: [40 / 43], [35+36i]
Result: (complex rectangular (real . -34.06976744186046) integer . -36)
[-34.06976744186046+-36i]


Running Test: (#<procedure:mul> 0.0 9.0) 
Applying #<procedure:mul> on: 0.0, 9.0
Result: 0.0
0.0

Running Test: (#<procedure:mul> 9 9.0) 
Applying #<procedure:mul> on: 9, 9.0
Result: 81.0
81.0

Running Test: (#<procedure:mul> 25 25) 
Applying #<procedure:mul> on: 25, 25
Result: 625
625

Running Test: (#<procedure:mul> 25 26) 
Applying #<procedure:mul> on: 25, 26
Result: 650
650

Running Test: (#<procedure:mul> (natural . 30) (natural . 31)) 
Applying #<procedure:mul> on: 30, 31
Result: 930
930

Running Test: (#<procedure:mul> (natural . 30) (integer . 36)) 
Applying #<procedure:mul> on: 30, 36
Result: 1080
1080

Running Test: (#<procedure:mul> (integer . 35) (integer . 36)) 
Applying #<procedure:mul> on: 35, 36
Result: 1260
1260

Running Test: (#<procedure:mul> (integer . 35) (natural . 31)) 
Applying #<procedure:mul> on: 35, 31
Result: 1085
1085

Running Test: (#<procedure:mul> (integer . 35) (integer . 35)) 
Applying #<procedure:mul> on: 35, 35
Result: 1225
1225

Running Test: (#<procedure:mul> (natural . 31) (natural . 31)) 
Applying #<procedure:mul> on: 31, 31
Result: 961
961

Running Test: (#<procedure:mul> (rational 40 . 43) (natural . 31)) 
Applying #<procedure:mul> on: [40 / 43], 31
Result: (rational 1240 . 43)
[1240 / 43]

Running Test: (#<procedure:mul> (rational 40 . 43) (rational 44 . 47)) 
Applying #<procedure:mul> on: [40 / 43], [44 / 47]
Result: (rational 1760 . 2021)
[1760 / 2021]

Running Test: (#<procedure:mul> (rational 44 . 47) (rational 44 . 47)) 
Applying #<procedure:mul> on: [44 / 47], [44 / 47]
Result: (rational 1936 . 2209)
[1936 / 2209]

Running Test: (#<procedure:mul> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
Applying #<procedure:mul> on: [50+55i], [25+26i]
Result: (complex polar (real . 2681.049234907856) real . 1.6379847609290847)
[-180.0000000000001+2675i]

Running Test: (#<procedure:mul> (complex polar 50 . 55) (complex polar 25 . 26)) 
Applying #<procedure:mul> on: [1.1063378130977866+-49.98775866793099i], [16.17298305821601+19.06396126199007i]
Result: (complex polar 1250 . 81)
[970.857477527039+-787.3599928430674i]

Running Test: (#<procedure:mul> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
Applying #<procedure:mul> on: [25+26i], [25+26i]
Result: (complex polar (real . 1300.9999999999998) real . 1.610006988509306)
[-51.000000000000014+1299.9999999999998i]

Running Test: (#<procedure:mul> (complex polar 50 . 55) (complex polar 50 . 55)) 
Applying #<procedure:mul> on: [1.1063378130977866+-49.98775866793099i], [1.1063378130977866+-49.98775866793099i]
Result: (complex polar 2500 . 110)
[-2497.55203328662+-110.60669521267741i]

Running Test: (#<procedure:mul> (complex polar 50 . 55) 9.0) 
Applying #<procedure:mul> on: [1.1063378130977866+-49.98775866793099i], 9.0
Result: (complex polar 450 . 55)
[9.957040317880079+-449.88982801137894i]

Running Test: (#<procedure:mul> (complex polar 50 . 55) 25) 
Applying #<procedure:mul> on: [1.1063378130977866+-49.98775866793099i], 25
Result: (complex polar 1250 . 55)
[27.658445327444664+-1249.6939666982748i]

Running Test: (#<procedure:mul> (natural . 30) (complex polar 50 . 55)) 
Applying #<procedure:mul> on: 30, [1.1063378130977866+-49.98775866793099i]
Result: (complex polar 1500 . 55)
[33.1901343929336+-1499.6327600379298i]

Running Test: (#<procedure:mul> (integer . 35) (complex polar 50 . 55)) 
Applying #<procedure:mul> on: 35, [1.1063378130977866+-49.98775866793099i]
Result: (complex polar 1750 . 55)
[38.721823458422534+-1749.5715533775847i]

Running Test: (#<procedure:mul> (rational 40 . 43) (complex polar 50 . 55)) 
Applying #<procedure:mul> on: [40 / 43], [1.1063378130977866+-49.98775866793099i]
Result: (complex polar (real . 46.51162790697674) . 55.0)
[1.0291514540444526+-46.50024062133115i]

Running Test: (#<procedure:mul> (complex rectangular 50 . 55) (complex polar 50 . 55)) 
Applying #<procedure:mul> on: [50+55i], [1.1063378130977866+-49.98775866793099i]
Result: (complex polar (real . 3716.517186829626) real . 55.83298126667443)
[2804.64361739109+-2438.539353676175i]

Running Test: (#<procedure:mul> (complex rectangular (rational 40 . 43) real . 45.3) (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:mul> on: [[40 / 43]+45.3i], [3.990371352370838+5.420049489642889i]
Result: (complex polar (real . 304.95717169355714) real . 2.4864344901873388)
[-241.81626852978013+185.80572876439237i]

Running Test: (#<procedure:mul> (rational 40 . 43) (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:mul> on: [40 / 43], [35+36i]
Result: (complex polar (real . 46.706568225611875) real . 0.7994817392203085)
[32.55813953488372+33.48837209302325i]


Running Test: (#<procedure:div> 0.0 9.0) 
Applying #<procedure:div> on: 0.0, 9.0
Result: 0.0
0.0

Running Test: (#<procedure:div> 9 9.0) 
Applying #<procedure:div> on: 9, 9.0
Result: 1.0
1.0

Running Test: (#<procedure:div> 25 25) 
Applying #<procedure:div> on: 25, 25
Result: 1
1

Running Test: (#<procedure:div> 25 26) 
Applying #<procedure:div> on: 25, 26
Result: 25/26
25/26

Running Test: (#<procedure:div> (natural . 30) (natural . 31)) 
Applying #<procedure:div> on: 30, 31
Result: 30/31
30/31

Running Test: (#<procedure:div> (natural . 30) (integer . 36)) 
Applying #<procedure:div> on: 30, 36
Result: 5/6
5/6

Running Test: (#<procedure:div> (integer . 35) (integer . 36)) 
Applying #<procedure:div> on: 35, 36
Result: 35/36
35/36

Running Test: (#<procedure:div> (integer . 35) (natural . 31)) 
Applying #<procedure:div> on: 35, 31
Result: 35/31
35/31

Running Test: (#<procedure:div> (integer . 35) (integer . 35)) 
Applying #<procedure:div> on: 35, 35
Result: 1
1

Running Test: (#<procedure:div> (natural . 31) (natural . 31)) 
Applying #<procedure:div> on: 31, 31
Result: 1
1

Running Test: (#<procedure:div> (rational 40 . 43) (natural . 31)) 
Applying #<procedure:div> on: [40 / 43], 31
Result: (rational 40 . 1333)
[40 / 1333]

Running Test: (#<procedure:div> (rational 40 . 43) (rational 44 . 47)) 
Applying #<procedure:div> on: [40 / 43], [44 / 47]
Result: (rational 470 . 473)
[470 / 473]

Running Test: (#<procedure:div> (rational 44 . 47) (rational 44 . 47)) 
Applying #<procedure:div> on: [44 / 47], [44 / 47]
Result: 1
1

Running Test: (#<procedure:div> (complex rectangular 50 . 55) (complex rectangular 25 . 26)) 
Applying #<procedure:div> on: [50+55i], [25+26i]
Result: (complex polar (real . 2.060760365032941) real . 0.027977772419778724)
[2.0599538816295158+0.057647963105303644i]

Running Test: (#<procedure:div> (complex polar 50 . 55) (complex polar 25 . 26)) 
Applying #<procedure:div> on: [1.1063378130977866+-49.98775866793099i], [16.17298305821601+19.06396126199007i]
Result: (complex polar 2 . 29)
[-1.4961150593780008+-1.327267768425935i]

Running Test: (#<procedure:div> (complex rectangular 25 . 26) (complex rectangular 25 . 26)) 
Applying #<procedure:div> on: [25+26i], [25+26i]
Result: 1
1

Running Test: (#<procedure:div> (complex polar 50 . 55) (complex polar 50 . 55)) 
Applying #<procedure:div> on: [1.1063378130977866+-49.98775866793099i], [1.1063378130977866+-49.98775866793099i]
Result: 1
1

Running Test: (#<procedure:div> (complex polar 50 . 55) 9.0) 
Applying #<procedure:div> on: [1.1063378130977866+-49.98775866793099i], 9.0
Result: (complex polar 50/9 . 55)
[0.12292642367753184+-5.554195407547888i]

Running Test: (#<procedure:div> (complex polar 50 . 55) 25) 
Applying #<procedure:div> on: [1.1063378130977866+-49.98775866793099i], 25
Result: (complex polar 2 . 55)
[0.044253512523911465+-1.9995103467172397i]

Running Test: (#<procedure:div> (natural . 30) (complex polar 50 . 55)) 
Applying #<procedure:div> on: 30, [1.1063378130977866+-49.98775866793099i]
Result: (complex polar 3/5 integer . -55)
[0.01327605375717344+0.5998531040151719i]

Running Test: (#<procedure:div> (integer . 35) (complex polar 50 . 55)) 
Applying #<procedure:div> on: 35, [1.1063378130977866+-49.98775866793099i]
Result: (complex polar 7/10 integer . -55)
[0.015488729383369012+0.6998286213510339i]

Running Test: (#<procedure:div> (rational 40 . 43) (complex polar 50 . 55)) 
Applying #<procedure:div> on: [40 / 43], [1.1063378130977866+-49.98775866793099i]
Result: (complex polar (real . 0.018604651162790697) integer . -55.0)
[0.00041166058161778106+0.018600096248532462i]

Running Test: (#<procedure:div> (complex rectangular 50 . 55) (complex polar 50 . 55)) 
Applying #<procedure:div> on: [50+55i], [1.1063378130977866+-49.98775866793099i]
Result: (complex polar (real . 1.4866068747318506) real . -54.16701873332557)
[-1.0776039344325246+1.0240946052467728i]

Running Test: (#<procedure:div> (complex rectangular (rational 40 . 43) real . 45.3) (complex polar (real . 6.730527468185536) rational 44 . 47)) 
Applying #<procedure:div> on: [[40 / 43]+45.3i], [3.990371352370838+5.420049489642889i]
Result: (complex polar (real . 6.731946395001262) real . 0.614094064655424)
[5.501991506222197+3.879070988088422i]

Running Test: (#<procedure:div> (rational 40 . 43) (complex rectangular (integer . 35) integer . 36)) 
Applying #<procedure:div> on: [40 / 43], [35+36i]
Result: (complex polar (real . 0.018527000486161) real . -0.7994817392203085)
[0.012914771731409648+-0.013283765209449922i]


TESTS FROM EXERCISE 2.87 and 2.88

Running Test: (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:=zero?> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:=zero?> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: #f
False

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (5 3) (4 6) (2 8) (1 (integer . -9)) (0 (integer . -20)))
{3x^5 + 6x^4 + 8x^2 - 9x - 20}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (10 2) (9 8) (8 8) (7 11) (6 11) (5 (integer . -47)) (4 (integer . -35)) (3 (integer . -31)) (2 (integer . -56)) (1 65) (0 75) (0 0))
{2x^10 + 8x^9 + 8x^8 + 11x^7 + 11x^6 - 47x^5 - 35x^4 - 31x^3 - 56x^2 + 65x + 75}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:NEGATE> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -3)) (1 2) (0 5))
{-x^5 - 2x^4 - 3x^2 + 2x + 5}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:NEGATE> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (5 (integer . -2)) (4 (integer . -4)) (2 (integer . -5)) (1 7) (0 15))
{-2x^5 - 4x^4 - 5x^2 + 7x + 15}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -2)) (1 5) (0 10))
{-x^5 - 2x^4 - 2x^2 + 5x + 10}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:sub> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms)
{}

Running Test: (#<procedure:=zero?> (polynomial x polynomial-sparse-terms)) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

TESTS FROM EXERCISE 2.89

Running Test: (#<procedure:term-list> (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:term-list> on: {3x + 5}
Result: (polynomial-dense-terms 1 3 5)
(1 3 5)

'x
Running Test: (#<procedure:term-list> (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:term-list> on: {4x + 9}
Result: (polynomial-dense-terms 1 4 9)
(1 4 9)

'x
Running Test: (#<procedure:term-list> (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:term-list> on: {3x^2 + 5x + 23}
Result: (polynomial-dense-terms 2 3 5 23)
(2 3 5 23)

'x
Running Test: (#<procedure:term-list> (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:term-list> on: {4x^2 + 9x + 19}
Result: (polynomial-dense-terms 2 4 9 19)
(2 4 9 19)

'x
Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:=zero?> on: {3x + 5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:=zero?> on: {4x + 9}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:=zero?> on: {3x^2 + 5x + 23}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:=zero?> on: {4x^2 + 9x + 19}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:=zero?> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:=zero?> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: #f
False

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:add> on: {3x + 5}, {4x + 9}
Result: (polynomial x polynomial-dense-terms 1 7 14)
{7x + 14}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:add> on: {3x^2 + 5x + 23}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-dense-terms 2 7 14 42)
{7x^2 + 14x + 42}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:add> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-dense-terms 5 3 6 0 8 (integer . -9) (integer . -20))
{3x^5 + 6x^4 + 8x^2 - 9x - 20}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:add> on: {3x^2 + 5x + 23}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-dense-terms 5 2 4 0 8 (integer . -2) 8)
{2x^5 + 4x^4 + 8x^2 - 2x + 8}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:add> on: {4x^2 + 9x + 19}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-dense-terms 5 1 2 0 7 7 14)
{x^5 + 2x^4 + 7x^2 + 7x + 14}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:mul> on: {3x + 5}, {4x + 9}
Result: (polynomial x polynomial-sparse-terms (2 12) (1 47) (0 45) (0 0))
{12x^2 + 47x + 45}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:mul> on: {3x^2 + 5x + 23}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-sparse-terms (4 12) (3 47) (2 194) (1 302) (0 437) (0 0))
{12x^4 + 47x^3 + 194x^2 + 302x + 437}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:mul> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (10 2) (9 8) (8 8) (7 11) (6 11) (5 (integer . -47)) (4 (integer . -35)) (3 (integer . -31)) (2 (integer . -56)) (1 65) (0 75) (0 0))
{2x^10 + 8x^9 + 8x^8 + 11x^7 + 11x^6 - 47x^5 - 35x^4 - 31x^3 - 56x^2 + 65x + 75}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:mul> on: {3x^2 + 5x + 23}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (7 3) (6 11) (5 33) (4 55) (3 9) (2 44) (1 (integer . -71)) (0 (integer . -115)) (0 0))
{3x^7 + 11x^6 + 33x^5 + 55x^4 + 9x^3 + 44x^2 - 71x - 115}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:mul> on: {4x^2 + 9x + 19}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (7 8) (6 34) (5 74) (4 96) (3 17) (2 (integer . -28)) (1 (integer . -268)) (0 (integer . -285)) (0 0))
{8x^7 + 34x^6 + 74x^5 + 96x^4 + 17x^3 - 28x^2 - 268x - 285}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:NEGATE> on: {3x^2 + 5x + 23}
Result: (polynomial x polynomial-dense-terms 2 (integer . -3) (integer . -5) (integer . -23))
{-3x^2 - 5x - 23}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:NEGATE> on: {4x^2 + 9x + 19}
Result: (polynomial x polynomial-dense-terms 2 (integer . -4) (integer . -9) (integer . -19))
{-4x^2 - 9x - 19}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:sub> on: {3x^2 + 5x + 23}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-dense-terms 2 (integer . -1) (integer . -4) 4)
{-x^2 - 4x + 4}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:sub> on: {3x^2 + 5x + 23}, {3x^2 + 5x + 23}
Result: (polynomial x polynomial-dense-terms)
{}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:sub> on: {3x^2 + 5x + 23}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-dense-terms 5 (integer . -2) (integer . -4) 0 (integer . -2) 12 38)
{-2x^5 - 4x^4 - 2x^2 + 12x + 38}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:sub> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-dense-terms 5 1 2 0 (integer . -1) (integer . -11) (integer . -24))
{x^5 + 2x^4 - x^2 - 11x - 24}

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms)) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms)) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

Running Test: (#<procedure:rest-terms> (polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:rest-terms> on: (2 3 5 23)
Result: (polynomial-dense-terms 1 5 23)
(1 5 23)

Running Test: (#<procedure:rest-terms> (polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:rest-terms> on: (2 4 9 19)
Result: (polynomial-dense-terms 1 9 19)
(1 9 19)

Running Test: (#<procedure:polynomial-order> (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:polynomial-order> on: {3x^2 + 5x + 23}
Result: 2
2

Running Test: (#<procedure:polynomial-order> (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:polynomial-order> on: {4x^2 + 9x + 19}
Result: 2
2

Running Test: (#<procedure:polynomial-order> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:polynomial-order> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: 5
5

Running Test: (#<procedure:polynomial-order> (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:polynomial-order> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: 5
5

'(3 5 23)
'(4 9 19)
'(1 2 0 3 -2 -5)
'(2 4 0 5 -7 -15)
TESTS FROM EXERCISE 2.90

Running Test: (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (0 0))) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

Running Test: (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:=zero?> on: {5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:=zero?> on: {7x + 5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:=zero?> on: {9x - 25}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 0 0)) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:=zero?> on: {5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:=zero?> on: {3x + 5}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:=zero?> on: {4x + 9}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:=zero?> on: {3x^2 + 5x + 23}
Result: #f
False

Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:=zero?> on: {4x^2 + 9x + 19}
Result: #f
False

'(polynomial x polynomial-sparse-terms (0 0))
'(polynomial-term 6 20)
'(polynomial x polynomial-sparse-terms (0 5))
'(polynomial-term 7 20)
'(polynomial x polynomial-sparse-terms (1 7) (0 5))
'(polynomial-term 9 20)
'(polynomial x polynomial-sparse-terms (1 9) (0 -25))
'(polynomial-term 9 40)
'(polynomial x polynomial-dense-terms 0 0)
'(polynomial x polynomial-dense-terms 0 5)
'(polynomial x polynomial-dense-terms 1 3 5)
'(polynomial x polynomial-dense-terms 1 4 9)
'(polynomial x polynomial-dense-terms 2 3 5 23)
'(polynomial x polynomial-dense-terms 2 4 9 19)
'(polynomial x polynomial-dense-terms 2 3 5 23)
'(polynomial-term 8 20)
'(polynomial x polynomial-dense-terms 2 4 9 19)
'(polynomial-term 30 20)
Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 0))) 
Applying #<procedure:add> on: {}, {}
Result: (polynomial x polynomial-sparse-terms (0 0))
{}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:add> on: {}, {5}
Result: (polynomial x polynomial-sparse-terms (0 5))
{5}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:add> on: {5}, {7x + 5}
Result: (polynomial x polynomial-sparse-terms (1 7) (0 10))
{7x + 10}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:add> on: {7x + 5}, {9x - 25}
Result: (polynomial x polynomial-sparse-terms (1 16) (0 (integer . -20)))
{16x - 20}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:add> on: {9x - 25}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 7) (0 (integer . -30)))
{x^5 + 2x^4 + 3x^2 + 7x - 30}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (5 3) (4 6) (2 8) (1 (integer . -9)) (0 (integer . -20)))
{3x^5 + 6x^4 + 8x^2 - 9x - 20}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}, {3x^10 + 4x^7 - 11x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 (integer . -9)) (4 4) (2 5) (1 (integer . -14)) (0 (integer . -30)))
{3x^10 + 4x^7 - 9x^5 + 4x^4 + 5x^2 - 14x - 30}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {3x^10 + 4x^7 - 11x^5 - 7x - 15}, {4x^12 + 13x^6 - 12x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (12 4) (10 3) (7 4) (6 13) (5 (integer . -23)) (1 (integer . -14)) (0 (integer . -30)))
{4x^12 + 3x^10 + 4x^7 + 13x^6 - 23x^5 - 14x - 30}


Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 0)) 
Applying #<procedure:add> on: {}, {}
Result: (polynomial x polynomial-dense-terms 0 0)
{}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:add> on: {}, {5}
Result: (polynomial x polynomial-dense-terms 0 5)
{5}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:add> on: {5}, {3x + 5}
Result: (polynomial x polynomial-dense-terms 1 3 10)
{3x + 10}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:add> on: {3x + 5}, {4x + 9}
Result: (polynomial x polynomial-dense-terms 1 7 14)
{7x + 14}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:add> on: {4x + 9}, {3x^2 + 5x + 23}
Result: (polynomial x polynomial-dense-terms 2 3 9 32)
{3x^2 + 9x + 32}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:add> on: {3x^2 + 5x + 23}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-dense-terms 2 7 14 42)
{7x^2 + 14x + 42}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:add> on: {4x^2 + 9x + 19}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-dense-terms 5 1 2 0 7 7 14)
{x^5 + 2x^4 + 7x^2 + 7x + 14}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:add> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-dense-terms 5 3 6 0 8 (integer . -9) (integer . -20))
{3x^5 + 6x^4 + 8x^2 - 9x - 20}


Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:add> on: {}, {5}
Result: (polynomial x polynomial-dense-terms 0 5)
{5}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (0 0))) 
Applying #<procedure:add> on: {5}, {}
Result: (polynomial x polynomial-dense-terms 0 5)
{5}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:add> on: {5}, {3x + 5}
Result: (polynomial x polynomial-dense-terms 1 3 10)
{3x + 10}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:add> on: {7x + 5}, {4x + 9}
Result: (polynomial x polynomial-dense-terms 1 11 14)
{11x + 14}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:add> on: {9x - 25}, {3x^2 + 5x + 23}
Result: (polynomial x polynomial-dense-terms 2 3 14 (integer . -2))
{3x^2 + 14x - 2}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:add> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-dense-terms 5 1 2 0 7 7 14)
{x^5 + 2x^4 + 7x^2 + 7x + 14}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:add> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-dense-terms 5 3 6 0 8 (integer . -9) (integer . -20))
{3x^5 + 6x^4 + 8x^2 - 9x - 20}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:add> on: {3x^10 + 4x^7 - 11x^5 - 7x - 15}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-dense-terms 10 3 0 0 4 0 (integer . -9) 4 0 5 (integer . -14) (integer . -30))
{3x^10 + 4x^7 - 9x^5 + 4x^4 + 5x^2 - 14x - 30}


Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:add> on: {}, {5}
Result: (polynomial x polynomial-sparse-terms (0 5))
{5}

Running Test: (#<procedure:add> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 0 0)) 
Applying #<procedure:add> on: {5}, {}
Result: (polynomial x polynomial-sparse-terms (0 5))
{5}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:add> on: {5}, {7x + 5}
Result: (polynomial x polynomial-sparse-terms (1 7) (0 10))
{7x + 10}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:add> on: {3x + 5}, {9x - 25}
Result: (polynomial x polynomial-sparse-terms (1 12) (0 (integer . -20)))
{12x - 20}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:add> on: {4x + 9}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 2) (0 4))
{x^5 + 2x^4 + 3x^2 + 2x + 4}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {3x^2 + 5x + 23}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 8) (1 (integer . -2)) (0 8))
{2x^5 + 4x^4 + 8x^2 - 2x + 8}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {4x^2 + 9x + 19}, {3x^10 + 4x^7 - 11x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (2 4) (1 2) (0 4))
{3x^10 + 4x^7 - 11x^5 + 4x^2 + 2x + 4}

Running Test: (#<procedure:add> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:add> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {4x^12 + 13x^6 - 12x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 (integer . -11)) (4 2) (2 3) (1 (integer . -9)) (0 (integer . -20)))
{4x^12 + 13x^6 - 11x^5 + 2x^4 + 3x^2 - 9x - 20}


Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 0))) 
Applying #<procedure:mul> on: {}, {}
Result: (polynomial x)
{}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:mul> on: {}, {5}
Result: (polynomial x)
{}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:mul> on: {5}, {5}
Result: (polynomial x polynomial-sparse-terms (0 25) (0 0))
{25}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:mul> on: {5}, {7x + 5}
Result: (polynomial x polynomial-sparse-terms (1 35) (0 25) (0 0))
{35x + 25}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:mul> on: {7x + 5}, {9x - 25}
Result: (polynomial x polynomial-sparse-terms (2 63) (1 (integer . -130)) (0 (integer . -125)) (0 0))
{63x^2 - 130x - 125}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:mul> on: {9x - 25}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (6 9) (5 (integer . -7)) (4 (integer . -50)) (3 27) (2 (integer . -93)) (1 5) (0 125) (0 0))
{9x^6 - 7x^5 - 50x^4 + 27x^3 - 93x^2 + 5x + 125}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (10 2) (9 8) (8 8) (7 11) (6 11) (5 (integer . -47)) (4 (integer . -35)) (3 (integer . -31)) (2 (integer . -56)) (1 65) (0 75) (0 0))
{2x^10 + 8x^9 + 8x^8 + 11x^7 + 11x^6 - 47x^5 - 35x^4 - 31x^3 - 56x^2 + 65x + 75}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}, {3x^10 + 4x^7 - 11x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (15 6) (14 12) (12 23) (11 (integer . -5)) (10 (integer . -67)) (9 (integer . -24)) (8 (integer . -28)) (7 (integer . -115)) (6 63) (5 107) (4 (integer . -60)) (3 (integer . -35)) (2 (integer . -26)) (1 210) (0 225) (0 0))
{6x^15 + 12x^14 + 23x^12 - 5x^11 - 67x^10 - 24x^9 - 28x^8 - 115x^7 + 63x^6 + 107x^5 - 60x^4 - 35x^3 - 26x^2 + 210x + 225}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {3x^10 + 4x^7 - 11x^5 - 7x - 15}, {4x^12 + 13x^6 - 12x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (22 12) (19 16) (17 (integer . -44)) (16 39) (15 (integer . -36)) (13 24) (12 (integer . -108)) (11 (integer . -164)) (10 87) (8 (integer . -28)) (7 (integer . -151)) (6 (integer . -34)) (5 345) (2 49) (1 210) (0 225) (0 0))
{12x^22 + 16x^19 - 44x^17 + 39x^16 - 36x^15 + 24x^13 - 108x^12 - 164x^11 + 87x^10 - 28x^8 - 151x^7 - 34x^6 + 345x^5 + 49x^2 + 210x + 225}


Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 0)) 
Applying #<procedure:mul> on: {}, {}
Result: (polynomial x)
{}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:mul> on: {}, {5}
Result: (polynomial x)
{}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:mul> on: {5}, {5}
Result: (polynomial x polynomial-sparse-terms (0 25) (0 0))
{25}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:mul> on: {5}, {3x + 5}
Result: (polynomial x polynomial-sparse-terms (1 15) (0 25) (0 0))
{15x + 25}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:mul> on: {3x + 5}, {4x + 9}
Result: (polynomial x polynomial-sparse-terms (2 12) (1 47) (0 45) (0 0))
{12x^2 + 47x + 45}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:mul> on: {4x + 9}, {3x^2 + 5x + 23}
Result: (polynomial x polynomial-sparse-terms (3 12) (2 47) (1 137) (0 207) (0 0))
{12x^3 + 47x^2 + 137x + 207}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:mul> on: {3x^2 + 5x + 23}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-sparse-terms (4 12) (3 47) (2 194) (1 302) (0 437) (0 0))
{12x^4 + 47x^3 + 194x^2 + 302x + 437}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:mul> on: {4x^2 + 9x + 19}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (7 4) (6 17) (5 37) (4 50) (3 19) (2 19) (1 (integer . -83)) (0 (integer . -95)) (0 0))
{4x^7 + 17x^6 + 37x^5 + 50x^4 + 19x^3 + 19x^2 - 83x - 95}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:mul> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (10 2) (9 8) (8 8) (7 11) (6 11) (5 (integer . -47)) (4 (integer . -35)) (3 (integer . -31)) (2 (integer . -56)) (1 65) (0 75) (0 0))
{2x^10 + 8x^9 + 8x^8 + 11x^7 + 11x^6 - 47x^5 - 35x^4 - 31x^3 - 56x^2 + 65x + 75}


Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:mul> on: {}, {5}
Result: (polynomial x)
{}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (0 0))) 
Applying #<procedure:mul> on: {5}, {}
Result: (polynomial x)
{}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:mul> on: {5}, {3x + 5}
Result: (polynomial x polynomial-sparse-terms (1 15) (0 25) (0 0))
{15x + 25}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:mul> on: {7x + 5}, {4x + 9}
Result: (polynomial x polynomial-sparse-terms (2 28) (1 83) (0 45) (0 0))
{28x^2 + 83x + 45}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:mul> on: {9x - 25}, {3x^2 + 5x + 23}
Result: (polynomial x polynomial-sparse-terms (3 27) (2 (integer . -30)) (1 82) (0 (integer . -575)) (0 0))
{27x^3 - 30x^2 + 82x - 575}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:mul> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-sparse-terms (7 4) (6 17) (5 37) (4 50) (3 19) (2 19) (1 (integer . -83)) (0 (integer . -95)) (0 0))
{4x^7 + 17x^6 + 37x^5 + 50x^4 + 19x^3 + 19x^2 - 83x - 95}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:mul> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (10 2) (9 8) (8 8) (7 11) (6 11) (5 (integer . -47)) (4 (integer . -35)) (3 (integer . -31)) (2 (integer . -56)) (1 65) (0 75) (0 0))
{2x^10 + 8x^9 + 8x^8 + 11x^7 + 11x^6 - 47x^5 - 35x^4 - 31x^3 - 56x^2 + 65x + 75}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:mul> on: {3x^10 + 4x^7 - 11x^5 - 7x - 15}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (15 6) (14 12) (12 23) (11 (integer . -5)) (10 (integer . -67)) (9 (integer . -24)) (8 (integer . -28)) (7 (integer . -115)) (6 63) (5 107) (4 (integer . -60)) (3 (integer . -35)) (2 (integer . -26)) (1 210) (0 225) (0 0))
{6x^15 + 12x^14 + 23x^12 - 5x^11 - 67x^10 - 24x^9 - 28x^8 - 115x^7 + 63x^6 + 107x^5 - 60x^4 - 35x^3 - 26x^2 + 210x + 225}


Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:mul> on: {}, {5}
Result: (polynomial x)
{}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 0 0)) 
Applying #<procedure:mul> on: {5}, {}
Result: (polynomial x)
{}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:mul> on: {5}, {7x + 5}
Result: (polynomial x polynomial-sparse-terms (1 35) (0 25) (0 0))
{35x + 25}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:mul> on: {3x + 5}, {9x - 25}
Result: (polynomial x polynomial-sparse-terms (2 27) (1 (integer . -30)) (0 (integer . -125)) (0 0))
{27x^2 - 30x - 125}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:mul> on: {4x + 9}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (6 4) (5 17) (4 18) (3 12) (2 19) (1 (integer . -38)) (0 (integer . -45)) (0 0))
{4x^6 + 17x^5 + 18x^4 + 12x^3 + 19x^2 - 38x - 45}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {3x^2 + 5x + 23}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (7 6) (6 22) (5 66) (4 107) (3 4) (2 35) (1 (integer . -236)) (0 (integer . -345)) (0 0))
{6x^7 + 22x^6 + 66x^5 + 107x^4 + 4x^3 + 35x^2 - 236x - 345}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {4x^2 + 9x + 19}, {3x^10 + 4x^7 - 11x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (12 12) (11 27) (10 57) (9 16) (8 36) (7 32) (6 (integer . -99)) (5 (integer . -209)) (3 (integer . -28)) (2 (integer . -123)) (1 (integer . -268)) (0 (integer . -285)) (0 0))
{12x^12 + 27x^11 + 57x^10 + 16x^9 + 36x^8 + 32x^7 - 99x^6 - 209x^5 - 28x^3 - 123x^2 - 268x - 285}

Running Test: (#<procedure:mul> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:mul> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {4x^12 + 13x^6 - 12x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (17 4) (16 8) (14 12) (13 (integer . -8)) (12 (integer . -20)) (11 13) (10 14) (9 (integer . -24)) (8 39) (7 (integer . -62)) (6 (integer . -48)) (5 31) (4 (integer . -30)) (3 (integer . -21)) (2 (integer . -31)) (1 65) (0 75) (0 0))
{4x^17 + 8x^16 + 12x^14 - 8x^13 - 20x^12 + 13x^11 + 14x^10 - 24x^9 + 39x^8 - 62x^7 - 48x^6 + 31x^5 - 30x^4 - 21x^3 - 31x^2 + 65x + 75}


Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (0 0))) 
Applying #<procedure:NEGATE> on: {}
Result: (polynomial x polynomial-sparse-terms (0 0))
{}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:NEGATE> on: {5}
Result: (polynomial x polynomial-sparse-terms (0 (integer . -5)))
{-5}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:NEGATE> on: {5}
Result: (polynomial x polynomial-sparse-terms (0 (integer . -5)))
{-5}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:NEGATE> on: {7x + 5}
Result: (polynomial x polynomial-sparse-terms (1 (integer . -7)) (0 (integer . -5)))
{-7x - 5}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:NEGATE> on: {9x - 25}
Result: (polynomial x polynomial-sparse-terms (1 (integer . -9)) (0 25))
{-9x + 25}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:NEGATE> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -3)) (1 2) (0 5))
{-x^5 - 2x^4 - 3x^2 + 2x + 5}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:NEGATE> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (5 (integer . -2)) (4 (integer . -4)) (2 (integer . -5)) (1 7) (0 15))
{-2x^5 - 4x^4 - 5x^2 + 7x + 15}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:NEGATE> on: {3x^10 + 4x^7 - 11x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (10 (integer . -3)) (7 (integer . -4)) (5 11) (1 7) (0 15))
{-3x^10 - 4x^7 + 11x^5 + 7x + 15}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:NEGATE> on: {4x^12 + 13x^6 - 12x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (12 (integer . -4)) (6 (integer . -13)) (5 12) (1 7) (0 15))
{-4x^12 - 13x^6 + 12x^5 + 7x + 15}


Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 0 0)) 
Applying #<procedure:NEGATE> on: {}
Result: (polynomial x polynomial-dense-terms 0 0)
{}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:NEGATE> on: {5}
Result: (polynomial x polynomial-dense-terms 0 (integer . -5))
{-5}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:NEGATE> on: {5}
Result: (polynomial x polynomial-dense-terms 0 (integer . -5))
{-5}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:NEGATE> on: {3x + 5}
Result: (polynomial x polynomial-dense-terms 1 (integer . -3) (integer . -5))
{-3x - 5}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:NEGATE> on: {4x + 9}
Result: (polynomial x polynomial-dense-terms 1 (integer . -4) (integer . -9))
{-4x - 9}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:NEGATE> on: {3x^2 + 5x + 23}
Result: (polynomial x polynomial-dense-terms 2 (integer . -3) (integer . -5) (integer . -23))
{-3x^2 - 5x - 23}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:NEGATE> on: {4x^2 + 9x + 19}
Result: (polynomial x polynomial-dense-terms 2 (integer . -4) (integer . -9) (integer . -19))
{-4x^2 - 9x - 19}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:NEGATE> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-dense-terms 5 (integer . -1) (integer . -2) 0 (integer . -3) 2 5)
{-x^5 - 2x^4 - 3x^2 + 2x + 5}

Running Test: (#<procedure:NEGATE> (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:NEGATE> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-dense-terms 5 (integer . -2) (integer . -4) 0 (integer . -5) 7 15)
{-2x^5 - 4x^4 - 5x^2 + 7x + 15}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 0))) 
Applying #<procedure:sub> on: {}, {}
Result: (polynomial x polynomial-sparse-terms (0 0))
{}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:sub> on: {}, {5}
Result: (polynomial x polynomial-sparse-terms (0 (integer . -5)))
{-5}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:sub> on: {5}, {5}
Result: (polynomial x polynomial-sparse-terms)
{}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:sub> on: {5}, {7x + 5}
Result: (polynomial x polynomial-sparse-terms (1 (integer . -7)))
{-7x}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:sub> on: {7x + 5}, {9x - 25}
Result: (polynomial x polynomial-sparse-terms (1 (integer . -2)) (0 30))
{-2x + 30}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:sub> on: {9x - 25}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -3)) (1 11) (0 (integer . -20)))
{-x^5 - 2x^4 - 3x^2 + 11x - 20}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -2)) (1 5) (0 10))
{-x^5 - 2x^4 - 2x^2 + 5x + 10}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}, {3x^10 + 4x^7 - 11x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (10 (integer . -3)) (7 (integer . -4)) (5 13) (4 4) (2 5))
{-3x^10 - 4x^7 + 13x^5 + 4x^4 + 5x^2}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {3x^10 + 4x^7 - 11x^5 - 7x - 15}, {4x^12 + 13x^6 - 12x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (12 (integer . -4)) (10 3) (7 4) (6 (integer . -13)) (5 1))
{-4x^12 + 3x^10 + 4x^7 - 13x^6 + x^5}


Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 0)) 
Applying #<procedure:sub> on: {}, {}
Result: (polynomial x polynomial-dense-terms 0 0)
{}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:sub> on: {}, {5}
Result: (polynomial x polynomial-dense-terms 0 (integer . -5))
{-5}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:sub> on: {5}, {5}
Result: (polynomial x polynomial-dense-terms)
{}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:sub> on: {5}, {3x + 5}
Result: (polynomial x polynomial-dense-terms 1 (integer . -3) 0)
{-3x}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:sub> on: {3x + 5}, {4x + 9}
Result: (polynomial x polynomial-dense-terms 1 (integer . -1) (integer . -4))
{-x - 4}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:sub> on: {4x + 9}, {3x^2 + 5x + 23}
Result: (polynomial x polynomial-dense-terms 2 (integer . -3) (integer . -1) (integer . -14))
{-3x^2 - x - 14}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:sub> on: {3x^2 + 5x + 23}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-dense-terms 2 (integer . -1) (integer . -4) 4)
{-x^2 - 4x + 4}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:sub> on: {4x^2 + 9x + 19}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-dense-terms 5 (integer . -1) (integer . -2) 0 1 11 24)
{-x^5 - 2x^4 + x^2 + 11x + 24}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:sub> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-dense-terms 5 (integer . -1) (integer . -2) 0 (integer . -2) 5 10)
{-x^5 - 2x^4 - 2x^2 + 5x + 10}


Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 0)) (polynomial x polynomial-dense-terms 0 5)) 
Applying #<procedure:sub> on: {}, {5}
Result: (polynomial x polynomial-dense-terms 0 (integer . -5))
{-5}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (0 0))) 
Applying #<procedure:sub> on: {5}, {}
Result: (polynomial x polynomial-dense-terms 0 5)
{5}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 1 3 5)) 
Applying #<procedure:sub> on: {5}, {3x + 5}
Result: (polynomial x polynomial-dense-terms 1 (integer . -3) 0)
{-3x}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (1 7) (0 5)) (polynomial x polynomial-dense-terms 1 4 9)) 
Applying #<procedure:sub> on: {7x + 5}, {4x + 9}
Result: (polynomial x polynomial-dense-terms 1 3 (integer . -4))
{3x - 4}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (1 9) (0 -25)) (polynomial x polynomial-dense-terms 2 3 5 23)) 
Applying #<procedure:sub> on: {9x - 25}, {3x^2 + 5x + 23}
Result: (polynomial x polynomial-dense-terms 2 (integer . -3) 4 (integer . -48))
{-3x^2 + 4x - 48}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5)) (polynomial x polynomial-dense-terms 2 4 9 19)) 
Applying #<procedure:sub> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {4x^2 + 9x + 19}
Result: (polynomial x polynomial-dense-terms 5 1 2 0 (integer . -1) (integer . -11) (integer . -24))
{x^5 + 2x^4 - x^2 - 11x - 24}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5)) 
Applying #<procedure:sub> on: {2x^5 + 4x^4 + 5x^2 - 7x - 15}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-dense-terms 5 1 2 0 2 (integer . -5) (integer . -10))
{x^5 + 2x^4 + 2x^2 - 5x - 10}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15)) (polynomial x polynomial-dense-terms 5 2 4 0 5 -7 -15)) 
Applying #<procedure:sub> on: {3x^10 + 4x^7 - 11x^5 - 7x - 15}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-dense-terms 10 3 0 0 4 0 (integer . -13) (integer . -4) 0 (integer . -5) 0 0)
{3x^10 + 4x^7 - 13x^5 - 4x^4 - 5x^2}


Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 0 0) (polynomial x polynomial-sparse-terms (0 5))) 
Applying #<procedure:sub> on: {}, {5}
Result: (polynomial x polynomial-sparse-terms (0 (integer . -5)))
{-5}

Running Test: (#<procedure:sub> (polynomial x polynomial-sparse-terms (0 5)) (polynomial x polynomial-dense-terms 0 0)) 
Applying #<procedure:sub> on: {5}, {}
Result: (polynomial x polynomial-sparse-terms (0 5))
{5}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 0 5) (polynomial x polynomial-sparse-terms (1 7) (0 5))) 
Applying #<procedure:sub> on: {5}, {7x + 5}
Result: (polynomial x polynomial-sparse-terms (1 (integer . -7)))
{-7x}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 1 3 5) (polynomial x polynomial-sparse-terms (1 9) (0 -25))) 
Applying #<procedure:sub> on: {3x + 5}, {9x - 25}
Result: (polynomial x polynomial-sparse-terms (1 (integer . -6)) (0 30))
{-6x + 30}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 1 4 9) (polynomial x polynomial-sparse-terms (5 1) (4 2) (2 3) (1 -2) (0 -5))) 
Applying #<procedure:sub> on: {4x + 9}, {x^5 + 2x^4 + 3x^2 - 2x - 5}
Result: (polynomial x polynomial-sparse-terms (5 (integer . -1)) (4 (integer . -2)) (2 (integer . -3)) (1 6) (0 14))
{-x^5 - 2x^4 - 3x^2 + 6x + 14}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 2 3 5 23) (polynomial x polynomial-sparse-terms (5 2) (4 4) (2 5) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {3x^2 + 5x + 23}, {2x^5 + 4x^4 + 5x^2 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (5 (integer . -2)) (4 (integer . -4)) (2 (integer . -2)) (1 12) (0 38))
{-2x^5 - 4x^4 - 2x^2 + 12x + 38}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 2 4 9 19) (polynomial x polynomial-sparse-terms (10 3) (7 4) (5 -11) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {4x^2 + 9x + 19}, {3x^10 + 4x^7 - 11x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (10 (integer . -3)) (7 (integer . -4)) (5 11) (2 4) (1 16) (0 34))
{-3x^10 - 4x^7 + 11x^5 + 4x^2 + 16x + 34}

Running Test: (#<procedure:sub> (polynomial x polynomial-dense-terms 5 1 2 0 3 -2 -5) (polynomial x polynomial-sparse-terms (12 4) (6 13) (5 -12) (1 -7) (0 -15))) 
Applying #<procedure:sub> on: {x^5 + 2x^4 + 3x^2 - 2x - 5}, {4x^12 + 13x^6 - 12x^5 - 7x - 15}
Result: (polynomial x polynomial-sparse-terms (12 (integer . -4)) (6 (integer . -13)) (5 13) (4 2) (2 3) (1 5) (0 10))
{-4x^12 - 13x^6 + 13x^5 + 2x^4 + 3x^2 + 5x + 10}


Running Test: (#<procedure:=zero?> (polynomial x polynomial-dense-terms)) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

Running Test: (#<procedure:=zero?> (polynomial x polynomial-sparse-terms)) 
Applying #<procedure:=zero?> on: {}
Result: #t
True

TESTS FROM EXERCISE 2.91

'(polynomial-list (x polynomial-sparse-terms (0 1)) (x))
'(polynomial-list (x polynomial-sparse-terms (2 1)) (x))
'(polynomial-list (x polynomial-sparse-terms (1 1)) (x polynomial-sparse-terms (1 1) (0 -1)))
'(polynomial-list (x polynomial-sparse-terms (3 1) (1 1)) (x polynomial-sparse-terms (1 1) (0 -1)))
'(polynomial-list (x polynomial-sparse-terms (3 3) (1 -2) (0 -4 1/2)) (x polynomial-sparse-terms (1 (real . 5 1/2)) (0 23) (0 0)))
TESTS FROM EXERCISE 2.92

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
{x}
{{x}y}
{{{x}y}z}
{{{{x}y}z}w}
{{{{{x}y}z}w}v}
{{{{{{x}y}z}w}v}u}
{{{{{{{x}y}z}w}v}u}t}
{{{{{{{{x}y}z}w}v}u}t}s}
{{{{{{{{{x}y}z}w}v}u}t}s}r}
{{{{{{{{{{x}y}z}w}v}u}t}s}r}q}
{{{{{{{{{{{x}y}z}w}v}u}t}s}r}q}p}
Py in terms of x: {{y}x}
Pz in terms of x: {{{y}z}x}
Pw in terms of x: {{{{y}z}w}x}
Pv in terms of x: {{{{{y}z}w}v}x}
Pu in terms of x: {{{{{{y}z}w}v}u}x}
Pt in terms of x: {{{{{{{y}z}w}v}u}t}x}
Ps in terms of x: {{{{{{{{y}z}w}v}u}t}s}x}
Pr in terms of x: {{{{{{{{{y}z}w}v}u}t}s}r}x}
Pq in terms of x: {{{{{{{{{{y}z}w}v}u}t}s}r}q}x}
Pp in terms of x: {{{{{{{{{{{y}z}w}v}u}t}s}r}q}p}x}
Pp in terms of u: {{{{{{{{{{{x}y}z}w}v}t}s}r}q}p}u}
Pt in terms of s: {{{{{{{{x}y}z}w}v}u}t}}
{5x - 9}

{{5x - 9}y + 4}
PP1y in terms of x: {{5y}x + { - 9y + 4}}

{{{5x - 9}y + 4}z + {5x - 9}}
PP1z in terms of x: {{{5y}z + 5}x + {{ - 9y + 4}z - 9}}

{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}
PP1w in terms of x: {{{{5y}z + 5}w + {5y}}x + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}

{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}
PP1v in terms of x: {{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}x + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}

{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}
PP1u in terms of x: {{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}x + {{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}}

{{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}
PP1t in terms of x: {{{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}x + {{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}}

{{{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}s + {{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}}
PP1s in terms of x: {{{{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}s + {{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}}x + {{{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}s + {{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}}}

{{{{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}s + {{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}}r + {{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}}
PP1r in terms of x: {{{{{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}s + {{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}}r + {{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}}x + {{{{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}s + {{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}}r + {{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}}}

{{{{{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}s + {{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}}r + {{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}}q + {{{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}s + {{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}}}
PP1q in terms of x: {{{{{{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}s + {{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}}r + {{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}}q + {{{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}s + {{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}}}x + {{{{{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}s + {{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}}r + {{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}}q + {{{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}s + {{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}}}}

{{{{{{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}s + {{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}}r + {{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}}q + {{{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}s + {{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}}}p + {{{{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}s + {{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}}r + {{{{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}u + {{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}}t + {{{{{5x - 9}y + 4}z + {5x - 9}}w + {{5x - 9}y + 4}}v + {{{5x - 9}y + 4}z + {5x - 9}}}}}}
PP1p in terms of x: {{{{{{{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}s + {{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}}r + {{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}}q + {{{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}s + {{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}}}p + {{{{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}s + {{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}}r + {{{{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}u + {{{5y}z + 5}w + {5y}}}t + {{{{5y}z + 5}w + {5y}}v + {{5y}z + 5}}}}}x + {{{{{{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}s + {{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}}r + {{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}}q + {{{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}s + {{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}}}p + {{{{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}s + {{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}}r + {{{{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}u + {{{ - 9y + 4}z - 9}w + { - 9y + 4}}}t + {{{{ - 9y + 4}z - 9}w + { - 9y + 4}}v + {{ - 9y + 4}z - 9}}}}}}

PP1z is: {{{5x - 9}y + 4}z + {5x - 9}}Running Test: (#<procedure:mul> (polynomial z polynomial-sparse-terms (1 (polynomial y polynomial-sparse-terms (1 (polynomial x polynomial-sparse-terms (1 5) (0 -9))) (0 4))) (0 (polynomial x polynomial-sparse-terms (1 5) (0 -9)))) (polynomial z polynomial-sparse-terms (1 (polynomial y polynomial-sparse-terms (1 (polynomial x polynomial-sparse-terms (1 5) (0 -9))) (0 4))) (0 (polynomial x polynomial-sparse-terms (1 5) (0 -9))))) 
Applying #<procedure:mul> on: {{{5x - 9}y + 4}z + {5x - 9}}, {{{5x - 9}y + 4}z + {5x - 9}}
Result: (polynomial z polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (2 (polynomial x polynomial-sparse-terms (2 25) (1 (integer . -90)) (0 81) (0 0))) (1 (polynomial x polynomial-sparse-terms (1 40) (0 (integer . -72)) (0 0))) (0 16) (0 0))) (1 (polynomial x polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (1 50) (0 0))) (1 (polynomial y polynomial-sparse-terms (1 (integer . -180)) (0 40) (0 0))) (0 (polynomial y polynomial-sparse-terms (1 162) (0 (integer . -72)) (0 0))) (0 0))) (0 (polynomial x polynomial-sparse-terms (2 25) (1 (integer . -90)) (0 81) (0 0))) (0 0))
{{{25x^2 - 90x + 81}y^2 + {40x - 72}y + 16}z^2 + {{50y}x^2 + {-180y + 40}x + {162y - 72}}z + {25x^2 - 90x + 81}}

{{{5x - 9}z}y + {4z + {5x - 9}}}{{{5x - 9}y + 4}z + {{5x - 9}}}{{{5y}z + 5}x + {{ - 9y + 4}z - 9}}Running Test: (#<procedure:mul> (polynomial z polynomial-sparse-terms (1 (polynomial y polynomial-sparse-terms (1 (polynomial x polynomial-sparse-terms (1 5) (0 -9))) (0 4))) (0 (polynomial x polynomial-sparse-terms (1 5) (0 -9)))) (polynomial x polynomial-sparse-terms (1 (polynomial z polynomial-sparse-terms (1 (polynomial y polynomial-sparse-terms (1 5))) (0 5))) (0 (polynomial z polynomial-sparse-terms (1 (polynomial y polynomial-sparse-terms (1 -9) (0 4))) (0 -9))))) 
Applying #<procedure:mul> on: {{{5x - 9}y + 4}z + {5x - 9}}, {{{5y}z + 5}x + {{ - 9y + 4}z - 9}}
Result: (polynomial x polynomial-sparse-terms (2 (polynomial z polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (2 25) (0 0))) (1 (polynomial y polynomial-sparse-terms (1 50) (0 0))) (0 25) (0 0))) (1 (polynomial z polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (2 (integer . -90)) (1 40) (0 0))) (1 (polynomial y polynomial-sparse-terms (1 (integer . -180)) (0 40) (0 0))) (0 (integer . -90)) (0 0))) (0 (polynomial z polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (2 81) (1 (integer . -72)) (0 16) (0 0))) (1 (polynomial y polynomial-sparse-terms (1 162) (0 (integer . -72)) (0 0))) (0 81) (0 0))) (0 0))
{{{25y^2}z^2 + {50y}z + 25}x^2 + {{-90y^2 + 40y}z^2 + {-180y + 40}z - 90}x + {{81y^2 - 72y + 16}z^2 + {162y - 72}z + 81}}

Running Test: (#<procedure:mul> (polynomial x polynomial-sparse-terms (2 (polynomial z polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (2 25) (0 0))) (1 (polynomial y polynomial-sparse-terms (1 50) (0 0))) (0 25) (0 0))) (1 (polynomial z polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (2 (integer . -90)) (1 40) (0 0))) (1 (polynomial y polynomial-sparse-terms (1 (integer . -180)) (0 40) (0 0))) (0 (integer . -90)) (0 0))) (0 (polynomial z polynomial-sparse-terms (2 (polynomial y polynomial-sparse-terms (2 81) (1 (integer . -72)) (0 16) (0 0))) (1 (polynomial y polynomial-sparse-terms (1 162) (0 (integer . -72)) (0 0))) (0 81) (0 0))) (0 0)) (polynomial y polynomial-sparse-terms (2 (polynomial z polynomial-sparse-terms (2 (polynomial x polynomial-sparse-terms (2 25) (1 (integer . -90)) (0 81) (0 0))) (0 0))) (1 (polynomial z polynomial-sparse-terms (2 (polynomial x polynomial-sparse-terms (1 40) (0 (integer . -72)) (0 0))) (1 (polynomial x polynomial-sparse-terms (2 50) (1 (integer . -180)) (0 162) (0 0))) (0 0))) (0 (polynomial z polynomial-sparse-terms (2 16) (1 (polynomial x polynomial-sparse-terms (1 40) (0 (integer . -72)) (0 0))) (0 (polynomial x polynomial-sparse-terms (2 25) (1 (integer . -90)) (0 81) (0 0))) (0 0))) (0 0))) 
Applying #<procedure:mul> on: {{{25y^2}z^2 + {50y}z + 25}x^2 + {{-90y^2 + 40y}z^2 + {-180y + 40}z - 90}x + {{81y^2 - 72y + 16}z^2 + {162y - 72}z + 81}}, {{{25x^2 - 90x + 81}z^2}y^2 + {{40x - 72}z^2 + {50x^2 - 180x + 162}z}y + {16z^2 + {40x - 72}z + {25x^2 - 90x + 81}}}
Result: (polynomial x polynomial-sparse-terms (4 (polynomial y polynomial-sparse-terms (4 (polynomial z polynomial-sparse-terms (4 625) (0 0))) (3 (polynomial z polynomial-sparse-terms (3 2500) (0 0))) (2 (polynomial z polynomial-sparse-terms (2 3750) (0 0))) (1 (polynomial z polynomial-sparse-terms (1 2500) (0 0))) (0 (polynomial z polynomial-sparse-terms (0 625) (0 0))) (0 0))) (3 (polynomial y polynomial-sparse-terms (4 (polynomial z polynomial-sparse-terms (4 (integer . -4500)) (0 0))) (3 (polynomial z polynomial-sparse-terms (4 2000) (3 (integer . -18000)) (0 0))) (2 (polynomial z polynomial-sparse-terms (3 6000) (2 (integer . -27000)) (0 0))) (1 (polynomial z polynomial-sparse-terms (2 6000) (1 (integer . -18000)) (0 0))) (0 (polynomial z polynomial-sparse-terms (1 2000) (0 (integer . -4500)) (0 0))) (0 0))) (2 (polynomial y polynomial-sparse-terms (4 (polynomial z polynomial-sparse-terms (4 12150) (0 0))) (3 (polynomial z polynomial-sparse-terms (4 (integer . -10800)) (3 48600) (0 0))) (2 (polynomial z polynomial-sparse-terms (4 2400) (3 (integer . -32400)) (2 72900) (0 0))) (1 (polynomial z polynomial-sparse-terms (3 4800) (2 (integer . -32400)) (1 48600) (0 0))) (0 (polynomial z polynomial-sparse-terms (2 2400) (1 (integer . -10800)) (0 12150) (0 0))) (0 0))) (1 (polynomial y polynomial-sparse-terms (4 (polynomial z polynomial-sparse-terms (4 (integer . -14580)) (0 0))) (3 (polynomial z polynomial-sparse-terms (4 19440) (3 (integer . -58320)) (0 0))) (2 (polynomial z polynomial-sparse-terms (4 (integer . -8640)) (3 58320) (2 (integer . -87480)) (0 0))) (1 (polynomial z polynomial-sparse-terms (4 1280) (3 (integer . -17280)) (2 58320) (1 (integer . -58320)) (0 0))) (0 (polynomial z polynomial-sparse-terms (3 1280) (2 (integer . -8640)) (1 19440) (0 (integer . -14580)) (0 0))) (0 0))) (0 (polynomial y polynomial-sparse-terms (4 (polynomial z polynomial-sparse-terms (4 6561) (0 0))) (3 (polynomial z polynomial-sparse-terms (4 (integer . -11664)) (3 26244) (0 0))) (2 (polynomial z polynomial-sparse-terms (4 7776) (3 (integer . -34992)) (2 39366) (0 0))) (1 (polynomial z polynomial-sparse-terms (4 (integer . -2304)) (3 15552) (2 (integer . -34992)) (1 26244) (0 0))) (0 (polynomial z polynomial-sparse-terms (4 256) (3 (integer . -2304)) (2 7776) (1 (integer . -11664)) (0 6561) (0 0))) (0 0))) (0 0))
{{{625z^4}y^4 + {2500z^3}y^3 + {3750z^2}y^2 + {2500z}y + {625}}x^4 + {{-4500z^4}y^4 + {2000z^4 - 18000z^3}y^3 + {6000z^3 - 27000z^2}y^2 + {6000z^2 - 18000z}y + {2000z - 4500}}x^3 + {{12150z^4}y^4 + {-10800z^4 + 48600z^3}y^3 + {2400z^4 - 32400z^3 + 72900z^2}y^2 + {4800z^3 - 32400z^2 + 48600z}y + {2400z^2 - 10800z + 12150}}x^2 + {{-14580z^4}y^4 + {19440z^4 - 58320z^3}y^3 + {-8640z^4 + 58320z^3 - 87480z^2}y^2 + {1280z^4 - 17280z^3 + 58320z^2 - 58320z}y + {1280z^3 - 8640z^2 + 19440z - 14580}}x + {{6561z^4}y^4 + {-11664z^4 + 26244z^3}y^3 + {7776z^4 - 34992z^3 + 39366z^2}y^2 + {-2304z^4 + 15552z^3 - 34992z^2 + 26244z}y + {256z^4 - 2304z^3 + 7776z^2 - 11664z + 6561}}}

TESTS FROM EXERCISE 2.93


Computing gcd of: {x^3 + 1} and {x^2 + 1}
Integerizing factor is: 1

Computing gcd of: {x^2 + 1} and {-x + 1}
Integerizing factor is: 1

Computing gcd of: {-x + 1} and {2}
Integerizing factor is: 4

Computing gcd of: {2} and {}
Running Test: (#<procedure:add> (rational (polynomial x polynomial-sparse-terms (3 1) (0 1)) polynomial x polynomial-sparse-terms (2 1) (0 1)) (rational (polynomial x polynomial-sparse-terms (3 1) (0 1)) polynomial x polynomial-sparse-terms (2 1) (0 1))) 
Applying #<procedure:add> on: [{x^3 + 1} / {x^2 + 1}], [{x^3 + 1} / {x^2 + 1}]

Computing gcd of: {2x^5 + 2x^3 + 2x^2 + 2} and {x^4 + 2x^2 + 1}
Integerizing factor is: 1

Computing gcd of: {x^4 + 2x^2 + 1} and {-2x^3 + 2x^2 - 2x + 2}
Integerizing factor is: 4

Computing gcd of: {-2x^3 + 2x^2 - 2x + 2} and {8x^2 + 8}
Integerizing factor is: 64

Computing gcd of: {8x^2 + 8} and {}
Result: (rational (polynomial x polynomial-sparse-terms (3 2) (0 2)) polynomial x polynomial-sparse-terms (2 1) (0 1))
[{2x^3 + 2} / {x^2 + 1}]

TESTS FROM EXERCISE 2.94

Running Test: (#<procedure:greatest-common-divisor> (polynomial x polynomial-sparse-terms (4 1) (3 -1) (2 -2) (1 2)) (polynomial x polynomial-sparse-terms (3 1) (1 -1))) 
Applying #<procedure:greatest-common-divisor> on: {x^4 - x^3 - 2x^2 + 2x}, {x^3 - x}

Computing gcd of: {x^4 - x^3 - 2x^2 + 2x} and {x^3 - x}
Integerizing factor is: 1

Computing gcd of: {x^3 - x} and {-x^2 + x}
Integerizing factor is: 1

Computing gcd of: {-x^2 + x} and {}
Result: (polynomial x polynomial-sparse-terms (2 -1) (1 1) (0 0))
{ - x^2 + x}

TESTS FROM EXERCISE 2.95

Running Test: (#<procedure:greatest-common-divisor> (polynomial x polynomial-sparse-terms (4 11) (3 (integer . -22)) (2 18) (1 (integer . -14)) (0 7) (0 0)) (polynomial x polynomial-sparse-terms (3 13) (2 (integer . -21)) (1 3) (0 5) (0 0))) 
Applying #<procedure:greatest-common-divisor> on: {11x^4 - 22x^3 + 18x^2 - 14x + 7}, {13x^3 - 21x^2 + 3x + 5}

Computing gcd of: {11x^4 - 22x^3 + 18x^2 - 14x + 7} and {13x^3 - 21x^2 + 3x + 5}
Integerizing factor is: 169

Computing gcd of: {13x^3 - 21x^2 + 3x + 5} and {1458x^2 - 2916x + 1458}
Integerizing factor is: 2125764

Computing gcd of: {1458x^2 - 2916x + 1458} and {}
Result: (polynomial x polynomial-sparse-terms (2 1) (1 -2) (0 1) (0 0))
{x^2 - 2x + 1}

TESTS FROM EXERCISE 2.96

P1-2.96 is: {x^2 - 2x + 1}
P2-2.96 is: {11x^2 + 7}
P3-2.96 is: {13x + 5}
Q1-2.96 is: {11x^4 - 22x^3 + 18x^2 - 14x + 7}
Q2-2.96 is: {13x^3 - 21x^2 + 3x + 5}
Running Test: (#<procedure:greatest-common-divisor> (polynomial x polynomial-sparse-terms (4 11) (3 (integer . -22)) (2 18) (1 (integer . -14)) (0 7) (0 0)) (polynomial x polynomial-sparse-terms (3 13) (2 (integer . -21)) (1 3) (0 5) (0 0))) 
Applying #<procedure:greatest-common-divisor> on: {11x^4 - 22x^3 + 18x^2 - 14x + 7}, {13x^3 - 21x^2 + 3x + 5}

Computing gcd of: {11x^4 - 22x^3 + 18x^2 - 14x + 7} and {13x^3 - 21x^2 + 3x + 5}
Integerizing factor is: 169

Computing gcd of: {13x^3 - 21x^2 + 3x + 5} and {1458x^2 - 2916x + 1458}
Integerizing factor is: 2125764

Computing gcd of: {1458x^2 - 2916x + 1458} and {}
Result: (polynomial x polynomial-sparse-terms (2 1) (1 -2) (0 1) (0 0))
{x^2 - 2x + 1}

P1-2.96 is: {x^2 - 2x + 1}TESTS FROM EXERCISE 2.97

P1-2.97 is: {x^2 - 2x + 1}
P2-2.97 is: {11x^2 + 7}
P3-2.97 is: {13x + 5}
Q1-2.97 is: {11x^4 - 22x^3 + 18x^2 - 14x + 7}
Q2-2.97 is: {13x^3 - 21x^2 + 3x + 5}
Running Test: (#<procedure:greatest-common-divisor> (polynomial x polynomial-sparse-terms (4 11) (3 (integer . -22)) (2 18) (1 (integer . -14)) (0 7) (0 0)) (polynomial x polynomial-sparse-terms (3 13) (2 (integer . -21)) (1 3) (0 5) (0 0))) 
Applying #<procedure:greatest-common-divisor> on: {11x^4 - 22x^3 + 18x^2 - 14x + 7}, {13x^3 - 21x^2 + 3x + 5}

Computing gcd of: {11x^4 - 22x^3 + 18x^2 - 14x + 7} and {13x^3 - 21x^2 + 3x + 5}
Integerizing factor is: 169

Computing gcd of: {13x^3 - 21x^2 + 3x + 5} and {1458x^2 - 2916x + 1458}
Integerizing factor is: 2125764

Computing gcd of: {1458x^2 - 2916x + 1458} and {}
Result: (polynomial x polynomial-sparse-terms (2 1) (1 -2) (0 1) (0 0))
{x^2 - 2x + 1}

P1-2.97 is: {x^2 - 2x + 1}

p1-2.97 is: {x + 1}
p2-2.97 is: {x^3 - 1}
p3-2.97 is: {x}
p4-2.97 is: {x^2 - 1}

Computing gcd of: {x + 1} and {x^3 - 1}
Integerizing factor is: 1

Computing gcd of: {x^3 - 1} and {x + 1}
Integerizing factor is: 1

Computing gcd of: {x + 1} and {-2}
Integerizing factor is: 4

Computing gcd of: {-2} and {}

Computing gcd of: {x} and {x^2 - 1}
Integerizing factor is: 1

Computing gcd of: {x^2 - 1} and {x}
Integerizing factor is: 1

Computing gcd of: {x} and {-1}
Integerizing factor is: 1

Computing gcd of: {-1} and {}
rf1-2.97 is: [{x + 1} / {x^3 - 1}]
rf2-2.97 is: [{x} / {x^2 - 1}]
Running Test: (#<procedure:add> (rational (polynomial x polynomial-sparse-terms (1 1) (0 1)) polynomial x polynomial-sparse-terms (3 1) (0 -1)) (rational (polynomial x polynomial-sparse-terms (1 1)) polynomial x polynomial-sparse-terms (2 1) (0 -1))) 
Applying #<procedure:add> on: [{x + 1} / {x^3 - 1}], [{x} / {x^2 - 1}]

Computing gcd of: {x^4 + x^3 + x^2 - 2x - 1} and {x^5 - x^3 - x^2 + 1}
Integerizing factor is: 1

Computing gcd of: {x^5 - x^3 - x^2 + 1} and {x^4 + x^3 + x^2 - 2x - 1}
Integerizing factor is: 1

Computing gcd of: {x^4 + x^3 + x^2 - 2x - 1} and {-x^3 + 2x^2 - x}
Integerizing factor is: 1

Computing gcd of: {-x^3 + 2x^2 - x} and {6x^2 - 5x - 1}
Integerizing factor is: 36

Computing gcd of: {6x^2 - 5x - 1} and {-7x + 7}
Integerizing factor is: 49

Computing gcd of: {-7x + 7} and {}
Result: (rational (polynomial x polynomial-sparse-terms (3 1) (2 2) (1 3) (0 1)) polynomial x polynomial-sparse-terms (4 1) (3 1) (1 -1) (0 -1))
[{x^3 + 2x^2 + 3x + 1} / {x^4 + x^3 - x - 1}]

> 
