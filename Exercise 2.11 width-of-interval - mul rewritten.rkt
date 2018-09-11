#lang racket

(define (make-interval a b)
	(cond
		((> a b) (error "First argument should be lesser than or equal to the second argument"))
	)
	(cons a b)
)

(define (upper-bound interval)
	(cdr interval)
)

(define (lower-bound interval)
	(car interval)
)

(define (width-of-interval interval)
	(/ (- (upper-bound interval) (lower-bound interval)) 2)
)

(define (interval-above-zero? interval)
	(> (lower-bound interval) 0)
)

(define (interval-spans-zero? interval)
	(and
		(>= (upper-bound interval) 0)
		(<= (lower-bound interval) 0)
	)
)

(define (interval-below-zero? interval)
	(< (upper-bound interval) 0)
)

(define (add-interval x y)
	(make-interval
		(+ (lower-bound x) (lower-bound y))
		(+ (upper-bound x) (upper-bound y))
	)
)

(define (sub-interval x y)
	(make-interval
		(- (lower-bound x) (upper-bound y))
		(- (upper-bound x) (lower-bound y))
	)
)

(define (mul-interval-new x y)
	(cond
		(
			(and (interval-above-zero? x) (interval-above-zero? y))
			(make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
		)
		(
		 	(and (interval-above-zero? x) (interval-spans-zero? y))
			(make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
		)
		(
			(and (interval-above-zero? x) (interval-below-zero? y))
			(make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y)))
		)
		(
		 	(and (interval-spans-zero? x) (interval-above-zero? y))
			(make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y)))
		)
		(
		 	(and (interval-spans-zero? x) (interval-spans-zero? y))
			(let
				(
					(p1 (* (lower-bound x) (upper-bound y)))
					(p2 (* (upper-bound x) (lower-bound y)))
					(p3 (* (lower-bound x) (lower-bound y)))
					(p4 (* (upper-bound x) (upper-bound y)))
        )
				(make-interval (min p1 p2) (max p3 p4))
			)
		)
		(
		 	(and (interval-spans-zero? x) (interval-below-zero? y))
			(make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y)))
		)
		(
		 	(and (interval-below-zero? x) (interval-above-zero? y))
			(make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
		)
		(
		 	(and (interval-below-zero? x) (interval-spans-zero? y))
			(make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))
		)
		(
		 	(and (interval-below-zero? x) (interval-below-zero? y))
			(make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))
		)
	)
)

(define (mul-interval x y)
	(let
		((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
		(make-interval
			(min p1 p2 p3 p4)
			(max p1 p2 p3 p4)
		)
	)
)

(define (div-interval x y)
	(cond
		((and (>= (upper-bound y) 0) (<= (lower-bound y) 0)) (error "Cannot divide by interval that spans zero"))
	)
	(mul-interval
		x
		(make-interval
			(/ 1.0 (upper-bound y))
			(/ 1.0 (lower-bound y))
		)
	)
)

; Tests

Welcome to DrRacket, version 6.9 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define A (make-interval 5 10))
(define B (make-interval 4 12))
(mul-interval A B)
'(20 . 120)
> (mul-interval-new A B)
'(20 . 120)
> (define C (make-interval -4 2))
> (mul-interval A C)
'(-40 . 20)
> (mul-interval-new A C)
'(-40 . 20)
> (define D (make-interval -6 -3))
> (mul-interval A D)
'(-60 . -15)
> (mul-interval-new A D)
'(-60 . -15)
> (mul-interval C A)
'(-40 . 20)
> (mul-interval-new C A)
'(-40 . 20)
> (define E (make-interval -5 12))
> (mul-interval C E)
'(-48 . 24)
> (mul-interval-new C E)
'(-48 . 24)
> (define F (make-interval -5 1))
> (mul-interval C F)
'(-10 . 20)
> (mul-interval-new C F)
'(-10 . 20)
> (define G (make-interval -3 13))
> (mul-interval C G)
'(-52 . 26)
> (mul-interval-new C G)
'(-52 . 26)
> (define H (make-interval -3 1))
> (mul-interval C H)
'(-6 . 12)
> (mul-interval-new C H)
'(-6 . 12)
> (mul-interval C D)
'(-12 . 24)
> (mul-interval-new C D)
'(-12 . 24)
> (mul-interval D A)
'(-60 . -15)
> (mul-interval-new D A)
'(-60 . -15)
> (mul-interval D C)
'(-12 . 24)
> (mul-interval-new D C)
'(-12 . 24)
> (define I (make-interval -20 -15))
> (mul-interval D I)
'(45 . 120)
> (mul-interval-new D I)
'(45 . 120)
> 
