#lang racket

(define (make-center-percent c p)
	(cond
		((< p 0) (error "Percent value should be non-negative"))
	)
	(let
		((w (abs (/ (* c p) 100.0))))
		(make-center-width c w)
	)
)

(define (make-center-width c w)
	(cond
		((< w 0) (error "Width should be non-negative"))
	)
	(make-interval (- c w) (+ c w))
)

(define (percent i)
	(* (/ (width i) (abs (center i))) 100.0)
)

(define (center i)
	(/ (+ (lower-bound i) (upper-bound i)) 2)
)

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2)
)

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
> (define AP (make-center-percent 10 20))
AP
(make-center-width 10 2)
(make-interval 8 12)
(define BP (make-center-percent 8 50))
BP
(make-center-width 8 4)
(make-interval 4 12)

'(8.0 . 12.0)
'(8 . 12)
'(8 . 12)
'(4.0 . 12.0)
'(4 . 12)
'(4 . 12)
> (mul-interval AP BP)
(mul-interval-new AP BP)
'(32.0 . 144.0)
'(32.0 . 144.0)
> (define CP (make-center-percent -1 300))
(make-center-width -1 3)
(make-interval -4 2)
'(-4 . 2)
'(-4 . 2)
> (define CP (make-center-percent -1 300))
CP
(make-center-width -1 3)
(make-interval -4 2)
'(-4.0 . 2.0)
'(-4 . 2)
'(-4 . 2)
> (mul-interval AP CP)
(mul-interval-new AP CP)
'(-48.0 . 24.0)
'(-48.0 . 24.0)
> (define DP (make-center-percent -4 50))
DP
(make-center-width -4 -2)
(make-interval -6 -3)
'(-6.0 . -2.0)
. . Width should be non-negative
> (define DP (make-center-percent -4 50))
DP
(make-center-width -4 2)
(make-interval -6 -3)
'(-6.0 . -2.0)
'(-6 . -2)
'(-6 . -3)
> (define DP (make-center-percent -4 50))
DP
(make-center-width -4 2)
(make-interval -6 -2)

'(-6.0 . -2.0)
'(-6 . -2)
'(-6 . -2)
> (mul-interval AP DP)
(mul-interval-new AP DP)
(mul-interval CP AP)
(mul-interval-new CP AP)
'(-72.0 . -16.0)
'(-72.0 . -16.0)
'(-48.0 . 24.0)
'(-48.0 . 24.0)
> (define EP (make-center-percent 4 225))
EP
(make-center-width 4 9)
(make-interval -5 13)

'(-5.0 . 13.0)
'(-5 . 13)
'(-5 . 13)
> (mul-interval CP EP)
(mul-interval-new CP EP)
'(-52.0 . 26.0)
'(-52.0 . 26.0)
> (define FP (make-center-percent -2 150))
FP
(make-center-width -2 3)
(make-interval -5 1)
'(-5.0 . 1.0)
'(-5 . 1)
'(-5 . 1)
> (mul-interval CP FP)
(mul-interval-new CP FP)
'(-10.0 . 20.0)
'(-10.0 . 20.0)
> (define GP (make-center-percent 5 160))
GP
(make-center-width 5 8)
(make-interval -3 13)

'(-3.0 . 13.0)
'(-3 . 13)
'(-3 . 13)
> (mul-interval CP GP)
(mul-interval-new CP GP)
'(-52.0 . 26.0)
'(-52.0 . 26.0)
> (define HP (make-interval -1 200))
HP
(make-center-width -1 2)
(make-interval -3 1)

'(-1 . 200)
'(-3 . 1)
'(-3 . 1)
> (define HP (make-center-percent -1 200))
HP
(make-center-width -1 2)
(make-interval -3 1)
'(-3.0 . 1.0)
'(-3 . 1)
'(-3 . 1)
> (mul-interval CP HP)
(mul-interval-new CP HP)
(mul-interval CP DP)
(mul-interval-new CP DP)
(mul-interval DP AP)
(mul-interval-new DP AP)
(mul-interval DP CP)
(mul-interval-new DP CP)
'(-6.0 . 12.0)
'(-6.0 . 12.0)
'(-12.0 . 24.0)
'(-12.0 . 24.0)
'(-72.0 . -16.0)
'(-72.0 . -16.0)
'(-12.0 . 24.0)
'(-12.0 . 24.0)
> (define IP (make-center-percent -16 25))
IP
(make-center-width -16 4)
(make-interval -20 -12)
'(-20.0 . -12.0)
'(-20 . -12)
'(-20 . -12)
> (mul-interval DP IP)
(mul-interval-new DP IP)
'(24.0 . 120.0)
'(24.0 . 120.0)
> (percent AP)
20.0
> (percent BP)
50.0
> (percent CP)
300.0
> (percent DP)
50.0
> (percent EP)
225.0
> (percent FP)
150.0
> (percent GP)
160.0
> (percent HP)
200.0
> (percent IP)
25.0
> 
