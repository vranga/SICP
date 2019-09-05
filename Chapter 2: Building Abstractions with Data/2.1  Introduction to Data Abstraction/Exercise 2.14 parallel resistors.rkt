#lang racket

; After considerable work, Alyssa P. Hacker delivers her finished system. Several years later,
; after she has forgotten all about it, she gets a frenzied call from an irate user, Lem E.
; Tweakit. It seems that Lem has noticed that the formula for parallel resistors can be
; written in two algebraically equivalent ways:

; R1 * R2
; -------
; R1 + R2

; and

;        1
; ---------------
; (1/R1) + (1/R2)

; He has written the following two programs, each of which computes the parallel-resistors
; formula differently:

; (define (par1 r1 r2)
;   (div-interval (mul-interval r1 r2)
;                 (add-interval r1 r2)))
; (define (par2 r1 r2)
;   (let ((one (make-interval 1 1))) 
;     (div-interval one
;                   (add-interval (div-interval one r1)
;                                 (div-interval one r2)))))

; Lem complains that Alyssa's program gives different answers for the two ways of computing.
; This is a serious complaint.

; Exercise 2.14.  Demonstrate that Lem is right. Investigate the behavior of the system on a
; variety of arithmetic expressions. Make some intervals A and B, and use them in computing
; the expressions A/A and A/B. You will get the most insight by using intervals whose width
; is a small percentage of the center value. Examine the results of the computation in
; center-percent form (see exercise 2.12).

; SOLUTION

(define (par1 r1 r2)
	(div-interval
		(mul-interval r1 r2)
		(add-interval r1 r2)
	)
)

(define (par2 r1 r2)
	(let ((one (make-interval 1 1))) 
		(div-interval
			one
			(add-interval (div-interval one r1) (div-interval one r2))
		)
	)
)

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

(define (show-int i)
	(display "Center: ")
	(display (center i))
	(newline)
	(display "Width: ")
	(display (width i))
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

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define R1 (make-center-percent 25 0.2))
> (show-int R1)
Center: 25.0
Width: 0.05000000000000071
> ; Expressions that algebraically evaluate to 1
(show-int (div-interval R1 R1))
Center: 1.0000080000320002
Width: 0.0040000160000639995
> ; Expressions that algebraically evaluate to 1
(show-int (div-interval (div-interval (mul-interval R1 R1) R1) R1))
Center: 1.0000320002560015
Width: 0.008000096000640056
> ; Expressions that algebraically evaluate to 1
(show-int (div-interval (div-interval (div-interval (mul-interval (mul-interval R1 R1) R1) R1) R1) R1))
          
Center: 1.0000720010560094
Width: 0.012000304003264128

; We can see that as intervals get repeated, the error bounds become looser and the center shifts too

> ; Expressions that algebraically evaluate to 0
(show-int (sub-interval R1 R1))
Center: 0.0
Width: 0.10000000000000142
> ; Expressions that algebraically evaluate to 0
(show-int (sub-interval (sub-interval (add-interval R1 R1) R1) R1))
Center: 0.0
Width: 0.20000000000000284
> (show-int (sub-interval (mul-interval R1 R1) (mul-interval R1 R1)))
Center: 0.0
Width: 5.000000000000114
> (define R2 (make-center-percent 45 0.1))
> (par1 R1 R2)
'(16.001530066338542 . 16.14158143194335)
> (par2 R1 R2)
'(16.045021815320794 . 16.09782794778515)
> (show-int (par1 R1 R2))
Center: 16.07155574914095
Width: 0.07002568280240418
> (show-int (par2 R1 R2))
Center: 16.07142488155297
Width: 0.026403066232177252
> ; we can see that par2 above produces tighter error bounds
(show-int (div-interval R1 R1))
Center: 1.0000080000320002
Width: 0.0040000160000639995
> (show-int (div-interval R2 R2))
Center: 1.000002000002
Width: 0.002000002000001999
> (show-int (div-interval R1 R2))
Center: 0.5555572222238889
Width: 0.0016666683333350085
> (show-int (div-interval R2 R1))
Center: 1.8000108000432
Width: 0.005400021600086347
> (show-int (mul-interval (div-interval R2 R1) (div-interval R1 R2)))
Center: 1.0000180000900003
Width: 0.006000042000186001
> (show-int (div-interval (mul-interval (div-interval R2 R1) (div-interval R1 R2)) (mul-interval (div-interval R2 R1) (div-interval R1 R2))))
Center: 1.000072001008008
Width: 0.012000300002964037
> ; the above two tests show that even though the expressions algebraically evaluate to 1, the error increases with repeated variables

