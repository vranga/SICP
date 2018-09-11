#lang racket

; Matrix Operations

(define (dot-product v w)
	(accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
	(map
		(lambda (vec)
			(dot-product vec v)
		)
		m
	)
)

(define (transpose mat)
	(accumulate-n cons null mat)
)

(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map
	 		(lambda (x)
 				(matrix-*-vector cols x)
 			)
			m
		)
	)
)

(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		null
		(cons
			(accumulate op init (seq-of-first-elements seqs))
			(accumulate-n op init (seqs-without-first-elements seqs))
		)
	)
)

(define (seq-of-first-elements seqs)
	(cond
		; the contained lists are empty
		((null? (car seqs)) seqs)
		; there is only one contained list
		((null? (cdr seqs)) (list (car (car seqs))))
		(else
			(cons (car (car seqs)) (seq-of-first-elements (cdr seqs)))
		)
	)
)

(define (seqs-without-first-elements seqs)
	(cond
		; the contained lists are empty
		((null? (car seqs)) seqs)
		; there is only one contained list
		((null? (cdr seqs)) (list (cdr (car seqs))))
		(else
			(cons (cdr (car seqs)) (seqs-without-first-elements (cdr seqs)))
		)
	)
)

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence) (accumulate op initial (cdr sequence)))
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.

> (define v (list -1 -3 -5 -7))
> v
'(-1 -3 -5 -7)
> (define w (list 5 6 7 8))
> w
'(5 6 7 8)
> (dot-product v w)
-114
> (define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
> m
'((1 2 3 4) (4 5 6 6) (6 7 8 9))
> (dot-product v (car m))
-50
> (dot-product v (car (cdr m)))
-91
> (dot-product v (car (cdr (cdr m))))
-130
> (matrix-*-vector m v)
'(-50 -91 -130)
> (transpose m)
'((1 4 6) (2 5 7) (3 6 8) (4 6 9))
> (transpose (transpose m))
'((1 2 3 4) (4 5 6 6) (6 7 8 9))
> 
> (define m (list (list 1 2) (list 3 4) (list 5 6)))
> (define n (list (list 7 8 9) (list 10 11 12)))
> m
'((1 2) (3 4) (5 6))
> n
'((7 8 9) (10 11 12))
> (matrix-*-matrix m n)
'((27 30 33) (61 68 75) (95 106 117))
> (matrix-*-matrix m (transpose m))
'((5 11 17) (11 25 39) (17 39 61))
> (define sem (list (list 5)))
> (transpose sem)
'((5))
> (matrix-*-matrix sem sem)
'((25))
> (define square-matrix (list (list -4 -3 -2) (list -1 0 1) (list 2 3 4)))
> square-matrix
'((-4 -3 -2) (-1 0 1) (2 3 4))
> (transpose square-matrix)
'((-4 -1 2) (-3 0 3) (-2 1 4))
> (matrix-*-matrix square-matrix square-matrix)
'((15 6 -3) (6 6 6) (-3 6 15))
> 
