#lang racket

; Implementation 1
(define (union-set-simple set1 set2)
	; Just adjoin each elements of set2 with set2
	(cond
		; if one set is null, then the union is the other set
		((null? set1) set2)
		((null? set2) set1)
		(else
			(adjoin-set (car set2) (union-set-simple set1 (cdr set2)))
		)
	)
)

; Implementation 2
(define (union-set set1 set2)
	; The union of two sets is a set constructed by combining the following two types of 
	; elements:
	; 1. All elements of set1
	; 2. All elements of set2 that are not present in set 1
	(cond
		; if one set is null, then the union is the other set
		((null? set1) set2)
		((null? set2) set1)
		((not (element-of-set? (car set2) set1))
			(cons (car set2) (union-set set1 (cdr set2)))
		)
		(else
			(union-set set1 (cdr set2))
		)
	)
)

(define (element-of-set? x set)
	(cond
		((null? set) false)
		((equal? x (car set)) true)
		(else
			(element-of-set? x (cdr set))
		)
	)
)

(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)
	)
)

(define (intersection-set set1 set2)
	(cond
		((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
			(cons (car set1) (intersection-set (cdr set1) set2))
		)
		(else
			(intersection-set (cdr set1) set2)
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define a (list 1 2 3 4 5))
> (define b (list 6 7 8 9 -1 0))
> (adjoin-set 45 a)
'(45 1 2 3 4 5)
> (adjoin-set 45 b)
'(45 6 7 8 9 -1 0)
> (intersection-set a b)
'()
> (union-set a b)
'(6 7 8 9 -1 0 1 2 3 4 5)
> (union-set-simple a b)
'(6 7 8 9 -1 0 1 2 3 4 5)
> (define c (list 3 4 5 6 7 8 9))
> (union-set a c)
'(6 7 8 9 1 2 3 4 5)
> (union-set-simple a c)
'(6 7 8 9 1 2 3 4 5)
> (define d (list 2 3 4))
> (union-set a d)
'(1 2 3 4 5)
> (union-set-simple a d)
'(1 2 3 4 5)
> 
