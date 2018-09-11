#lang racket

; Exercise 2.61.  Give an implementation of adjoin-set using the ordered representation.
; By analogy with element-of-set? show how to take advantage of the ordering to produce a
; procedure that requires on the average about half as many steps as with the
; unordered representation.

(define (adjoin-set x set)
	(cond
		; If the set is null, just construct a new set with only x in it
		((null? set) (list x))
		; If the set already contains x, then evaluate to the set itself
		((= x (car set)) set)
		; If the set does not contain x, then insert it into the set so that the
		; ordering is perserved. (This means, just make it the first element of the set
		((< x (car set))
			(cons x set)
		)
		(else
			; continue looking
			(cons (car set) (adjoin-set x (cdr set)))
		)
	)
)

(define (element-of-set? x set)
	(cond
		((null? set) false)
		((= x (car set)) true)
		((< x (car set)) false)
		(else
			(element-of-set? x (cdr set))
		)
	)
)

(define (intersection-set set1 set2)
	(if (or (null? set1) (null? set2))
		'()    
		(let ((x1 (car set1)) (x2 (car set2)))
			(cond
				((= x1 x2)
					(cons x1
					(intersection-set (cdr set1)
					(cdr set2)))
				)
				((< x1 x2)
					(intersection-set (cdr set1) set2)
				)
				((< x2 x1)
					(intersection-set set1 (cdr set2))
				)
			)
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define a (list 1 2 4 6 9 11 20))
> (adjoin-set 0 a)
'(0 1 2 4 6 9 11 20)
> (adjoin-set 1 a)
'(1 2 4 6 9 11 20)
> (adjoin-set 2 a)
'(1 2 4 6 9 11 20)
> (adjoin-set 3 a)
'(1 2 3 4 6 9 11 20)
> (adjoin-set 4 a)
'(1 2 4 6 9 11 20)
> (adjoin-set 5 a)
'(1 2 4 5 6 9 11 20)
> (adjoin-set 6 a)
'(1 2 4 6 9 11 20)
> (adjoin-set 7 a)
'(1 2 4 6 7 9 11 20)
> (adjoin-set 8 a)
'(1 2 4 6 8 9 11 20)
> (adjoin-set 8 (adjoin-set 7 a))
'(1 2 4 6 7 8 9 11 20)
> (adjoin-set 7 (adjoin-set 8 a))
'(1 2 4 6 7 8 9 11 20)
> (adjoin-set 300 a)
'(1 2 4 6 9 11 20 300)
> (adjoin-set 17 a)
'(1 2 4 6 9 11 17 20)
> (adjoin-set -17 a)
'(-17 1 2 4 6 9 11 20)
> 
