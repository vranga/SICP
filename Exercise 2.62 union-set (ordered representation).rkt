#lang racket

; Exercise 2.62.  Give a Theta(n) implementation of union-set for sets represented as
; ordered lists

; In the implementation below, we step through the lists once only. So its performance is
; Theta(n)

(define (union-set set1 set2)
	(cond
		; If either of the sets is null the other one is the union
		((null? set1) set2)
		((null? set2) set1)
		; If the first element of set1 is lesser than the first element of set2 then
		; the first element is added to the union and the remaining data is processed
		; recursively
		((< (car set1) (car set2))
			(cons (car set1) (union-set (cdr set1) set2))
		)
		; If the first element of set1 is equal to the first element of set2 then
		; this element is added to the union and the remaining data is processed
		; recursively
		((= (car set1) (car set2))
			(cons (car set1) (union-set (cdr set1) (cdr set2)))
		)
		; If the first element of set1 is greater than the first element of set2 then
		; the first element of set2 is added to the union and the remaining data is processed
		; recursively
		((> (car set1) (car set2))
			(cons (car set2) (union-set set1 (cdr set2)))
		)
	)
)

(define (adjoin-set x set)
	(cond
		; If the set is null, just construct a new set with only x in it
		((null? set) (list x))
		; If the set already contains x, then evaluate to the set itself
		((= x (car set)) set)
		; If the set does not contain x, then insert it into the set so that the
		; ordering is perserved. This means, just make it the first element of the set
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
> (define a (list -5 -3 -2 0 2 4))
> (define b (list 6 7 9 12 16 22))
> (union-set a b)
'(-5 -3 -2 0 2 4 6 7 9 12 16 22)
> (define c (list 4 6 9 12 16 22))
> (union-set a b)
'(-5 -3 -2 0 2 4 6 7 9 12 16 22)
> (union-set a c)
'(-5 -3 -2 0 2 4 6 9 12 16 22)
> (define d (list 3 4 6 9 12 16 22))
> (union-set a d)
'(-5 -3 -2 0 2 3 4 6 9 12 16 22)
> (define e (list -5 -4 -1 1 3 4 16 22))
> (union-set a e)
'(-5 -4 -3 -2 -1 0 1 2 3 4 16 22)
> (define f (list -5 -3 -2 0 2 4))
> (union-set a f)
'(-5 -3 -2 0 2 4)
> (define g (list -10 -5 -3 -2 0 2 3 4 72))
> (union-set a g)
'(-10 -5 -3 -2 0 2 3 4 72)
> (define g (list -10 -9 -8 -7 -6 -5 -4 -3 -2))
> (define h (list -10 -9 -8 -7 -6 -5 -4 -3 -2))
> (union-set a h)
'(-10 -9 -8 -7 -6 -5 -4 -3 -2 0 2 4)
> (define i (list -10 -9 -8 -7 -6 -5))
> (union-set a i)
'(-10 -9 -8 -7 -6 -5 -3 -2 0 2 4)
> (define j (list -10 -9 -8 -7))
> (union-set a j)
'(-10 -9 -8 -7 -5 -3 -2 0 2 4)
> 
