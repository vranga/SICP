#lang racket

; Suppose we allow duplicates. For instance, the set {1,2,3} could be represented as the list
; (2 3 2 1 3 2 2). Design procedures element-of-set?, adjoin-set, union-set, and intersection-set
; that operate on this representation.
; How does the efficiency of each compare with the corresponding procedure for the
; non-duplicate representation? Are there applications for which you would use this
; representation in preference to the non-duplicate one?

; Since we allow duplicates, the union operation is simple: just append one set to the other
; This procedure will have the performance characteristics of the primitive 'append' which
; is Theta(n) where n is the number of elements in one of the sets (depending upon which set
; 'append' chooses to walk through to the end.)
(define (union-set set1 set2)
	(append set1 set2)
)

; No change in this implemenation.
; When the first occurrence of x is found, we evaluate to true
; else, continue looking and evaluate to false when we reach the end of the set. I can't think of 
; how to make it more efficient
; Peformance characteristic is Theta(n) where n is the number of elements in the list
; Since duplicates are allowed, the list may be longer for the same set so the performance
; will be accordingly less than when duplicates are not allowed
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
	; Since we allow duplicates, there is no need to check if the element
	; is already present in the set. We just tack it on.
	; So this procedure will work in constant time unlike the earlier implementation
	; which which grows as Theta(n) where n is the number of elements in the set
	(cons x set)
)

; To construct the intersection set, we need to find elements that are present in 
; both the sets. So the checks below are necessary.
; So we don't change this implemenation from the previous exercise.
; Now since duplicate elements are allowed, the lists are likely to be longer, so this procedure
; will be less efficient simply because it handles larger lists for the same sets
; Since intersection-set grows as Theta(n squared), this can become a problem

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
> (define a (list 2 3 2 1 3 2 2))
> (define b (list 3 4 5 3 4 5 6 6 4 3))
> (element-of-set? 0 a)
#f
> (element-of-set? 1 a)
#t
> (element-of-set? 2 a)
#t
> (element-of-set? 3 a)
#t
> (element-of-set? 4 a)
#f
> (element-of-set? 5 a)
#f
> (adjoin-set 10 a)
'(10 2 3 2 1 3 2 2)
> (adjoin-set 10 b)
'(10 3 4 5 3 4 5 6 6 4 3)
> (element-of-set? 10 (adjoin-set 10 a))
#t
> (intersection-set a b)
'(3 3)
> (union-set a b)
'(2 3 2 1 3 2 2 3 4 5 3 4 5 6 6 4 3)
> (element-of-set? 2 (union-set a b))
#t
> (element-of-set? 3 (union-set a b))
#t
> (element-of-set? 4 (union-set a b))
#t
> (element-of-set? 5 (union-set a b))
#t
> (element-of-set? 6 (union-set a b))
#t
> (element-of-set? 7 (union-set a b))
#f
> 

; Comments:

; In this implementation, 'adjoin-set' and 'union-set' are more efficient than their previous
; implementations. 'element-of-set?' and 'intersection-set' have not changed but are likely to
; degrade slightly because the input lists may be longer. So I would use this impelementation
; of set operations when I know that the sets are unlikely to become too large
