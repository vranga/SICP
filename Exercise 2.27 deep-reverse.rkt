#lang racket

(define (deep-reverse items)
	(cond
		; Not a list, just return it
		((not (pair? items)) items)
		; List has no elements, so just return it
		((null? items) items)

		; List has one element
		((null? (cdr items))
			(cond
				((pair? (car items)) (list (deep-reverse (car items))))
				(else items)
			)
		)

		; List has two or more elements
		(else
			; (cons (deep-reverse (last-pair items)) (deep-reverse (all-but-last-pair items)))
			(cons (deep-reverse (car (last-pair items))) (deep-reverse (all-but-last-pair items)))
		)
	)
)

(define (reverse items)
	(cond
		; List has no elements, so just return it
		((null? items) items)
		; List has one element
		((null? (cdr items)) items)
		; List has two or more elements
		(else (cons (car (last-pair items)) (reverse (all-but-last-pair items))))
	)
)

; Evaluates the last pair in the list
(define (last-pair items)
	(cond
		; List has no elements, so just return it
		((null? items) items)
		; List has one element
		((null? (cdr items)) items)
		(else (last-pair (cdr items)))
	)
)

; Evaluates to the same list without the last pair
(define (all-but-last-pair items)
	(cond
		; List has no elements, so just return it
		((null? items) items)
		; List has one element; return empty list
		((null? (cdr items)) (list))
		; List has two or more elements
		(else (cons (car items) (all-but-last-pair (cdr items))))
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define a (list))
a
(last-pair a)
(all-but-last-pair a)
(reverse a)
(deep-reverse a)

'()
'()
'()
'()
'()
> (define b (list 5))
b
(last-pair b)
(all-but-last-pair b)
(reverse b)
(deep-reverse b)
'(5)
'(5)
'()
'(5)
'(5)
> (define c (list 5 7))
c
(last-pair c)
(all-but-last-pair c)
(reverse c)
(deep-reverse c)
'(5 7)
'(7)
'(5)
'(7 5)
'(7 5)
> (define c1 (list 5 7 8))
c1
(last-pair c1)
(all-but-last-pair c1)
(reverse c1)
(deep-reverse c1)
'(5 7 8)
'(8)
'(5 7)
'(8 7 5)
'(8 7 5)
> (define c2 (list 5 7 8 6 0 1 3 9 2 4))
c2
(last-pair c2)
(all-but-last-pair c2)
(reverse c2)
(deep-reverse c2)
'(5 7 8 6 0 1 3 9 2 4)
'(4)
'(5 7 8 6 0 1 3 9 2)
'(4 2 9 3 1 0 6 8 7 5)
'(4 2 9 3 1 0 6 8 7 5)
> (define x (list (list 1 2) (list 3 4)))
x
(last-pair x)
(all-but-last-pair x)
(reverse x)
(deep-reverse x)
'((1 2) (3 4))
'((3 4))
'((1 2))
'((3 4) (1 2))
'((4 3) (2 1))
> (define d (list (list 1 (list 5 7 8)) (list 3 4 (list 29 43) 6 9)))
d
(last-pair d)
(all-but-last-pair d)
(reverse d)
(deep-reverse d)

'((1 (5 7 8)) (3 4 (29 43) 6 9))
'((3 4 (29 43) 6 9))
'((1 (5 7 8)))
'((3 4 (29 43) 6 9) (1 (5 7 8)))
'((9 6 (43 29) 4 3) ((8 7 5) 1))
> (deep-reverse (list 1 3 (list 5 7) 9))
'(9 (7 5) 3 1)
> (reverse (list 1 3 (list 5 7) 9))
'(9 (5 7) 3 1)
> (reverse (list (list 7)))
'((7))
> (deep-reverse (list (list 7)))
'((7))
> (reverse (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
'((2 (3 (4 (5 (6 7))))) 1)
> (deep-reverse (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
'((((((7 6) 5) 4) 3) 2) 1)
> 
