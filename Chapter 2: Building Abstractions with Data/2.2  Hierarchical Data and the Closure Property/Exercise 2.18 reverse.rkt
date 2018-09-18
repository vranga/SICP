#lang racket

(define (reverse items)
	(cond
		; Not a list, just return it
		((not (pair? items)) items)
		; List has no elements
		((null? items) (error "The given list is empty"))
		; List has one element
		((null? (cdr items)) items)
		; List has two or more elements
		(else (cons (car (last-pair items)) (reverse (all-but-last-pair items))))
	)
)

; Evaluates the last pair in the list
(define (last-pair items)
	(cond
		; List has no elements
		((null? items) (error "The given list is empty"))
		; List has one element
		((null? (cdr items)) items)
		(else (last-pair (cdr items)))
	)
)

; Evaluates to the same list without the last pair
(define (all-but-last-pair items)
	(cond
		; List has no elements
		((null? items) (error "The given list is empty"))
		; List has one element; return empty list
		((null? (cdr items)) (list))
		; List has two or more elements
		(else (cons (car items) (all-but-last-pair (cdr items))))
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> L
. . L: undefined;
 cannot reference an identifier before its definition
> (define L (list 1 3 5 7 9 11 13 15 17 19 21))
> (reverse L)
'(21 19 17 15 13 11 9 7 5 3 1)
> (define one-through-four (list 1 2 3 4))
> (reverse one-through-four)
'(4 3 2 1)
> (car one-through-four)
1
> (reverse (car one-through-four))
1
> (cdr one-through-four)
'(2 3 4)
> (reverse (cdr one-through-four))
'(4 3 2)
> (car (cdr one-through-four))
2
> (reverse (car (cdr one-through-four)))
2
> (cons 10 one-through-four)
'(10 1 2 3 4)
> (reverse (cons 10 one-through-four))
'(4 3 2 1 10)
> (reverse (list 1 4 9 16 25))
'(25 16 9 4 1)
> (cons (list 1 2) (list 3 4))
'((1 2) 3 4)
> (reverse (cons (list 1 2) (list 3 4)))
'(4 3 (1 2))
> (reverse (list 1 3 (list 5 7) 9))
'(9 (5 7) 3 1)
> (reverse (list (list 7)))
'((7))
> (reverse (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
'((2 (3 (4 (5 (6 7))))) 1)
> 
