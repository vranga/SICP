#lang racket

; Exercise 2.39.   Complete the following definitions of reverse (exercise 2.18) in terms of
; fold-right and fold-left from exercise 2.38:

; (define (reverse sequence)
;   (fold-right (lambda (x y) <??>) nil sequence))
; (define (reverse sequence)
;   (fold-left (lambda (x y) <??>) nil sequence))

; SOLUTION

(define (reverse-fr sequence)
	(cond
		; Not a list or empty list, just return it
		((or (not (pair? sequence)) (null? sequence)) sequence)
		; List has one or more elements
		(else
			(fold-right
				(lambda (x y)
					(append y (list x))
				)
				null
				sequence
			)
		)
	)
)

(define (reverse-fl sequence)
	(cond
		; Not a list or empty list, just return it
		((or (not (pair? sequence)) (null? sequence)) sequence)
		; List has one or more elements
		(else
			(fold-left
				(lambda (x y)
					(append (list y) x)
				)
				null
				sequence
			)
		)
	)
)

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

; fold-right and fold-left

(define (fold-right op initial sequence)
	(accumulate op initial sequence)
)

(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest)) (cdr rest))
		)
	)
	(iter initial sequence)
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
> (reverse (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
'((2 (3 (4 (5 (6 7))))) 1)
> (reverse-fr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
'((2 (3 (4 (5 (6 7))))) 1)
> (reverse-fl (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
'((2 (3 (4 (5 (6 7))))) 1)
> (define one-through-four (list 1 2 3 4))
(reverse (car one-through-four))
(reverse-fr (car one-through-four))
(reverse-fl (car one-through-four))
1
1
1
> (define L (list 1 3 5 7 9 11 13 15 17 19 21))
L
(reverse L)
(reverse-fr L)
(reverse-fl L)
'(1 3 5 7 9 11 13 15 17 19 21)
'(21 19 17 15 13 11 9 7 5 3 1)
'(21 19 17 15 13 11 9 7 5 3 1)
'(21 19 17 15 13 11 9 7 5 3 1)
> (reverse 10)
(reverse-fr 10)
(reverse-fl 10)

10
10
10
> (define sel (list 25))
sel
(reverse sel)
(reverse-fr sel)
(reverse-fl sel)
'(25)
'(25)
'(25)
'(25)
> (reverse (cdr one-through-four))
(reverse-fr (cdr one-through-four))
(reverse-fl (cdr one-through-four))

'(4 3 2)
'(4 3 2)
'(4 3 2)
> (reverse (list 1 4 9 16 25))
(reverse-fr (list 1 4 9 16 25))
(reverse-fl (list 1 4 9 16 25))

'(25 16 9 4 1)
'(25 16 9 4 1)
'(25 16 9 4 1)
> (reverse (cons (list 1 2) (list 3 4)))
(reverse-fr (cons (list 1 2) (list 3 4)))
(reverse-fl (cons (list 1 2) (list 3 4)))
'(4 3 (1 2))
'(4 3 (1 2))
'(4 3 (1 2))
> (reverse (list 1 3 (list 5 7) 9))
(reverse-fr (list 1 3 (list 5 7) 9))
(reverse-fl (list 1 3 (list 5 7) 9))
'(9 (5 7) 3 1)
'(9 (5 7) 3 1)
'(9 (5 7) 3 1)
> (reverse (list (list 7)))
(reverse-fr (list (list 7)))
(reverse-fl (list (list 7)))
'((7))
'((7))
'((7))
> (reverse (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(reverse-fr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(reverse-fl (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
'((2 (3 (4 (5 (6 7))))) 1)
'((2 (3 (4 (5 (6 7))))) 1)
'((2 (3 (4 (5 (6 7))))) 1)
> 
