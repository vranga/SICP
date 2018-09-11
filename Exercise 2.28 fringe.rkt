#lang racket

(define (fringe items)
	(cond
		; List has no elements, so return it
		((null? items) items)

		; Not a list, so return it as a single-element list
		((not (pair? items)) (list items))

		; List is non-empty
		(else
			(append (fringe (car items)) (fringe (cdr items)))
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define x (list 1 (list 2 3)))
> x
'(1 (2 3))
> (fringe x)
'(1 2 3)
> (define x (list (list 1 2) 3))
> (fringe x)
'(1 2 3)
> (define x (list (list 1 2) (list 3 4)))
> (fringe x)
'(1 2 3 4)
> (fringe (list x x))
'(1 2 3 4 1 2 3 4)
> (define one-through-four (list 1 2 3 4))
> (fringe one-through-four)
'(1 2 3 4)
> (cons (list 1 2) (list 3 4))
'((1 2) 3 4)
> (fringe (cons (list 1 2) (list 3 4)))
'(1 2 3 4)
> (fringe (list 1 3 (list 5 7) 9))
'(1 3 5 7 9)
> (fringe (list (list 7)))
'(7)
> (fringe (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
'(1 2 3 4 5 6 7)
> (fringe (list (list 1 2) (list 3 4)))
'(1 2 3 4)
> (fringe (list (list 3 4) (list 1 2)))
'(3 4 1 2)
> (fringe (list (list 4 3) (list 2 1)))
'(4 3 2 1)
> 
