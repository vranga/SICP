#lang racket

; Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an accumulation:

; (define (count-leaves t)
;   (accumulate <??> <??> (map <??> <??>)))

; SOLUTION

(define (count-leaves t)
	; (accumulate <??> <??> (map <??> <??>))
	(accumulate
		(lambda (x y)
			(+ x y)
		)
		0
		; The lambda function below returns a 1 regardless of its input
		; This allows us to enumerate the leaves in the tree
		(map (lambda (x) 1) (fringe t))
	)
)

; The following procedure also accomplishes the task but it does not
; use 'map'. (Therefore it does not adhere strictly to the template
; given in the problem.) Accordingly, the operator has been modified to
; produce the correct result

(define (count-leaves1 t)
	; (accumulate <??> <??> (map <??> <??>))
	(accumulate
		(lambda (x y)
			(+ y 1)
		)
		0
		; (map <??> <??>)
		(fringe t)
	)
)

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence) (accumulate op initial (cdr sequence)))
	)
)

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
> (count-leaves (list 1 (list 2 3)))
(count-leaves1 (list 1 (list 2 3)))
3
3
> (count-leaves (list (list 1 2) 3))
(count-leaves1 (list (list 1 2) 3))
3
3
> (define x (list (list 1 2) (list 3 4)))
(count-leaves (list x x))
(count-leaves1 (list x x))
8
8
> (count-leaves (list 1 2 3 4))
(count-leaves1 (list 1 2 3 4))
4
4
> (count-leaves (cons (list 1 2) (list 3 4)))
(count-leaves1 (cons (list 1 2) (list 3 4)))
4
4
> (count-leaves (list 1 3 (list 5 7) 9))
(count-leaves1 (list 1 3 (list 5 7) 9))
5
5
> (count-leaves (list (list 7)))
(count-leaves1 (list (list 7)))
1
1
> (count-leaves (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(count-leaves1 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
7
7
> (count-leaves (list (list 1 2) (list 3 4)))
(count-leaves1 (list (list 1 2) (list 3 4)))
4
4
> (count-leaves (list (list 3 4) (list 1 2)))
(count-leaves1 (list (list 3 4) (list 1 2)))
4
4
> (count-leaves (list (list 4 3) (list 2 1)))
(count-leaves1 (list (list 4 3) (list 2 1)))
4
4
> 
