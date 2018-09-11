#lang racket

(define (make-mobile left right)
	(cons left right)
)

(define (make-branch length structure)
	(cons length structure)
)

; Part a

(define (left-branch mobile)
	(car mobile)
)

(define (right-branch mobile)
	; (car (cdr mobile)) ----> Change 1
	(cdr mobile)
)

(define (branch-length branch)
	(car branch)
)

(define (branch-structure branch)
	; (car (cdr branch)) ----> Change 2
	(cdr branch)
)

; Part b

(define (total-weight mobile)
	(+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile)))
)

; Part c

(define (balanced-mobile? mobile)
	(and
		(= (torque (left-branch mobile)) (torque (right-branch mobile)))
		(balanced-branch? (left-branch mobile))
		(balanced-branch? (right-branch mobile))
	)
)

(define (torque branch)
	(* (branch-length branch) (branch-weight branch))
)

(define (balanced-branch? branch)
	; for this to be true each of the submobiles hanging off its branches should be balanced
	(cond
		((not (pair? (branch-structure branch)))
			; structure is a simple weight
			true
		)
		(else
			; structure is a binary mobile
			(balanced-mobile? (branch-structure branch))
		)
	)
)

(define (branch-weight branch)
	(cond
		((not (pair? (branch-structure branch)))
			; structure is a simple weight
			(branch-structure branch)
		)
		(else
			; structure is a binary mobile
			(total-weight (branch-structure branch))
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define left (make-branch 3 6))
> (define right (make-branch 8 5))
> (define m (make-mobile left right))
> (total-weight m)
11
> (branch-weight (left-branch m))
6
> (branch-weight (right-branch m))
5
> m
'((3 . 6) 8 . 5)
> (torque (left-branch m))
18
> (torque (right-branch m))
40
> (balanced-mobile? m)
#f
> (define n (make-mobile (make-branch 11 13) (make-branch 7 9)))
> n
'((11 . 13) 7 . 9)
> m
'((3 . 6) 8 . 5)
> (define o (make-mobile (make-branch 21 m) (make-branch 16 n)))
> o
'((21 (3 . 6) 8 . 5) 16 (11 . 13) 7 . 9)
> (balanced-mobile? o)
#f
> (torque (left-branch o))
231
> (torque (right-branch o))
352
> (branch-weight (left-branch o))
11
> (branch-weight (right-branch o))
22
> 
