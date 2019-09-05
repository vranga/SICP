#lang racket

; Exercise 2.29.  A binary mobile consists of two branches, a left branch and a right branch.
; Each branch is a rod of a certain length, from which hangs either a weight or another binary
; mobile. We can represent a binary mobile using compound data by constructing it from two
; branches (for example, using list):

; (define (make-mobile left right)
;   (list left right))

; A branch is constructed from a length (which must be a number) together with a structure,
; which may be either a number (representing a simple weight) or another mobile:

; (define (make-branch length structure)
;   (list length structure))

; a.  Write the corresponding selectors left-branch and right-branch, which return the branches
; of a mobile, and branch-length and branch-structure, which return the components of a branch.

; b.  Using your selectors, define a procedure total-weight that returns the total weight of a
; mobile.

; c.  A mobile is said to be balanced if the torque applied by its top-left branch is equal to
; that applied by its top-right branch (that is, if the length of the left rod multiplied by
; the weight hanging from that rod is equal to the corresponding product for the right side)
; and if each of the submobiles hanging off its branches is balanced. Design a predicate that
; tests whether a binary mobile is balanced.

; d.  Suppose we change the representation of mobiles so that the constructors are

; (define (make-mobile left right)
;   (cons left right))
; (define (make-branch length structure)
;   (cons length structure))

; How much do you need to change your programs to convert to the new representation?

; SOLUTION

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
