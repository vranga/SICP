#lang racket

; Exercise 2.66. Implement the lookup procedure for the case where the set of records
; is structured as a binary tree, ordered by the numerical values of the keys.

(define (lookup given-key ordered-binary-tree)
	(cond
		((null? ordered-binary-tree) false)
		; Found the key
		((= given-key (key (entry ordered-binary-tree))) (entry ordered-binary-tree))
		((< given-key (key (entry ordered-binary-tree)))
			(lookup given-key (left-branch ordered-binary-tree))
		)
		((> given-key (key (entry ordered-binary-tree)))
			(lookup given-key (right-branch ordered-binary-tree))
		)
	)
)

; Record definition

(define (make-record key value)
	(cons key value)
)

(define (key record)
	(car record)
)

(define (value record)
	(cdr record)
)

; Tree definition

(define (entry tree)
	(if (not (pair? tree))
		tree
		(car tree)
	)
)

(define (left-branch tree)
	(if (not (pair? tree))
		tree
		(cadr tree)
	)
)

(define (right-branch tree)
	(if (not (pair? tree))
		tree
		(caddr tree)
	)
)

(define (make-tree entry left right)
	(list entry left right)
)

; List to Tree conversion procedures

(define (list->tree elements)
	(car (partial-tree elements (length elements)))
)

(define (partial-tree elts n)
	(if (= n 0)
		(cons '() elts)
		(let ((left-size (quotient (- n 1) 2)))
			(let ((left-result (partial-tree elts left-size)))
				(let
					(
						(left-tree (car left-result))
						(non-left-elts (cdr left-result))
						(right-size (- n (+ left-size 1)))
					)
					(let
						(
							(this-entry (car non-left-elts))
							(right-result (partial-tree (cdr non-left-elts) right-size))
						)
						(let
							(
								(right-tree (car right-result))
								(remaining-elts (cdr right-result))
							)
							(cons (make-tree this-entry left-tree right-tree) remaining-elts)
						)
					)
				)
			)
		)
	)
)

; Tree to List conversion procedures

(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(cond
			((null? tree) result-list)
			((not (pair? tree)) (cons tree result-list))
			(else
				(copy-to-list
					(left-branch tree)
					(cons
						(entry tree)
						(copy-to-list (right-branch tree) result-list)
					)
				)
			)
		)
	)
	(copy-to-list tree '())
)

; Helper procedures

; Make a list of key-value pairs. Keys are integers ranging from 'start' to 'end'

(define (make-list start end)
	(cond
		((> start end) (list))
		(else
			(cons (make-record start (random 1000)) (make-list (+ start 1) end))
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (list->tree (make-list 1 10))
'((5 . 917)
  ((2 . 84) ((1 . 673) () ()) ((3 . 64) () ((4 . 547) () ())))
  ((8 . 285) ((6 . 855) () ((7 . 563) () ())) ((9 . 958) () ((10 . 786) () ()))))
> (tree->list-2 (list->tree (make-list 1 10)))
'((1 . 918) (2 . 721) (3 . 534) (4 . 611) (5 . 219) (6 . 71) (7 . 606) (8 . 456) (9 . 264) (10 . 46))
> (define btree (list->tree (make-list 1 10)))
> btree
'((5 . 989)
  ((2 . 424) ((1 . 774) () ()) ((3 . 210) () ((4 . 522) () ())))
  ((8 . 888) ((6 . 393) () ((7 . 266) () ())) ((9 . 808) () ((10 . 287) () ()))))
> (lookup 9 btree)
'(9 . 808)
> (lookup 4 btree)
'(4 . 522)
> (lookup 2 btree)
'(2 . 424)
> (lookup 10 btree)
'(10 . 287)
> (lookup 11 btree)
#f
> 
