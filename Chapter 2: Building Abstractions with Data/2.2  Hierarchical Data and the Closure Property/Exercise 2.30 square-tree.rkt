#lang racket

; Exercise 2.30.  Define a procedure square-tree analogous to the square-list procedure of
; exercise 2.21. That is, square-list should behave as follows:

; (square-tree
;  (list 1
;        (list 2 (list 3 4) 5)
;        (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

; Define square-tree both directly (i.e., without using any higher-order procedures) and also
; by using map and recursion.

; SOLUTION

(define (square-tree tree)
	(cond
		((null? tree) tree)
		((not (pair? tree)) (square tree))
		(else
			(cons (square-tree (car tree)) (square-tree (cdr tree)))
		)
	)
)

(define (square-tree-with-map tree)
	(map
		(lambda (tree)
			(cond
				((null? tree) tree)
				((not (pair? tree)) (square tree))
				(else
					(square-tree-with-map tree)
				)
			)
		)
		tree
	)
)

(define (square x)
	(* x x)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (square-tree
	(list 1 (list 2 (list 3 4) 5) (list 6 7))
)
'(1 (4 (9 16) 25) (36 49))
> (square-tree-with-map
	(list 1 (list 2 (list 3 4) 5) (list 6 7))
)
'(1 (4 (9 16) 25) (36 49))
> (square-tree (square-tree-with-map
	(list 1 (list 2 (list 3 4) 5) (list 6 7))
))
'(1 (16 (81 256) 625) (1296 2401))
> (square-tree-with-map (square-tree-with-map
	(list 1 (list 2 (list 3 4) 5) (list 6 7))
))
'(1 (16 (81 256) 625) (1296 2401))
> (square-tree-with-map (square-tree
	(list 1 (list 2 (list 3 4) 5) (list 6 7))
))
'(1 (16 (81 256) 625) (1296 2401))
> 
