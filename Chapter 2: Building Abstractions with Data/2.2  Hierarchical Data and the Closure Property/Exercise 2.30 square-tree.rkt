#lang racket

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
