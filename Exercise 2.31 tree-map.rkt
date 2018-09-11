#lang racket

(define (square-tree tree) (tree-map square tree))
(define (cube-tree tree) (tree-map cube tree))
(define (triple-tree tree) (tree-map triple tree))

(define (square-tree-with-map tree) (tree-map-with-map square tree))
(define (cube-tree-with-map tree) (tree-map-with-map cube tree))
(define (triple-tree-with-map tree) (tree-map-with-map triple tree))

(define (tree-map func tree)
	(cond
		((null? tree) tree)
		((not (pair? tree)) (func tree))
		(else
			(cons (tree-map func (car tree)) (tree-map func (cdr tree)))
		)
	)
)

(define (tree-map-with-map func tree)
	(map
		(lambda (tree)
			(cond
				((null? tree) tree)
				((not (pair? tree)) (func tree))
				(else
					(tree-map-with-map func tree)
				)
			)
		)
		tree
	)
)

(define (square x)
	(* x x)
)

(define (cube x)
	(* x x x)
)

(define (triple x)
	(* 3 x)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-with-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree (square-tree-with-map (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(square-tree-with-map (square-tree-with-map (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(square-tree-with-map (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

'(1 (4 (9 16) 25) (36 49))
'(1 (4 (9 16) 25) (36 49))
'(1 (16 (81 256) 625) (1296 2401))
'(1 (16 (81 256) 625) (1296 2401))
'(1 (16 (81 256) 625) (1296 2401))
> (cube-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(cube-tree-with-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(cube-tree (cube-tree-with-map (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(cube-tree-with-map (cube-tree-with-map (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(cube-tree-with-map (cube-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
'(1 (8 (27 64) 125) (216 343))
'(1 (8 (27 64) 125) (216 343))
'(1 (512 (19683 262144) 1953125) (10077696 40353607))
'(1 (512 (19683 262144) 1953125) (10077696 40353607))
'(1 (512 (19683 262144) 1953125) (10077696 40353607))
> (triple-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(triple-tree-with-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(triple-tree (triple-tree-with-map (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(triple-tree-with-map (triple-tree-with-map (list 1 (list 2 (list 3 4) 5) (list 6 7))))
(triple-tree-with-map (triple-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
'(3 (6 (9 12) 15) (18 21))
'(3 (6 (9 12) 15) (18 21))
'(9 (18 (27 36) 45) (54 63))
'(9 (18 (27 36) 45) (54 63))
'(9 (18 (27 36) 45) (54 63))
> 
