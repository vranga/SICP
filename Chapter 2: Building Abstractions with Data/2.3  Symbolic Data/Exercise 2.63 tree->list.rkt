#lang racket

; Tests

; a. Do the two procedures produce the same result for every tree? If not, how do the results
; differ? What lists do the two procedures produce for the trees in figure 2.16?

; b. Do the two procedures have the same order of growth in the number of steps required
; to convert a balanced tree with n elements to a list? If not, which one grows more slowly?

(define (tree->list-1 tree)
	(cond
		((null? tree) tree)
		((not (pair? tree)) (list tree))
		(else
			(append
				(tree->list-1 (left-branch tree))
				(cons (entry tree) (tree->list-1 (right-branch tree)))
			)
		)
	)
)

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

; Tests

; Construct the trees in Figure 2.16

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.

> (tree->list-1 (make-tree 9 '() 11))
'(9 11)
> (tree->list-1 (make-tree 9 11 '()))
'(11 9)
> (tree->list-1 (make-tree 9 11 13))
'(11 9 13)

(define tree1 (make-tree 7 (make-tree 3 1 5) (make-tree 9 (list) 11)))
(define tree2 (make-tree 3 1 (make-tree 7 5 (make-tree 9 (list) 11))))
(define tree3 (make-tree 5 (make-tree 3 1 (list)) (make-tree 9 7 11)))
(define tree4 (make-tree 10 tree1 (make-tree 20 tree2 tree3)))
(define tree5 (make-tree 21 tree4 tree4))
(define tree6 (make-tree 21 tree5 tree5))
(define tree7 (make-tree 21 tree6 tree6))
(define tree8 (make-tree 21 tree7 tree7))

(define fully-unbalanced-tree
	(make-tree
		1
		(list)
		(make-tree
			2
			(list)
			(make-tree
				3
				(list)
				(make-tree
					4
					(list)
					(make-tree
						5
						(list)
						(make-tree
							6
							(list)
							(make-tree 7 (list) (make-tree 8 (list) 9))
						)
					)
				)
			)
		)
	)
)

> (tree->list-1 tree1)
'(1 3 5 7 9 11)
> (tree->list-1 tree2)
'(1 3 5 7 9 11)
> (tree->list-1 tree3)
'(1 3 5 7 9 11)
> (tree->list-1 fully-unbalanced-tree)
'(1 2 3 4 5 6 7 8 9)

> (tree->list-2 tree1)
'(1 3 5 7 9 11)
> (tree->list-2 tree2)
'(1 3 5 7 9 11)
> (tree->list-2 tree3)
'(1 3 5 7 9 11)
> (tree->list-2 fully-unbalanced-tree)
'(1 2 3 4 5 6 7 8 9)


; Observations

; The two procedures produce the same result for every tree.

; tree->list-1 processes its left tree and right trees separately and combines them whereas,
; tree->list-2 needs to completely process its right branch before it starts processing
; its left branch. 
; Because of this, the depth of recursion will be larger in tree->list2.
; Let us take the example of a simple tree with only 3 elements (example: b <-- a --> c). Here a
; is at the root and b and c are the leaves.

; Case 1: tree->list-1
; Expanded Form

(append
	(
		(append
				(tree->list-1 null)
				(cons
					b
					(tree->list-1 null)
				)
		)
	)
	(cons
		a
		(
			(append
				(tree->list-1 null)
				(cons
					c
					(tree->list-1 null)
				)
			)
		)
	)
)

; Here the depth is 2 i.e. one level of nesting of 'append'

; Case 2: tree->list-2

(copy-to-list
	null
	(cons
		b
		(copy-to-list
			null
			(cons
				a
				(
					(copy-to-list
						null
						(cons
							c
							(copy-to-list
								null
								result-list
							)
						)
					)
				)
			)
		)
	)
)

; Here the depth is 4 i.e. three levels of nesting of copy-to-list

; So for a triad (like a, b, c above):

; In tree->list1, the total number of calls is 3 with one level of nesting
; In tree->list2, the total number of calls is 4 with three levels of nesting

; The following table shows the order of growth for these two procedures for balanced trees

; Balanced Tree Depth		tree->list1 (number of calls)		tree-list2 (number of calls)

; 0 (1 elements)			1									1
; 1 (3 elements)			3									4
; 2 (7 elements)			9									12
; 3 (15 elements)			21									28

; Even though the number of steps is different between the two procedures in the above table,
; both grow at similar rates since both traverse the entire tree by visiting each node once.
; So the growth for both is O(n).

; But there is one other factor: the use of 'append' in tree->list1. The order of growth of 
; 'append' is proportional to the size of the first 'list' argument that is passed to it.
; Append is used at every level of the tree for all the left branches in that level. At each level
; the left branch is reduced in size (i.e. number of elements in that branch) by half of the
; previous level but the number of left branches doubles. So the size of the first list argument
; that is supplied to 'append' at each level halves as we go down the tree but the number of
; append operations doubles keeping the over all number of steps the same.
; Therefore, the number of steps per level in the tree is O(n/2) where n is the total number
; of elements in the binary tree. But there are log(n) levels
; so the overall number of steps for the binary tree will be O(nlog(n)). (I have ignored the
; division by 2 here.)

; The above order of growth of O(nlog(n)) due to the use of 'append' is larger than the
; growth shown in the table above which is basically dependent directly on the number of steps
; to walk the entire tree.

; Summary: 

; tree->list1 has an order of growth of O(nlog(n))
; tree->list2 has an order of growth of O(n)
