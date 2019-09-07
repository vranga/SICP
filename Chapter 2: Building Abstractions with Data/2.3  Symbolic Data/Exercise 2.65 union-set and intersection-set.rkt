#lang racket

; Exercise 2.65.  Use the results of exercises 2.63 and  2.64 to give Theta(n)
; implementations of union-set and intersection-set for sets implemented as (balanced)
; binary trees.

(define (tree-union-set set1 set2)
	; The input sets are binary trees
	; 1. Convert each tree into an ordered set implemented as a list
	; 2. Construct the union set from the above ordered lists
	; 3. Convert the union set (which will be an ordered list) to a balanced binary tree

	(list->tree (union-set (tree->list-2 set1) (tree->list-2 set2)))
)

(define (tree-intersection-set set1 set2)
	; The input sets are binary trees
	; 1. Convert each tree into an ordered set implemented as a list
	; 2. Construct the intersection set from the above ordered lists
	; 3. Convert the intersection set (which will be an ordered list) to a balanced binary tree

	(list->tree (intersection-set (tree->list-2 set1) (tree->list-2 set2)))
)

(define (list->tree elements)
	(car (partial-tree elements (length elements)))
)

(define (union-set set1 set2)
	(cond
		; If either of the sets is null the other one is the union
		((null? set1) set2)
		((null? set2) set1)
		; If the first element of set1 is lesser than the first element of set2 then
		; the first element is added to the union and the remaining data is processed
		; recursively
		((< (car set1) (car set2))
			(cons (car set1) (union-set (cdr set1) set2))
		)
		; If the first element of set1 is equal to the first element of set2 then
		; this element is added to the union and the remaining data is processed
		; recursively
		((= (car set1) (car set2))
			(cons (car set1) (union-set (cdr set1) (cdr set2)))
		)
		; If the first element of set1 is greater than the first element of set2 then
		; the first element of set2 is added to the union and the remaining data is processed
		; recursively
		((> (car set1) (car set2))
			(cons (car set2) (union-set set1 (cdr set2)))
		)
	)
)

(define (intersection-set set1 set2)
	(cond
		; If either of the sets is null the intersection is also null
		((null? set1) (list))
		((null? set2) (list))
		; If the first element of set1 is lesser than the first element of set2 then
		; the first element of set1 is not present in set2 and it will not belong to the
		; intersection set
		; So the first element of set1 is ignored and we continue looking recursively
		((< (car set1) (car set2))
			(intersection-set (cdr set1) set2)
		)
		; If the first element of set1 is equal to the first element of set2 then
		; this element is added to the intersection and the remaining data is processed
		; recursively
		((= (car set1) (car set2))
			(cons (car set1) (intersection-set (cdr set1) (cdr set2)))
		)
		; If the first element of set1 is greater than the first element of set2 then
		; the first element if set2 is not present in set1 and it will not belong to the
		; intersection set
		; So the first element of set2 is ignored and we continue looking recursively
		((> (car set1) (car set2))
			(intersection-set set1 (cdr set2))
		)
	)
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

(define (make-list start end)
	(cond
		((> start end) (list))
		(else
			(cons start (make-list (+ start 1) end))
		)
	)
)

; Tests

(define (test-tree-union-set range1-start range1-end range2-start range2-end)
	(define tree1 (list->tree (make-list range1-start range1-end)))
	(define tree2 (list->tree (make-list range2-start range2-end)))

	(tree-union-set tree1 tree2)
)

(define (test-tree-intersection-set range1-start range1-end range2-start range2-end)
	(define tree1 (list->tree (make-list range1-start range1-end)))
	(define tree2 (list->tree (make-list range2-start range2-end)))

	(tree-intersection-set tree1 tree2)
)

(define (timed-test-tree-union-set range1-start range1-end range2-start range2-end)
	(define start-time (current-milliseconds))
	(define result (test-tree-union-set range1-start range1-end range2-start range2-end))
	(define end-time (current-milliseconds))
	(display "Elapsed Time: ")
	(display (- end-time start-time))
	(newline)
)

(define (timed-test-tree-intersection-set range1-start range1-end range2-start range2-end)
	(define start-time (current-milliseconds))
	(define result (test-tree-intersection-set range1-start range1-end range2-start range2-end))
	(define end-time (current-milliseconds))
	(display "Elapsed Time: ")
	(display (- end-time start-time))
	(newline)
)

; Union tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 256 MB.
> (test-tree-union-set 0 5 6 10)
'(5 (2 (0 () (1 () ())) (3 () (4 () ()))) (8 (6 () (7 () ())) (9 () (10 () ()))))
> (tree->list-2 (test-tree-union-set 0 5 6 10))
'(0 1 2 3 4 5 6 7 8 9 10)
> (tree->list-2 (test-tree-union-set 0 5 5 10))
'(0 1 2 3 4 5 6 7 8 9 10)
> (test-tree-union-set 0 5 3 8)
'(4 (1 (0 () ()) (2 () (3 () ()))) (6 (5 () ()) (7 () (8 () ()))))
> (tree->list-2 (test-tree-union-set 0 5 3 8))
'(0 1 2 3 4 5 6 7 8)
> (test-tree-union-set 0 5 1 5)
'(2 (0 () (1 () ())) (4 (3 () ()) (5 () ())))
> (tree->list-2 (test-tree-union-set 0 5 1 5))
'(0 1 2 3 4 5)
> (test-tree-union-set 0 5 0 5)
'(2 (0 () (1 () ())) (4 (3 () ()) (5 () ())))
> (tree->list-2 (test-tree-union-set 0 5 0 5))
'(0 1 2 3 4 5)
> (test-tree-union-set 0 5 -3 5)
'(1 (-2 (-3 () ()) (-1 () (0 () ()))) (3 (2 () ()) (4 () (5 () ()))))
> (tree->list-2 (test-tree-union-set 0 5 -3 5))
'(-3 -2 -1 0 1 2 3 4 5)
> (test-tree-union-set 0 5 -3 1)
'(1 (-2 (-3 () ()) (-1 () (0 () ()))) (3 (2 () ()) (4 () (5 () ()))))
> (tree->list-2 (test-tree-union-set 0 5 -3 1))
'(-3 -2 -1 0 1 2 3 4 5)
> (test-tree-union-set 0 5 -3 0)
'(1 (-2 (-3 () ()) (-1 () (0 () ()))) (3 (2 () ()) (4 () (5 () ()))))
> (tree->list-2 (test-tree-union-set 0 5 -3 0))
'(-3 -2 -1 0 1 2 3 4 5)
> (test-tree-union-set 0 5 -17 -6)
'(-9
  (-14 (-16 (-17 () ()) (-15 () ())) (-12 (-13 () ()) (-11 () (-10 () ()))))
  (1 (-7 (-8 () ()) (-6 () (0 () ()))) (3 (2 () ()) (4 () (5 () ())))))
> (tree->list-2 (test-tree-union-set 0 5 -17 -6))
'(-17 -16 -15 -14 -13 -12 -11 -10 -9 -8 -7 -6 0 1 2 3 4 5)
> 

; Intersection tests

> (test-tree-intersection-set 0 5 6 10) (tree->list-2 (test-tree-intersection-set 0 5 6 10))
'()
'()
> (test-tree-intersection-set 0 5 5 10) (tree->list-2 (test-tree-intersection-set 0 5 5 10))
'(5 () ())
'(5)
> (test-tree-intersection-set 0 5 3 8) (tree->list-2 (test-tree-intersection-set 0 5 3 8))
'(4 (3 () ()) (5 () ()))
'(3 4 5)
> (test-tree-intersection-set 0 5 1 5) (tree->list-2 (test-tree-intersection-set 0 5 1 5))
'(3 (1 () (2 () ())) (4 () (5 () ())))
'(1 2 3 4 5)
> (test-tree-intersection-set 0 5 0 5) (tree->list-2 (test-tree-intersection-set 0 5 0 5))
'(2 (0 () (1 () ())) (4 (3 () ()) (5 () ())))
'(0 1 2 3 4 5)
> (test-tree-intersection-set 0 5 -3 5) (tree->list-2 (test-tree-intersection-set 0 5 -3 5))
'(2 (0 () (1 () ())) (4 (3 () ()) (5 () ())))
'(0 1 2 3 4 5)
> (test-tree-intersection-set 0 5 -3 1) (tree->list-2 (test-tree-intersection-set 0 5 -3 1))
'(0 () (1 () ()))
'(0 1)
> (test-tree-intersection-set 0 5 -3 0) (tree->list-2 (test-tree-intersection-set 0 5 -3 0))
'(0 () ())
'(0)
> (test-tree-intersection-set 0 5 -17 -6) (tree->list-2 (test-tree-intersection-set 0 5 -17 -6))
'()
'()
> 

; Performance Tests

(time (timed-test-tree-union-set -10000 -10 10 10000))
(time (timed-test-tree-union-set -20000 -10 10 20000))
(time (timed-test-tree-union-set -30000 -10 10 30000))
(time (timed-test-tree-union-set -40000 -10 10 40000))
(time (timed-test-tree-union-set -50000 -10 10 50000))
(time (timed-test-tree-union-set -60000 -10 10 60000))
(time (timed-test-tree-union-set -70000 -10 10 70000))
(time (timed-test-tree-union-set -80000 -10 10 80000))
(time (timed-test-tree-union-set -90000 -10 10 90000))
(time (timed-test-tree-union-set -100000 -10 10 100000))
(time (timed-test-tree-union-set -200000 -10 10 200000))
(time (timed-test-tree-union-set -300000 -10 10 300000))
(time (timed-test-tree-union-set -400000 -10 10 400000))
(time (timed-test-tree-union-set -500000 -10 10 500000))
(time (timed-test-tree-union-set -600000 -10 10 600000))
(time (timed-test-tree-union-set -700000 -10 10 700000))
(time (timed-test-tree-union-set -800000 -10 10 800000))
(time (timed-test-tree-union-set -900000 -10 10 900000))
(time (timed-test-tree-union-set -1000000 -10 10 1000000))
(time (timed-test-tree-union-set -2000000 -10 10 2000000))
(time (timed-test-tree-union-set -3000000 -10 10 3000000))
(time (timed-test-tree-union-set -4000000 -10 10 4000000))
(time (timed-test-tree-union-set -5000000 -10 10 5000000))
(time (timed-test-tree-union-set -6000000 -10 10 6000000))
(time (timed-test-tree-union-set -7000000 -10 10 7000000))
(time (timed-test-tree-union-set -8000000 -10 10 8000000))
(time (timed-test-tree-union-set -9000000 -10 10 9000000))
(time (timed-test-tree-union-set -10000000 -10 10 10000000))

(time (timed-test-tree-intersection-set -10000 -10 -100 10000))
(time (timed-test-tree-intersection-set -20000 -10 -100 20000))
(time (timed-test-tree-intersection-set -30000 -10 -100 30000))
(time (timed-test-tree-intersection-set -40000 -10 -100 40000))
(time (timed-test-tree-intersection-set -50000 -10 -100 50000))
(time (timed-test-tree-intersection-set -60000 -10 -100 60000))
(time (timed-test-tree-intersection-set -70000 -10 -100 70000))
(time (timed-test-tree-intersection-set -80000 -10 -100 80000))
(time (timed-test-tree-intersection-set -90000 -10 -100 90000))
(time (timed-test-tree-intersection-set -100000 -10 -100 100000))
(time (timed-test-tree-intersection-set -200000 -10 -100 200000))
(time (timed-test-tree-intersection-set -300000 -10 -100 300000))
(time (timed-test-tree-intersection-set -400000 -10 -100 400000))
(time (timed-test-tree-intersection-set -500000 -10 -100 500000))
(time (timed-test-tree-intersection-set -600000 -10 -100 600000))
(time (timed-test-tree-intersection-set -700000 -10 -100 700000))
(time (timed-test-tree-intersection-set -800000 -10 -100 800000))
(time (timed-test-tree-intersection-set -900000 -10 -100 900000))
(time (timed-test-tree-intersection-set -1000000 -10 -1000 1000000))
(time (timed-test-tree-intersection-set -2000000 -10 -1000 2000000))
(time (timed-test-tree-intersection-set -3000000 -10 -1000 3000000))
(time (timed-test-tree-intersection-set -4000000 -10 -1000 4000000))
(time (timed-test-tree-intersection-set -5000000 -10 -1000 5000000))
(time (timed-test-tree-intersection-set -6000000 -10 -1000 6000000))
(time (timed-test-tree-intersection-set -7000000 -10 -1000 7000000))
(time (timed-test-tree-intersection-set -8000000 -10 -1000 8000000))
(time (timed-test-tree-intersection-set -9000000 -10 -1000 9000000))
(time (timed-test-tree-intersection-set -10000000 -10 -1000 10000000))
