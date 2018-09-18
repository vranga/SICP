#lang racket
	
; a. Write a short paragraph explaining as clearly as you can how partial-tree works.
; Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

; b. What is the order of growth in the number of steps required by list->tree
; to convert a list of n elements?

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

(define (test-list->tree n)
	(define start-time (current-milliseconds))
	(define result (list->tree (make-list 1 n)))
	(define end-time (current-milliseconds))
	(display "Elapsed Time: ")
	(display (- end-time start-time))
	(newline)
)

(time (test-list->tree 10000))
(time (test-list->tree 15000))
(time (test-list->tree 20000))
(time (test-list->tree 25000))
(time (test-list->tree 30000))
(time (test-list->tree 35000))
(time (test-list->tree 40000))
(time (test-list->tree 45000))
(time (test-list->tree 50000))
(time (test-list->tree 55000))
(time (test-list->tree 60000))
(time (test-list->tree 65000))
(time (test-list->tree 70000))
(time (test-list->tree 75000))
(time (test-list->tree 80000))
(time (test-list->tree 85000))
(time (test-list->tree 90000))
(time (test-list->tree 95000))
(time (test-list->tree 100000))
(time (test-list->tree 150000))
(time (test-list->tree 200000))
(time (test-list->tree 250000))
(time (test-list->tree 300000))
(time (test-list->tree 350000))
(time (test-list->tree 400000))
(time (test-list->tree 450000))
(time (test-list->tree 500000))
(time (test-list->tree 550000))
(time (test-list->tree 600000))
(time (test-list->tree 650000))
(time (test-list->tree 700000))
(time (test-list->tree 750000))
(time (test-list->tree 800000))
(time (test-list->tree 850000))
(time (test-list->tree 900000))
(time (test-list->tree 950000))
(time (test-list->tree 1000000))
(time (test-list->tree 1050000))
(time (test-list->tree 1100000))
(time (test-list->tree 1150000))
(time (test-list->tree 1200000))
(time (test-list->tree 1250000))
(time (test-list->tree 1300000))
(time (test-list->tree 1350000))
(time (test-list->tree 1400000))
(time (test-list->tree 1450000))
(time (test-list->tree 1500000))
(time (test-list->tree 1550000))
(time (test-list->tree 1600000))
(time (test-list->tree 1650000))
(time (test-list->tree 1700000))
(time (test-list->tree 1750000))
(time (test-list->tree 1800000))

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 256 MB.
Elapsed Time: 27
cpu time: 35 real time: 36 gc time: 23
Elapsed Time: 3
cpu time: 18 real time: 19 gc time: 14
Elapsed Time: 4
cpu time: 4 real time: 4 gc time: 0
Elapsed Time: 16
cpu time: 15 real time: 16 gc time: 10
Elapsed Time: 5
cpu time: 8 real time: 8 gc time: 2
Elapsed Time: 9
cpu time: 13 real time: 13 gc time: 3
Elapsed Time: 15
cpu time: 18 real time: 18 gc time: 7
Elapsed Time: 16
cpu time: 17 real time: 17 gc time: 8
Elapsed Time: 33
cpu time: 28 real time: 34 gc time: 15
Elapsed Time: 27
cpu time: 27 real time: 27 gc time: 16
Elapsed Time: 27
cpu time: 27 real time: 28 gc time: 14
Elapsed Time: 29
cpu time: 30 real time: 31 gc time: 12
Elapsed Time: 32
cpu time: 33 real time: 34 gc time: 18
Elapsed Time: 38
cpu time: 38 real time: 38 gc time: 23
Elapsed Time: 48
cpu time: 48 real time: 48 gc time: 31
Elapsed Time: 44
cpu time: 44 real time: 44 gc time: 25
Elapsed Time: 71
cpu time: 74 real time: 74 gc time: 43
Elapsed Time: 115
cpu time: 110 real time: 123 gc time: 49
Elapsed Time: 82
cpu time: 78 real time: 84 gc time: 44
Elapsed Time: 114
cpu time: 114 real time: 115 gc time: 73
Elapsed Time: 1370
cpu time: 1354 real time: 1370 gc time: 1176
Elapsed Time: 123
cpu time: 122 real time: 123 gc time: 45
Elapsed Time: 169
cpu time: 172 real time: 173 gc time: 47
Elapsed Time: 288
cpu time: 286 real time: 288 gc time: 93
Elapsed Time: 228
cpu time: 226 real time: 234 gc time: 81
Elapsed Time: 940
cpu time: 942 real time: 944 gc time: 743
Elapsed Time: 326
cpu time: 330 real time: 333 gc time: 123
Elapsed Time: 1071
cpu time: 1071 real time: 1075 gc time: 784
Elapsed Time: 412
cpu time: 416 real time: 419 gc time: 152
Elapsed Time: 1134
cpu time: 1131 real time: 1141 gc time: 821
Elapsed Time: 1318
cpu time: 1317 real time: 1326 gc time: 973
Elapsed Time: 551
cpu time: 537 real time: 554 gc time: 196
Elapsed Time: 1299
cpu time: 1294 real time: 1303 gc time: 892
Elapsed Time: 740
cpu time: 726 real time: 744 gc time: 266
Elapsed Time: 1437
cpu time: 1411 real time: 1446 gc time: 945
Elapsed Time: 1473
cpu time: 1468 real time: 1476 gc time: 968
Elapsed Time: 1458
cpu time: 1444 real time: 1460 gc time: 958
Elapsed Time: 1716
cpu time: 1686 real time: 1717 gc time: 1079
Elapsed Time: 925
cpu time: 917 real time: 940 gc time: 331
Elapsed Time: 1795
cpu time: 2260 real time: 1818 gc time: 1139
Elapsed Time: 1763
cpu time: 1746 real time: 1772 gc time: 1009
Elapsed Time: 1792
cpu time: 1736 real time: 1795 gc time: 1038
Elapsed Time: 2650
cpu time: 2617 real time: 2651 gc time: 1872
Elapsed Time: 2036
cpu time: 2014 real time: 2040 gc time: 1205
Elapsed Time: 2197
cpu time: 2147 real time: 2197 gc time: 1326
Elapsed Time: 2219
cpu time: 2148 real time: 2222 gc time: 1310
Elapsed Time: 2241
cpu time: 2184 real time: 2242 gc time: 1303
Elapsed Time: 2307
cpu time: 2238 real time: 2308 gc time: 1299
Elapsed Time: 2477
cpu time: 2374 real time: 2477 gc time: 1291
Elapsed Time: 2355
cpu time: 2272 real time: 2359 gc time: 1206
Elapsed Time: 2492
cpu time: 2449 real time: 2500 gc time: 1365
Elapsed Time: 2630
cpu time: 2597 real time: 2633 gc time: 1429
Elapsed Time: 2697
cpu time: 2638 real time: 2709 gc time: 1437
> 
