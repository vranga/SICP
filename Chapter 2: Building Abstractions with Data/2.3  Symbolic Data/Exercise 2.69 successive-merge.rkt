#lang racket

; Exercise 2.69. The following procedure takes as its argument a list of symbol-frequency
; pairs (where no symbol appears in more than one pair) and generates a Huffman encoding
; tree according to the Huffman algorithm.

; (define (generate-huffman-tree pairs)
;   (successive-merge (make-leaf-set pairs)))

; Make-leaf-set is the procedure given above that transforms the list of pairs into an
; ordered set of leaves. Successive-merge is the procedure you must write, using
; make-code-tree to successively merge the smallest-weight elements of the set until
; there is only one element left, which is the desired Huffman tree. (This procedure is
; slightly tricky, but not really complicated. If you find yourself designing a complex
; procedure, then you are almost certainly doing something wrong. You can take
; significant advantage of the fact that we are using an ordered set representation.)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
)

(define (successive-merge ordered-leaf-set)
	(newline)
	(display ordered-leaf-set)
	(newline)
	(cond
		((not (pair? ordered-leaf-set)) (error "Not a pair"))
		; If there is only one element in the set, then there are two possibilities:
		; 1. The original ordered leaf set had only one element
		; 2. We have reached the end of the merge process and have constructed the whole tree
		; In either case, just return the element
		((null? (cdr ordered-leaf-set)) (car ordered-leaf-set))
		; If there are two or more elements in the set, then:
		; Note: the leaves are ordered in increasing order of weight so combine
		; the first two leaves to make a node that has these two leaves as its left and
		; right branches. Then continue the merge process
		(else
			(let ((new-node (make-code-tree (car ordered-leaf-set) (cadr ordered-leaf-set))))
				(successive-merge
					(adjoin-set
						new-node
						(cdr (cdr ordered-leaf-set))
					)
				)
			)
		)
	)
)

(define (make-leaf-set pairs)
	(if (null? pairs)
		'()
		(let ((pair (car pairs)))
			(adjoin-set
				(make-leaf
					(car pair)	; symbol
					(cadr pair)	; frequency
				)
				(make-leaf-set (cdr pairs))
			)
		)
	)
)

; This adjoin implementation produces sets with elements arranged in increasing order of weight
(define (adjoin-set x set)
	(cond
		((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else
			(cons (car set) (adjoin-set x (cdr set)))
		)
	)
)

; Tree definition

(define (make-code-tree left right)
	(list
		left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))
	)
)

(define (left-branch tree)
	(car tree)
)

(define (right-branch tree)
	(cadr tree)
)

(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)
	)
)

(define (weight tree)
	(if (leaf? tree)
		(weight-leaf tree)
		(cadddr tree)
	)
)

; Leaf definition

(define (make-leaf symbol weight)
	(list 'leaf symbol weight)
)

(define (leaf? object)
	(eq? (car object) 'leaf)
)

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

; Tests

(define pairs (list (list 'A 8) (list 'B 3) (list 'C 1) (list 'D 1) (list 'E 1) (list 'F 1) (list 'G 1) (list 'H 1)))
(generate-huffman-tree pairs)

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.

((leaf H 1) (leaf G 1) (leaf F 1) (leaf E 1) (leaf D 1) (leaf C 1) (leaf B 3) (leaf A 8))

((leaf F 1) (leaf E 1) (leaf D 1) (leaf C 1) ((leaf H 1) (leaf G 1) (H G) 2) (leaf B 3) (leaf A 8))

((leaf D 1) (leaf C 1) ((leaf H 1) (leaf G 1) (H G) 2) ((leaf F 1) (leaf E 1) (F E) 2) (leaf B 3) (leaf A 8))

(((leaf H 1) (leaf G 1) (H G) 2) ((leaf F 1) (leaf E 1) (F E) 2) ((leaf D 1) (leaf C 1) (D C) 2) (leaf B 3) (leaf A 8))

(((leaf D 1) (leaf C 1) (D C) 2) (leaf B 3) (((leaf H 1) (leaf G 1) (H G) 2) ((leaf F 1) (leaf E 1) (F E) 2) (H G F E) 4) (leaf A 8))

((((leaf H 1) (leaf G 1) (H G) 2) ((leaf F 1) (leaf E 1) (F E) 2) (H G F E) 4) (((leaf D 1) (leaf C 1) (D C) 2) (leaf B 3) (D C B) 5) (leaf A 8))

((leaf A 8) ((((leaf H 1) (leaf G 1) (H G) 2) ((leaf F 1) (leaf E 1) (F E) 2) (H G F E) 4) (((leaf D 1) (leaf C 1) (D C) 2) (leaf B 3) (D C B) 5) (H G F E D C B) 9))

(((leaf A 8) ((((leaf H 1) (leaf G 1) (H G) 2) ((leaf F 1) (leaf E 1) (F E) 2) (H G F E) 4) (((leaf D 1) (leaf C 1) (D C) 2) (leaf B 3) (D C B) 5) (H G F E D C B) 9) (A H G F E D C B) 17))
'((leaf A 8)
  ((((leaf H 1) (leaf G 1) (H G) 2) ((leaf F 1) (leaf E 1) (F E) 2) (H G F E) 4) (((leaf D 1) (leaf C 1) (D C) 2) (leaf B 3) (D C B) 5) (H G F E D C B) 9)
  (A H G F E D C B)
  17)
> 
