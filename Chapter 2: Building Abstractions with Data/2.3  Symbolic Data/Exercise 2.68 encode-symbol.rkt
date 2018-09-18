#lang racket

; The encode procedure takes as arguments a message and a tree and produces the list of bits
; that gives the encoded message.

; encode-symbol is a procedure, which you must write, that returns the list of bits that
; encodes a given symbol according to a given tree. You should design encode-symbol so that
; it signals an error if the symbol is not in the tree at all. Test your procedure by encoding
; the result you obtained in exercise 2.67 with the sample tree and seeing whether it is the
; same as the original sample message.

(define (encode-symbol symbol tree)
	(cond
		((leaf? tree) null)
		((element-of-set? symbol (symbols (left-branch tree)))
			(cons '0 (encode-symbol symbol (left-branch tree)))
		)
		((element-of-set? symbol (symbols (right-branch tree)))
			(cons '1 (encode-symbol symbol (right-branch tree)))
		)
		(else
			(error "Symbol not found in tree:" symbol)
		)
	)
)

(define (element-of-set? x set)
	(cond
		((null? set) false)
		((equal? x (car set)) true)
		(else
			(element-of-set? x (cdr set))
		)
	)
)

(define (encode message tree)
	(if (null? message)
		'()
		(append
			(encode-symbol (car message) tree)
			(encode (cdr message) tree)
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

(define sample-tree
	(make-code-tree
		(make-leaf 'A 4)
		(make-code-tree
			(make-leaf 'B 2)
			(make-code-tree
				(make-leaf 'D 1)
				(make-leaf 'C 1)
			)
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define sample-message '(A D A B B C A))
> (encode sample-message sample-tree)

'(0 1 1 0 0 1 0 1 0 1 1 1 0)
> (define sample-message2 '(A D A R B C A))
> (encode sample-message2 sample-tree)
. . Symbol not found in tree: R
> 

; Note: The encoded message obtained is the same as the sample message in Exercise 2.67
