#lang racket

; Exercise 2.70.  The following eight-symbol alphabet with associated relative frequencies
; was designed to efficiently encode the lyrics of 1950s rock songs.
; (Note that the ''symbols'' of an ''alphabet'' need not be individual letters.)

; A		2
; NA	16
; BOOM	1
; SHA	3
; GET	2
; YIP	9
; JOB	2
; WAH	1

; Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman tree,
; and use encode (exercise 2.68) to encode the following message:

; Get a job

; Sha na na na na na na na na

; Get a job

; Sha na na na na na na na na

; Wah yip yip yip yip yip yip yip yip yip

; Sha boom

; How many bits are required for the encoding? What is the smallest number of bits that
; would be needed to encode this song if we used a fixed-length code for the eight-symbol
; alphabet?

; SOLUTION

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
)

(define (successive-merge ordered-leaf-set)
	; (newline)
	; (display ordered-leaf-set)
	; (newline)
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

; Codec definition

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

(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
			'()
			(let ((next-branch (choose-branch (car bits) current-branch)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
					(decode-1 (cdr bits) next-branch)
				)
			)
		)
	)

	(decode-1 bits tree)
)

(define (choose-branch bit branch)
	(cond ((= bit 0) (left-branch branch))
		((= bit 1) (right-branch branch))
		(else (error "bad bit -- CHOOSE-BRANCH" bit))
	)
)

; Tests

(define pairs (list (list 'A 2) (list 'NA 16) (list 'BOOM 1) (list 'SHA 3) (list 'GET 2) (list 'YIP 9) (list 'JOB 2) (list 'WAH 1)))

(define huffman-tree (generate-huffman-tree pairs))

(newline)
(display "Encoding tree is: ")
(newline)
(display huffman-tree)

(define test-message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(newline)
(newline)
(display "Test message is: ")
(display test-message)

(define encoded-message (encode test-message huffman-tree))
(newline)
(newline)
(display "Encoded message is: ")
(display encoded-message)

(define decoded-message (decode encoded-message huffman-tree))
(newline)
(newline)
(display "Decoded bits is: ")
(display decoded-message)

(newline)
(newline)
(display "Number of bits in the encoded message is: ")
(display (length encoded-message))

; In a fixed length representation, since there are 8 symbols, we need at least
; 3 bits per symbol. (2 raised to 3 = 8)

(newline)
(newline)
(display "Number of symbols in the test message is: ")
(display (length test-message))

(newline)
(newline)
(display "Minimum Number of bits per symbol in fixed-length representation: 3 (Since there are eight symbols and 2 raised to 3 = 8)")

(newline)
(newline)
(display "The smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet: ")
(display (* (length test-message) 3))

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.

Encoding tree is: 
((leaf NA 16) ((leaf YIP 9) (((leaf A 2) ((leaf WAH 1) (leaf BOOM 1) (WAH BOOM) 2) (A WAH BOOM) 4) ((leaf SHA 3) ((leaf JOB 2) (leaf GET 2) (JOB GET) 4) (SHA JOB GET) 7) (A WAH BOOM SHA JOB GET) 11) (YIP A WAH BOOM SHA JOB GET) 20) (NA YIP A WAH BOOM SHA JOB GET) 36)

Test message is: (GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)

Encoded message is: (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

Decoded bits is: (GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)

Number of bits in the encoded message is: 84

Number of symbols in the test message is: 36

Minimum Number of bits per symbol in fixed-length representation: 3 (Since there are eight symbols and 2 raised to 3 = 8)

The smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet: 108
> 
