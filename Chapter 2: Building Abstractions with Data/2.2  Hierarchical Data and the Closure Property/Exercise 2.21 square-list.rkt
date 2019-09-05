#lang racket

; Exercise 2.21.  The procedure square-list takes a list of numbers as argument and returns a
; list of the squares of those numbers.

; (square-list (list 1 2 3 4))
; (1 4 9 16)

; Here are two different definitions of square-list. Complete both of them by filling in the
; missing expressions:

; (define (square-list items)
;   (if (null? items)
;       nil
;       (cons <??> <??>)))
; (define (square-list items)
;   (map <??> <??>))

; SOLUTION

(define (square-list items)
	(if (null? items)
		(list)
		(cons (square (car items)) (square-list (cdr items)))
	)
)

(define (square-list-with-map items)
	(map (lambda (x) (* x x)) items)
)

(define (square x)
	(* x x)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (square-list (list 1 2 3 4))
'(1 4 9 16)
> (square-list-with-map (list 1 2 3 4))
'(1 4 9 16)
> (square-list (square-list (list 1 2 3 4)))
'(1 16 81 256)
> (square-list-with-map (square-list-with-map (list 1 2 3 4)))
'(1 16 81 256)
> (square-list (square-list-with-map (square-list-with-map (list 1 2 3 4))))
'(1 256 6561 65536)
> 
