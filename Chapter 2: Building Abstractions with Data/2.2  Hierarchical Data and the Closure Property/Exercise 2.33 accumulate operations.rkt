#lang racket

(define (length sequence)
	(accumulate
		(lambda (x y)
			(+ y 1)
		)
		0
		sequence
	)
)

(define (append seq1 seq2)
	(accumulate cons seq2 seq1)
)

(define (map p sequence)
	(accumulate
		(lambda (x y)
			(cons (p x) y)
		)
		null
		sequence
	)
)

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence) (accumulate op initial (cdr sequence)))
	)
)

(define (inc x)
	(+ x 1)
)

(define (square x)
	(* x x)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define odds (list 1 3 5 7 9))
> odds
'(1 3 5 7 9)
> (inc 2)
3
> (map inc odds)
'(2 4 6 8 10)
> (map inc (map inc odds))
'(3 5 7 9 11)
> (map square odds)
'(1 9 25 49 81)
> (define evens (list 2 4 6 8 10))
> evens
'(2 4 6 8 10)
> (map square evens)
'(4 16 36 64 100)
> (map inc odds)
'(2 4 6 8 10)
> (append evens odds)
'(2 4 6 8 10 1 3 5 7 9)
> (append evens odds)
'(2 4 6 8 10 1 3 5 7 9)
> (append odds evens)
'(1 3 5 7 9 2 4 6 8 10)
> (append (map square odds) (map square evens))
'(1 9 25 49 81 4 16 36 64 100)
> (length (append (map square odds) (map square evens)))
10
