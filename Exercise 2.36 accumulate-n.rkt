#lang racket

(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		null
		(cons
			(accumulate op init (seq-of-first-elements seqs))
			(accumulate-n op init (seqs-without-first-elements seqs))
		)
	)
)

(define (seq-of-first-elements seqs)
	(cond
		; the contained lists are empty
		((null? (car seqs)) seqs)
		; there is only one contained list
		((null? (cdr seqs)) (list (car (car seqs))))
		(else
			(cons (car (car seqs)) (seq-of-first-elements (cdr seqs)))
		)
	)
)

(define (seqs-without-first-elements seqs)
	(cond
		; the contained lists are empty
		((null? (car seqs)) seqs)
		; there is only one contained list
		((null? (cdr seqs)) (list (cdr (car seqs))))
		(else
			(cons (cdr (car seqs)) (seqs-without-first-elements (cdr seqs)))
		)
	)
)

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence) (accumulate op initial (cdr sequence)))
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define lol (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
> lol
'((1 2 3) (4 5 6) (7 8 9) (10 11 12))
> (seqs-without-first-elements lol)
'((2 3) (5 6) (8 9) (11 12))
> (seqs-without-first-elements (seqs-without-first-elements lol))
'((3) (6) (9) (12))
> (seqs-without-first-elements (seqs-without-first-elements (seqs-without-first-elements lol)))
'(() () () ())
> (seqs-without-first-elements (seqs-without-first-elements (seqs-without-first-elements (seqs-without-first-elements lol))))
'(() () () ())
> (seq-of-first-elements lol)
'(1 4 7 10)
> (define small (list (list 3) (list 6) (list 9) (list 12)))
> small
'((3) (6) (9) (12))
> (seq-of-first-elements small)
'(3 6 9 12)
> (seqs-without-first-elements small)
'(() () () ())
> (seq-of-first-elements (seqs-without-first-elements small))
'(() () () ())
> (accumulate-n + 0 lol)
'(22 26 30)
> (seq-of-first-elements (seqs-without-first-elements small))
'(() () () ())
> (accumulate-n + 0 (seq-of-first-elements (seqs-without-first-elements small)))
'()
> 
