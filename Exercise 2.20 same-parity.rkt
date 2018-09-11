#lang racket

(define (same-parity . list-of-integers)

	(define (pick-evens number-list)
		(cond
			((null? number-list) (list))
			((even? (car number-list)) (cons (car number-list) (pick-evens (cdr number-list))))
			(else
				; first element is odd, so continue looking
				(pick-evens (cdr number-list))
			)
		)
	)

	(define (pick-odds number-list)
		(cond
			((null? number-list) (list))
			((odd? (car number-list)) (cons (car number-list) (pick-odds (cdr number-list))))
			(else
				; first element is even, so continue looking
				(pick-odds (cdr number-list))
			)
		)
	)

	(cond
		((null? list-of-integers) (error "Supplied list is empty"))
		((even? (car list-of-integers)) (pick-evens list-of-integers))
		(else
			(pick-odds list-of-integers)
		)
	)

)

(define (even? n)
	(= (remainder n 2) 0)
)

(define (odd? n)
	(= (remainder n 2) 1)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (same-parity 1 2 3 4 5 6 7)
'(1 3 5 7)
> (same-parity 2 3 4 5 6 7)
'(2 4 6)
> (same-parity 3 4 5 6 7)
'(3 5 7)
> (same-parity 4 5 6 7)
'(4 6)
> (same-parity 5 6 7)
'(5 7)
> (same-parity 6 7)
'(6)
> (same-parity 7)
'(7)
> (same-parity 8)
'(8)
> (same-parity)
. . Supplied list is empty
> (same-parity 9 5 1 7 3 6 2 4 8 11 18 17 16 14 12 13 10)
'(9 5 1 7 3 11 17 13)
> (same-parity 50 9 5 1 7 3 6 2 4 8 11 18 17 16 14 12 13 10)
'(50 6 2 4 8 18 16 14 12 10)
> 
