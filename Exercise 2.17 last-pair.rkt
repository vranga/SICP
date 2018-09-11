#lang racket

(define (last-pair items)
	(cond
		((null? items) (error "The given list is empty"))
		((null? (cdr items)) items)
		(else (last-pair (cdr items)))
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (last-pair (list 23 72 148 34))
'(34)
> (last-pair (list 23 72 148))
'(148)
> (last-pair (list 23 72))
'(72)
> (last-pair (list 23))
'(23)
> (last-pair (cdr (list 23)))
. . The given list is empty
> 
