#lang racket

(define (equal? list1 list2)
	(cond
		((and (not (pair? list1)) (not (pair? list2))) (eq? list1 list2))
		((and (pair? list1) (pair? list2))
			(and
				(equal? (car list1) (car list2))
				(equal? (cdr list1) (cdr list2))
			)
		)
		(else
			false
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (equal? '(this is a list) '(this is a list))
#t
> (equal? '(this is a list) '(this (is a) list))
#f
> (equal? 'king 'queen)
#f
> (equal? 'king 'king)
#t
> (equal? 26 25)
#f
> (equal? 26 26)
#t
> (equal? '(this is a longer list) '(this is another short list))
#f
> (equal? '(this is a longer list than the next one) '(this is another short list))
#f
> (equal? '(this is a longer list than the next one) 'word)
#f
> (equal? '(this is a longer list than the next one) '(this is a longer list than the next one))
#t
> (equal? 'football '(football volleyball))
#f
> (equal? '() '())
#t
> (equal? '(first) '(next))
#f
> (equal? '(first) '(first))
#t
> (equal? '(first) '((first)))
#f
> 
