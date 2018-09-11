#lang racket

(define (for-each proc items)
	(cond
		((not (null? items)) (proc (car items)) (for-each proc (cdr items)))
		(else
			(void)
		)
	)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (for-each (lambda (x) (display (* x x)) (newline)) (list 57 321 88))
3249
103041
7744
> (for-each (lambda (x) (display (* x 2.0)) (newline)) (list 57 321 88))
114.0
642.0
176.0
> (for-each (lambda (x) (display (/ x 2.0)) (newline)) (list 57 321 88))
28.5
160.5
44.0
> (for-each (lambda (x) (newline) (display x)) (list 57 321 88))

57
321
88
> (for-each (lambda (x) (display x) (newline)) (list 57 321 88))
57
321
88
> 
