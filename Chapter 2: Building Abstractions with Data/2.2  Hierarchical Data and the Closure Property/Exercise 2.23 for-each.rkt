#lang racket

; Exercise 2.23.  The procedure for-each is similar to map. It takes as arguments a procedure
; and a list of elements. However, rather than forming a list of the results, for-each just
; applies the procedure to each of the elements in turn, from left to right. The values
; returned by applying the procedure to the elements are not used at all -- for-each is used
; with procedures that perform an action, such as printing. For example,

; (for-each (lambda (x) (newline) (display x))
;           (list 57 321 88))
; 57
; 321
; 88

; The value returned by the call to for-each (not illustrated above) can be something
; arbitrary, such as true. Give an implementation of for-each.

; SOLUTION

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
