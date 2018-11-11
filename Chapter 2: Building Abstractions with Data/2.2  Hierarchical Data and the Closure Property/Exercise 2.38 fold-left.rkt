#lang racket

; fold-right and fold-left

(define (fold-right op initial sequence)
	(accumulate op initial sequence)
)

(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest)) (cdr rest))
		)
	)
	(iter initial sequence)
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
> (fold-right / 1 (list 1 2 3))
1 1/2
> (fold-left / 1 (list 1 2 3))
1/6
> (fold-right list null (list 1 2 3))
'(1 (2 (3 ())))
> (fold-left list null (list 1 2 3))
'(((() 1) 2) 3)
> (fold-right * 1 (list 7 8 9))
504
> (fold-left * 1 (list 7 8 9))
504
> (fold-right + 0 (list 7 8 9))
24
> (fold-left + 0 (list 7 8 9))
24
> (fold-right - 0 (list 7 8 9))
8
> (fold-left - 0 (list 7 8 9))
-24
> 

; Notes: 'op' should satisfy the associative property to guarantee that fold-right and fold-left
; will produce the same values for any sequence.
; We can see in the above examples that for addition and multiplication which are associative
; operations, the same result is produced by both fold-right and fold-left. On the other hand,
; for division and subtraction different results are produced by fold-right and fold-left