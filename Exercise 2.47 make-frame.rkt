#lang racket

(define (make-frame1 origin edge1 edge2)
	(list origin edge1 edge2)
)

(define (origin-frame1 f)
	(car f)
)

(define (edge1-frame1 f)
	(car (cdr f))
)

(define (edge2-frame1 f)
	(car (cdr (cdr f)))
)

(define (make-frame2 origin edge1 edge2)
	(cons origin (cons edge1 edge2))
)

(define (origin-frame2 f)
	(car f)
)

(define (edge1-frame2 f)
	(car (cdr f))
)

(define (edge2-frame2 f)
	(cdr (cdr f))
)

(define (make-vect x y)
	(cons x y)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (define origin (make-vect 3 5))
> (define edge1 (make-vect 2 -4))
> (define edge2 (make-vect 4 9))
> (make-frame1 origin edge1 edge2)
'((3 . 5) (2 . -4) (4 . 9))
> (origin-frame1 (make-frame1 origin edge1 edge2))
'(3 . 5)
> (edge1-frame1 (make-frame1 origin edge1 edge2))
'(2 . -4)
> (edge2-frame1 (make-frame1 origin edge1 edge2))
'(4 . 9)
> (make-frame2 origin edge1 edge2)
'((3 . 5) (2 . -4) 4 . 9)
> (origin-frame2 (make-frame2 origin edge1 edge2))
'(3 . 5)
> (edge1-frame2 (make-frame2 origin edge1 edge2))
'(2 . -4)
> (edge2-frame2 (make-frame2 origin edge1 edge2))
'(4 . 9)
> 
