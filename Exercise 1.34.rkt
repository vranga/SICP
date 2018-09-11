#lang racket

(define (f g)
	(g 2)
)

(define (square x)
	(* x x)
)

; Tests

i> (f square)
4
> (f (lambda (z) (* z (+ z 1))))
6
> (f f)
. . application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 2
  arguments...:
> 

Explanation:

(f f) evaluates to (f 2) which in turn results in (2 2) where the first term 
is expected to be a procedure. So the evaluator doesn't accept it and gives an
error
