#lang racket

; Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is
; using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
	(if (= x 0)
		0
		y
	)
)

; Then he evaluates the expression

(test 0 (p))

; What behavior will Ben observe with an interpreter that uses applicative-order evaluation?
; What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer.
; (Assume that the evaluation rule for the special form if is the same whether the interpreter is using
; normal or applicative order: The predicate expression is evaluated first, and the result determines
; whether to evaluate the consequent or the alternative expression.)

; S O L U T I O N

; Normal-order evaluation: Fully expand, then reduce
; Applicative-order evaluation: Evaluate the arguments & then apply

; For (test 0 (p))

; Case 1: Applicative Order Evaluation

; (p) is evaluated first. But since (p) is defined as (define (p) (p)), this evaluation will
; enter an infinite loop. The call to (p) does not return.

; Case 2: Normal Order Evaluation

; Expansion happens as follows:

(test 0 (p))

-->	(if (= 0 0)
		0
		(p)
	)

; Since the if condition is satisfied
--> 0

; (p) is not evaluated at all so there is no infinite loop. The final resulst is zero.

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
> (p)
. . user break (Infinite Loop)
> (test 0 (p))
. . user break (Infinite Loop)
> 

; When I actually tried this in Racket, I saw an infinite loop so the conclusion is that the Racket
; interpreter uses applicative-order evaluation
