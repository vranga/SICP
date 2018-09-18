#lang racket

(define (subsets s)
	(if (null? s)
		(list null)
		(let
			((rest (subsets (cdr s))))
			(append
				rest
				(map
					(lambda (element)
						(cond
							((null? (car s)) element)
							(else
								(cond
									((null? element) (list (car s)))
									(else
										(cons (car s) element)
									)
								)
							)
						)
					)
					rest
				)
			)
		)
	)
)

; Explanation: This problem is identical to the count-change problem that appears earlier in this
; book. Let's say that from a given set S, we pick any element e. Then, the elements in the set S' of
; all subsets of S can be segregated into two groups: one whose members contain e and the other
; whose members do not contain e.

; The logic used here is as follows: The set S' of all subsets of a given set S is the union of
; the following two sets:

; 1.	The set of all subsets of the set obtained after removing one element, say e from S.
; 	Let's call this set P.
; 2.	The set Q generated from P by adding e to each element in P

; The 'map' function above does exactly what is described in  step 2 above. The variable 'rest' is
; the same as set P above.

; S' = P union Q

; Note: The procedure above works well even when the set elements are lists/pairs themselves

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (subsets null)
'(())
> (subsets (list 1))
'(() (1))
> (subsets (list 1 2))
'(() (2) (1) (1 2))
> (subsets (list 1 2 3))
'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
> (subsets (list 1 2 3 4))
'(()
  (4)
  (3)
  (3 4)
  (2)
  (2 4)
  (2 3)
  (2 3 4)
  (1)
  (1 4)
  (1 3)
  (1 3 4)
  (1 2)
  (1 2 4)
  (1 2 3)
  (1 2 3 4))
> (subsets (subsets null))
'(() ())
> (subsets (subsets (list 1)))
'(() ((1)) () ((1)))
> (define lol (list (list 1 2) (list 3 4) 5))
> lol
'((1 2) (3 4) 5)
> (subsets lol)
'(() (5) ((3 4)) ((3 4) 5) ((1 2)) ((1 2) 5) ((1 2) (3 4)) ((1 2) (3 4) 5))
> (define clol (list (list 1 2) 3))
> clol
'((1 2) 3)
> (subsets clol)
'(() (3) ((1 2)) ((1 2) 3))
> (define nlist (list 1 (list 2) (list 3 (list 4)) (list (list 5))))
> nlist
'(1 (2) (3 (4)) ((5)))
> (subsets nlist)
'(()
  (((5)))
  ((3 (4)))
  ((3 (4)) ((5)))
  ((2))
  ((2) ((5)))
  ((2) (3 (4)))
  ((2) (3 (4)) ((5)))
  (1)
  (1 ((5)))
  (1 (3 (4)))
  (1 (3 (4)) ((5)))
  (1 (2))
  (1 (2) ((5)))
  (1 (2) (3 (4)))
  (1 (2) (3 (4)) ((5))))
> 
