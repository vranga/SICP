#lang racket

; Exercise 2.55.  Eva Lu Ator types to the interpreter the expression

; (car ''abracadabra)

; To her surprise, the interpreter prints back quote. Explain.

; SOLUTION

''abracadabra
; is actually
'(quote abracadabra)
; is actually
(quote (quote abracadabra))

; This evaluates to a list of two elements, the first one being a single-quote and the
; second one being the literal "abracadabra". So 'car' of this will evaluate to quote
; Naturally, 'cdr' of the same thing will evaluate to the single element list that 
; contains the literal abracadabra

> (car ''abracadabra)
'quote
> (cdr ''abracadabra)
'(abracadabra)
> (car (cdr ''abracadabra))
'abracadabra
> 
