; Exercise 2.15.  Eva Lu Ator, another user, has also noticed the different intervals computed
; by different but algebraically equivalent expressions. She says that a formula to compute
; with intervals using Alyssa's system will produce tighter error bounds if it can be written
; in such a form that no variable that represents an uncertain number is repeated. Thus, she
; says, par2 is a ``better'' program for parallel resistances than par1. Is she right? Why?

; EXPLANATION

; Eva Lu Ator is right. We can see in the solution to exercise 2.14 that the system will produce
; tighter error bounds if we minimize the repetition of intervals. So par2 is a better program
; than par1.

; We can also see from the examples in the solution to exercise 2.14 that expressions that use
; the same interval everywhere also produce larger error bounds with more repetitions of the
; interval

; In real-world engineering problems that involve complex arithmetic expressions, it may not
; always be possible to avoid repeating variables which represent uncertain numbers.
