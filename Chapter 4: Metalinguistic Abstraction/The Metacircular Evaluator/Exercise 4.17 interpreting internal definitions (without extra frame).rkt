#lang racket

; Exercise 4.17.  Draw diagrams of the environment in effect when evaluating the expression
; <e3> in the procedure in the text, comparing how this will be structured when definitions
; are interpreted sequentially with how it will be structured if definitions are scanned out
; as described. Why is there an extra frame in the transformed program? Explain why this
; difference in environment structure can never make a difference in the behavior of a correct
; program. Design a way to make the interpreter implement the ``simultaneous'' scope rule for
; internal definitions without constructing the extra frame.

; S O L U T I O N

; This following transforamtion ensures "simultaneous" scope rule for internal definitions
; without constructing the extra frame that the program in Exercise 4.16 does. See below.
; Since we dispense with the 'let' expression, the extra frame is not created

; The procedure

; (lambda <vars>
; 	(define u <e1>)
; 	(define v <e2>)
; 	<e3>
; )

; would be transformed into

; (lambda <vars>
; 	(define u '*unassigned*)
; 	(define v '*unassigned*)
; 	(set! u <e1>)
; 	(set! v <e2>)
; 	<e3>
; )
