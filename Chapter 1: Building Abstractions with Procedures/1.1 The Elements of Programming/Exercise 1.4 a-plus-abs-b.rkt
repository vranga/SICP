#lang racket

; Exercise 1.4. Observe that our model of evaluation allows for combinations whose operators
; are compound expressions. Use this observation to describe the behavior of the following
; procedure:

; (define (a-plus-abs-b a b)
;   ((if (> b 0) + -) a b))

; Explanation: 

; The expression (if (> b 0) + -) evaluates to either + or a - based on the value of 'b'.

; This then becomes the procedure that operates on arguments 'a' and 'b'.

; So the behaviour is that if b is positive, then b gets added to a. If b is negative or zero,
; then b is subtracted from a. So the overall effect is:

; "a plus the absolute value of b"
