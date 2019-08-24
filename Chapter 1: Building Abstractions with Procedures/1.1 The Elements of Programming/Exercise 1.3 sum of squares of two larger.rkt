#lang racket

; Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers.

; SOLUTION

(define (square x) (* x x)) 

(define (sum_of_squares a b) (+ (square a) (square b)))

(define
    (greatest a b c)
    (cond
      ((and (>= a b) (>= a c)) a)
      ((and (>= b a) (>= b c)) b)
      ((and (>= c a) (>= c b)) c)
    )
)

(define
    (middle a b c)
    (cond
      ((or
       (and (<= a b) (>= a c))
       (and (<= a c) (>= a b))
      ) a)
      ((or
       (and (<= b a) (>= b c))
       (and (<= b c) (>= b a))
      ) b)
      ((or
       (and (<= c a) (>= c b))
       (and (<= c b) (>= c a))
      ) c)
    )
)

(define (sum_of_squares_of_two_larger a b c) (sum_of_squares (greatest a b c) (middle a b c)))

 
