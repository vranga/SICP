#lang racket

; Exercise 1.8.  Newton's method for cube roots is based on the fact that if y is an
; approximation to the cube root of x, then a better approximation is given by the value

; (x/(y^2) + 2y) / 3

; Use this formula to implement a cube-root procedure analogous to the square-root procedure.

; SOLUTION

(define startingGuess 1.0)

(define (cbrt x)
  (cbrt-iter startingGuess x)
)

(define (cbrt-iter guess x)
 (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)
 )
)

(define fractionOfGuessLimit 0.000001)

(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) fractionOfGuessLimit)
)

(define (cube x) (* x x x)) 

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)

(define (square x) (* x x))

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (cbrt 8)
2.000000000012062
> (cbrt 7)
1.9129320405969417
> (cbrt 6)
1.817120681877066
> (cbrt 5)
1.709975950782189
> (cbrt 4)
1.5874010520152708
> (cbrt 3)
1.4422497895989996
> (cbrt 2)
1.2599210500177698
> (cbrt 1)
1.0
> 
