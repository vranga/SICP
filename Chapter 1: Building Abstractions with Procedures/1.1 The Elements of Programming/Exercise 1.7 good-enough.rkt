; Exercise 1.7.  The good-enough? test used in computing square roots will not be very effective for
; finding the square roots of very small numbers. Also, in real computers, arithmetic operations are
; almost always performed with limited precision. This makes our test inadequate for very large numbers.
; Explain these statements, with examples showing how the test fails for small and large numbers.
; An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration
; to the next and to stop when the change is a very small fraction of the guess. Design a square-root
; procedure that uses this kind of end test. Does this work better for small and large numbers?

1. Try with numbers close to tolerance

> (sqrt 1)
1.0
> (sqrt 0.5)
0.7071078431372548
> (sqrt 0.1)
0.316245562280389
> (sqrt 0.01)
0.10032578510960605
> (sqrt 0.0001)
0.03230844833048122
> (sqrt 0.04)
0.2006099040777959

(Doesn't work if the number we pick is less then the tolerance itself)

2. Larger and larger numbers:

> (sqrt 1)
1.0
> (sqrt 2)
1.4142156862745097
> (sqrt 25)
5.000023178253949
> (sqrt 64)
8.000001655289593
> (sqrt 121)
11.000000001611474
> (sqrt 196)
14.000000310617537
> (sqrt 225)
15.000001132796916
> (sqrt 289)
17.000009637317497
> (sqrt 3136)
56.00000127934874
> (sqrt 111556)
334.0000000148877
> (sqrt 12444741136)
111556.0
> (sqrt 44755941136)
211556.0
> (sqrt 999999961946176)
31622776.0

Accuracy seems to increase, then decrease, then increase again!

===========================================

NOW USING "alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess."

(define startingGuess 1.0)

(define (sqrt x)
  (sqrt-iter startingGuess x)
)

(define (sqrt-iter guess x)
 (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
 )
)

(define fractionOfGuessLimit 0.00001)

(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) fractionOfGuessLimit)
)

(define (square x) (* x x)) 

(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)

==============================

> (sqrt 9)
3.000000001396984
> (sqrt 4)
2.0000000929222947
> (sqrt 1)
1.0
> (sqrt 0.09)
0.30000000149658457
> (sqrt 0.04)
0.20000092713015796
> (sqrt 0.0004)
0.020000000050877154
> (sqrt 0.000004)
0.0020000003065983023
> (sqrt 0.00000004)
0.0002000008122246471
> (sqrt 0.0000000004)
2.0000000031206467e-005
> (sqrt 0.000000000004)
2.0000002069224837e-006
> (sqrt 0.00000000000004)
  )
2.0000059300556747e-007
. read: unexpected `)'
> (sqrt 0.00000000000004)
2.0000059300556747e-007
> (sqrt 0.0000000000000004)
2.000000001886501e-008
> (sqrt 0.000000000000000004)
2.0000001383367664e-009
> (sqrt 0.00000000000000000004)
2.000004296972247e-010
> (sqrt 0.0000000000000000000004)
2.000000001126739e-011
> (sqrt 0.0000000000000000000004)
2.000000001126739e-011
> (sqrt 0.000000000000000000000004)
2.0000000915947702e-012
> (sqrt 999999961946176)
31622778.678310443
> (define fractionOfGuessLimit 0.00000001)
> (sqrt 999999961946176)
31622776.00000011
> 

Works much better for small numbers. Works well for larger numbers but we need to make the fractionOfGuessLimit very small
