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