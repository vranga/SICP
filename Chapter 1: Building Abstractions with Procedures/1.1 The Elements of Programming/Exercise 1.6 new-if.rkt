(define (sqrt x)
  (sqrt-iter 1.0 x)
)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
  )
)

(define tolerance 0.001)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) tolerance)
)

(define (square x) (* x x)) 

(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)

# These are the above procedures redefined with the new 'if' definition

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)
  )
)

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x) x)
  )
)

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x)
)

Explanation: new-if is a procedure. So when it is called, it tries to evaluate all its arguments first (applicative order evaluation). One of the arguments is 'new-sqrt-iter' so this results in recursive calls that never end. 'new-if' does not fully execute and return even once.



(I could not figure it out. Had to peek.)