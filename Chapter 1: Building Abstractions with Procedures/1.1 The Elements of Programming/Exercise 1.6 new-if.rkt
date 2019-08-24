; Exercise 1.6.  Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
; ``Why can't I just define it as an ordinary procedure in terms of cond?'' she asks.
; Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version
; of if:

; (define (new-if predicate then-clause else-clause)
;   (cond (predicate then-clause)
;         (else else-clause)))

; Eva demonstrates the program for Alyssa:

; (new-if (= 2 3) 0 5)
; 5

; (new-if (= 1 1) 0 5)
; 0

; Delighted, Alyssa uses new-if to rewrite the square-root program:

; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x)
;                      x)))

; What happens when Alyssa attempts to use this to compute square roots? Explain.

; SOLUTION

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
