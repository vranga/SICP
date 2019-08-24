; Exercise 1.11.  A function f is defined by the rule that
; f(n) = n if n < 3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3. Write a procedure
; that computes f by means of a recursive process. Write a procedure that computes f by
; means of an iterative process.

(define (f-iter n)
  (f-iter-internal 2 1 0 3 n)
)

(define (f-iter-internal val-n-1 val-n-2 val-n-3 counter max_count)
  (cond
    ((= max_count 0) 0)
    ((= max_count 1) 1)
    ((= max_count 2) 2)
    ((> counter max_count) val-n-1)
    (else
      (f-iter-internal
        (+ val-n-1 (* 2 val-n-2) (* 3 val-n-3))
        val-n-1
        val-n-2
	(+ counter 1)
	max_count
      )
    )
  )
)
