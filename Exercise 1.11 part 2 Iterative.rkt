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
