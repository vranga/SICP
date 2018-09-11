(define (f-recur n)
  (cond
    ((< n 3) n)
    (else
      (+
        (f-recur (- n 1))
        (* 2 (f-recur (- n 2)))
        (* 3 (f-recur (- n 3)))
      )
    )
  )
)