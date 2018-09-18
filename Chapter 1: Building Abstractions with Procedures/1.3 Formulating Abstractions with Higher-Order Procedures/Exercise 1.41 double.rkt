#lang racket

(define (double f)
	(lambda (w) (f (f w)))
)

(define (inc x)
	(+ x 1)
)

; Tests
> (inc 25)
26
> (inc 25)
26
> ((double inc) 25)
27
> ((double inc) 27)
29
> ((double (double inc)) 27)
31
> (((double (double double)) inc) 5)
21
> (((double (double double)) inc) 0)
16
> (((double double) inc) 0)
4
> ((double inc) 0)
2
> (((double double) inc) 0)
4
> (((double (double double)) inc) 0)
16
> 
