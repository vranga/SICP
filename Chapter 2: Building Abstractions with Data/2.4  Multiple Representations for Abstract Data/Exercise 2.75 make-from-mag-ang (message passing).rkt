#lang racket

; Exercise 2.75.  Implement the constructor make-from-mag-ang in message-passing style.
; This procedure should be analogous to the make-from-real-imag procedure given above.

; SOLUTION

(define (make-from-mag-ang mag ang)
	(define (dispatch op)
		(cond
			((eq? op 'real-part) (* mag (cos ang)))
			((eq? op 'imag-part) (* mag (sin ang)))
			((eq? op 'magnitude) mag)
			((eq? op 'angle) ang)
			(else
				(error "Unknown op -- MAKE-FROM-MAG-ANG" op)
			)
		)
	)
	dispatch
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (define mag 5)
> (define ang (atan (/ 3 4)))
> mag
5
> ang
0.6435011087932844
> ((make-from-mag-ang mag ang) 'real-part)
4.0
> ((make-from-mag-ang mag ang) 'imag-part)
3.0
> ((make-from-mag-ang mag ang) 'magnitude)
5
> ((make-from-mag-ang mag ang) 'angle)
0.6435011087932844
> ((make-from-mag-ang mag ang) 'xyz)
. . Unknown op -- MAKE-FROM-MAG-ANG xyz
> 
