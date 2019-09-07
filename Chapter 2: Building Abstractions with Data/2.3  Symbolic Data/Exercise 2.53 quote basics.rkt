#lang racket

; Exercise 2.53.  What would the interpreter print in response to evaluating each of the
; following expressions?

; (list 'a 'b 'c)

; (list (list 'george))
; (cdr '((x1 x2) (y1 y2)))

; (cadr '((x1 x2) (y1 y2)))
; (pair? (car '(a short list)))
; (memq 'red '((red shoes) (blue socks)))

; (memq 'red '(red shoes blue socks))

; SOLUTION

> (display (list 'a 'b 'c))
(a b c)
> (display (list (list 'george)))
((george))
> (display (cdr '((x1 x2) (y1 y2))))
((y1 y2))
> (display (cadr '((x1 x2) (y1 y2))))
(y1 y2)
> (display (pair? (car '(a short list))))
#f
> (display (memq 'red '((red shoes) (blue socks))))
#f
> (display (memq 'red '(red shoes blue socks)))
(red shoes blue socks)
> 
