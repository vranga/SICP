#lang racket

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
