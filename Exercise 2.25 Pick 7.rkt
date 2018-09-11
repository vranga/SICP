Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.

> (list 1 (list 2 (list 3 4)))
'(1 (2 (3 4)))
> (define L (list 1 3 (list 5 7) 9))
> L
'(1 3 (5 7) 9)
> (car (cdr (car (cdr (cdr L)))))
7
> (define M (list (list 7)))
> M
'((7))
> (car (car M))
7
> (define N (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
> N
'(1 (2 (3 (4 (5 (6 7))))))
> (cdr N)
'((2 (3 (4 (5 (6 7))))))
> (car (cdr N))
'(2 (3 (4 (5 (6 7)))))
> (cdr (car (cdr N)))
'((3 (4 (5 (6 7)))))
> (car (cdr (car (cdr N))))
'(3 (4 (5 (6 7))))
> (cdr (car (cdr (car (cdr N)))))
'((4 (5 (6 7))))
> (car (cdr (car (cdr (car (cdr N))))))
'(4 (5 (6 7)))
> (cdr (car (cdr (car (cdr (car (cdr N)))))))
'((5 (6 7)))
> (car (cdr (car (cdr (car (cdr (car (cdr N))))))))
'(5 (6 7))
> (cdr (car (cdr (car (cdr (car (cdr (car (cdr N)))))))))
'((6 7))
> (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr N))))))))))
'(6 7)
> (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr N)))))))))))
'(7)
> (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr N))))))))))))
7
> 
