#lang racket

; Exercise 3.40.  Give all possible values of x that can result from executing

; (define x 10)

; (parallel-execute (lambda () (set! x (* x x)))
;                   (lambda () (set! x (* x x x))))

; Which of these possibilities remain if we instead use serialized procedures:

; (define x 10)

; (define s (make-serializer))

; (parallel-execute (s (lambda () (set! x (* x x))))
;                   (s (lambda () (set! x (* x x x)))))

; S O L U T I O N

; When there is no serialization:

1000000: P1 executes fully first, then P2 executes
1000000: P2 executes fully first, then P1 executes
10000: P2 changes x from 10 to 1000 between the two times that P1 accesses the value of x during the evaluation of (* x x)
100000: P1 changes x from 10 to 100 between the first and second access of x in P2 during the evaluation of (* x x x)
10000: P1 changes x from 10 to 100 between the second and third access of x in P2 during the evaluation of (* x x x)
1000: P2 accesses x, then P1 sets x to 100, then P2 sets x
100: P1 accesses x, then P2 sets x to 1000, then P1 sets x

; When there is serialization:

1000000: P1 executes fully first, then P2 executes
1000000: P2 executes fully first, then P1 executes
