; Exercise 3.47.  A semaphore (of size n) is a generalization of a mutex. Like a mutex, a semaphore
; supports acquire and release operations, but it is more general in that up to n processes can acquire
; it concurrently. Additional processes that attempt to acquire the semaphore must wait for release
; operations. Give implementations of semaphores

; a. in terms of mutexes

; b. in terms of atomic test-and-set! operations.

; S O L U T I O N

(load "lib/parallel.scm")

(define (make-serializer)
	(let ((mutex (make-mutex)))
		(lambda (p)
			(define (serialized-p . args)
				(mutex 'acquire)
				(let ((val (apply p args)))
					(mutex 'release)
					val
				)
			)
			serialized-p
		)
	)
)

(define (make-mutex)
	(let ((cell (list false)))
		(define (the-mutex m)
			(cond
				((eq? m 'acquire)
					(if (test-and-set! cell)
						(the-mutex 'acquire) ; retry
					)
				)
				((eq? m 'release) (clear! cell))
			)
		)
		the-mutex
	)
)

(define (clear! cell)
	(set-car! cell false)
)

(define (test-and-set! cell)
	(without-interrupts
		(lambda ()
			(if (car cell)
				true
				(begin (set-car! cell true)
					false
				)
			)
		)
	)
)

(define (make-concurrency-limiter max-concurrency)
	(let ((semaphore (make-semaphore max-concurrency)))
		(lambda (p)
			(define (concurrency-limited-p . args)
				(semaphore 'acquire)
				(let ((val (apply p args)))
					(semaphore 'release)
					val
				)
			)
			concurrency-limited-p
		)
	)
)

(define (make-semaphore size)
	(let ((cell (list 0)))
		
		(define (test-and-set-semaphore! s)
			(without-interrupts
				(lambda ()
					(if (>= (car s) size)
						true
						(begin (set-car! s (+ (car s) 1))
							false
						)
					)
				)
			)
		)

		(define (release-semaphore! s)
			(without-interrupts
				(lambda ()
					(if (> (car s) 0)
						(set-car! s (- (car s) 1))
					)
				)
			)
		)

		(define (the-semaphore s)
			(cond
				((eq? s 'acquire)
					(if (test-and-set-semaphore! cell)
						(the-semaphore 'acquire) ; retry
					)
				)
				((eq? s 'release) (release-semaphore! cell))
			)
		)

		(if (and (> size 0) (integer? size))
			the-semaphore
			(error "Size of semaphore should be a natural number")
		)
	)
)

; Testing and getting familiar with scheme
(define (mult a b)

	(define (mult-internal p1 p2 count-down)
		(if (> count-down 0)
			(begin
				(sleep-current-thread 1000)
				(display (current-thread))
				(display (* p1 p2))
				(display "\n")
				(mult-internal (- p1 1) (+ p2 1) (- count-down 1))
			)
		)
	)

	(mult-internal a b 10)
)

(define s (make-concurrency-limiter 3))

(define e (lambda () ((s mult) 9 19)))
(define f (lambda () ((s mult) 45 72)))
(define g (lambda () ((s mult) 9 78)))
(define h (lambda () ((s mult) 235 19)))
(define i (lambda () ((s mult) 23 91)))
(define j (lambda () ((s mult) 35 62)))
(define k (lambda () ((s mult) 25 26)))
(define l (lambda () ((s mult) 5 4598)))
(define m (lambda () ((s mult) 765 342)))
(define n (lambda () ((s mult) 98 765598)))

(parallel-execute e f g h i j k l m n)

; Test Results

BANL154931268:3.4 Concurrency - Time Is of the Essence vranganath$ scheme
MIT/GNU Scheme running under OS X
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2014 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even
for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Saturday May 17, 2014 at 2:39:25 AM
  Release 9.2 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118 || Edwin 3.116

1 ]=> (load "Exercise 3.47 semaphore b (in terms of atomic test-and-set operations).scm")

;Loading "Exercise 3.47 semaphore b (in terms of atomic test-and-set operations).scm"...
;  Loading "lib/parallel.scm"... done
;... done
;Value 13: #[compound-procedure 13 terminator]

1 ]=> #[thread 14]171
#[thread 15]3240
#[thread 16]702
#[thread 14]160
#[thread 15]3212
#[thread 16]632
#[thread 14]147
#[thread 15]3182
#[thread 16]560
#[thread 14]132
#[thread 15]3150
#[thread 16]486
#[thread 14]115
#[thread 15]3116
#[thread 16]410
#[thread 14]96
#[thread 15]3080
#[thread 16]332
#[thread 14]75
#[thread 15]3042
#[thread 16]252
#[thread 14]52
#[thread 15]3002
#[thread 16]170
#[thread 14]27
#[thread 15]2960
#[thread 16]86
#[thread 14]0
#[thread 15]2916
#[thread 16]0
#[thread 17]75028604
#[thread 18]4465
#[thread 19]2093
#[thread 17]74263103
#[thread 18]4680
#[thread 19]2024
#[thread 17]73497600
#[thread 18]4893
#[thread 19]1953
#[thread 17]72732095
#[thread 18]5104
#[thread 19]1880
#[thread 17]71966588
#[thread 18]5313
#[thread 19]1805
#[thread 17]71201079
#[thread 18]5520
#[thread 19]1728
#[thread 17]70435568
#[thread 18]5725
#[thread 19]1649
#[thread 17]69670055
#[thread 18]5928
#[thread 19]1568
#[thread 17]68904540
#[thread 18]6129
#[thread 19]1485
#[thread 17]68139023
#[thread 18]6328
#[thread 19]1400
#[thread 20]22990
#[thread 21]261630
#[thread 22]2170
#[thread 20]18396
#[thread 21]262052
#[thread 22]2142
#[thread 20]13800
#[thread 21]262472
#[thread 22]2112
#[thread 20]9202
#[thread 21]262890
#[thread 22]2080
#[thread 20]4602
#[thread 21]263306
#[thread 22]2046
#[thread 20]0
#[thread 21]263720
#[thread 22]2010
#[thread 20]-4604
#[thread 21]264132
#[thread 22]1972
#[thread 20]-9210
#[thread 21]264542
#[thread 22]1932
#[thread 20]-13818
#[thread 21]264950
#[thread 22]1890
#[thread 20]-18428
#[thread 21]265356
#[thread 22]1846
#[thread 23]650
#[thread 23]648
#[thread 23]644
#[thread 23]638
#[thread 23]630
#[thread 23]620
#[thread 23]608
#[thread 23]594
#[thread 23]578
#[thread 23]560


