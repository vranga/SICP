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
