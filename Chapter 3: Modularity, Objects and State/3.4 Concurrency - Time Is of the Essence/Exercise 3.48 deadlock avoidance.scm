; Exercise 3.48.  Explain in detail why the deadlock-avoidance method described above, (i.e., the accounts
; are numbered, and each process attempts to acquire the smaller-numbered account first) avoids deadlock
; in the exchange problem. Rewrite serialized-exchange to incorporate this idea. (You will also need to
; modify make-account so that each account is created with a number, which can be accessed by sending an
; appropriate message.)

; S O L U T I O N

; Explanation: I run four tests here:
;
; Test 1: Main thread runs serialized-exchange. There is no concurrency. So it just works. 
;
; Test 2: Two threads run serialized-exchange in parallel with opposite account orders. In this case
; the first thread almost always acquires both the accounts before the 2nd thread tries to acquire account 2.
; So again, thread 1 is able to complete the exchange and thread 2 starts its work only after that. So
; even though a deadlock is theoretically possible, practically it doesn't happen.
; This is where the next test comes in.
;
; Test 3: Two threads run serialized-exchange in parallel with opposite account orders. However, there is
; a 1 second delay after any mutex is acquired so as to increase the chances of a deadlock.
; The idea is for a thread to acquire mutex 1 and delay before acquiring the mutex 2. This gives the other
; thread a chance to acquire mutex 2 thereby creating a deadlock. This is the reason the sleep command
; needs to be in 'make-serializer' immediately after the mutex is acquired. This sleep command is
; commented out while running tests 1 and 2.
; Sure enough, when we run test 3 we encounter a deadlock situation (See test results)
;
; Test 4: Two threads run serialized-exchange-da in parallel with opposite account orders.
; The suffice 'da' stands for deadlock avoidance.  The 1 second delay after the mutex is acquired is still
; there. This new procedure avoids the deadlock using the technique described in the question.
; Accounts are acquired in the same order. See results below. There is no deadlock
;
; So why does this technique work? The reason is that we have added another level of serialization here.
; The first level of serialization is to make concurrent processes execute a procedure serially. This works
; well when there is only resource to serialize over. But when we have two or more resources and they
; need to be acquired together by each thread but are acquired in different orders by different threads,
; we get deadlocks. To overcome this situation, we essentially 'serialize the serialization' so that threads
; acquire a set of resources in the same order. In other words, we make the threads 'line-up' with each
; other by acquiring resources in the same order. This ensures that all threads also release resources in
; the same order (which will be the reverse of the order in which the resources were acquired.)
; We can also look at this as a 'pipeline' of resources that each thread acquires from beginning to end
; and releases from end to beginning.
; So in effect, the threads will get serialized over the entire set of resources as if it is one big
; resource.

(load "lib/parallel.scm")

; Serialized exchange without deadlock avoidance
(define (serialized-exchange account1 account2)
	(let ((serializer1 (account1 'serializer)) (serializer2 (account2 'serializer)))
		((serializer1 (serializer2 exchange)) account1 account2)
	)
)

; Serialized exchange with deadlock avoidance
; Give each account a unique identification number. serialized-exchange is written such that a process will
; always attempt to enter a procedure protecting the lowest-numbered account first.
(define (serialized-exchange-da account1 account2)
	(let (
			(account1-id (account1 'identification-number))
			(account2-id (account2 'identification-number))
			(serializer1 (account1 'serializer))
			(serializer2 (account2 'serializer))
		 )
		(cond
			((< account1-id account2-id)
				((serializer2 (serializer1 exchange)) account1 account2)
			)
			((> account1-id account2-id)
				((serializer1 (serializer2 exchange)) account1 account2)
			)
			(else
				(error "Two accounts cannot have the same identification number." account1-id)
			)
		)
	)
)

(define (exchange account1 account2)
	(let ((difference (- (account1 'balance) (account2 'balance))))

		(display-with-thread-id "A1 balance is" (A1 'balance))
		(display-with-thread-id "A2 balance is" (A2 'balance))

		((account1 'withdraw) difference)
		((account2 'deposit) difference)

		(display-with-thread-id "After the exchange:")
		(display-with-thread-id "A1 balance is" (A1 'balance))
		(display-with-thread-id "A2 balance is" (A2 'balance))
	)
)


(define (make-account-and-serializer balance identification-number)

	(define (withdraw amount)
		(if (>= balance amount)
			(begin
				(set! balance (- balance amount))
				balance
			)
			"Insufficient funds"
		)
	)
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance
	)

	(let ((balance-serializer (make-serializer)))
		(define (dispatch m)
			(cond
				((eq? m 'withdraw) withdraw)
				((eq? m 'deposit) deposit)
				((eq? m 'balance) balance)
				((eq? m 'serializer) balance-serializer)
				((eq? m 'identification-number) identification-number)
				(else
					(error "Unknown request -- MAKE-ACCOUNT" m)
				)
			)
		)
		dispatch
	)
)

(define	(display-with-thread-id . args)
	(display (current-thread))
	(display " ")
	(display args)
	(display "\n")
)

(define (make-serializer)
	(let ((mutex (make-mutex)))
		(lambda (p)
			(define (serialized-p . args)
				(mutex 'acquire)
				(display-with-thread-id "Acquired mutex" (mutex 'identifier))
				; This sleep to delay the execution so that we can simulate deadlock conditions
				(sleep-current-thread 1000)
				(let ((val (apply p args)))
					(display-with-thread-id "Finished executing procedure" p)
					(mutex 'release)
					(display-with-thread-id "Released mutex" (mutex 'identifier))
					val
				)
			)
			serialized-p
		)
	)
)

(define (make-mutex)
	(let ((cell (list false)) (identifier (random 100)))
		(define (the-mutex m)
			(cond
				((eq? m 'acquire)
					(if (test-and-set! cell)
						(the-mutex 'acquire) ; retry
					)
				)
				((eq? m 'release) (clear! cell))
				((eq? m 'identifier) identifier)
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

; Tests
(define (mult a b)

	(define (mult-internal p1 p2 count-down)
		(if (> count-down 0)
			(begin
				(sleep-current-thread 1000)
				(display-with-thread-id (* p1 p2))
				(mult-internal (- p1 1) (+ p2 1) (- count-down 1))
			)
		)
	)

	(mult-internal a b 10)
)

(define A1 (make-account-and-serializer 734 15))
(define A2 (make-account-and-serializer 1349 16))

(display "\n")
(display-with-thread-id "Test 1: Main thread runs serialized-exchange. There is no concurrency.")
(display-with-thread-id "----------------------------------------------------------------------")
(serialized-exchange A1 A2)

(define P1 (lambda () (serialized-exchange A1 A2)))
(define P2 (lambda () (serialized-exchange A2 A1)))

(display "\n")
(display-with-thread-id "Test 2: Two threads run serialized-exchange in parallel with opposite account orders.")
(display-with-thread-id "-------------------------------------------------------------------------------------")
(parallel-execute P1 P2)

(define P3 (lambda () (serialized-exchange-da A1 A2)))
(define P4 (lambda () (serialized-exchange-da A2 A1)))

(parallel-execute P3 P4)

; Test Results

BANL154931268:3.4 Concurrency - Time Is of the Essence vranganath$ scheme
MIT/GNU Scheme running under OS X
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2014 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even
for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Saturday May 17, 2014 at 2:39:25 AM
  Release 9.2 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118 || Edwin 3.116

1 ]=> (load "Exercise 3.48 deadlock avoidance.scm")

;Loading "Exercise 3.48 deadlock avoidance.scm"...
;  Loading "lib/parallel.scm"... done

#[thread 13] (Test 1: Main thread runs serialized-exchange. There is no concurrency.)
#[thread 13] (----------------------------------------------------------------------)
#[thread 13] (Acquired mutex 37)
#[thread 13] (Acquired mutex 87)
#[thread 13] (A1 balance is 734)
#[thread 13] (A2 balance is 1349)
#[thread 13] (After the exchange:)
#[thread 13] (A1 balance is 1349)
#[thread 13] (A2 balance is 734)
#[thread 13] (Finished executing procedure #[compound-procedure 14 exchange])
#[thread 13] (Released mutex 87)
#[thread 13] (Finished executing procedure #[compound-procedure 15 serialized-p])
#[thread 13] (Released mutex 37)

#[thread 13] (Test 2: Two threads run serialized-exchange in parallel with opposite account orders.)
#[thread 13] (-------------------------------------------------------------------------------------)
;... done
;Value 16: #[compound-procedure 16 terminator]

1 ]=> #[thread 17] (Acquired mutex 37)
#[thread 17] (Acquired mutex 87)
#[thread 17] (A1 balance is 1349)
#[thread 17] (A2 balance is 734)
#[thread 17] (After the exchange:)
#[thread 17] (A1 balance is 734)
#[thread 17] (A2 balance is 1349)
#[thread 17] (Finished executing procedure #[compound-procedure 14 exchange])
#[thread 17] (Released mutex 87)
#[thread 17] (Finished executing procedure #[compound-procedure 18 serialized-p])
#[thread 17] (Released mutex 37)
#[thread 19] (Acquired mutex 87)
#[thread 19] (Acquired mutex 37)
#[thread 19] (A1 balance is 734)
#[thread 19] (A2 balance is 1349)
#[thread 19] (After the exchange:)
#[thread 19] (A1 balance is 1349)
#[thread 19] (A2 balance is 734)
#[thread 19] (Finished executing procedure #[compound-procedure 14 exchange])
#[thread 19] (Released mutex 37)
#[thread 19] (Finished executing procedure #[compound-procedure 20 serialized-p])
#[thread 19] (Released mutex 87)


Test 3: Two threads run serialized-exchange in parallel with opposite account orders.
However, I have added a 1 second delay after the mutex is acquired so as to create a 
deadlock
-------------------------------------------------------------------------------------

;... done
;Value 14: #[compound-procedure 14 terminator]

1 ]=> #[thread 15] (Acquired mutex 9)
#[thread 16] (Acquired mutex 94)

See above: Threads 15 and 16 are deadlocked with each other. Thread 15 is not able to acquire mutex 94
and thread 16 is not able to acquire mutex 9.


Test 4: Two threads run serialized-exchange-da in parallel with opposite account orders.
I have added a 1 second delay after the mutex is acquired.
This new procedure avoids the deadlock using the technique described in the question.
Accounts are acquired in the same order. See results below.
-------------------------------------------------------------------------------------

;... done
;Value 13: #[compound-procedure 13 terminator]

1 ]=> #[thread 14] (Acquired mutex 74)
#[thread 14] (Acquired mutex 64)
#[thread 14] (A1 balance is 734)
#[thread 14] (A2 balance is 1349)
#[thread 14] (After the exchange:)
#[thread 14] (A1 balance is 1349)
#[thread 14] (A2 balance is 734)
#[thread 14] (Finished executing procedure #[compound-procedure 15 exchange])
#[thread 14] (Released mutex 64)
#[thread 14] (Finished executing procedure #[compound-procedure 16 serialized-p])
#[thread 14] (Released mutex 74)
#[thread 17] (Acquired mutex 74)
#[thread 17] (Acquired mutex 64)
#[thread 17] (A1 balance is 1349)
#[thread 17] (A2 balance is 734)
#[thread 17] (After the exchange:)
#[thread 17] (A1 balance is 734)
#[thread 17] (A2 balance is 1349)
#[thread 17] (Finished executing procedure #[compound-procedure 15 exchange])
#[thread 17] (Released mutex 64)
#[thread 17] (Finished executing procedure #[compound-procedure 18 serialized-p])
#[thread 17] (Released mutex 74)
