#lang racket

; Exercise 3.42.  Ben Bitdiddle suggests that it's a waste of time to create a new serialized procedure
; in response to every withdraw and deposit message. He says that make-account could be changed so that
; the calls to protected are done outside the dispatch procedure. That is, an account would return the
; same serialized procedure (which was created at the same time as the account) each time it is asked
; for a withdrawal procedure.

; (define (make-account balance)
;   (define (withdraw amount)
;     (if (>= balance amount)
;         (begin (set! balance (- balance amount))
;                balance)
;         "Insufficient funds"))
;   (define (deposit amount)
;     (set! balance (+ balance amount))
;     balance)
;   (let ((protected (make-serializer)))
;     (let ((protected-withdraw (protected withdraw))
;           (protected-deposit (protected deposit)))
;       (define (dispatch m)
;         (cond ((eq? m 'withdraw) protected-withdraw)
;               ((eq? m 'deposit) protected-deposit)
;               ((eq? m 'balance) balance)
;               (else (error "Unknown request -- MAKE-ACCOUNT"
;                            m))))
;       dispatch)))

; Is this a safe change to make? In particular, is there any difference in what concurrency is allowed
; by these two versions of make-account ?

; S O L U T I O N

Quoting from section 3.4.2 of the SICP text: "serialization creates distinguished sets of procedures
such that only one execution of a procedure in each serialized set is permitted to happen at a time.
If some procedure in the set is being executed, then a process that attempts to execute any procedure
in the set will be forced to wait until the first execution has finished."

Note the last part of the quoted text above: A process that attempts to execute *any* procedure in the
set will be forced to wait. This includes the procedure that is currently executing. Therefore, it is
safe to make this change. The same concurrency is allowed by this version and the previous version
of make-account. There is no difference.

If the withdrawal procedure is being executed by one process, and another process tries to execute it,
it will be forced to wait. So two executions of the withdrawal procedure will not get interleaved.
