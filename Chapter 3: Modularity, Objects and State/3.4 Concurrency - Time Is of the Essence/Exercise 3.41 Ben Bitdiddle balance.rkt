#lang racket

; Exercise 3.41.  Ben Bitdiddle worries that it would be better to implement the bank account as
; follows (where the commented line has been changed):

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
;     (define (dispatch m)
;       (cond ((eq? m 'withdraw) (protected withdraw))
;             ((eq? m 'deposit) (protected deposit))
;             ((eq? m 'balance)
;              ((protected (lambda () balance)))) ; serialized
;             (else (error "Unknown request -- MAKE-ACCOUNT"
;                          m))))
;     dispatch))

; because allowing unserialized access to the bank balance can result in anomalous behavior.
; Do you agree? Is there any scenario that demonstrates Ben's concern?

; S O L U T I O N
;
Yes, Ben's concern is valid in the case where the write to 'balance' is not atomic. In such a case,
the balance may be in a partially updated state, i.e. corrupt when it is accessed. By serializing 
access to balance we ensure that if a process tries to access balance, it will have to wait if
another process is in the middle of changing it and once the accessing process starts reading the
balance, other threads will have to wait until the access is complete.

