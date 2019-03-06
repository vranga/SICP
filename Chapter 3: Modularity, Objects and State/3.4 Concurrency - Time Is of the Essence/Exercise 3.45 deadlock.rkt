#lang racket

; Exercise 3.45.  Louis Reasoner thinks our bank-account system is unnecessarily complex and error-prone
; now that deposits and withdrawals aren't automatically serialized. He suggests that
; make-account-and-serializer should have exported the serializer (for use by such procedures as
; serialized-exchange) in addition to (rather than instead of) using it to serialize accounts and
; deposits as make-account did. He proposes to redefine accounts as follows:

; (define (make-account-and-serializer balance)
;   (define (withdraw amount)
;     (if (>= balance amount)
;         (begin (set! balance (- balance amount))
;                balance)
;         "Insufficient funds"))
;   (define (deposit amount)
;     (set! balance (+ balance amount))
;     balance)
;   (let ((balance-serializer (make-serializer)))
;     (define (dispatch m)
;       (cond ((eq? m 'withdraw) (balance-serializer withdraw))
;             ((eq? m 'deposit) (balance-serializer deposit))
;             ((eq? m 'balance) balance)
;             ((eq? m 'serializer) balance-serializer)
;             (else (error "Unknown request -- MAKE-ACCOUNT"
;                          m))))
;     dispatch))

; Then deposits are handled as with the original make-account:

; (define (deposit account amount)
;  ((account 'deposit) amount))

; Explain what is wrong with Louis's reasoning. In particular, consider what happens when
; serialized-exchange is called.

; S O L U T I O N

This implementation will cause a deadlock when "serialized-exchange" is called. When
"serialized-exchange" runs, it first acquires the serializers of both accounts. Then it uses both
these serializers to serialize the "exchange" procedure. This means that when "exchange" is executing,
none of the deposit and withdraw procedures inside the two account objects can start
executing. But "exchange" itself calls "withdraw" first and then "deposit". The call to "withdraw"
will block since "withdraw" will wait for "exchange" to finish.

So "exchange" and "withdraw" will block on each other and the program will hang.

By exporting the serializer (for use by external procedures) and also using it to serialize deposits
and withdrawals internally, Louis Reasoner opens up the risk of new procedure definitions that contain
deadlock situations as in "serialized-exchange".
