#lang racket

; Exercise 3.44.  Consider the problem of transferring an amount from one account to another.
; Ben Bitdiddle claims that this can be accomplished with the following procedure, even if there are
; multiple people concurrently transferring money among multiple accounts, using any account mechanism
; that serializes deposit and withdrawal transactions, for example, the version of make-account in the
; text above.

; (define (transfer from-account to-account amount)
;   ((from-account 'withdraw) amount)
;   ((to-account 'deposit) amount))

; Louis Reasoner claims that there is a problem here, and that we need to use a more sophisticated method,
; such as the one required for dealing with the exchange problem. Is Louis right? If not, what is the
; essential difference between the transfer problem and the exchange problem?
; (You should assume that the balance in from-account is at least amount.)

; S O L U T I O N

Louis Reasoner is wrong. Ben Bitdiddle is right: Even if there are multiple people concurrently
transferring money among multiple accounts, using any account mechanism that serializes deposit and
withdrawal transactions, there will be no problem with the 'transfer' procedure above.

Since every account object serializes deposit and withdrawal transactions within itself, 'transfer' does a
*safe* withdraw from one account and does a *safe* deposit into another (or even the same) account.
Money removed from one account is faithfully added to another thereby fulfilling the semantics of a
transfer. It does not matter how else these accounts were modified by other concurrent processes during
the execution of this transfer.

'exchange' on the other hand requires a more sophisticated implementation because the semantics of an
exchange require the two account balances to not be tampered with by any other concurrent processes
during the execution of this process. So 'exchange' requires serialization of the procedures across both
the account objects. For example, for a 10-20 to become a 20-10, both account balances should remain 
unaffected by any other concurrent processes. See previous exercise solution for an example of how
'exchange' can behave strangely when its non-serialized implementation is used.
