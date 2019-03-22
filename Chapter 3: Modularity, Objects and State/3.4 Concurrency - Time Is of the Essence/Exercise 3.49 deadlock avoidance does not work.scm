; Exercise 3.49.  Give a scenario where the deadlock-avoidance mechanism described above does not work.
; (Hint: In the exchange problem, each process knows in advance which accounts it will need to get access to.
; Consider a situation where a process must get access to some shared resources before it can know which
; additional shared resources it will require.)

; S O L U T I O N

; Consider the case where we have two bank branches in different locations and an account can be accessed
; at each branch. For fast local access, we allow withdrawals and deposits at each location and design
; a way to keep the account balance at both locations synchronized. Concurrent processes can access and change
; the balance in either location. In order to keep the balance the same in both locations, we write a procedure
; similar to 'synchronized-exchange' but which first updates the local balance and then the remote balance. This
; procedure acquires a lock on the local account first and then the remote account. This 'sychronized-update'
; procedure can encounter a deadlock if it runs simultaneously in two locations on the same account.
; 
; Our deadlock avoidance mechanism will fail here because we do not know in advance, in what order to acquire
; locks on the accounts.
