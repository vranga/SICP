#lang racket

; Exercise 3.43.  Suppose that the balances in three accounts start out as $10, $20, and $30, and that
; multiple processes run, exchanging the balances in the accounts. Argue that if the processes are run
; sequentially, after any number of concurrent exchanges, the account balances should be $10, $20, and $30
; in some order. Draw a timing diagram like the one in figure 3.29 to show how this condition can be
; violated if the exchanges are implemented using the first version of the account-exchange program in
; this section. On the other hand, argue that even with this exchange program, the sum of the balances
; in the accounts will be preserved. Draw a timing diagram to show how even this condition would be
; violated if we did not serialize the transactions on individual accounts.

; S O L U T I O N

Part 1: Suppose that the balances start out as $10, $20 and $30 and the processes run sequentially
and they execute the first version (i.e. the non-serialized version) of the exchange procedure
--------------------------------------------------------------------------------------------------

As time progresses, the balances could change like this:

A		B		C
-		-		-
10		20		30
20		10		30
30		10		20
30		20		10
20		30		10
10		30		20
etc.

Since the processes are run sequentially, at any time at most one process will be executing 'exchange'.
And this will result in the swapping of two of the three balances in the columns above. There is no
interleaving of processes and the third balance is untouched. The above happens with each run of
'exchange'. Each run of exchange will/may swap a different combination of accounts: A & B, B & C or C & A.

So after any number of sequential exchanges, the account balances will be $10, $20 and $30 in some order.

Part 2: Balances start out as $10, $20 and $30 and the processes run concurrently and they execute the
first version (i.e. the non-serialized version) of the exchange procedure
------------------------------------------------------------------------------------------------------

See timing diagram included with this solution. The balances becomes $20 each

Part 3: Explain how even with this exchange program, the sum of the balances in the accounts will be
preserved.
---------------------------------------------------------------------------------------------------

First of all, note that all transactions on an individual account are serialized. This means that 
the integrity of each account is preserved regardless of how many concurrent exchange transactions
happen.

Secondly, every deposit to one account is matched by a withdrawal from another account. So if the
three acccounts are considered together, the sum of the balances of the three accounts remains the
same because money is just being moved around within these accounts. No new money is being added,
nor is any money being taken away from these three accounts. With any amount of interleaving of
an arbitrary number of processes executing 'exchange', the money just bounces around within the
three accounts. So the total sum is preserved.

Look at the timing diagram shown for Part 2. The account balances become $20 each in this case and 
they add up to $60 like before. Peter tries to swap Acc1 with Acc2 and intends that 10-20 becomes
20-10. But in the middle of this, Paul swaps Acc2 and Acc3 so $10 gets added to Acc2. Since each account is
serialized, Peter has to wait until Paul finishes setting Acc2 to $30. Then Peter subtracts $10 from $30
(not $20) and so the accounts Acc1 and Acc2 become 20-20 instead of 20-10. The extra $10 comes from
Acc3 which now has $20.

Part 4: Draw a timing diagram to show how even this condition would be violated if we did not
serialize the transactions on individual accounts.
---------------------------------------------------------------------------------------------

See timing diagram included with this solution
