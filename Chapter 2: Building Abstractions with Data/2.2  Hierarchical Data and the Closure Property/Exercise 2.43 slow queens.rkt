#lang racket

; The new (inefficient) implementation is shown below

(define (queens board-size)

	; Note: The way we represent queen positions in this procedure is as follows:
	; (1 4 8 7) means a configuration in which
	; queen 1 is in the 1st row of the 1st column
	; queen 2 is in the 4th row of the 2nd column
	; queen 3 is in the 8th row of the 3rd column
	; queen 4 is in the 7th row of the 4th column
	; In this way of representing queen positions, the row position is explicit and
	; the column position is implicit. This changes the signature of some of the
	; procedures below. 

 	(define (queen-cols k)  
		(if (= k 0)
			; If k is 0, produce a list which contains a single element which is an empty list
			; This will be the starting point of the process of building up a list of lists
			; that contain valid queen positions

			; 'empty-board' is just an empty list
			(list (list))

			(filter
				(lambda (queen-positions) (safe? k queen-positions))
				(flatmap
					(lambda (new-row)
						(map
							(lambda (rest-of-queens)
								; Here I don't need to use the 'adjoin-position'
								; procedure because all I need to do is append the
								; position of the next queen to the existing list
								; 'rest-of-queens' is an existing position of queens
								; 'new-row' is the position of a new queen
								(append rest-of-queens (list new-row))
							)
							(queen-cols (- k 1))
						)
					)
					(enumerate-interval 1 board-size)
				)
			)
		)
	)
	(queen-cols board-size)
)

(define (safe? k queen-positions)
	; Tests whether the queen in the kth column in this configuration is safe
	; with respect to all the other queens
	; Example: (2 5 8 4 1)

	; The process here is to 'car' through the positions one by one and test.
	; So safe-internal? needs to know which column of the board the first element
	; of the list is in

	(define queen-column k)

	; The following procedure evaluates the position of a queen in a given column
	(define (position-of-queen-in-column x)
		(define (position-of-queen-in-column-internal positions count)
			(if (= count 1)
				(car positions)
				(position-of-queen-in-column-internal (cdr positions) (- count 1))
			)
		)
		(position-of-queen-in-column-internal queen-positions x)
	)

	(define queen-row (position-of-queen-in-column k))

	(define (safe-internal? starting-column positions)
		(cond
			((null? positions) true)
			(else
				(if (check? starting-column (car positions) queen-column queen-row)
					false
					(safe-internal? (+ starting-column 1) (cdr positions))
				)
			)
		)
	)

	(define (check? q1-column q1-row q2-column q2-row)
		; The way we test is:
		; 1. The two queens should not be in the same row
		; 2. The two queens should not be in the same column. (This will never happen because of
		;		the way this program works. But is included here just for completeness.)
		; 3. The two queens should not be in the same diagonal
		(cond
			; if both queens are the same then it it not a check
			((and (= q1-row q2-row) (= q1-column q2-column)) false)
			; row and column check
			((or (= q1-row q2-row) (= q1-column q2-column)) true)
			; diagonal check
			((= (abs (- q1-column q2-column)) (abs (- q1-row q2-row))) true)
			(else
				false
			)
		)
	)

	(safe-internal? 1 queen-positions)
)

(define (flatmap proc seq)
	(accumulate append null (map proc seq))
)

(define (enumerate-interval low high)
	(if (> low high)
		null
		(cons low (enumerate-interval (+ low 1) high))
	)
)

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence) (accumulate op initial (cdr sequence)))
	)
)

; Explanation
; ===========
; In this procedure, the 'lambda (new-row)' function is applied to every element in the sequence
; produced by (enumerate-interval 1 board-size). But note that in the implementation of this
; lambda function (queen-cols (- k 1)) is evaluated. Therefore, for a given 'k' queen-cols
; is evaluated 'board-size' times. Now, since k itself ranges from 0 to board-size, queen-cols
; is evaluated a total of 'board-size raised to the power (board-size + 1)'.
;
; For a 2x2 board, it will be 8 evaluations
; For a 3x3 board, it will be 81 evaluations
; For a 4x4 board, it will be 1024 evaluations
; For a 5x5 board, it will be 15625 evaluations
; For a 6x6 board, it will be 279936 evaluations
; For a 7x7 board, it will be 5764801 evaluations
; For a 8x8 board, it will be 134217728 evaluations

; That is why the program runs more slowly than the version in the previous exercise. 

; For the eight-queen puzzle, the implementation in the previous exercise calls queen-cols
; 9 times (since k ranges from 8 to 0)
; Therefore, assuming that the program in exercise 2.42 solves the puzzle in time T
; it will take Louis's program (134217728/9)T = 14913081T to solve the eight-queens puzzle,

; In my testing, the difference between the two implementations first became noticeable
; for a board size of 7x7 where the slower implementation took about 1.2 seconds of CPU time.
; For 8x8 it took about 22 seconds of CPU time. For 9x9 it took 793 seconds of CPU time
; (about 13.2 minutes). For 10x10 it took about 242 minutes of CPU time (about 4 hours).

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 128 MB.
> (queens 0)
'(())
> (time (queens 0))
cpu time: 0 real time: 0 gc time: 0
'(())
> (time (queens 1))
cpu time: 1 real time: 0 gc time: 0
'((1))
> (time (queens 2))
cpu time: 0 real time: 0 gc time: 0
'()
> (time (queens 3))
cpu time: 0 real time: 0 gc time: 0
'()
> (time (queens 4))
cpu time: 0 real time: 0 gc time: 0
'((3 1 4 2) (2 4 1 3))
> (time (queens 5))
cpu time: 3 real time: 3 gc time: 0
'((4 2 5 3 1)
  (3 5 2 4 1)
  (5 3 1 4 2)
  (4 1 3 5 2)
  (5 2 4 1 3)
  (1 4 2 5 3)
  (2 5 3 1 4)
  (1 3 5 2 4)
  (3 1 4 2 5)
  (2 4 1 3 5))
> (time (queens 6))
cpu time: 75 real time: 83 gc time: 24
'((5 3 1 6 4 2) (4 1 5 2 6 3) (3 6 2 5 1 4) (2 4 6 1 3 5))
> (time (queens 7))
cpu time: 1191 real time: 1262 gc time: 258
'((6 4 2 7 5 3 1)
  (5 2 6 3 7 4 1)
  (4 7 3 6 2 5 1)
  (3 5 7 2 4 6 1)
  (6 3 5 7 1 4 2)
  (7 5 3 1 6 4 2)
  (6 3 7 4 1 5 2)
  (6 4 7 1 3 5 2)
  (6 3 1 4 7 5 2)
  (5 1 4 7 3 6 2)
  (4 6 1 3 5 7 2)
  (4 7 5 2 6 1 3)
  (5 7 2 4 6 1 3)
  (1 6 4 2 7 5 3)
  (7 4 1 5 2 6 3)
  (5 1 6 4 2 7 3)
  (6 2 5 1 4 7 3)
  (5 7 2 6 3 1 4)
  (7 3 6 2 5 1 4)
  (6 1 3 5 7 2 4)
  (2 7 5 3 1 6 4)
  (1 5 2 6 3 7 4)
  (3 1 6 2 5 7 4)
  (2 6 3 7 4 1 5)
  (3 7 2 4 6 1 5)
  (1 4 7 3 6 2 5)
  (7 2 4 6 1 3 5)
  (3 1 6 4 2 7 5)
  (4 1 3 6 2 7 5)
  (4 2 7 5 3 1 6)
  (3 7 4 1 5 2 6)
  (2 5 7 4 1 3 6)
  (2 4 1 7 5 3 6)
  (2 5 1 4 7 3 6)
  (1 3 5 7 2 4 6)
  (2 5 3 1 7 4 6)
  (5 3 1 6 4 2 7)
  (4 1 5 2 6 3 7)
  (3 6 2 5 1 4 7)
  (2 4 6 1 3 5 7))
> (time (queens 8))
cpu time: 22221 real time: 22474 gc time: 3034
'((4 2 7 3 6 8 5 1)
  (5 2 4 7 3 8 6 1)
  (3 5 2 8 6 4 7 1)
  (3 6 4 2 8 5 7 1)
  (5 7 1 3 8 6 4 2)
  (4 6 8 3 1 7 5 2)
  (3 6 8 1 4 7 5 2)
  (5 3 8 4 7 1 6 2)
  (5 7 4 1 3 8 6 2)
  (4 1 5 8 6 3 7 2)
  (3 6 4 1 8 5 7 2)
  (4 7 5 3 1 6 8 2)
  (6 4 2 8 5 7 1 3)
  (6 4 7 1 8 2 5 3)
  (1 7 4 6 8 2 5 3)
  (6 8 2 4 1 7 5 3)
  (6 2 7 1 4 8 5 3)
  (4 7 1 8 5 2 6 3)
  (5 8 4 1 7 2 6 3)
  (4 8 1 5 7 2 6 3)
  (2 7 5 8 1 4 6 3)
  (1 7 5 8 2 4 6 3)
  (2 5 7 4 1 8 6 3)
  (4 2 7 5 1 8 6 3)
  (5 7 1 4 2 8 6 3)
  (6 4 1 5 8 2 7 3)
  (5 1 4 6 8 2 7 3)
  (5 2 6 1 7 4 8 3)
  (6 3 7 2 8 5 1 4)
  (2 7 3 6 8 5 1 4)
  (7 3 1 6 8 5 2 4)
  (5 1 8 6 3 7 2 4)
  (1 5 8 6 3 7 2 4)
  (3 6 8 1 5 7 2 4)
  (6 3 1 7 5 8 2 4)
  (7 5 3 1 6 8 2 4)
  (7 3 8 2 5 1 6 4)
  (5 3 1 7 2 8 6 4)
  (2 5 7 1 3 8 6 4)
  (3 6 2 5 8 1 7 4)
  (6 1 5 2 8 3 7 4)
  (8 3 1 6 2 5 7 4)
  (2 8 6 1 3 5 7 4)
  (5 7 2 6 3 1 8 4)
  (3 6 2 7 5 1 8 4)
  (6 2 7 1 3 5 8 4)
  (3 7 2 8 6 4 1 5)
  (6 3 7 2 4 8 1 5)
  (4 2 7 3 6 8 1 5)
  (7 1 3 8 6 4 2 5)
  (1 6 8 3 7 4 2 5)
  (3 8 4 7 1 6 2 5)
  (6 3 7 4 1 8 2 5)
  (7 4 2 8 6 1 3 5)
  (4 6 8 2 7 1 3 5)
  (2 6 1 7 4 8 3 5)
  (2 4 6 8 3 1 7 5)
  (3 6 8 2 4 1 7 5)
  (6 3 1 8 4 2 7 5)
  (8 4 1 3 6 2 7 5)
  (4 8 1 3 6 2 7 5)
  (2 6 8 3 1 4 7 5)
  (7 2 6 3 1 4 8 5)
  (3 6 2 7 1 4 8 5)
  (4 7 3 8 2 5 1 6)
  (4 8 5 3 1 7 2 6)
  (3 5 8 4 1 7 2 6)
  (4 2 8 5 7 1 3 6)
  (5 7 2 4 8 1 3 6)
  (7 4 2 5 8 1 3 6)
  (8 2 4 1 7 5 3 6)
  (7 2 4 1 8 5 3 6)
  (5 1 8 4 2 7 3 6)
  (4 1 5 8 2 7 3 6)
  (5 2 8 1 4 7 3 6)
  (3 7 2 8 5 1 4 6)
  (3 1 7 5 8 2 4 6)
  (8 2 5 3 1 7 4 6)
  (3 5 2 8 1 7 4 6)
  (3 5 7 1 4 2 8 6)
  (5 2 4 6 8 3 1 7)
  (6 3 5 8 1 4 2 7)
  (5 8 4 1 3 6 2 7)
  (4 2 5 8 6 1 3 7)
  (4 6 1 5 2 8 3 7)
  (6 3 1 8 5 2 4 7)
  (5 3 1 6 8 2 4 7)
  (4 2 8 6 1 3 5 7)
  (6 3 5 7 1 4 2 8)
  (6 4 7 1 3 5 2 8)
  (4 7 5 2 6 1 3 8)
  (5 7 2 6 3 1 4 8))
> (time (queens 9))
cpu time: 792775 real time: 805490 gc time: 75777
<output removed>
> (time (queens 10))
cpu time: 14528912 real time: 136641051 gc time: 1830508
<output removed>
