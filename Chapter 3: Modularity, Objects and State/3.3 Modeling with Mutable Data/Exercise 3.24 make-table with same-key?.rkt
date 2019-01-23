#lang racket

; Exercise 3.24.  In the table implementations above, the keys are tested for equality using equal?
; (called by assoc). This is not always the appropriate test. For instance, we might have a table with
; numeric keys in which we don't need an exact match to the number we're looking up, but only a number
; within some tolerance of it. Design a table constructor make-table that takes as an argument a same-key?
; procedure that will be used to test "equality" of keys. make-table should return a dispatch procedure
; that can be used to access appropriate lookup and insert! procedures for a local table.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (make-table same-key?)
	(let ((local-table (mlist '*table*)))

		(define (lookup key-1 key-2)
			(let ((subtable (assoc same-key? key-1 (mcdr local-table))))
				(if subtable
					(let ((record (assoc same-key? key-2 (mcdr subtable))))
						(if record
							(mcdr record)
							false
						)
					)
					false
				)
			)
		)

		(define (insert! key-1 key-2 value)
			(let ((subtable (assoc same-key? key-1 (mcdr local-table))))
				(if subtable
					(let ((record (assoc same-key? key-2 (mcdr subtable))))
						(if record
							(set-cdr! record value)
							(set-cdr!
								subtable
								(mcons (mcons key-2 value) (mcdr subtable))
							)
						)
					)
					(set-cdr!
						local-table
						(mcons (mlist key-1 (mcons key-2 value)) (mcdr local-table))
					)
				)
			)
			'ok
		)

		(define (print-table)

			(define (print-table-internal table)
				(cond
					((null? table) (void))
					((not (mpair? table)) (display "Table element not a pair"))	
					(else
						(begin
							(print-row (mcar table))
							(print-table-internal (mcdr table))
						)
					)
				)
			)

			(define (print-row row)

				(define (print-row-internal r)
					(cond
						((null? r) (void))	
						((not (mpair? r)) (display "Table element not a pair"))	
						(else
							(print-field (mcar r))
							(print-row-internal (mcdr r))
						)
					)
				)

				(display (mcar row))
				(display ":")
				(newline)
				(print-row-internal (mcdr row))
			)

			(define (print-field f)
				(cond
					((null? f) (void))
					((not (mpair? f)) (display "Table element not a pair"))	
					(else
						(display "	")
						(display (mcar f))
						(display ":	")
						(display (mcdr f))
						(newline)
					)
				)
			)

			(display "TABLE")
			(newline)
			(display "-----")
			(newline)
			(print-table-internal (mcdr local-table))
		)

		(define (dispatch m)
			(cond
				((eq? m 'lookup-proc) lookup)
				((eq? m 'insert-proc!) insert!)
				((eq? m 'print) print-table)
				(else (error "Unknown operation -- TABLE" m))
			)
		)

		dispatch
	)
)

(define (assoc same-key-proc key records)
	(cond
		((null? records) false)
		((same-key-proc key (mcar (mcar records))) (mcar records))
		(else (assoc same-key-proc key (mcdr records)))
	)
)

; Test Driver

(define (run-test return-type proc . args)

	(define (print-item-list items first-time?)
		(cond
			((not (pair? items)) (void))
			(else
				(if (not first-time?)
					(display ", ")
					(void)
				)
				(print (car items))
				(print-item-list (cdr items) false)
			)
		)
	)

	(display "Applying ")
	(display proc)
	(display " on: ")
	(print-item-list args true)
	(newline)
	(let ((result (apply proc args)))
		(if (not (eq? return-type 'none))
			(display "Result: ")
			(void)
		)
		(cond
			((procedure? result) ((result 'print)))
			; ((eq? return-type 'deque) (print-deque result))
			((eq? return-type 'none) (void))
			(else
				(print result)
				(newline)
			)
		)
	)
	(newline)
)

(define (execution-time proc . args)
	(define start-time (current-milliseconds))
	; (display start-time)
	; (display " ")
	(apply proc args)
	(define end-time (current-milliseconds))
	; (display end-time) 
	(display "Execution time of ")
	(display proc)
	(display ": ")
	(- end-time start-time)
)

; Tests

(define (same? a b)
	(equal? a b)
)

(define T1 (make-table same?))
((T1 'insert-proc!) 'math '+ 43)
((T1 'print))
((T1 'insert-proc!) 'letters 'b 98)
((T1 'print))
((T1 'insert-proc!) 'math '* 42)
((T1 'print))
((T1 'insert-proc!) 'letters 'a 96)
((T1 'print))
((T1 'insert-proc!) 'math '- 45)
((T1 'print))

(define T2 (make-table same?))
((T2 'insert-proc!) 'punctuation "," 32)
((T2 'print))
((T2 'insert-proc!) 'punctuation "!" 33)
((T2 'print))
((T2 'insert-proc!) 'punctuation ";" 42)

(display "T2 is: ")
(newline)
((T2 'print))

(display "T1 is: ")
(newline)
((T1 'print))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
'ok
TABLE
-----
math:
	+:	43
'ok
TABLE
-----
letters:
	b:	98
math:
	+:	43
'ok
TABLE
-----
letters:
	b:	98
math:
	*:	42
	+:	43
'ok
TABLE
-----
letters:
	a:	96
	b:	98
math:
	*:	42
	+:	43
'ok
TABLE
-----
letters:
	a:	96
	b:	98
math:
	-:	45
	*:	42
	+:	43
'ok
TABLE
-----
punctuation:
	,:	32
'ok
TABLE
-----
punctuation:
	!:	33
	,:	32
'ok
T2 is: 
TABLE
-----
punctuation:
	;:	42
	!:	33
	,:	32
T1 is: 
TABLE
-----
letters:
	a:	96
	b:	98
math:
	-:	45
	*:	42
	+:	43
> 
