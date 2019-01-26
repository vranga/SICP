#lang racket

; Exercise 3.25.  Generalizing one- and two-dimensional tables, show how to implement a table in which
; values are stored under an arbitrary number of keys and different values may be stored under different
; numbers of keys. The lookup and insert! procedures should take as input a list of keys used to access
; the table.

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; Supported table structure is something like:

; TABLE
; -----
; K1: val1
; K2: val2
; K3: val3
; 	K31: val31
; 	K32: (no value)
; 		K321: val321
; 		K322: val322
; 	K33: val33
; K4: val4
; 
; So the key combinations to retrieve data would be:
; 
; K1 retrieves val1
; K2 retrieves val2
; K3 retrieves val3
; K3, K31 retrieves val31
; K3, K32, K321 retrieves val321
; K3, K32, K322 retrieves val322
; K3, K33 retrieves val33
; K4 retrieves val4

; Note: I want to allow for the possibility for not just primitives like string literals and numbers
; but objects to be inserted and looked up from the table. Since every non-primitive object is 
; implemented using pairs, this makes it necessary for us to be able to distinguish between a sub-table
; and a value which is an object. So I am introducing tags so that the table procedures can correctly
; identify the data they handle as they traverse through the table. So a 'key' will not be just a
; string literal but a pair in which the 'car' is a tag with the value 'key and the 'cdr' is the actual key
; Similarly, a 'value' will not just be a string literal but a pair in which the 'car' is a tag with the
; value 'value and the 'cdr' is the actual value which could be a primitive or a complex object

(define (make-table same-key?)

	(define (make-generic-table)
		; makes a generic table with its subtable set to null
		(mlist '*generic-table*)
	)

	(let ((local-table (make-generic-table)))

		(define (get-records table)
			(if (mpair? table)
				(mcdr table)
				null
			)
		)
		(define (get-first-key-value-pair records)
			(if (mpair? records)
				(mcar records)
				null
			)
		)
		(define (get-remaining-key-value-pairs records)
			(if (mpair? records)
				(mcdr records)
				null
			)
		)
		(define (insert-at-top-level! table key-value-pair)
			(set-mcdr! table (mcons key-value-pair (get-records table)))
		)
	
		(define (find-key same-key-proc key-to-find records)
			(cond
				((null? records) false)
				; check if the first key-value pair matches
				((same-key-proc key-to-find (get-key (get-first-key-value-pair records)))
					(get-first-key-value-pair records)
				)
				(else
					; continue looking at this level in the table
					(find-key same-key-proc key-to-find (get-remaining-key-value-pairs records))
				)
			)
		)

		(define (lookup same-key-proc list-of-keys)

			(define (lookup-internal key-list table)
				(cond
					((null? key-list) false)
					((not (pair? key-list)) false)
					(else
						(let ((key-value-pair (find-key same-key-proc (car key-list) (get-records table))))
							(if key-value-pair
								; Found key
								; If this is the last key in the list of keys, then we have found the
								; data we are looking for in the table.
								(if (null? (cdr key-list))
									; we have reached the last key, so return the value if there is one
									(if (not (null? (get-value key-value-pair)))
										(get-value key-value-pair)
										false
									)
									; we have to look for more keys
									(lookup-internal (cdr key-list) (get-subtable key-value-pair))
								)
								; Could not find key, so stop looking and return false
								false
							)
						)
					)
				)
			)

			(lookup-internal list-of-keys local-table)	
		)

		(define (insert! key-list value)
			; Logic: Look for the first key from the key-list in the first level of the table
			; If found, then look for the 2nd key in the sub-table pointed to by the first key
			; and continue this process. If all the keys are found, then the insert operation will 
			; replace the existing value with the new value.
			; If at any level the key is not found, then insert that key at that level in the table and 
			; continue inserting keys below it and finally the value

			(define (insert-internal! list-of-keys value table)
				(let ((key-value-pair (find-key same-key? (car list-of-keys) (get-records table))))
					(if key-value-pair
						; found the key
						; if this is the last key in the list of keys, then we have found the
						; correct location in the table. So replace the old value with the new value
						(if (null? (cdr list-of-keys))
							; we are at the last key, so replace the old value with the new value
							(set-value! key-value-pair (make-value value))
							; there are more keys so continue
							(begin
								; If there is no subtable inside this key-value pair, create it
								(if (null? (get-subtable key-value-pair))
									(set-subtable! key-value-pair (make-generic-table))
									(void)
								)
								(insert-internal! (cdr list-of-keys) value (get-subtable key-value-pair))
							)
						)
						; did not find the key so we need to insert key(s) and the value TODO
						(insert-new-keys-and-value list-of-keys value table)
					)
				)
			)

			; This procedure is for inserting a new set of hierarchical keys in the supplied
			; set of records
			(define (insert-new-keys-and-value list-of-keys value table)
				(cond
					((not (pair? list-of-keys)) (error "Not a list" list-of-keys))
					; if this is the last key then we insert the value also and stop
					((null? (cdr list-of-keys))
						(let ((new-key-value-pair (make-key-value-pair (car list-of-keys) null value)))
							(insert-at-top-level! table new-key-value-pair)
						)
					)
					(else
						; the tricky part						
						(let ((new-key-value-pair (make-key-value-pair (car list-of-keys) null null)))
							(let ((subtable (make-generic-table)))
								(insert-new-keys-and-value (cdr list-of-keys) value subtable)
								(set-subtable! new-key-value-pair subtable)
							)
							(insert-at-top-level! table new-key-value-pair)
						)
					)
			 	)
			)

			(if (pair? key-list)
				(begin
					(insert-internal! key-list value local-table)
					'ok
				)
				(error "Supplied parameter is not a pair! -- INSERT!" key-list)
			)
		)

		(define (print-table)

			(define (print-table-internal records indentation-level)
				(cond
					((null? records) (void))
					((not (mpair? records)) (display "Supplied records not a pair -- PRINT-TABLE"))
					(else
						(let ((first-record (mcar records)) (remaining-records (mcdr records)))
							(indent indentation-level)
							(print-key-and-value first-record)
							(print-table-internal (get-records (get-subtable first-record)) (+ indentation-level 1))
							(print-table-internal remaining-records indentation-level)
						)
					)
				)
			)

			(define (indent indentation-level)
				(if (= indentation-level 0)
					(void)
					(begin
						(display "  ")
						(indent (- indentation-level 1))
					)
				)
			)

			(define (print-key-and-value key-value-pair)
				(display (get-key key-value-pair))
				(display ": ")
				(if (not (null? (get-value key-value-pair)))
					(display (get-value key-value-pair))
					(void)
				)
				(newline)
			)

			(display "GENERIC-TABLE")
			(newline)
			(display "-------------")
			(newline)
			(print-table-internal (get-records local-table) 0)
		)

		(define (make-key-value-pair key subtable value)
			; A key-value pair is the basic building block of the table. It is a pair in which
			; the 'car' is the key and the 'cdr' is another pair in which the 'car' is a pointer to
			; a generic table and the 'cdr' is the value. This structure allows us to create an
			; arbitrary number of levels in the table. If it is just a simple key-value pair with no
			; sub-keys, then the contained generic table will be null. If it is a key with sub-keys
			; but no value for this key, then the contained generic table will be non-null and the
			; value will be null. And of course, both the contained generic table and the value
			; can be non-null to allow for data like:
			; K3, K31:			val31
			; K3, K32, K321:	val321
			(mcons (make-key key) (mcons subtable (make-value value)))
		)
		(define (get-key key-value-pair)
			(if (mpair? key-value-pair)
				(mcdr (mcar key-value-pair))
				null
			)
		)
		(define (get-value key-value-pair)
			(mcdr (mcdr (mcdr key-value-pair)))
		)
		(define (get-subtable key-value-pair)
			(mcar (mcdr key-value-pair))
		)
		(define (set-subtable! key-value-pair subtable)
			(set-mcar! (mcdr key-value-pair) subtable)
		)
		(define (set-value! key-value-pair x)
			(set-mcdr! (mcdr key-value-pair) x)
		)

		(define (make-key key) (mcons 'key key))
		(define (make-value value) (mcons 'value value))

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
	(if (not (null? args))
		(begin
			(display " on: ")
			(print-item-list args true)
		)
		(void)
	)
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

(define (same? a b)
	(equal? a b)
)

; Tests

(define T1 (make-table same?))
(run-test 'unknown (T1 'insert-proc!) (list 'Afghanistan) 'AF)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Kazhakhistan) 'KZ)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Turkmenistan) 'TM)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Kyrgiztan) 'KZ)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'India 'Karnataka) 'KA)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'India) 'IN)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'India 'TamilNadu) 'TU)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Pakistan) 'PK)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Pakistan 'Sindh) 'SD)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Pakistan 'Sindh) 'SN)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'India 'TamilNadu) 'TN)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'India 'TamilNadu 'Chennai) 'CHEN)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Pakistan 'Sindh 'Rawalpindi) 'RAWA)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'India 'TamilNadu 'Chennai 'BesantNagar 'ElliotsBeach) 'ELLIOTS)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Pakistan) 'PoK)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Uzbekistan) 'UB)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list 'Tajikistan) 'TJ)
(run-test 'none (T1 'print))

(display "Testing the lookup proc")
(newline)

(run-test 'unknown (T1 'lookup-proc) same? (list 'Tajikistan))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Uzbekistan))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Burma))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Pakistan))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Sindh))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Pakistan 'Sindh))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Pakistan 'Sindh 'Rawalpindi))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Pakistan 'Sind 'Rawalpindi))
(run-test 'unknown (T1 'lookup-proc) same? (list 'India))
(run-test 'unknown (T1 'lookup-proc) same? (list 'TamilNadu))
(run-test 'unknown (T1 'lookup-proc) same? (list 'India 'TamilNadu))
(run-test 'unknown (T1 'lookup-proc) same? (list 'India 'TamilNadu 'Chennai))
(run-test 'unknown (T1 'lookup-proc) same? (list 'India 'TamilNadu 'Chennai 'BesantNagar))
(run-test 'unknown (T1 'lookup-proc) same? (list 'India 'TamilNadu 'Chennai 'BesantNagar 'ElliotsBeach))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Karnataka))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Karnataka 'India))
(run-test 'unknown (T1 'lookup-proc) same? (list 'India 'Karnataka))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Kyrgiztan))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Turkmenistan))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Kazhakhstan))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Kazakhstan))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Kazhakhistan))
(run-test 'unknown (T1 'lookup-proc) same? (list 'Afghanistan))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Applying #<procedure:insert!> on: '(Afghanistan), 'AF
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF

Applying #<procedure:insert!> on: '(Kazhakhistan), 'KZ
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(Turkmenistan), 'TM
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(Kyrgiztan), 'KZ
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(India Karnataka), 'KA
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
India: 
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(India), 'IN
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
India: IN
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(India TamilNadu), 'TU
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
India: IN
  TamilNadu: TU
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(Pakistan), 'PK
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Pakistan: PK
India: IN
  TamilNadu: TU
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(Pakistan Sindh), 'SD
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Pakistan: PK
  Sindh: SD
India: IN
  TamilNadu: TU
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(Pakistan Sindh), 'SN
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Pakistan: PK
  Sindh: SN
India: IN
  TamilNadu: TU
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(India TamilNadu), 'TN
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Pakistan: PK
  Sindh: SN
India: IN
  TamilNadu: TN
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(India TamilNadu Chennai), 'CHEN
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Pakistan: PK
  Sindh: SN
India: IN
  TamilNadu: TN
    Chennai: CHEN
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(Pakistan Sindh Rawalpindi), 'RAWA
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Pakistan: PK
  Sindh: SN
    Rawalpindi: RAWA
India: IN
  TamilNadu: TN
    Chennai: CHEN
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(India TamilNadu Chennai BesantNagar ElliotsBeach), 'ELLIOTS
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Pakistan: PK
  Sindh: SN
    Rawalpindi: RAWA
India: IN
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(Pakistan), 'PoK
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
India: IN
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(Uzbekistan), 'UB
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Uzbekistan: UB
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
India: IN
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Applying #<procedure:insert!> on: '(Tajikistan), 'TJ
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Tajikistan: TJ
Uzbekistan: UB
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
India: IN
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
  Karnataka: KA
Kyrgiztan: KZ
Turkmenistan: TM
Kazhakhistan: KZ
Afghanistan: AF

Testing the lookup proc
Applying #<procedure:lookup> on: #<procedure:same?>, '(Tajikistan)
Result: 'TJ

Applying #<procedure:lookup> on: #<procedure:same?>, '(Uzbekistan)
Result: 'UB

Applying #<procedure:lookup> on: #<procedure:same?>, '(Burma)
Result: #f

Applying #<procedure:lookup> on: #<procedure:same?>, '(Pakistan)
Result: 'PoK

Applying #<procedure:lookup> on: #<procedure:same?>, '(Sindh)
Result: #f

Applying #<procedure:lookup> on: #<procedure:same?>, '(Pakistan Sindh)
Result: 'SN

Applying #<procedure:lookup> on: #<procedure:same?>, '(Pakistan Sindh Rawalpindi)
Result: 'RAWA

Applying #<procedure:lookup> on: #<procedure:same?>, '(Pakistan Sind Rawalpindi)
Result: #f

Applying #<procedure:lookup> on: #<procedure:same?>, '(India)
Result: 'IN

Applying #<procedure:lookup> on: #<procedure:same?>, '(TamilNadu)
Result: #f

Applying #<procedure:lookup> on: #<procedure:same?>, '(India TamilNadu)
Result: 'TN

Applying #<procedure:lookup> on: #<procedure:same?>, '(India TamilNadu Chennai)
Result: 'CHEN

Applying #<procedure:lookup> on: #<procedure:same?>, '(India TamilNadu Chennai BesantNagar)
Result: #f

Applying #<procedure:lookup> on: #<procedure:same?>, '(India TamilNadu Chennai BesantNagar ElliotsBeach)
Result: 'ELLIOTS

Applying #<procedure:lookup> on: #<procedure:same?>, '(Karnataka)
Result: #f

Applying #<procedure:lookup> on: #<procedure:same?>, '(Karnataka India)
Result: #f

Applying #<procedure:lookup> on: #<procedure:same?>, '(India Karnataka)
Result: 'KA

Applying #<procedure:lookup> on: #<procedure:same?>, '(Kyrgiztan)
Result: 'KZ

Applying #<procedure:lookup> on: #<procedure:same?>, '(Turkmenistan)
Result: 'TM

Applying #<procedure:lookup> on: #<procedure:same?>, '(Kazhakhstan)
Result: #f

Applying #<procedure:lookup> on: #<procedure:same?>, '(Kazakhstan)
Result: #f

Applying #<procedure:lookup> on: #<procedure:same?>, '(Kazhakhistan)
Result: 'KZ

Applying #<procedure:lookup> on: #<procedure:same?>, '(Afghanistan)
Result: 'AF

> 
