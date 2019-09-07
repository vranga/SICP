#lang racket

; Exercise 2.74.  Insatiable Enterprises, Inc., is a highly decentralized conglomerate
; company consisting of a large number of independent divisions located all over the
; world. The company's computer facilities have just been interconnected by means of a
; clever network-interfacing scheme that makes the entire network appear to any user to
; be a single computer. Insatiable's president, in her first attempt to exploit the
; ability of the network to extract administrative information from division files, is
; dismayed to discover that, although all the division files have been implemented as
; data structures in Scheme, the particular data structure used varies from division to
; division. A meeting of division managers is hastily called to search for a strategy to
; integrate the files that will satisfy headquarters' needs while preserving the existing
; autonomy of the divisions.

; Show how such a strategy can be implemented with data-directed programming. As an
; example, suppose that each division's personnel records consist of a single file, which
; contains a set of records keyed on employees' names. The structure of the set varies
; from division to division. Furthermore, each employee's record is itself a set
; (structured differently from division to division) that contains information keyed
; under identifiers such as address and salary. In particular:

; a.  Implement for headquarters a get-record procedure that retrieves a specified
; employee's record from a specified personnel file. The procedure should be applicable
; to any division's file. Explain how the individual divisions' files should be
; structured. In particular, what type information must be supplied?

; b.  Implement for headquarters a get-salary procedure that returns the salary
; information from a given employee's record from any division's personnel file. How
; should the record be structured in order to make this operation work?

; c.  Implement for headquarters a find-employee-record procedure. This should search all
; the divisions' files for the record of a given employee and return the record. Assume
; that this procedure takes as arguments an employee's name and a list of all the
; divisions' files.

; d.  When Insatiable takes over a new company, what changes must be made in order to
; incorporate the new personnel information into the central system?

; SOLUTION

; Headquarter level procedures
(define (get-record employee-id division-id)
	((get-proc division-id 'get-record) employee-id)
)

(define (get-salary employee-id division-id)
	((get-proc division-id 'get-salary) employee-id)
)

(define all-divisions (list 1 2))

(define (find-employee-record employee-id division-file-list)
	(if (null? division-file-list)
		(display "Employee not found: employee-id")
		(let ((record (get-record employee-id (car division-file-list))))
			(if (null? record)
				; search the next division
				(find-employee-record employee-id (cdr division-file-list))
				record
			)
		)
	)
)

; Division level procedures
(define div1-file
	(list
		"Division File"
		(list "Division ID" 1)
		(list "Division Name" "Insatiable Bengaluru")
		(list
			"Employee Records"
			(list "Employee Record" (list "Employee ID" 3456) (list 'Name "Ravi Varma") (list 'Address "123 Mariamman Kovil St., Nungambakkam, Chennai - 600045") (list 'Salary "Rs. 100000 per month"))
			(list "Employee Record" (list "Employee ID" 3457) (list 'Name "Susheel Javadi") (list 'Address "234 5th Cross, Malleshwaram, Bengaluru - 560050") (list 'Salary "Rs. 150000 per month"))
			(list "Employee Record" (list "Employee ID" 3458) (list 'Name "Brahma Joshi") (list 'Address "345 Pusa Road, New Delhi - 100046") (list 'Salary "Rs. 50000 per month"))
			(list "Employee Record" (list "Employee ID" 3459) (list 'Name "Sitara Shanbag") (list 'Address "456 Udupi Road, Tirthahalli - 740082") (list 'Salary "Rs. 175000 per month"))
		)
	)
)

(define (get-record-div1 employee-id)

	(define (get-employee-record employee-list)
		(if (null? employee-list)
			; (error "Employee not found: " employee-id)
			; Return null if the employee was not found
			(list)
			(if (eq? employee-id (car (cdadar employee-list)))
				(car employee-list)
				(get-employee-record (cdr employee-list))
			)
		)
	)

	(let ((emp-records (cdr (cadddr div1-file))))
		(get-employee-record emp-records)
	)
)

(define (get-salary-div1 employee-id)
	(cadadr (cdddr (get-record-div1 employee-id)))
)

(define (get-name-div1 employee-id)
	(car (cdaddr (get-record-div1 employee-id)))
)

(define (get-address-div1 employee-id)
	(cadr (cadddr (get-record-div1 employee-id)))
)

(define div2-file
	(list
		(cons "Division ID" 2)
		(cons "Division Name" "Insatiable America")
		(list
			(list 3460 "Bhuvan Seth" "3421 Bloomfield Hills, Scranton, MA - 52846" "$5000 per month")
			(list 3461 "Sameer Jois" "49 Crooked Street, San Diego, CA - 95862" "$1000 per month")
			(list 3462 "Pavni Duggal" "123 Hana Road, Edison NJ - 08817" "$10000 per month")
		)
	)
)

(define (get-record-div2 employee-id)

	(define (get-employee-record employee-list)
		(if (null? employee-list)
			; (error "Employee not found: " employee-id)
			; Return null if the employee was not found
			(list)
			(if (eq? employee-id (car (car employee-list)))
				(car employee-list)
				(get-employee-record (cdr employee-list))
			)
		)
	)

	(let ((emp-records (caddr div2-file)))
		(get-employee-record emp-records)
	)
)

(define (get-salary-div2 employee-id)
	(cadddr (get-record-div2 employee-id))
)

(define (get-name-div2 employee-id)
	(cadr (get-record-div2 employee-id))
)

(define (get-address-div2 employee-id)
	(caddr (get-record-div2 employee-id))
)

; Implementation of get on the operation table.
; The table is hard-coded here so no 'put's are needed
(define (get-proc division-id proc-name)
	(define op-table
		(list
			(cons
				1
				(list
					(cons 'get-record get-record-div1)
					(cons 'get-salary get-salary-div1)
					(cons 'get-name get-name-div1)
					(cons 'get-address get-address-div1)
				)
			)
			(cons
				2
				(list
					(cons 'get-record get-record-div2)
					(cons 'get-salary get-salary-div2)
					(cons 'get-name get-name-div2)
					(cons 'get-address get-address-div2)
				)
			)
		)
	)

	(define (find-division-row division table)
		(cond
			((not (pair? table)) (error "op-table Not a pair!"))
			((null? table) (error "Procedure not found for " division))
			(else
				(if (eq? division (car (car table)))
					(car table)
					(find-division-row division (cdr table))
				)
			)
		)
	)

	(define (find-proc-in-division division-proc-list proc)
		(cond
			((not (pair? division-proc-list)) (error "division-proc-list Not a pair!"))
			((null? division-proc-list) (error "proc not found: " proc))
			(else
				(if (eq? proc (car (car division-proc-list)))
					(cdr (car division-proc-list))
					(find-proc-in-division (cdr division-proc-list) proc)
				)
			)
		)
	)

	(find-proc-in-division (cdr (find-division-row division-id op-table)) proc-name)
)

; Tests

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
> (get-record 3456 1)
'("Employee Record"
  ("Employee ID" 3456)
  (Name "Ravi Varma")
  (Address "123 Mariamman Kovil St., Nungambakkam, Chennai - 600045")
  (Salary "Rs. 100000 per month"))
> (get-record 3457 1)
'("Employee Record"
  ("Employee ID" 3457)
  (Name "Susheel Javadi")
  (Address "234 5th Cross, Malleshwaram, Bengaluru - 560050")
  (Salary "Rs. 150000 per month"))
> (get-record 3458 1)
'("Employee Record"
  ("Employee ID" 3458)
  (Name "Brahma Joshi")
  (Address "345 Pusa Road, New Delhi - 100046")
  (Salary "Rs. 50000 per month"))
> (get-record 3459 1)
'("Employee Record"
  ("Employee ID" 3459)
  (Name "Sitara Shanbag")
  (Address "456 Udupi Road, Tirthahalli - 740082")
  (Salary "Rs. 175000 per month"))
> (get-record 3460 1)
'()
> (get-record 3460 2)
'(3460 "Bhuvan Seth" "3421 Bloomfield Hills, Scranton, MA - 52846" "$5000 per month")
> (get-record 3461 2)
'(3461 "Sameer Jois" "49 Crooked Street, San Diego, CA - 95862" "$1000 per month")
> (get-record 3462 2)
'(3462 "Pavni Duggal" "123 Hana Road, Edison NJ - 08817" "$10000 per month")
> (get-record 3463 2)
'()
> (get-salary 3456 1)
(get-salary 3457 1)
(get-salary 3458 1)
(get-salary 3459 1)
(get-salary 3460 2)
(get-salary 3461 2)
(get-salary 3462 2)
"Rs. 100000 per month"
"Rs. 150000 per month"
"Rs. 50000 per month"
"Rs. 175000 per month"
"$5000 per month"
"$1000 per month"
"$10000 per month"

> (find-employee-record 3456 all-divisions)
'("Employee Record"
  ("Employee ID" 3456)
  (Name "Ravi Varma")
  (Address "123 Mariamman Kovil St., Nungambakkam, Chennai - 600045")
  (Salary "Rs. 100000 per month"))
> (find-employee-record 3457 all-divisions)
'("Employee Record"
  ("Employee ID" 3457)
  (Name "Susheel Javadi")
  (Address "234 5th Cross, Malleshwaram, Bengaluru - 560050")
  (Salary "Rs. 150000 per month"))
> (find-employee-record 3458 all-divisions)
'("Employee Record"
  ("Employee ID" 3458)
  (Name "Brahma Joshi")
  (Address "345 Pusa Road, New Delhi - 100046")
  (Salary "Rs. 50000 per month"))
> (find-employee-record 3459 all-divisions)
'("Employee Record"
  ("Employee ID" 3459)
  (Name "Sitara Shanbag")
  (Address "456 Udupi Road, Tirthahalli - 740082")
  (Salary "Rs. 175000 per month"))
> (find-employee-record 3460 all-divisions)
'(3460 "Bhuvan Seth" "3421 Bloomfield Hills, Scranton, MA - 52846" "$5000 per month")
> (find-employee-record 3461 all-divisions)
'(3461 "Sameer Jois" "49 Crooked Street, San Diego, CA - 95862" "$1000 per month")
> (find-employee-record 3462 all-divisions)
'(3462 "Pavni Duggal" "123 Hana Road, Edison NJ - 08817" "$10000 per month")
> (find-employee-record 3463 all-divisions)
Employee not found: employee-id
> 

; Notes: As we can see, the procedure get-record works on any division's file. The structure of the
; individual divisions' files is not of consequence. What matters is the presence of
; division-level procedures that fetch the fields of interest. These procedures are then placed
; in the operation table that is indexed at run-time to find the procedure that will work for
; a particular division

; The 'type' information to be supplied is 'get-record i.e. the implementation of the
; headquarter-level get-record procedure should indicate to the generic get proc that it is
; interested in the get-record procedure for a particular division

; The structure of an employee record is not important. This is because this is hidden behind
; the implementation of 'get-salary' at the division level. And the generic procedure is
; able to find the correct get-salary procedure by indexing into the operation table

; When Insatiable takes over a new company the changes are all additive:
; 1. The new company's divisions will implement the same procedures registered in the
; operation table above.
; 2. The operation table maintainer will add new rows (one per division) to it to register
; the new procedures. Everything will work after these changes
