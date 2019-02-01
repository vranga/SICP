#lang racket

; Exercise 3.26.  To search a table as implemented above, one needs to scan through the list of records.
; This is basically the unordered list representation of section 2.3.3. For large tables, it may be more
; efficient to structure the table in a different manner. Describe a table implementation where the
; (key, value) records are organized using a binary tree, assuming that keys can be ordered in some
; way (e.g., numerically or alphabetically). (Compare exercise 2.66 of chapter 2.)

; S O L U T I O N

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; This took more time than I expected probably because of the number of interruptions I had to handle while working
; on this problem. My approach was to replace the list data structure from the previous exercise with a binary tree. This
; was not straightforward because in the previous (Exercise 3.25) implementation there is no clean separation between the
; procedure "make-table" and the list data structure that it uses. Operations such as 'get-first-key-value-pair' and 
; 'get-remaining-key-value-pairs' do not make sense for a binary-tree. So I had to redo all of this. So it was a good opportunity
; to introduce a clean separation between the table object and the binary-tree structure that it uses to store its key-value
; pairs. This is exactly what I did. So the generic table level operations like lookup, insert!, find-key-value-pair etc. make no
; assumptions about the underlying data structure. Instead they use the message passing strategy to invoke operations on the
; binary-tree objects which exist in the generic table.

; For key ordering I have used string comparisons. However, the 'same', 'less-than' and 'greater-than' operations
; are passed in as parameters to the make-table procedure so make-table is agnostic to how the keys are ordered. 

; I liked the idea of procedures with local state so I continued to use this technique to implement the binary-tree. I found
; the encapsulation that this technique gives, quite useful because it allowed me to think of the table and its contents as
; objects with operations within them.

; I re-wrote the monolithic print procedure from the previous exercise and distributed the print work across the three
; objects: generic table, binary tree and key-value-pair. This made it a little clunky to handle the indentations while
; printing different levels in the generic table but overall, I think this is a huge improvement over the way I wrote 
; the solution for Exercise 3.25.

; I am a stickler for separation of concerns and clean interfaces. In this problem, the generic table contains binary trees
; which in turn contain key-value pairs. The key-value pair objects contain generic tables themselves in order to allow
; an arbitrary number of keys for any value. This circular relationship made the print procedure a little unclean as you can
; see where I call the procedure "print-generic-table"

; As you can see the tests below, my generic table implementation works pretty well. It allows values to be stored under
; an arbitrary number of keys, different values may be stored under different numbers of keys and since I use a binary tree
; to store the values, both the lookups and inserts have a time complexity of (Theta(log n)) where n is the number of
; elements at a given level in the multi-level table.

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
; but also objects to be inserted and looked up from the table. Since every non-primitive object is 
; implemented using pairs, this makes it necessary for us to be able to distinguish between a sub-table
; and a value which is an object. So I am introducing tags so that the table procedures can correctly
; identify the data they handle as they traverse through the table. So a 'key' will not be just a
; string literal but a pair in which the 'car' is a tag with the value 'key and the 'cdr' is the actual key
; Similarly, a 'value' will not just be a string literal but a pair in which the 'car' is a tag with the
; value 'value and the 'cdr' is the actual value which could be a primitive or a complex object

(define (make-table less-than-key? same-key? greater-than-key?)

	; Key-Value Pair operations
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
		(if (mpair? key-value-pair)
			(if (mpair? (mcdr key-value-pair))
				(if (mpair? (mcdr (mcdr key-value-pair)))
					(mcdr (mcdr (mcdr key-value-pair)))
					null
				)
				null
			)
			null
		)
	)
	(define (get-subtable key-value-pair)
		(if (mpair? key-value-pair)
			(if (mpair? (mcdr key-value-pair))
				(mcar (mcdr key-value-pair))
				null
			)
			null
		)
	)
	(define (set-value! key-value-pair x)
		(set-mcdr! (mcdr key-value-pair) x)
	)
	(define (set-subtable! key-value-pair subtable)
		(set-mcar! (mcdr key-value-pair) subtable)
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

	(define (make-key key) (mcons 'key key))
	(define (make-value value) (mcons 'value value))

	; Binary tree implementation

	; Each node will be a list of three items: the entry at the node, the left subtree,
	; and the right subtree. A left or a right subtree of the empty list will indicate that
	; there is no subtree connected there.

	(define (make-kvp-binary-tree less-than-key? same-key? greater-than-key?)

		(let ((local-tree (mlist null null null)))
			(define (empty-tree?)
				(if (and (null? (entry)) (null? (left-branch)) (null? (right-branch)))
					true
					false
				)
			)
			(define (entry) (mcar local-tree))
			(define (left-branch)
				(if (mpair? (mcdr local-tree))
					(mcar (mcdr local-tree))
					null
				)
			)
			(define (right-branch)
				(if (mpair? (mcdr local-tree))
					(if (mpair? (mcdr (mcdr local-tree)))
						(mcar (mcdr (mcdr local-tree)))
						null
					)
					null
				)
			)

			; returns the key-value pair in which the key is k. If there is no such key-value-pair
			; it returns null
			(define (lookup k)
				(cond
					((null? local-tree) null)
					((same-key? k (get-key (entry))) (entry))
					((less-than-key? k (get-key (entry)))
						(if (not (null? (left-branch)))
							(((left-branch) 'lookup) k)
							null
						)
					)
					((greater-than-key? k (get-key (entry)))
						(if (not (null? (right-branch)))
							(((right-branch) 'lookup) k)
							null
						)
					)
				)
			)

			(define (set-tree! entry left right)
				(set-entry! entry)
				(set-left-branch! left)
				(set-right-branch! right)
			)

			(define (set-entry! entry)
				(set-mcar! local-tree entry)
			)
			(define (set-left-branch! tree)
				(set-mcar! (mcdr local-tree) tree)
			)
			(define (set-right-branch! tree)
				(set-mcar! (mcdr (mcdr local-tree)) tree)
			)

			; Inserting an item into this tree requires (log n) steps. To insert an item x, we compare
			; x with the node entry to determine whether x should be added to the right or to the left
			; branch, and having inserted x into the appropriate branch we piece this newly constructed
			; branch together with the original entry and the other branch. If we are asked to insert x
			; into an empty tree, we generate a tree that has x as the entry and empty right and left
			; branches. If the tree already contains an element that is 'equal to' the new element,
			; then we replace the old element with the new one.

			(define (insert-key-value-pair! kvp)
				(if (empty-tree?)
					; if the tree is empty, then insert the kvp at this node
					(set-entry! kvp)
					; tree is not empty so look for the right place to insert the kvp
					(if (same-key? (get-key kvp) (get-key (entry)))
						(set-entry! kvp)
						(begin
							(if (less-than-key? (get-key kvp) (get-key (entry)))
								(begin
									(if (null? (left-branch))
										(set-left-branch! (make-kvp-binary-tree less-than-key? same-key? greater-than-key?))
										(void)
									)
									(((left-branch) 'insert-key-value-pair!) kvp)
								)
								(begin
									(if (null? (right-branch))
										(set-right-branch! (make-kvp-binary-tree less-than-key? same-key? greater-than-key?))
										(void)
									)
									(((right-branch) 'insert-key-value-pair!) kvp)
								)
							)
						)
					)
				)
			)

			(define (print-tree indentation-level)

				(define (indent indentation-level)
					(if (= indentation-level 0)
						(void)
						(begin
							(display "  ")
							(indent (- indentation-level 1))
						)
					)
				)

				; In-order traversal: We want to print in left-to-right order
				; print the left branch first, then the entry in this node and then the right branch
				(if (not (null? (left-branch)))
					(((left-branch) 'print) indentation-level)
					(void)
				)
				(indent indentation-level)
				(print-key-and-value (entry))
				; print the subtable
				(if (not (null? (get-subtable (entry))))
					(print-generic-table (get-subtable (entry)) (+ indentation-level 1))
					(void)
				)
				(if (not (null? (right-branch)))
					(((right-branch) 'print) indentation-level)
					(void)
				)
			)

			(define (dispatch m)
				(cond
					((eq? m 'entry) entry)
					((eq? m 'left-branch) left-branch)
					((eq? m 'right-branch) right-branch)
					((eq? m 'lookup) lookup)
					((eq? m 'insert-key-value-pair!) insert-key-value-pair!)
					((eq? m 'print) print-tree)
					(else (error "Unknown operation -- KVP-BINARY-TREE" m))
				)
			)

			dispatch
		)
	)

	(define (make-generic-table)
		(mlist '*generic-table*)
	)

	(define (print-generic-table table indentation-level)
		(if (mpair? table)
			(((mcdr table) 'print) indentation-level)
			(void)
		)
	)

	(let ((local-table (make-generic-table)))

		(define (get-records table)
			(if (mpair? table)
				(mcdr table)
				null
			)
		)

		(define (insert-at-top-level! table key-value-pair)
			(if (null? (get-records table))
				(set-mcdr! table (make-kvp-binary-tree less-than-key? same-key? greater-than-key?))
				(void)
			)
			(((get-records table) 'insert-key-value-pair!) key-value-pair)
		)
	
		(define (find-key-value-pair key-to-find records)
			; Note that 'records' here is a binary tree
			; This procedure searches for the key at this level in the table and if it is found,
			; returns the key-value pair object. If not found, it returns false
			(if (not (null? records))
				((records 'lookup) key-to-find)
				null
			)
		)

		(define (lookup list-of-keys)

			(define (lookup-internal key-list table)
				(cond
					((null? key-list) false)
					((not (pair? key-list)) false)
					(else
						(let ((key-value-pair (find-key-value-pair (car key-list) (get-records table))))
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
				(let ((key-value-pair (find-key-value-pair (car list-of-keys) (get-records table))))
					(if (not (null? key-value-pair))
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
			(displayln "GENERIC-TABLE")
			(displayln "-------------")
			(((get-records local-table) 'print) 0)
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

; Tests

(define (key-equality-comparator key1 key2)
	(equal? (to-string key1) (to-string key2))
)

(define (key-less-than-comparator key1 key2)
	(string<? (to-string key1) (to-string key2))
)

(define (key-greater-than-comparator key1 key2)
	(string>? (to-string key1) (to-string key2))
)

(define (to-string x)
	(cond
		((null? x) "")
		((symbol? x) (symbol->string x))
		(else
			x
		)
	)
)

(define T1 (make-table key-less-than-comparator key-equality-comparator key-greater-than-comparator))
(run-test 'unknown (T1 'insert-proc!) (list "Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India" "Deccan" "Karnataka" "Bengaluru" "J P Nagar" "Brigade Millennium" "Jacaranda" "Floor 7" "702" "Study Room" "Writing Desk" "Drawer" "Pencil") 'Lead)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Kazhakhistan") 'KZ)
(run-test 'unknown (T1 'lookup-proc)  (list "Kazhakhistan"))
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Afghanistan") 'AF)
(run-test 'unknown (T1 'lookup-proc)  (list "Kazhakhistan"))
(run-test 'unknown (T1 'lookup-proc)  (list "Afghanistan"))
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Turkmenistan") 'TM)
(run-test 'unknown (T1 'lookup-proc)  (list "Turkmenistan"))
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Kyrgiztan") 'KY)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "Karnataka") 'KA)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India") 'IN)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "TamilNadu") 'TU)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India" "Deccan") 'Plateau)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Pakistan") 'PK)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Pakistan" "Sindh") 'SD)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Pakistan" "Sindh") 'SN)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "TamilNadu") 'TN)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "TamilNadu" "Chennai") 'CHEN)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Pakistan" "Sindh" "Rawalpindi") 'RAWA)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "TamilNadu" "Chennai" "BesantNagar" "ElliotsBeach") 'ELLIOTS)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Pakistan") 'PoK)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Uzbekistan") 'UB)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "Tajikistan") 'TJ)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "TamilNadu" "Chennai" "BesantNagar" "Kalakshetra") 'Dance)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "TamilNadu" "Chennai" "Mogapper") 'Jikky)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "TamilNadu" "Chennai" "Saidapet") 'Bus-stand)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "TamilNadu" "Chennai" "Adayar") 'A2B)
(run-test 'none (T1 'print))
(run-test 'unknown (T1 'insert-proc!) (list "India" "TamilNadu" "Chennai" "Adayar" "Sangeeta" "Breakfast" "Plate") 'Idli)
(run-test 'none (T1 'print))

(display "Testing the lookup proc")
(newline)

(run-test 'unknown (T1 'lookup-proc) (list "Tajikistan"))
(run-test 'unknown (T1 'lookup-proc) (list "Uzbekistan"))
(run-test 'unknown (T1 'lookup-proc) (list "Burma"))
(run-test 'unknown (T1 'lookup-proc) (list "Pakistan"))
(run-test 'unknown (T1 'lookup-proc) (list "Sindh"))
(run-test 'unknown (T1 'lookup-proc) (list "Pakistan" "Sindh"))
(run-test 'unknown (T1 'lookup-proc) (list "Pakistan" "Sindh" "Rawalpindi"))
(run-test 'unknown (T1 'lookup-proc) (list "Pakistan" "Sindh" "Rawalpindi"))
(run-test 'unknown (T1 'lookup-proc) (list "India"))
(run-test 'unknown (T1 'lookup-proc) (list "TamilNadu"))
(run-test 'unknown (T1 'lookup-proc) (list "India" "TamilNadu"))
(run-test 'unknown (T1 'lookup-proc) (list "India" "TamilNadu" "Chennai"))
(run-test 'unknown (T1 'lookup-proc) (list "India" "TamilNadu" "Chennai" "BesantNagar"))
(run-test 'unknown (T1 'lookup-proc) (list "India" "TamilNadu" "Chennai" "BesantNagar" "ElliotsBeach"))
(run-test 'unknown (T1 'lookup-proc) (list "Karnataka"))
(run-test 'unknown (T1 'lookup-proc) (list "Karnataka" "India"))
(run-test 'unknown (T1 'lookup-proc) (list "India" "Karnataka"))
(run-test 'unknown (T1 'lookup-proc) (list "Kyrgiztan"))
(run-test 'unknown (T1 'lookup-proc) (list "Turkmenistan"))
(run-test 'unknown (T1 'lookup-proc) (list "Kazhakhstan"))
(run-test 'unknown (T1 'lookup-proc) (list "Kazakhstan"))
(run-test 'unknown (T1 'lookup-proc) (list "Kazhakhistan"))
(run-test 'unknown (T1 'lookup-proc) (list "Afghanistan"))
(run-test 'unknown (T1 'lookup-proc) (list "India" "TamilNadu" "Chennai" "Adayar" "Sangeeta" "Breakfast" "Plate"))
(run-test 'unknown (T1 'lookup-proc) (list "India" "TamilNadu" "Chennai" "Adayar"))
(run-test 'unknown (T1 'lookup-proc) (list "India" "TamilNadu" "Chennai" "Adayar" "Sangeeta" "Plate"))
(run-test 'unknown (T1 'lookup-proc) (list "Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India" "Deccan" "Karnataka" "Bengaluru" "J P Nagar" "Brigade Millennium" "Jacaranda" "Floor 7" "702" "Study Room" "Writing Desk" "Drawer" "Pencil"))
(run-test 'unknown (T1 'lookup-proc) (list "Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India" "Deccan"))
(run-test 'unknown (T1 'lookup-proc) (list "Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India"))
(run-test 'unknown (T1 'insert-proc!) (list "Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India") 'Dharmarajya)
(run-test 'unknown (T1 'lookup-proc) (list "Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India"))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.
Applying #<procedure:insert!> on: '("Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India" "Deccan" "Karnataka" "Bengaluru" "J P Nagar" "Brigade Millennium" "Jacaranda" "Floor 7" "702" "Study Room" "Writing Desk" "Drawer" "Pencil"), 'Lead
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: 
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead

Applying #<procedure:insert!> on: '("Kazhakhistan"), 'KZ
Result: 'ok

Applying #<procedure:lookup> on: '("Kazhakhistan")
Result: 'KZ

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: 
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
Kazhakhistan: KZ

Applying #<procedure:insert!> on: '("Afghanistan"), 'AF
Result: 'ok

Applying #<procedure:lookup> on: '("Kazhakhistan")
Result: 'KZ

Applying #<procedure:lookup> on: '("Afghanistan")
Result: 'AF

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: 
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
Kazhakhistan: KZ

Applying #<procedure:insert!> on: '("Turkmenistan"), 'TM
Result: 'ok

Applying #<procedure:lookup> on: '("Turkmenistan")
Result: 'TM

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: 
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
Kazhakhistan: KZ
Turkmenistan: TM

Applying #<procedure:insert!> on: '("Kyrgiztan"), 'KY
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: 
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
Kazhakhistan: KZ
Kyrgiztan: KY
Turkmenistan: TM

Applying #<procedure:insert!> on: '("India" "Karnataka"), 'KA
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: 
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: 
  Karnataka: KA
Kazhakhistan: KZ
Kyrgiztan: KY
Turkmenistan: TM

Applying #<procedure:insert!> on: '("India"), 'IN
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: 
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
Kazhakhistan: KZ
Kyrgiztan: KY
Turkmenistan: TM

Applying #<procedure:insert!> on: '("India" "TamilNadu"), 'TU
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: 
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TU
Kazhakhistan: KZ
Kyrgiztan: KY
Turkmenistan: TM

Applying #<procedure:insert!> on: '("Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India" "Deccan"), 'Plateau
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TU
Kazhakhistan: KZ
Kyrgiztan: KY
Turkmenistan: TM

Applying #<procedure:insert!> on: '("Pakistan"), 'PK
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TU
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PK
Turkmenistan: TM

Applying #<procedure:insert!> on: '("Pakistan" "Sindh"), 'SD
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TU
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PK
  Sindh: SD
Turkmenistan: TM

Applying #<procedure:insert!> on: '("Pakistan" "Sindh"), 'SN
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TU
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PK
  Sindh: SN
Turkmenistan: TM

Applying #<procedure:insert!> on: '("India" "TamilNadu"), 'TN
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PK
  Sindh: SN
Turkmenistan: TM

Applying #<procedure:insert!> on: '("India" "TamilNadu" "Chennai"), 'CHEN
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PK
  Sindh: SN
Turkmenistan: TM

Applying #<procedure:insert!> on: '("Pakistan" "Sindh" "Rawalpindi"), 'RAWA
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PK
  Sindh: SN
    Rawalpindi: RAWA
Turkmenistan: TM

Applying #<procedure:insert!> on: '("India" "TamilNadu" "Chennai" "BesantNagar" "ElliotsBeach"), 'ELLIOTS
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PK
  Sindh: SN
    Rawalpindi: RAWA
Turkmenistan: TM

Applying #<procedure:insert!> on: '("Pakistan"), 'PoK
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
Turkmenistan: TM

Applying #<procedure:insert!> on: '("Uzbekistan"), 'UB
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
Turkmenistan: TM
Uzbekistan: UB

Applying #<procedure:insert!> on: '("Tajikistan"), 'TJ
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
Tajikistan: TJ
Turkmenistan: TM
Uzbekistan: UB

Applying #<procedure:insert!> on: '("India" "TamilNadu" "Chennai" "BesantNagar" "Kalakshetra"), 'Dance
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
        Kalakshetra: Dance
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
Tajikistan: TJ
Turkmenistan: TM
Uzbekistan: UB

Applying #<procedure:insert!> on: '("India" "TamilNadu" "Chennai" "Mogapper"), 'Jikky
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
        Kalakshetra: Dance
      Mogapper: Jikky
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
Tajikistan: TJ
Turkmenistan: TM
Uzbekistan: UB

Applying #<procedure:insert!> on: '("India" "TamilNadu" "Chennai" "Saidapet"), 'Bus-stand
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
      BesantNagar: 
        ElliotsBeach: ELLIOTS
        Kalakshetra: Dance
      Mogapper: Jikky
      Saidapet: Bus-stand
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
Tajikistan: TJ
Turkmenistan: TM
Uzbekistan: UB

Applying #<procedure:insert!> on: '("India" "TamilNadu" "Chennai" "Adayar"), 'A2B
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
      Adayar: A2B
      BesantNagar: 
        ElliotsBeach: ELLIOTS
        Kalakshetra: Dance
      Mogapper: Jikky
      Saidapet: Bus-stand
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
Tajikistan: TJ
Turkmenistan: TM
Uzbekistan: UB

Applying #<procedure:insert!> on: '("India" "TamilNadu" "Chennai" "Adayar" "Sangeeta" "Breakfast" "Plate"), 'Idli
Result: 'ok

Applying #<procedure:print-table>
GENERIC-TABLE
-------------
Afghanistan: AF
Andromeda: 
  Milky Way: 
    Outer Spiral: 
      Sun Solar System: 
        Earth: 
          Asia: 
            South Asia: 
              India: 
                Deccan: Plateau
                  Karnataka: 
                    Bengaluru: 
                      J P Nagar: 
                        Brigade Millennium: 
                          Jacaranda: 
                            Floor 7: 
                              702: 
                                Study Room: 
                                  Writing Desk: 
                                    Drawer: 
                                      Pencil: Lead
India: IN
  Karnataka: KA
  TamilNadu: TN
    Chennai: CHEN
      Adayar: A2B
        Sangeeta: 
          Breakfast: 
            Plate: Idli
      BesantNagar: 
        ElliotsBeach: ELLIOTS
        Kalakshetra: Dance
      Mogapper: Jikky
      Saidapet: Bus-stand
Kazhakhistan: KZ
Kyrgiztan: KY
Pakistan: PoK
  Sindh: SN
    Rawalpindi: RAWA
Tajikistan: TJ
Turkmenistan: TM
Uzbekistan: UB

Testing the lookup proc
Applying #<procedure:lookup> on: '("Tajikistan")
Result: 'TJ

Applying #<procedure:lookup> on: '("Uzbekistan")
Result: 'UB

Applying #<procedure:lookup> on: '("Burma")
Result: #f

Applying #<procedure:lookup> on: '("Pakistan")
Result: 'PoK

Applying #<procedure:lookup> on: '("Sindh")
Result: #f

Applying #<procedure:lookup> on: '("Pakistan" "Sindh")
Result: 'SN

Applying #<procedure:lookup> on: '("Pakistan" "Sindh" "Rawalpindi")
Result: 'RAWA

Applying #<procedure:lookup> on: '("Pakistan" "Sindh" "Rawalpindi")
Result: 'RAWA

Applying #<procedure:lookup> on: '("India")
Result: 'IN

Applying #<procedure:lookup> on: '("TamilNadu")
Result: #f

Applying #<procedure:lookup> on: '("India" "TamilNadu")
Result: 'TN

Applying #<procedure:lookup> on: '("India" "TamilNadu" "Chennai")
Result: 'CHEN

Applying #<procedure:lookup> on: '("India" "TamilNadu" "Chennai" "BesantNagar")
Result: #f

Applying #<procedure:lookup> on: '("India" "TamilNadu" "Chennai" "BesantNagar" "ElliotsBeach")
Result: 'ELLIOTS

Applying #<procedure:lookup> on: '("Karnataka")
Result: #f

Applying #<procedure:lookup> on: '("Karnataka" "India")
Result: #f

Applying #<procedure:lookup> on: '("India" "Karnataka")
Result: 'KA

Applying #<procedure:lookup> on: '("Kyrgiztan")
Result: 'KY

Applying #<procedure:lookup> on: '("Turkmenistan")
Result: 'TM

Applying #<procedure:lookup> on: '("Kazhakhstan")
Result: #f

Applying #<procedure:lookup> on: '("Kazakhstan")
Result: #f

Applying #<procedure:lookup> on: '("Kazhakhistan")
Result: 'KZ

Applying #<procedure:lookup> on: '("Afghanistan")
Result: 'AF

Applying #<procedure:lookup> on: '("India" "TamilNadu" "Chennai" "Adayar" "Sangeeta" "Breakfast" "Plate")
Result: 'Idli

Applying #<procedure:lookup> on: '("India" "TamilNadu" "Chennai" "Adayar")
Result: 'A2B

Applying #<procedure:lookup> on: '("India" "TamilNadu" "Chennai" "Adayar" "Sangeeta" "Plate")
Result: #f

Applying #<procedure:lookup> on: '("Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India" "Deccan" "Karnataka" "Bengaluru" "J P Nagar" "Brigade Millennium" "Jacaranda" "Floor 7" "702" "Study Room" "Writing Desk" "Drawer" "Pencil")
Result: 'Lead

Applying #<procedure:lookup> on: '("Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India" "Deccan")
Result: 'Plateau

Applying #<procedure:lookup> on: '("Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India")
Result: #f

Applying #<procedure:insert!> on: '("Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India"), 'Dharmarajya
Result: 'ok

Applying #<procedure:lookup> on: '("Andromeda" "Milky Way" "Outer Spiral" "Sun Solar System" "Earth" "Asia" "South Asia" "India")
Result: 'Dharmarajya

> 
