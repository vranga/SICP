#lang racket

; Exercise 3.66.  Examine the stream (pairs integers integers). Can you make any general
; comments about the order in which the pairs are placed into the stream? For example,
; about how many pairs precede the pair (1,100)? the pair (99,100)? the pair (100,100)?
; (If you can make precise mathematical statements here, all the better. But feel free
; to give more qualitative answers if you find yourself getting bogged down.)

; E X P L A N A T I O N

; The pairs produced by (pairs integers integers) will look like this:
;
; (1, 1) (1, 2) (1, 3) (1, 4) (1, 5) (1, 6) (1, 7) (1, 8) (1, 9)  (1, 10)  (1, 11) (1, 12) ...
;        (2, 2) (2, 3) (2, 4) (2, 5) (2, 6) (2, 7) (2, 8) (2, 9)  (2, 10)  (2, 11) (2, 12) ...
;               (3, 3) (3, 4) (3, 5) (3, 6) (3, 7) (3, 8) (3, 9)  (3, 10)  (3, 11) (3, 12) ...
;                      (4, 4) (4, 5) (4, 6) (4, 7) (4, 8) (4, 9)  (4, 10)  (4, 11) (4, 12) ...
;                             (5, 5) (5, 6) (5, 7) (5, 8) (5, 9)  (5, 10)  (5, 11) (5, 12) ...
;                                    (6, 6) (6, 7) (6, 8) (6, 9)  (6, 10)  (6, 11) (6, 12) ...
;                                           (7, 7) (7, 8) (7, 9)  (7, 10)  (7, 11) (7, 12) ...
;                                                  (8, 8) (8, 9)  (8, 10)  (8, 11) (8, 12) ...
;                                                         (9, 9)  (9, 10)  (9, 11) (9, 12) ...
;                                                                (10, 10) (10, 11) (10, 12) ...
;                                                                         (11, 11) (11, 12) ...
;                                                                                  (12, 12) ...
;
; The recursive calls to 'pairs' and 'interleave' will generate pairs as follows:
; (pairs indented for readability)

;1	(1, 1)
;2	(1, 2)
;3			(2, 2)
;4	(1, 3)
;5			(2, 3)
;6	(1, 4)
;7					(3, 3)
;8	(1, 5)
;9			(2, 4)
;10	(1, 6)
;11					(3, 4)
;12	(1, 7)
;13			(2, 5)
;14	(1, 8)
;15							(4, 4)
;16	(1, 9)
;17			(2, 6)
;18	(1, 10)
;19					(3, 5)
;20	(1, 11)
;21			(2, 7)
;22	(1, 12)
;23							(4, 5)
;24	(1, 13)
;25			(2, 8)
;26	(1, 14)
;27					(3, 6)
;28	(1, 15)
;29			(2, 9)
;30	(1, 16)
;31									(5, 5)
;32	(1, 17)
;33			(2, 10)
;34	(1, 18)
;35					(3, 7)
;36	(1, 19)
;37			(2, 11)
;38	(1, 20)
;39							(4, 6)
;40	(1, 21)
;41			(2, 12)
;42	(1, 22)
;43					(3, 8)
;44	
;	... and so on
;
; Note that for all (m, n), m <= n
;
; Position of (m, m)
; (1, 1) —> 1
; (2, 2) —> 1 + 2^1 = 3
; (3, 3) —> 3 + 2^2 = 7
; (4, 4) —> 7 + 2^3 = 15
; (5, 5) —> 15 + 2^4 = 31
; (m, m) —> (2^m) - 1
; 
; Gap between pairs (m, n) and (m, n+1) is:
; 	2^m for n > m
; 	2^(m - 1) for n = m
; 
; So the position of (m, n) would be:
; 
; (2^m) - 1											when m = n
; {(2^m) - 1}  + 2^(m - 1)							when n = m + 1
; {(2^m) - 1}  + 2^(m - 1) + (2^m) * (n - m -1)		when n > m + 1
; 
; Using the formula above, the position of (1, 100) would be:
; {(2^1) - 1}  + 2^(1 - 1) + (2^1) * (100 - 1 - 1)
; = 1 + 1 + 2 * 98
; = 198
; 
; The position of (99, 100) would be:
; {(2^99) - 1}  + 2^(99 - 1)
; = 2^99 + 2^98 - 1
; 
; The position of (100, 100) would be:
; (2^100) - 1
; 
; C O D E

(define (pairs s t)
	(stream-cons
		(list (stream-first s) (stream-first t))
		(interleave
			(stream-map (lambda (x) (list (stream-first s) x)) (stream-rest t))
			(pairs (stream-rest s) (stream-rest t))
		)
	)
)

(define (interleave s1 s2)
	(if (stream-empty? s1)
		s2
		(stream-cons
			(stream-first s1)
			(interleave s2 (stream-rest s1))
		)
	)
)

(define (partial-sums S)
	(stream-cons (stream-first S) (add-streams (partial-sums S) (stream-rest S)))
)

(define (ln2-summands n)
	(stream-cons
		(/ 1.0 n)
		(stream-map - (ln2-summands (+ n 1)))
	)
)

(define ln2-stream
	(partial-sums (ln2-summands 1))
)

(define (sqrt x tolerance)
	(stream-limit (sqrt-stream x) tolerance)
)

(define (stream-limit s t)
	(let ((s0 (stream-ref s 0)) (s1 (stream-ref s 1)))
			(if (< (abs (- s1 s0)) t)
			s1
			(stream-limit (stream-rest s) t)
		)
	)
)

(define (sqrt-stream x)
	(define guesses
		(stream-cons
			1.0
			(stream-map
				(lambda (guess) (sqrt-improve guess x))
				guesses
			)
		)
	)
	guesses
)

(define (sqrt-improve guess x)
	(average guess (/ x guess))
)

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (euler-transform s)
	(let ((s0 (stream-ref s 0))          ; Sn-1
		 (s1 (stream-ref s 1))           ; Sn
		 (s2 (stream-ref s 2)))          ; Sn+1
		(stream-cons
			(-
				s2
				(/ (square (- s2 s1)) (+ s0 (* -2 s1) s2))
			)
			(euler-transform (stream-rest s))
		)
	)
)

(define (make-tableau transform s)
	(stream-cons
		s
		(make-tableau transform (transform s))
	)
)

(define (accelerated-sequence transform s)
	(stream-map stream-first (make-tableau transform s))
)

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream)
)

(define (stream-map proc . argstreams)
	; (displayln "Entered stream-map")
	(if (stream-empty? (car argstreams))
		empty-stream
		(stream-cons
			(apply proc (map stream-first argstreams))
			(apply stream-map (cons proc (map stream-rest argstreams)))
		)
	)
)

(define (stream-ref s n)
	; (display "Entered stream-ref with n = ")
	; (display n)
	; (newline)
	(if (= n 0)
		(stream-first s)
		(stream-ref (stream-rest s) (- n 1))
	)
)

; This procedure displays a finite number of elements from the supplied stream
; as specified by 'count'
(define (display-stream-elements count s)
	(if (= 0 count)
		(begin
			(newline)
			'done
		)
		(begin
			(newline)
			(display (stream-first s))
			(display-stream-elements (- count 1) (stream-rest s))
		)
	)
)

(define (display-stream s)
	(stream-for-each display-line s)
)

(define (display-line x)
	(newline)
	(display x)
)

(define (div-series dividend-series divisor-series)
	(cond
		((= 0 (stream-first divisor-series))
			(error "Denominator should not have a zero constant: " (stream-first divisor-series))
		)
		(else
			(mul-series
				dividend-series
				(invert-unit-series (scale-stream divisor-series (/ 1 (stream-first divisor-series))))
			)
		)
	)
)

(define (invert-unit-series s)
	(stream-cons
		1
		(mul-series
			(scale-stream (stream-rest s) -1)
			(invert-unit-series s)
		)
	)
)

(define (mul-series s1 s2)
	(stream-cons
		; (a0 * b0) (The following is the constant term of the series resulting from
		; the multiplication)
		(* (stream-first s1) (stream-first s2))
		; The following is the rest of the series starting with the x^1 term
		(add-streams
			; {a0 * (b1x + b2x^2 + b3x^3 + ...)} +
			; {b0 * (a1x + a2x^2 + a3x^3 + ...)} +
			(add-streams
				(scale-stream (stream-rest s2) (stream-first s1))
				(scale-stream (stream-rest s1) (stream-first s2))
			)
			; {(a1x + a2x^2 + a3x^3 + ...) * (b1x + b2x^2 + b3x^3 + ...)}
			(stream-cons
				; 0 needs to be prepended to this stream so that the first term of the stream
				; is the x^1 term. Only then the outer add-streams will add like terms in the
				; two series supplied to it
				0
				(mul-series (stream-rest s1) (stream-rest s2))
			)
		)
	)
)

(define exp-series
	(stream-cons 1 (integrate-series exp-series))
)

; the integral of negative sine is cosine
(define cosine-series
	(stream-cons 1 (integrate-series (scale-stream sine-series -1)))
)

; the integral of cosine is sine
(define sine-series
	(stream-cons 0 (integrate-series cosine-series))
)

(define tan-series 
	(div-series sine-series cosine-series)
)

(define (integrate-series s)
	(div-streams s integers)
)

(define (add-streams s1 s2)
	(stream-map + s1 s2)
)

(define (div-streams s1 s2)
	(stream-map / s1 s2)
)

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams ones integers)))

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

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 1024 MB.
> (stream-ref (pairs integers integers) 0)
'(1 1)
> (stream-ref (pairs integers integers) 197)
'(1 100)
> (display-stream-elements 50 (pairs integers integers))

(1 1)
(1 2)
(2 2)
(1 3)
(2 3)
(1 4)
(3 3)
(1 5)
(2 4)
(1 6)
(3 4)
(1 7)
(2 5)
(1 8)
(4 4)
(1 9)
(2 6)
(1 10)
(3 5)
(1 11)
(2 7)
(1 12)
(4 5)
(1 13)
(2 8)
(1 14)
(3 6)
(1 15)
(2 9)
(1 16)
(5 5)
(1 17)
(2 10)
(1 18)
(3 7)
(1 19)
(2 11)
(1 20)
(4 6)
(1 21)
(2 12)
(1 22)
(3 8)
(1 23)
(2 13)
(1 24)
(5 6)
(1 25)
(2 14)
(1 26)
'done
> 
