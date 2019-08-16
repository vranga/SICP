#lang racket

; Exercise 3.72.  In a similar way to exercise 3.71 generate a stream of all numbers that
; can be written as the sum of two squares in three different ways (showing how they can
; be so written).

; S O L U T I O N

(define (pair-weight-sum-of-squares pair)
	(+ (square (car pair)) (square (cadr pair)))
)

(define (pair-weight-ramanujan pair)
	(+ (cube (car pair)) (cube (cadr pair)))
)

(define (extract-three-consecutive-pairs-with-same-weight stream-of-pairs weight-proc)
	(let ((p1 (stream-first stream-of-pairs))
		  (p2 (stream-first (stream-rest stream-of-pairs)))
		  (p3 (stream-first (stream-rest (stream-rest stream-of-pairs)))))
		(let ((wp1 (weight-proc p1)) (wp2 (weight-proc p2)) (wp3 (weight-proc p3)))
			(cond
				((and (= wp1 wp2) (= wp2 wp3))
					(stream-cons
						(list wp1 p1 p2 p3)
						(extract-three-consecutive-pairs-with-same-weight
							(stream-rest stream-of-pairs)
							weight-proc
						)
					)
				)
				((and (not (= wp1 wp2)) (= wp2 wp3))
					(extract-three-consecutive-pairs-with-same-weight
						(stream-rest stream-of-pairs)
						weight-proc
					)
				)
				(else
					(extract-three-consecutive-pairs-with-same-weight
						(stream-rest (stream-rest stream-of-pairs))
						weight-proc
					)
				)
			)
		)
	)
)

(define (extract-consecutive-pairs-with-same-weight stream-of-pairs weight-proc)
	(let ((p1 (stream-first stream-of-pairs)) (p2 (stream-first (stream-rest stream-of-pairs))))
		(if (= (weight-proc p1) (weight-proc p2))
			(stream-cons
				(list (weight-proc p1) p1 p2)
				(extract-consecutive-pairs-with-same-weight
					(stream-rest stream-of-pairs)
					weight-proc
				)
			)
			(extract-consecutive-pairs-with-same-weight
				(stream-rest stream-of-pairs)
				weight-proc
			)
		)
	)
)

; This procedure displays 'count' number of elements from the supplied stream
; of pairs along with the weight of each pair
(define (display-pairs-with-weight stream-of-pairs count weight-proc)
	(if (= 0 count)
		(begin
			(newline)
			'done
		)
		(begin
			(newline)
			(display (stream-first stream-of-pairs))
			(display " ")
			(display (weight-proc (stream-first stream-of-pairs)))
			(display-pairs-with-weight (stream-rest stream-of-pairs) (- count 1) weight-proc)
		)
	)
)

(define (weighted-pairs s t weight)
	(stream-cons
		(list (stream-first s) (stream-first t))
		(merge-weighted
			(stream-map (lambda (x) (list (stream-first s) x)) (stream-rest t))
			(weighted-pairs (stream-rest s) (stream-rest t) weight)
			weight
		)
	)
)

(define (merge-weighted s1 s2 weight)
	(cond
		((stream-empty? s1) s2)
		((stream-empty? s2) s1)
		(else
			(let ((s1car (stream-first s1)) (s2car (stream-first s2)))
				(cond
					((< (weight s1car) (weight s2car))
						(stream-cons s1car (merge-weighted (stream-rest s1) s2 weight))
					)
					((> (weight s1car) (weight s2car))
						(stream-cons s2car (merge-weighted s1 (stream-rest s2) weight))
					)
					(else
						(stream-cons
							s1car
							(stream-cons
								s2car
								(merge-weighted (stream-rest s1) (stream-rest s2) weight)
							)
						)
					)
				)
			)
		)
	)
)

; The following procedure expects two ascending infinite streams,
; one of which needs to be a subset of the other. Let S be the stream with the superset
; and let T be the stream with the subset. Then this procedure will produce a new stream
; that contains all those elements that are present in S but not in T
(define (s-minus-t s t)
	; Assumes that S and T are ordered ascending and T is a subset of S
	(cond
		((stream-empty? t) s)
		((stream-empty? s) empty-stream)
		(else
			(let ((scar (stream-first s)) (tcar (stream-first t)))
				(cond
					((< scar tcar)
						(stream-cons
							scar
							(s-minus-t (stream-rest s) t)
						)
					)
					((> scar tcar)
						(s-minus-t s (stream-rest t))
					)
					(else
						(s-minus-t (stream-rest s) (stream-rest t))
					)
				)
			)
		)
	)
)

; The following proc copied from SICP Exercise 3.56
(define (merge s1 s2)
	(cond
		((stream-empty? s1) s2)
		((stream-empty? s2) s1)
		(else
			(let ((s1car (stream-first s1)) (s2car (stream-first s2)))
				(cond
					((< s1car s2car)
						(stream-cons s1car (merge (stream-rest s1) s2))
					)
					((> s1car s2car)
						(stream-cons s2car (merge s1 (stream-rest s2)))
					)
					(else
						(stream-cons s1car
							(merge (stream-rest s1) (stream-rest s2))
						)
					)
				)
			)
		)
	)
)

; Implementation of procedure triples
; The infinite streams S, T and U are represented as follows:

; S0		T0		U0
; S1		T1		U1
; S2		T2		U2
; S3		T3		U3
; S4		T4		U4
; .		.		.
; .		.		.
; .		.		.

; All triples (Si, Tj, Uk) such that i < j < k can be produced by combining the following:

; 1.	(S0, T1, U2)
; 2.	S0 combined with all the pairs produced using the streams (T1, T2, T3, ...) and
; 		(U1, U2, U3, ...) such that for every pair (Tj, Uk), j < k. We need to exclude the 
; 		first element from this combined stream because the first element will be
; 		(S0, T1, U2) which is already accounted for.
; 3. 	triples called recursively on the streams (S1, S2, ...) (T1, T2, ...) and (U1, U2, ...)

(define (triples s t u)
	(stream-cons
		(list
			(stream-first s)
			(stream-first (stream-rest t))
			(stream-first (stream-rest (stream-rest u)))
		)
		(interleave
			(stream-map
				(lambda (x) (cons (stream-first s) x))
				(stream-rest (less-than-pairs (stream-rest t) (stream-rest u)))
			)
			(triples (stream-rest s) (stream-rest t) (stream-rest u))
		)
	)
)

(define (is-pythagorean-triple? triple)
	; This procedure assumes that the elements in the triple are in increasing order 
	(= (+ (square (car triple)) (square (car (cdr triple)))) (square (car (cdr (cdr triple)))))
)

; This procedures produces all pairs (Si, Tj) where i < j
(define (less-than-pairs s t)
	(stream-cons
		(list (stream-first s) (stream-first (stream-rest t)))
		(interleave
			(stream-map (lambda (x) (list (stream-first s) x)) (stream-rest (stream-rest t)))
			(less-than-pairs (stream-rest s) (stream-rest t))
		)
	)
)

(define (all-pairs s t)
	(stream-cons
		(list (stream-first s) (stream-first t))
		(interleave
			(stream-map (lambda (x) (list (stream-first s) x)) (stream-rest t))
			(interleave
				(stream-map (lambda (x) (list x (stream-first t))) (stream-rest s))
				(all-pairs (stream-rest s) (stream-rest t))
			)
		)
	)
)

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
(define (cube x) (* x x x))

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

(define (stream-filter pred stream)
	(cond
		((stream-empty? stream) empty-stream)
		((pred (stream-first stream))
			(stream-cons
				(stream-first stream)
				(stream-filter pred (stream-rest stream))
			)
		)
		(else
			(stream-filter pred (stream-rest stream))
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

; This procedure produces a stream with a maximum number of count elements from
; the supplied stream
(define (truncate-stream s count)
	(if (<= count 0)
		empty-stream
		(stream-cons
			(stream-first s)
			(truncate-stream (stream-rest s) (- count 1))
		)
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

(define (indent-and-display pair)
	(display-spaces (car pair))
	(display pair)
	(newline)
)

(define (display-spaces n)
	(if (<= n 0)
		(void)
		(begin
			(display " ")
			(display-spaces (- n 1))
		)
	)
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
Language: racket, with debugging; memory limit: 4096 MB.
> (define P (weighted-pairs integers integers pair-weight-sum-of-squares))
> (define Q (extract-three-consecutive-pairs-with-same-weight P pair-weight-sum-of-squares))
> (stream-ref Q 0)
'(325 (1 18) (6 17) (10 15))
> (stream-ref Q 1)
'(425 (5 20) (8 19) (13 16))
> (display-stream-elements 20 Q)

(325 (1 18) (6 17) (10 15))
(425 (5 20) (8 19) (13 16))
(650 (5 25) (11 23) (17 19))
(725 (7 26) (10 25) (14 23))
(845 (2 29) (13 26) (19 22))
(850 (3 29) (11 27) (15 25))
(925 (5 30) (14 27) (21 22))
(1025 (1 32) (8 31) (20 25))
(1105 (4 33) (9 32) (12 31))
(1105 (9 32) (12 31) (23 24))
(1250 (5 35) (17 31) (25 25))
(1300 (2 36) (12 34) (20 30))
(1325 (10 35) (13 34) (22 29))
(1445 (1 38) (17 34) (22 31))
(1450 (9 37) (15 35) (19 33))
(1525 (2 39) (9 38) (25 30))
(1625 (5 40) (16 37) (20 35))
(1625 (16 37) (20 35) (28 29))
(1690 (3 41) (13 39) (27 31))
(1700 (10 40) (16 38) (26 32))
'done
> 
