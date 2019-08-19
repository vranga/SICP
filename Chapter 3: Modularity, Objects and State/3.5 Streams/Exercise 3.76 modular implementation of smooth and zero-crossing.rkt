#lang racket

; Exercise 3.76.  Eva Lu Ator has a criticism of Louis's approach in exercise 3.75. The
; program he wrote is not modular, because it intermixes the operation of smoothing with
; the zero-crossing extraction. For example, the extractor should not have to be changed
; if Alyssa finds a better way to condition her input signal. Help Louis by writing a
; procedure smooth that takes a stream as input and produces a stream in which each
; element is the average of two successive input stream elements. Then use smooth as a
; component to implement the zero-crossing detector in a more modular style.

; S O L U T I O N

; See in the tests below that the 'smooth' procedure after processing the parabolic 
; stream from Exercise 3.75, produces the same stream as what we saw produced by the
; corrected procedure in Exercise 3.75. The new procedures are also tested on a random
; value stream after this.

(define (smooth input-stream)
	(make-stream-of-averages input-stream)
)

(define (make-zero-crossings input-stream)
	(stream-cons
		(sign-change-detector
			(stream-first input-stream)
			(stream-first (stream-rest input-stream))
		)
		(make-zero-crossings (stream-rest input-stream)) 
	)
)

(define (make-stream-of-averages input-stream)
	(let ((avpt (/ (+ (stream-first input-stream) (stream-first (stream-rest input-stream))) 2)))
		(stream-cons
			avpt
			(make-stream-of-averages (stream-rest input-stream))
		)
	)
)

; Proc to create a stream from a list
(define (make-stream-from-list l)
	(if (not (null? l))
		(stream-cons
			(car l)
			(make-stream-from-list (cdr l))
		)
		empty-stream
	)
)

; Proc to create a stream of values from a function y = f(x)
(define (make-stream-from-function f starting-x-value x-increment)
	(stream-cons
		(f starting-x-value)
		(make-stream-from-function f (+ starting-x-value x-increment) x-increment)
	)
)

; Proc to create a stream of random values in the range (min, max)
(define (make-random-value-stream range-min range-max)
	(if (<= range-max range-min)
		(error "make-random-value-stream: range max needs to be larger than range-min")
		(stream-cons
			(+ (* (random) (- range-max range-min)) range-min)
			(make-random-value-stream range-min range-max)
		)
	)
)

(define sense-data (make-stream-from-function sin 0 0.5))

(define (sign-change-detector a b)
	(cond
		((and (< a 0) (>= b 0))
			1
		)
		((and (>= a 0) (< b 0))
			-1
		)
		(else
			0
		)
	)
)

(define (RC resistance capacitance dt)
	(lambda (current-values-stream initial-capacitor-voltage)
		(add-streams
			(scale-stream current-values-stream resistance)
			(integral
				(scale-stream current-values-stream (/ 1.0 capacitance))
				initial-capacitor-voltage
				dt
			)
		)
	)
)

(define (integral integrand initial-value dt)
	(define int
		(stream-cons
			initial-value
			(add-streams (scale-stream integrand dt) int)
		)
	)
	int
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
(define (display-stream-elements s count)
	(if (= 0 count)
		(begin
			(newline)
			'done
		)
		(begin
			(newline)
			(display (stream-first s))
			(display-stream-elements (stream-rest s) (- count 1))
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
> (define parabolic-stream (make-stream-from-function (lambda (x) (- (* x x) 15)) 0 0.1))
> (define ss (smooth parabolic-stream))
> (display-stream-elements ss 200)

-14.995000000000001
-14.975000000000001
-14.935
-14.875
-14.795
-14.695
-14.575
-14.434999999999999
-14.274999999999999
-14.094999999999999
-13.895
-13.675
-13.435
-13.175
-12.895
-12.594999999999999
-12.274999999999999
-11.934999999999999
-11.574999999999998
-11.194999999999997
-10.794999999999998
-10.374999999999996
-9.934999999999997
-9.474999999999996
-8.994999999999996
-8.494999999999996
-7.974999999999994
-7.434999999999994
-6.874999999999993
-6.294999999999992
-5.694999999999991
-5.074999999999991
-4.43499999999999
-3.774999999999989
-3.094999999999988
-2.394999999999987
-1.6749999999999865
-0.9349999999999854
-0.17499999999998384
0.6050000000000155
1.4050000000000136
2.2250000000000103
3.0650000000000066
3.9250000000000043
4.8050000000000015
5.704999999999998
6.624999999999995
7.564999999999991
8.524999999999988
9.504999999999985
10.50499999999998
11.524999999999975
12.564999999999973
13.624999999999968
14.704999999999963
15.804999999999959
16.924999999999955
18.064999999999948
19.22499999999994
20.404999999999937
21.604999999999933
22.82499999999993
24.064999999999923
25.324999999999918
26.60499999999991
27.904999999999905
29.2249999999999
30.56499999999989
31.924999999999887
33.30499999999988
34.70499999999987
36.124999999999865
37.564999999999856
39.02499999999985
40.504999999999846
42.00499999999984
43.52499999999983
45.06499999999982
46.624999999999815
48.20499999999981
49.804999999999794
51.424999999999784
53.06499999999978
54.72499999999977
56.40499999999976
58.104999999999755
59.82499999999975
61.564999999999735
63.324999999999726
65.10499999999972
66.90499999999972
68.7249999999997
70.56499999999969
72.42499999999967
74.30499999999967
76.20499999999966
78.12499999999966
80.06499999999964
82.02499999999964
84.00499999999963
86.0049999999996
88.02499999999958
90.06499999999957
92.12499999999957
94.20499999999956
96.30499999999955
98.42499999999953
100.56499999999951
102.72499999999951
104.90499999999949
107.10499999999948
109.32499999999948
111.56499999999946
113.82499999999945
116.10499999999944
118.40499999999942
120.7249999999994
123.06499999999939
125.42499999999939
127.80499999999938
130.20499999999936
132.62499999999932
135.06499999999932
137.5249999999993
140.0049999999993
142.50499999999928
145.02499999999927
147.56499999999926
150.12499999999923
152.70499999999922
155.3049999999992
157.9249999999992
160.56499999999917
163.22499999999917
165.90499999999915
168.60499999999914
171.32499999999914
174.0649999999991
176.82499999999908
179.60499999999905
182.40499999999906
185.22499999999903
188.06499999999903
190.924999999999
193.80499999999898
196.70499999999896
199.62499999999892
202.56499999999892
205.5249999999989
208.50499999999892
211.50499999999886
214.52499999999884
217.56499999999886
220.6249999999988
223.7049999999988
226.80499999999876
229.92499999999876
233.06499999999875
236.22499999999872
239.40499999999872
242.60499999999868
245.82499999999868
249.06499999999872
252.32499999999877
255.6049999999988
258.90499999999884
262.2249999999989
265.5649999999989
268.92499999999893
272.30499999999904
275.704999999999
279.1249999999991
282.56499999999915
286.0249999999992
289.5049999999992
293.0049999999993
296.5249999999993
300.0649999999994
303.62499999999943
307.2049999999995
310.8049999999995
314.4249999999996
318.0649999999996
321.7249999999997
325.4049999999997
329.1049999999998
332.8249999999998
336.5649999999998
340.32499999999993
344.10499999999996
347.90500000000003
351.7250000000001
355.56500000000017
359.4250000000002
363.3050000000003
367.20500000000027
371.12500000000034
375.0650000000004
379.0250000000005
383.00500000000056
'done
> (define rvs (make-random-value-stream -2 2))
> (define smooth-stream (smooth rvs))
> (define zc (make-zero-crossings smooth-stream))
> (display-stream-elements rvs 50)

0.15454248807971327
-1.1313917830887927
0.6389028460001089
-0.7349003033850487
0.7339807247435655
-0.22080446731469783
0.6533172288653404
0.9455499492293202
-0.24258318880044438
0.7522458686649665
1.9979084310040238
-1.4593455641399782
1.6173166484576336
-1.1175854588993301
-0.9041542774215547
-0.42516404679839526
-0.800569265735896
0.40465313013825854
-0.27701063305563545
-1.0263328276288761
-1.922710260358577
0.2810963435256948
-1.0417971156290267
1.560173759357106
-0.6807106979162956
0.880985750640984
-1.9529280723559295
-1.8263014331158944
1.2207340630499384
0.08693059582299645
-1.3962138831644522
0.6336921816244665
0.34497422719249515
-1.7739635158759568
-1.633913258056612
1.4688645530314717
-0.7289219786449734
-1.2585316472161985
-0.34392762173361713
0.45709589381607874
0.17449118948871467
-0.09720315323636286
0.9133686185769445
-0.4108638962404081
-1.80594011387684
-0.005305777560826508
-1.8175413827525004
-1.962396893691866
0.5411806610798413
-1.5428365070628918
'done
> (display-stream-elements smooth-stream 50)

-0.48842464750453973
-0.2462444685443419
-0.047998728692469905
-0.0004597893207416437
0.2565881287144338
0.21625638077532128
0.7994335890473303
0.3514833802144379
0.25483133993226104
1.3750771498344951
0.2692814334320228
0.07898554215882769
0.2498655947791517
-1.0108698681604424
-0.664659162109975
-0.6128666562671456
-0.19795806779881875
0.06382124854131155
-0.6516717303422558
-1.4745215439937267
-0.8208069584164411
-0.38035038605166593
0.2591883218640396
0.43973153072040516
0.10013752636234419
-0.5359711608574728
-1.889614752735912
-0.30278368503297803
0.6538323294364674
-0.6546416436707279
-0.38126085076999283
0.4893332044084808
-0.7144946443417308
-1.7039383869662843
-0.08252435251257018
0.3699712871932491
-0.993726812930586
-0.8012296344749078
0.056584136041230804
0.3157935416523967
0.038644018126175905
0.4080827326702908
0.2512523611682682
-1.108402005058624
-0.9056229457188333
-0.9114235801566635
-1.8899691382221833
-0.7106081163060124
-0.5008279229915252
-0.5990895681573613
'done
> (display-stream-elements zc 50)

0
0
0
1
0
0
0
0
0
0
0
0
-1
0
0
0
1
-1
0
0
0
1
0
0
-1
0
0
1
-1
0
1
-1
0
0
1
-1
0
1
0
0
0
0
-1
0
0
0
0
0
0
1
'done
> 
