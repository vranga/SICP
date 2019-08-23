#lang racket

; Exercise 3.80.  A series RLC circuit consists of a resistor, a capacitor, and an inductor
; connected in series, as shown in figure 3.36. If R, L, and C are the resistance,
; inductance, and capacitance, then the relations between voltage (v) and current (i) for
; the three components are described by the equations

; vR = (iR)R
; vL = L(diL/dt)
; iC = C(dvC/dt)

; and the circuit connections dictate the relations

; iR = iL = -iC
; vC = vL + vR

; Combining these equations shows that the state of the circuit (summarized by vC, the
; voltage across the capacitor, and iL, the current in the inductor) is described by the
; pair of differential equations

; dvC/dt = -iL/C
; diL/dt = (1/L)vC - (R/L)iL

; The signal-flow diagram representing this system of differential equations is shown in
; figure 3.37.

; Write a procedure RLC that takes as arguments the parameters R, L, and C of the circuit
; and the time increment dt. In a manner similar to that of the RC procedure of exercise
; 3.73, RLC should produce a procedure that takes the initial values of the state
; variables, vC0 and iL0, and produces a pair (using cons) of the streams of states vC and
; iL. Using RLC, generate the pair of streams that models the behavior of a series RLC
; circuit with R = 1 ohm, C = 0.2 farad, L = 1 henry, dt = 0.1 second, and initial values
; iL0 = 0 amps and vC0 = 10 volts.

; S O L U T I O N

(define (RLC R L C dt)
	(lambda (vC0 iL0)
		(define vC (integral (delay dvC) vC0 dt))
		(define iL (integral (delay diL) iL0 dt))
		(define dvC (scale-stream iL (* -1.0 C)))
		(define diL
			(add-streams
				(scale-stream vC (/ 1.0 L))
				(scale-stream iL (* -1.0 (/ R L)))
			)
		)
		(cons vC iL)
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

(define (solve-general-2nd f dt y0 dy0)
	(define y (integral (delay dy) y0 dt))
	(define dy (integral (delay ddy) dy0 dt))
	(define ddy
		(f dy y)
	)
	y
)

(define (solve-2nd a b dt y0 dy0)
	(define y (integral (delay dy) y0 dt))
	(define dy (integral (delay ddy) dy0 dt))
	(define ddy
		(add-streams
			(scale-stream dy a)
			(scale-stream y b)
		)
	)
	y
)

(define (integral delayed-integrand initial-value dt)
	(stream-cons
		initial-value
		(let ((integrand (force delayed-integrand)))
			(if (stream-empty? integrand)
				empty-stream
				(integral
					(stream-rest integrand)
					(+ (* dt (stream-first integrand))
					initial-value)
					dt
				)
			)
		)
	)
)

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
> (define RLC1 (RLC 1 1 0.2 0.1))
> (define vCiLPair (RLC1 10 0))
> (display-stream-elements (car vCiLPair) 100)

10
10
9.98
9.942
9.88784
9.819212
9.73767112
9.644645904
9.54144786736
9.429280342576
9.30924667453568
9.18235781261424
9.049539343535873
8.911638005740114
8.76942772303686
8.62361519259245
8.474845059746407
8.323704709799784
8.170728704728331
8.016402890744423
7.861168200749449
7.705424173972484
7.549532213471717
7.3938186006730815
7.2385772847273655
7.084072463174875
6.930540969208179
6.778194479711803
6.6272215572266475
6.477789538030585
6.330046277639675
6.184121764211795
6.040129609571424
5.898168426866666
5.7583231032132405
5.620665975071424
5.485257913537364
5.3521493262065665
5.221381081781774
5.092985363147048
4.966986454212231
4.843401465444601
4.72224100264531
4.603509783195059
4.487207203684542
4.373327862558686
4.2618620411380475
4.152796146134355
4.046113116548756
3.941792797629448
3.839812284368973
3.740146236839287
3.6427671694938315
3.547645716409243
3.4547508742941258
3.3640502249577016
3.2755101388063315
3.189095960820183
3.1047721803550368
3.022502586014765
2.94225040674781
2.8639784402355213
2.7876491695609658
2.713224869073395
2.640667700295459
2.56993979865717
2.501003351782119
2.4338206699972584
2.36835424968732
2.3045668300683806
2.2424214439119607
2.181881462711046
2.122910636742399
2.0654731304451945
2.009533553504226
1.9550569879964634
1.9020090119324689
1.850355719498881
1.8000637382847866
1.751100243753104
1.70343297119802
1.6570302254109384
1.6118608882601688
1.5678944243736543
1.5251008850992709
1.4834509109035785
1.4429157323572568
1.4034671698437602
1.3650776321168987
1.327720113823036
1.2913681920943256
1.2559960223108402
1.2215783331215146
1.1880904208065
1.1555081430567438
1.1238079122403501
1.0929666882194824
1.0629619707762208
1.0337717917008464
1.005374706591457
'done
> (display-stream-elements (cdr vCiLPair) 100)

0
1.0
1.9
2.708
3.4314
4.077044
4.6512608
5.159901832
5.6083762392
6.001683402016
6.344443096072
6.640923453918369
6.895066889787956
7.110514135162748
7.290626522220484
7.438506642302122
7.557017497331155
7.648800253572681
7.716290699195391
7.761734499748686
7.787201338848259
7.794598025038378
7.785680639931789
7.762065797285781
7.725241077624511
7.676574698334797
7.617324474818805
7.5486461242577425
7.4716009598031485
7.387163019545499
7.296225671394008
7.199607732018574
7.0980591352378966
6.9922661826712496
6.882856407090792
6.770403076703037
6.655429366539876
6.538412221239624
6.419785931736318
6.299945446740864
6.179249438381483
6.058023139964558
5.936560972512562
5.815128975525837
5.693967056292759
5.573291071031937
5.453294750184612
5.334151479279956
5.216015945965395
5.099025663023731
4.9833023764843025
4.86895336727277
4.756072654229421
4.644742105755863
4.5350324668212005
4.4270043075684935
4.320708899307414
4.216189023257305
4.113479717013593
4.012608963347738
3.9135983256144407
3.8164635337277777
3.721215024378552
3.6278584388967934
3.5363950819144536
3.446822343752554
3.3591340892430157
3.273321015496926
3.1893709809469595
3.1072693078209954
3.0269990600457337
2.9485412984323562
2.871875314860225
2.7969788470484427
2.723828275388118
2.6523988031997288
2.582664621679402
2.514599060704709
2.448174726584126
2.3833636277541923
2.3201372893540837
2.2584668575384774
2.1983231943257233
2.1396769637191677
2.082498709784616
2.0267589273160818
1.9724281256748315
1.919476886343074
1.8678759146931427
1.8175960864355183
1.76860848917427
1.7208844594662756
1.674395615750732
1.6291138874878104
1.5850115408196794
1.5420612010433858
1.5002358721630822
1.4595089537687223
1.4198542554694722
1.3812460090926095
'done
> 
