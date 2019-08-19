#lang racket

; Exercise 3.75.  Unfortunately, Alyssa's zero-crossing detector in exercise 3.74 proves to be
; insufficient, because the noisy signal from the sensor leads to spurious zero crossings.
; Lem E. Tweakit, a hardware specialist, suggests that Alyssa smooth the signal to filter out
; the noise before extracting the zero crossings. Alyssa takes his advice and decides to
; extract the zero crossings from the signal constructed by averaging each value of the sense
; data with the previous value. She explains the problem to her assistant, Louis Reasoner, who
; attempts to implement the idea, altering Alyssa's program as follows:

; (define (make-zero-crossings input-stream last-value)
;   (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;     (cons-stream (sign-change-detector avpt last-value)
;                  (make-zero-crossings (stream-cdr input-stream)
;                                       avpt))))

; This does not correctly implement Alyssa's plan. Find the bug that Louis has installed and
; fix it without changing the structure of the program. (Hint: You will need to increase the
; number of arguments to make-zero-crossings.)

; S O L U T I O N

; Explanation: Louis Reasoner's logic does not average the right quantities. Instead of
; averaging each value of the sensor data with the previous value, it averages each value
; with the previous *average*. So the smoothed signal will gradually drift away from the
; raw sensor signal.
; The attached graphs show how the different signals compare with each other. The first
; graph shows what happens when we do the two types of average-smoothing of a stream of
; random values. We can see that my implementation more closely tracks the raw signal
; than Louis' implementation.
;
; The second graph shows the same graphs for a values produced by a parabolic function:
; f(x) = x^2 - 15
; It is easy to see how Louis's stream of average values gradually diverges from the 
; raw signal whereas my average stream tracks the raw stream more closely.

(define (make-zero-crossings input-stream last-avpt)
	(let ((avpt (/ (+ (stream-first input-stream) (stream-first (stream-rest input-stream))) 2)))
		(stream-cons
			(sign-change-detector avpt last-avpt)
			(make-zero-crossings (stream-rest input-stream) avpt)
		)
	)
)

(define (louis-reasoner-make-zero-crossings input-stream last-value)
	(let ((avpt (/ (+ (stream-first input-stream) last-value) 2)))
		(stream-cons
			(sign-change-detector avpt last-value)
			(louis-reasoner-make-zero-crossings (stream-rest input-stream) avpt)
		)
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

(define (louis-reasoner-make-stream-of-averages input-stream last-value)
	(let ((avpt (/ (+ (stream-first input-stream) last-value) 2)))
		(stream-cons
			avpt
			(louis-reasoner-make-stream-of-averages (stream-rest input-stream) avpt)
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
> (define rvs (make-random-value-stream -5 5))
> (define lr-avgs-s (louis-reasoner-make-stream-of-averages rvs 0))
> (define avgs-s (make-stream-of-averages rvs))
> (display-stream-elements rvs 50)

4.563160468623364
3.769919600836765
-1.6915989764622847
4.825967087829756
-2.8111305261764556
-1.801123806422984
4.793097962849862
0.7226744481167486
-1.787324294392823
-0.26453510276584424
-2.4166203715505628
1.6865422019736798
2.400155437465835
-4.399427376939192
-0.3601551300176107
2.5001056282832224
-0.9243701543353939
3.539587614646699
1.1303704406872983
-3.748757976512811
3.606897115296359
-3.7391895306649205
1.7065394983999935
-4.91907363132744
1.9659774096038438
-3.339404236664083
-3.3313225123367927
0.7554898008568864
-0.7154840204912878
4.283413044398166
4.215847974851815
-0.4138006610028757
4.4934167956539195
-3.6474506833287283
-2.7262837456229647
-3.3152802613513295
-4.769498855354209
-2.345250104044569
-2.6798053545410543
1.5004335022741397
-1.068299059338449
-2.864620733503511
-4.341420331740618
-4.692082739424242
-1.6321964816881502
3.289632753991432
-3.8285435797500105
-2.923811468796987
-2.7784701152522544
-4.806255921651896
'done
> (display-stream-elements lr-avgs-s 50)

2.281580234311682
3.0257499175742235
0.6670754705559694
2.746521279192863
-0.0323046234917963
-0.9167142149573901
1.9381918739462356
1.3304331610314921
-0.2284455666806654
-0.24649033472325482
-1.3315553531369089
0.17749342441838545
1.2888244309421102
-1.5553014729985408
-0.9577283015080758
0.7711886633875733
-0.07659074547391032
1.7314984345863942
1.4309344376368462
-1.1589117694379825
1.2239926729291883
-1.257598428867866
0.2244705347660637
-2.3473015482806883
-0.19066206933842222
-1.7650331530012526
-2.5481778326690225
-0.896344015906068
-0.8059140181986779
1.7387495130997443
2.97729874397578
1.281749041486452
2.887582918570186
-0.3799338823792713
-1.553108814001118
-2.4341945376762237
-3.6018466965152163
-2.973548400279893
-2.8266768774104736
-0.6631216875681669
-0.8657103734533079
-1.8651655534784095
-3.1032929426095137
-3.897687841016878
-2.764942161352514
0.262345296319459
-1.7830991417152757
-2.3534553052561313
-2.565962710254193
-3.6861093159530447
'done
> (display-stream-elements avgs-s 50)

4.1665400347300645
1.0391603121872401
1.5671840556837358
1.0074182808266503
-2.30612716629972
1.4959870782134388
2.757886205483305
-0.5323249231380371
-1.0259296985793336
-1.3405777371582035
-0.3650390847884415
2.0433488197197573
-0.9996359697366786
-2.3797912534784014
1.0699752491328058
0.7878677369739142
1.3076087301556525
2.3349790276669986
-1.3091937679127563
-0.0709304306082259
-0.0661462076842807
-1.0163250161324635
-1.6062670664637233
-1.4765481108617982
-0.6867134135301196
-3.335363374500438
-1.2879163557399531
0.020002890182799327
1.7839645119534393
4.249630509624991
1.9010236569244698
2.039808067325522
0.42298305616259557
-3.1868672144758463
-3.020782003487147
-4.042389558352769
-3.557374479699389
-2.5125277292928114
-0.5896859261334573
0.2160672214678454
-1.96645989642098
-3.6030205326220646
-4.51675153558243
-3.162139610556196
0.8287181361516409
-0.2694554128792892
-3.3761775242734986
-2.851140792024621
-3.7923630184520754
-3.1200506826794108
'done
> (define parabolic-stream (make-stream-from-function (lambda (x) (- (* x x) 15)) 0 0.1))
> (define lr-avgs-ps (louis-reasoner-make-stream-of-averages parabolic-stream 0))
> (define avgs-ps (make-stream-of-averages parabolic-stream))
> (display-stream-elements parabolic-stream 200)

-15
-14.99
-14.96
-14.91
-14.84
-14.75
-14.64
-14.51
-14.36
-14.19
-14.0
-13.790000000000001
-13.56
-13.31
-13.04
-12.75
-12.44
-12.11
-11.759999999999998
-11.389999999999997
-10.999999999999998
-10.589999999999998
-10.159999999999997
-9.709999999999997
-9.239999999999995
-8.749999999999996
-8.239999999999995
-7.709999999999995
-7.159999999999994
-6.589999999999993
-5.999999999999991
-5.389999999999992
-4.759999999999991
-4.109999999999989
-3.439999999999989
-2.7499999999999876
-2.0399999999999867
-1.3099999999999863
-0.5599999999999845
0.21000000000001684
1.0000000000000142
1.810000000000013
2.6400000000000077
3.4900000000000055
4.360000000000003
5.25
6.159999999999997
7.089999999999993
8.039999999999988
9.009999999999987
9.999999999999982
11.009999999999977
12.039999999999974
13.089999999999971
14.159999999999965
15.249999999999961
16.359999999999957
17.489999999999952
18.639999999999944
19.80999999999994
20.999999999999936
22.20999999999993
23.439999999999927
24.68999999999992
25.959999999999916
27.249999999999908
28.559999999999903
29.889999999999894
31.23999999999989
32.609999999999886
33.99999999999988
35.40999999999987
36.83999999999986
38.28999999999986
39.75999999999985
41.249999999999844
42.759999999999835
44.28999999999982
45.83999999999982
47.40999999999981
48.9999999999998
50.609999999999786
52.23999999999978
53.88999999999977
55.55999999999976
57.24999999999976
58.95999999999975
60.68999999999974
62.43999999999973
64.20999999999972
65.99999999999972
67.8099999999997
69.63999999999969
71.48999999999968
73.35999999999967
75.24999999999966
77.15999999999966
79.08999999999965
81.03999999999964
83.00999999999962
84.99999999999962
87.0099999999996
89.03999999999958
91.08999999999958
93.15999999999957
95.24999999999956
97.35999999999954
99.48999999999953
101.63999999999952
103.8099999999995
105.99999999999949
108.20999999999948
110.43999999999947
112.68999999999946
114.95999999999944
117.24999999999943
119.5599999999994
121.88999999999939
124.23999999999938
126.60999999999939
128.99999999999937
131.40999999999934
133.83999999999932
136.2899999999993
138.7599999999993
141.2499999999993
143.75999999999928
146.28999999999925
148.83999999999924
151.40999999999923
153.9999999999992
156.6099999999992
159.23999999999918
161.88999999999916
164.55999999999915
167.24999999999915
169.95999999999913
172.68999999999912
175.4399999999991
178.20999999999907
180.99999999999906
183.80999999999904
186.63999999999902
189.48999999999901
192.359999999999
195.24999999999898
198.15999999999894
201.08999999999892
204.0399999999989
207.0099999999989
209.9999999999989
213.00999999999885
216.03999999999886
219.08999999999884
222.1599999999988
225.24999999999878
228.35999999999876
231.48999999999876
234.63999999999874
237.80999999999872
240.9999999999987
244.20999999999867
247.4399999999987
250.68999999999875
253.9599999999988
257.2499999999988
260.55999999999887
263.8899999999989
267.23999999999893
270.609999999999
273.99999999999903
277.40999999999906
280.8399999999991
284.28999999999917
287.7599999999992
291.24999999999926
294.7599999999993
298.28999999999934
301.8399999999994
305.40999999999946
308.9999999999995
312.60999999999956
316.2399999999996
319.88999999999965
323.55999999999966
327.2499999999997
330.9599999999998
334.6899999999998
338.4399999999999
342.2099999999999
346.0
349.81000000000006
353.6400000000001
357.4900000000002
361.36000000000024
365.2500000000003
369.1600000000003
373.0900000000004
377.0400000000005
381.0100000000005
'done
> (display-stream-elements lr-avgs-ps 200)

-15/2
-11.245000000000001
-13.102500000000001
-14.006250000000001
-14.423125
-14.5865625
-14.61328125
-14.561640624999999
-14.4608203125
-14.32541015625
-14.162705078125
-13.9763525390625
-13.76817626953125
-13.539088134765624
-13.289544067382812
-13.019772033691407
-12.729886016845704
-12.419943008422852
-12.089971504211425
-11.73998575210571
-11.369992876052855
-10.979996438026426
-10.569998219013211
-10.139999109506604
-9.6899995547533
-9.219999777376648
-8.729999888688322
-8.219999944344158
-7.689999972172076
-7.139999986086035
-6.569999993043012
-5.979999996521502
-5.369999998260747
-4.739999999130368
-4.089999999565178
-3.4199999997825827
-2.7299999998912847
-2.0199999999456355
-1.28999999997281
-0.5399999999863966
0.2300000000068088
1.0200000000034108
1.8300000000017094
2.6600000000008572
3.51000000000043
4.380000000000215
5.270000000000106
6.1800000000000495
7.110000000000019
8.060000000000002
9.029999999999992
10.019999999999985
11.02999999999998
12.059999999999976
13.109999999999971
14.179999999999966
15.26999999999996
16.379999999999956
17.50999999999995
18.659999999999943
19.82999999999994
21.019999999999936
22.229999999999933
23.459999999999926
24.709999999999923
25.979999999999915
27.26999999999991
28.579999999999902
29.909999999999897
31.25999999999989
32.62999999999988
34.019999999999875
35.429999999999865
36.85999999999986
38.30999999999985
39.779999999999845
41.26999999999984
42.77999999999983
44.309999999999825
45.859999999999815
47.42999999999981
49.0199999999998
50.62999999999979
52.25999999999978
53.90999999999977
55.579999999999764
57.269999999999754
58.97999999999975
60.70999999999974
62.45999999999973
64.22999999999972
66.01999999999971
67.8299999999997
69.65999999999968
71.50999999999968
73.37999999999967
75.26999999999967
77.17999999999967
79.10999999999964
81.05999999999963
83.02999999999963
85.01999999999961
87.0299999999996
89.05999999999959
91.10999999999959
93.17999999999958
95.26999999999956
97.37999999999954
99.50999999999954
101.65999999999951
103.8299999999995
106.0199999999995
108.22999999999948
110.45999999999947
112.70999999999945
114.97999999999945
117.26999999999943
119.57999999999942
121.9099999999994
124.2599999999994
126.62999999999938
129.01999999999936
131.42999999999932
133.85999999999933
136.30999999999932
138.7799999999993
141.2699999999993
143.7799999999993
146.30999999999926
148.85999999999925
151.4299999999992
154.0199999999992
156.6299999999992
159.2599999999992
161.90999999999917
164.57999999999916
167.26999999999913
169.9799999999991
172.7099999999991
175.45999999999907
178.22999999999905
181.01999999999904
183.82999999999902
186.659999999999
189.509999999999
192.37999999999897
195.26999999999896
198.17999999999893
201.10999999999893
204.05999999999892
207.0299999999989
210.01999999999887
213.02999999999886
216.05999999999887
219.10999999999882
222.1799999999988
225.2699999999988
228.37999999999877
231.50999999999874
234.65999999999872
237.8299999999987
241.01999999999867
244.22999999999868
247.45999999999873
250.70999999999876
253.97999999999877
257.26999999999884
260.5799999999989
263.90999999999894
267.25999999999897
270.629999999999
274.019999999999
277.42999999999904
280.8599999999991
284.30999999999915
287.7799999999992
291.26999999999924
294.7799999999993
298.3099999999994
301.85999999999945
305.4299999999995
309.0199999999995
312.62999999999954
316.2599999999996
319.9099999999996
323.5799999999997
327.26999999999975
330.9799999999998
334.7099999999998
338.45999999999987
342.2299999999999
346.02
349.83000000000004
353.6600000000001
357.51000000000016
361.3800000000002
365.27000000000027
369.1800000000003
373.11000000000035
377.0600000000004
'done
> (display-stream-elements avgs-ps 200)

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
> 
