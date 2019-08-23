#lang racket

; Exercise 3.78.  

; Consider the problem of designing a signal-processing system to study the homogeneous
; second-order linear differential equation

; (d^2y/dt^2) - a (dy/dt) - by = 0

; The output stream, modeling y, is generated by a network that contains a loop. This is
; because the value of d^2y/dt^2 depends upon the values of y and dy/dt and both of these
; are determined by integrating d^2y/dt^2. The diagram we would like to encode is shown
; in figure 3.35. Write a procedure solve-2nd that takes as arguments the constants a, b,
; and dt and the initial values y0 and dy0 for y and dy/dt and generates the stream of
; successive values of y.

; S O L U T I O N

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

(define y1 (solve-2nd 1 2 0.01 0.2 0.5))
(define y2 (solve-2nd -1 2 0.01 0.2 0.5))
(define y3 (solve-2nd 1 -2 0.01 0.2 0.5))
(define y4 (solve-2nd -1 -2 0.01 0.2 0.5))

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 4096 MB.

> (display-stream-elements y1 100)
0.2
0.20500000000000002
0.21009000000000003
0.21527190000000002
0.22054763700000002
0.22591918575000003
0.23138855951490003
0.23695781085459902
0.242629032419598
0.24840435776241787
0.25428596216514987
0.26027606348346166
0.26637692300738963
0.27259084633925357
0.2789201842890376
0.28536733378758733
0.2919347388179804
0.29862489136543485
0.3054403323861275
0.3123836527953001
0.3194574944750417
0.3266645513021397
0.33400757019640376
0.34148935218987087
0.34911275351731197
0.35688068672846546
0.3647961218224339
0.37286208740468774
0.3810816718671286
0.38945802459167483
0.39799435717783993
0.40669394469478504
0.4155601269583351
0.42459630983345964
0.43380596656272713
0.44319263912125395
0.4527599395986786
0.4625115516087018
0.4724512317267449
0.4825828109562902
0.4929101962244763
0.5034373719075355
0.5141684013866702
0.5251074286349777
0.5362586798360457
0.5476264650348514
0.5592151798216123
0.5710293070492478
0.583073418585124
0.5953521770977688
0.607870337879257
0.6206327507039797
0.6336443617245254
0.6469102154054175
0.6604354564954633
0.6742253320394906
0.6882851934302574
0.7026204985013397
0.7172368136618189
0.7321398160736031
0.7473352958722376
0.7628291584320731
0.7786274266766814
0.7947362434354222
0.8111618738470857
0.827910707811553
0.8449892624904344
0.8624041848576669
0.8801622543010698
0.8982703852758782
0.916735630011295
0.9355651812711211
0.9547663751695478
0.9743466940432128
0.9943137693806485
1.0146753848102672
1.0354394791480581
1.0566141495061891
1.0782076544637311
1.1002284173007497
1.1226850292970312
1.1455862530967356
1.1689410261402964
1.1927584641649123
1.2170478647750023
1.2418187110840262
1.2670806754290953
1.292843623159832
1.3191176165029619
1.345912918504155
1.3732399970486606
1.4011095289623121
1.4295324041945099
1.458519730084822
1.4880828357148763
1.5182332763472481
1.5489828379530866
1.580343541830253
1.6123276493137815
1.6449476665805114
'done
> (display-stream-elements y2 100)
0.2
0.20500000000000002
0.20999
0.2149711
0.219944387
0.22491093534999998
0.22987180709389998
0.23482805230743098
0.23978070943024546
0.24473080559229327
0.24967935693460666
0.25462736892461535
0.2595758366661109
0.2645257452039764
0.2694780698237965
0.2744337763464592
0.27939382141786
0.2843591527938161
0.28933070962029617
0.29430942270907023
0.29929621480888063
0.30429200087223474
0.30929768831791704
0.314314177289317
0.3193423609086665
0.32438312552728044
0.3294373509718899
0.3345059107871588
0.33958967247446936
0.34468949772706425
0.3498062426616281
0.3549407580463917
0.36009388952584
0.3652664778421031
0.3704593590531087
0.3756733647475727
0.38090932225690266
0.38616805486408884
0.39145038200965454
0.3967571194947374
0.40208907968137136
0.4074470716900379
0.4128319015945541
0.4182443726143631
0.42368528530429295
0.42915543774184634
0.43465562571208505
0.4401866428901698
0.4457492810216161
0.45134433010032593
0.456972578544453
0.4626348133701589
0.4683318203633166
0.47406438424921676
0.4798332888603306
0.4856393173021831
0.4914832521173892
0.4973658754479036
0.5032879691955364
0.5092503151807825
0.5152536953000152
0.5212988916810917
0.5273866868374174
0.5335178638205161
0.5396932063711514
0.5459134990690443
0.5521795274812326
0.5584920783091127
0.5648519395342104
0.5712599005627189
0.5777167523688491
0.5842232876370306
0.590780300903004
0.5973885886938451
0.6040489496669584
0.6107621847480794
0.6175290972683225
0.6243504931003128
0.6312271807934369
0.6381599717082498
0.6451496801500732
0.6521971235018201
0.6593031223560796
0.6664685006464968
0.673694085778481
0.6809807087592746
0.6883292043274161
0.695740411081628
0.7032151716091632
0.7107543326136394
0.7183587450423927
0.7260292642133812
0.7337667499416682
0.7415720666655151
0.7494460835721118
0.7573896747229757
0.7654037191790454
0.773489101125499
0.7816467099963238
0.7898774405986655
'done
> (display-stream-elements y3 100)
0.2
0.20500000000000002
0.21001
0.2150291
0.220056389
0.22509094507
0.2301318354229
0.235178116490315
0.24022883400131959
0.24528302306413613
0.25033970825078056
0.25539790368467863
0.2604566131312655
0.26551483009158133
0.27057153789887406
0.27562570981822143
0.28067630914918246
0.28572228933148947
0.29076259405378974
0.2957961573654467
0.30082190379140944
0.30583874845015874
0.31084559717473725
0.3158413466368715
0.32082488447419216
0.3257950894205587
0.330750831439494
0.3356909718607346
0.34061436351989965
0.34551985090128423
0.3504062702837787
0.3552724498899178
0.3601172100380616
0.36493936329770876
0.36973771464794486
0.37451106163902376
0.37925819455708387
0.38397789659199677
0.3886689440083474
0.3933301063195431
0.3979601464650491
0.4025578209907463
0.40712188023240736
0.41165106850228694
0.4161441242788188
0.42059978039941553
0.4250167642563625
0.429393797995799
0.4337295987197787
0.43802287869139894
0.44227234554299144
0.4464767024873616
0.45063464853206686
0.4547448786967217
0.4588060842333167
0.4628169528495383
0.4667761689350754
0.470682413790898
0.4745343658614918
0.47833070097003333
0.48207009255648803
0.48575121191861326
0.48937272845584845
0.49293330991607226
0.4964316226452071
0.4998663318396501
0.5032361018015086
0.5065395961966176
0.5097754783153174
0.512942411335965
0.5160390585911558
0.5190640838366315
0.5220161515228436
0.5248939270691506
0.5276960771406161
0.5304212699273824
0.5330681754265882
0.5356354657268007
0.5381218152949299
0.540525901265595
0.5428464037329078
0.5450820060446406
0.5472313950987442
0.5492932616421798
0.5512663005720302
0.5531492112388505
0.5549406977522247
0.5566394692884847
0.558244240400557
0.5597537313298923
0.5611666683204408
0.5624817839346289
0.5636978173712948
0.5648135147855403
0.5658276296104541
0.5667389228806599
0.5675461635576456
0.5682481288568252
0.5688436045762849
0.5693313854271679
'done
> (display-stream-elements y4 100)
0.2
0.20500000000000002
0.20991
0.2147299
0.219459619
0.22409909482999998
0.2286482839779
0.23310716141535498
0.23747572042163986
0.2417539724055788
0.24594194672559402
0.250039690507928
0.2540472684630935
0.25796476270060575
0.2617922725420503
0.26552991433254025
0.2691778212506169
0.2727361431166463
0.27620504619976527
0.2795847130234297
0.2828753421696175
0.2860771480817388
0.28919036086630495
0.2922152260934091
0.29515200459606894
0.2980009722684835
0.3007624198632547
0.3034366527876245
0.3060239908987779
0.3085247682982623
0.3109393331255721
0.3132680473509491
0.31551128656744726
0.3176694397823102
0.3197429092077111
0.32173211005090147
0.3236374703038184
0.325459430532196
0.32719844366422907
0.32885497477883535
0.3304295008935627
0.33192251075218704
0.3333345046120464
0.3346659940311567
0.33591750165515355
0.3370895610041042
0.3381827162592343
0.3391975220496123
0.34013454323883463
0.3409943547117548
0.34177754116129805
0.3424846968754035
0.3431164255241356
0.34367333994700533
0.34415606194054155
0.344565222046153
0.34490145933832017
0.34516542121315646
0.34535776317737676
0.34547914863771223
0.34553024869080884
0.345511741913647
0.3454243141545185
0.34526865832459863
0.34504547419014703
0.34475546816537506
0.3443993531060128
0.34397784810361104
0.3434916782806121
0.34294157458622243
0.34232827359312057
0.34165251729503243
0.3409150529052066
0.34011663265582
0.3392580135983462
0.33833995740491596
0.33736323017070036
0.33632860221734595
0.335236847897491
0.3340887454003911
0.33288507655868266
0.33162662665631126
0.33031418423765185
0.32894854091784775
0.32753049119439415
0.3260608322599915
0.32454036381669404
0.32296988789137754
0.3213502086525509
0.3196821322285342
0.31796646652702715
0.31620402105608947
0.31439560674655576
0.31254203577590617
0.3106441213936138
0.30870267774798915
0.306718519714542
0.30469246272587974
0.3026253226031612
0.3005179153891247
'done
> 