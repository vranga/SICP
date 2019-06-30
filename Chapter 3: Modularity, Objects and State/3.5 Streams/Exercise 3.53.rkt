#lang racket

; Exercise 3.53.  Without running the program, describe the elements of the stream defined by

; (define s (cons-stream 1 (add-streams s s)))

; A N S W E R

; (add-streams s s) produces a stream the first element of which is 1 + 1 = 2. So s produces:
; 1, 2 ...
; The next element in the stream will be the addition of the next element of s with itself
; i.e. 2 + 2 = 4. So s will be
; 1, 2, 4 ...
; So the elements of s will be:
;
; 1, 2, 4, 8, 16, 32 ...

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

(define (add-streams s1 s2)
	(stream-map + s1 s2)
)

(define (stream-for-each proc s)
	(if (stream-empty? s)
		'done
		(begin
			(proc (stream-first s))
			(stream-for-each proc (stream-rest s))
		)
	)
)

(define (display-stream s)
	(stream-for-each display-line s)
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

(define (display-line x)
	(newline)
	(display x)
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

(define s (stream-cons 1 (add-streams s s)))
(display-stream-elements 100 s)

; Test Results

Welcome to DrRacket, version 6.11 [3m].
Language: racket, with debugging; memory limit: 512 MB.

1
2
4
8
16
32
64
128
256
512
1024
2048
4096
8192
16384
32768
65536
131072
262144
524288
1048576
2097152
4194304
8388608
16777216
33554432
67108864
134217728
268435456
536870912
1073741824
2147483648
4294967296
8589934592
17179869184
34359738368
68719476736
137438953472
274877906944
549755813888
1099511627776
2199023255552
4398046511104
8796093022208
17592186044416
35184372088832
70368744177664
140737488355328
281474976710656
562949953421312
1125899906842624
2251799813685248
4503599627370496
9007199254740992
18014398509481984
36028797018963968
72057594037927936
144115188075855872
288230376151711744
576460752303423488
1152921504606846976
2305843009213693952
4611686018427387904
9223372036854775808
18446744073709551616
36893488147419103232
73786976294838206464
147573952589676412928
295147905179352825856
590295810358705651712
1180591620717411303424
2361183241434822606848
4722366482869645213696
9444732965739290427392
18889465931478580854784
37778931862957161709568
75557863725914323419136
151115727451828646838272
302231454903657293676544
604462909807314587353088
1208925819614629174706176
2417851639229258349412352
4835703278458516698824704
9671406556917033397649408
19342813113834066795298816
38685626227668133590597632
77371252455336267181195264
154742504910672534362390528
309485009821345068724781056
618970019642690137449562112
1237940039285380274899124224
2475880078570760549798248448
4951760157141521099596496896
9903520314283042199192993792
19807040628566084398385987584
39614081257132168796771975168
79228162514264337593543950336
158456325028528675187087900672
316912650057057350374175801344
633825300114114700748351602688
'done
> (display-stream-elements 2 s)

1
2
'done
> (display-stream-elements 5 s)

1
2
4
8
16
'done
> 
