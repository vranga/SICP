; Exercise 1.12.  The following pattern of numbers is called Pascal's triangle.

;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1

; The numbers at the edge of the triangle are all 1, and each number inside the triangle
; is the sum of the two numbers above it.35 Write a procedure that computes elements of
; Pascal's triangle by means of a recursive process.

> (pascal-internal 1 1)
1
> (pascal-internal 1 2)
"error"
> (pascal-internal 2 0)
"error"
> (pascal-internal 2 -3)
"error"
> (pascal-internal 2 1)
1
> (pascal-internal 2 2)
1
> (pascal-internal 5 1)
1
> (pascal-internal 5 2)
4
> (pascal-internal 5 3)
6
> (pascal-internal 5 4)
4
> (pascal-internal 5 5)
1
