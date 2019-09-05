; Exercise 2.16.  Explain, in general, why equivalent algebraic expressions may lead to
; different answers. Can you devise an interval-arithmetic package that does not have this
; shortcoming, or is this task impossible? (Warning: This problem is very difficult.)

; EXPLANATION

; Equivalent arithmetic expressions can lead to different answers. This is because each
; primitive operation with intervals i.e. addition, subtraction, multiplication, division and
; reciprocal introduces its own error bounds into the calculation. The center and width of the
; final computed interval depends upon the way in which the component intervals are combined.

; Can we devise an interval-arithmetic package that does not have this shortcoming? First of
; all, we need to be clear about the algebraic expressions that the package will support. For
; each of these expressions, we will first need to determine the algebraic form that minimizes
; the repetitions of intervals. Then we will need to implement this algebraic form in the
; package. So, yes is a difficult problem.
