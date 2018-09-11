(define error "error")

(define (pascal-internal depth position)
	(cond
		((or (< depth 1) (< position 1)) error)
		((> position depth) error)
		((= depth 1) 1)
		((= position 1) 1)
		((= position depth) 1)
		(else
			(+
				(pascal-internal (- depth 1) (- position 1))
				(pascal-internal (- depth 1) position)
			)
		)
	)
)
