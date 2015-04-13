(define (pairs s t)
	(cons-stream (list (stream-car s) (stream-car t))
		(interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
					(pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s t)
	(if (stream-null? s) t
		(cons-stream (stream-car s)
			(interleave t (stream-cdr s)))))

;; exercise 3.67
(define (helper-stream n)
	(interleave (stream-map (lambda (x) (list n x))
					(stream-enumerate-interval 1 (- n 1)))
				(helper-stream (+ n 1))))
(define complement-stream (helper-stream 2))
(define (all-pairs s t)
	(cons-stream (list (stream-car s) (stream-car t))
		(interleave 
			(interleave (stream-map (lambda (x) (list (stream-car s) x))
										(stream-cdr t))
						(stream-map (lambda (x) (list x (stream-car t)))
										(stream-cdr s)))
			(all-pairs (stream-cdr s) (stream-cdr t)))))
	

; exercise 3.69
(define (triples s t u)
	(cons-stream (list (stream-car s) (stream-car t) (stream-car u))
		(interleave (stream-map (lambda x (cons (stream-car s) x))
								(stream-cdr (pairs t u)))
					(triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
