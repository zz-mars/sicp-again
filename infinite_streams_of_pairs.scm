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

; exercise 3.70
(define (merge-weighted s t weight)
	(cond ((stream-null? s) t)
		  ((stream-null? t) s)
		  (else
		   (let ((cars (stream-car s))
				 (cart (stream-car t)))
			(let ((wcars (weight cars))
				  (wcart (weight cart)))
			 (cond ((< wcars wcart)
					(cons-stream cars (merge-weighted (stream-cdr s) t weight)))
				   (else (< wcart wcars)
					(cons-stream cart (merge-weighted s (stream-cdr t) weight)))))))))

(define (e-370-pairs s t weight)
	(cons-stream (list (stream-car s) (stream-car t))
		(merge-weighted
			(stream-map (lambda (x) (list (stream-car s) x))
						(stream-cdr t))
			(e-370-pairs (stream-cdr s) (stream-cdr t) weight)
			weight)))

(define e-370-a (e-370-pairs integers integers (lambda (lst) (+ (car lst) (cadr lst)))))
(define (divisable-by-2-3-5 x) (or (= (remainder x 2) 0) (= (remainder x 3) 0) (= (remainder x 5) 0)))
(define integer235 (stream-filter (lambda (x) (and (divisable-by-2-3-5 x))) integers))
(define e-370-b (e-370-pairs integer235 integer235 (lambda (x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x))))))

; exercise 3.71
(define s (e-370-pairs integers integers (lambda (x) (+ (cube (car x)) (cube (cadr x))))))
(define (ramanujan-stream s)
	(let ((pair1 (stream-car s))
		  (pair2 (stream-car (stream-cdr s))))
	 (let ((wp1 (+ (cube (car pair1)) (cube (cadr pair1))))
		   (wp2 (+ (cube (car pair2)) (cube (cadr pair2)))))
	  (if (= wp1 wp2) (cons-stream wp1 (ramanujan-stream (stream-cdr s)))
		  (ramanujan-stream (stream-cdr s))))))

; exercise 3.72
(define s (e-370-pairs integers integers (lambda (x) (+ (square (car x)) (square (cadr x))))))
(define (stream-e-372 s)
	(let ((pair1 (stream-car s))
		  (pair2 (stream-car (stream-cdr s)))
		  (pair3 (stream-car (stream-cdr (stream-cdr s)))))
	 (let ((wp1 (+ (cube (car pair1)) (cube (cadr pair1))))
		   (wp2 (+ (cube (car pair2)) (cube (cadr pair2))))
		   (wp3 (+ (cube (car pair3)) (cube (cadr pair3)))))
	  (if (= wp1 wp2) 
		(if (= wp2 wp3) (cons-stream (list wp1 pair1 pair2 pair3) (stream-e-372 (stream-cdr s)))
			(stream-e-372 (stream-cdr (stream-cdr s))))
		(stream-e-372 (stream-cdr s))))))
