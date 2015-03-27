; mstreams with delayed evaluation
(load "utils.scm")

(define the-empty-mstream '())
(define (mstream-null? s) (null? s))

(define (cons-mstream a b) (cons a (delay b)))
(define (mstream-car s) (car s))
(define (mstream-cdr s) (force (cdr s)))

(define (mstream-ref s n)
	(if (= n 0) (mstream-car s)
		(mstream-ref (mstream-cdr s) (- n 1))))

(define (mstream-map proc s)
	(if (mstream-null? s) the-empty-mstream
		(cons-mstream (proc (mstream-car s))
			(mstream-map proc (mstream-cdr s)))))

(define (mstream-for-each proc s)
	(if (mstream-null? s)
		'done
		(begin (proc (mstream-car s))
			(mstream-for-each proc (mstream-cdr s)))))

(define (display-line x)
	(newline)
	(display x))

(define (display-mstream s)
	(mstream-for-each display-line s))

(define (mstream-enumerate-interval low high)
	(if (> low high) the-empty-mstream
		(cons-mstream low
			(mstream-enumerate-interval (+ 1 low) high))))

(define (mstream-filter f s)
	(if (mstream-null? s) the-empty-mstream
		(if (f (mstream-car s))
		 (cons-mstream (mstream-car s)
			(mstream-filter f (mstream-cdr s)))
		 (mstream-filter f (mstream-cdr s)))))

(define (second-prime)
	(mstream-car 
	 (mstream-cdr
	  (mstream-filter prime?
	   (mstream-enumerate-interval 10000 1000000)))))

