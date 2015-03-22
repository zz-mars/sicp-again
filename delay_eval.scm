; delayed evaluation
(define stream-null? null?)
(define the-empty-stream '())
(define (stream-ref s n)
  (if (= n 0) (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s) the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s) 'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (dispaly-line x)
  (newline)
  (display x))

(define (cons-stream a b) (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;(define (force delayed-obj) (delayed-obj))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred (stream-cdr stream))))
	(else
	 (stream-filter pred (stream-cdr stream)))))

; exercise 3.50
;(define (stream-map proc . argstreams)
 ; (if (stream-null? (car argstreams))
  ;    the-empty-stream
 ;     (cons-stream
;      (apply proc (map stream-car argstreams))
;       (apply stream-map
;	      (cons proc (map stream-cdr argstreams))))))

; infinite streams
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define (divisible? x y) (= (remainder x y) 0))
;(define no-sevens 
;  (stream-filter (lambda (x) (not (divisible? x 7)))
;		 (integers-starting-from 1)))

(define (fib-gen a b)
  (if (> b 300) the-empty-stream
      (cons-stream a (fib-gen b (+ a b)))))
(define fibs (fib-gen 0 1))

(define (sieve stream)
  (if (stream-empty? stream)
      the-empty-stream
      (cons-stream 
       (stream-car stream)
       (sieve 
	(stream-filter
	 (lambda (x) (not (divisible? x (stream-car stream))))
	 (stream-cdr stream))))))