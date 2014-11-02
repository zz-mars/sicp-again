(define (abs x)
  (if (< x 0) (- x) x))
(define (average x y)
  (/ (+ x y) 2))
(define (<= x y)
  (not (> x y)))
(define (>= x y)
  (not (< x y)))
(define (square x)
  (* x x))
(define (sum-of-square x y)
  (+ (square x) (square y)))
(define (old-sqrt x)
  (define (sqrt-iter guess)
    (define (next-guess)
      (average guess (/ x guess)))
    (define (good-enough)
      (< (abs (- guess (next-guess))) 0.000000001))
    (if (good-enough) guess
	(sqrt-iter (next-guess))))
  (sqrt-iter 1.0))

(define (sqrt x)
  (define (sqrt-iter guess)
    (let ((next-guess (average guess (/ x guess))))
      (define (good-enough)
	(< (abs (- guess next-guess)) 0.000000001))
      (if (good-enough) guess
	  (sqrt-iter next-guess))))
  (sqrt-iter 1.0))

(define (cbrt x)
  (define (cr-iter guess)
    (let ((next-guess
	   (average guess
		    (/ (+ (/ x (square guess)) (* 2 guess)) 3))))
      (define (good-enough)
	(< (abs (- guess next-guess)) 0.0000001))
      (if (good-enough) guess
	  (cr-iter next-guess))))
  (cr-iter 1.0))

(define (recur-factorial n)
  (if (= n 1) 1
      (* n (recur-factorial (- n 1)))))

(define (iter-factorial n)
  (define (fact-iter step res)
    (if (= step n) (* step res)
	(fact-iter (+ step 1) (* res step))))
  (fact-iter 1 1))

; exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(define (fib n)
  (define (fib-iter count a b)
    (if (= count n) a
	(fib-iter (+ count 1) b (+ b a))))
  (fib-iter 0 0 1))

(define (count-change amount)
  (define (first-denomination type)
    (cond ((= type 1) 1)
	  ((= type 2) 5)
	  ((= type 3) 10)
	  ((= type 4) 25)
	  ((= type 5) 50)))
  (define (cc amt money-type-range)
    (cond ((= amt 0) 1)
	  ((or (< amt 0) (= money-type-range 0)) 0)
	  (else (+ (cc (- amt (first-denomination money-type-range)) money-type-range)
		   (cc amt (- money-type-range 1))))))
  (cc amount 5))

;exercise 1.11
(define (f-recur n)
  (if (< n 3) n
      (+ (f-recur (- n 1))
(edit	 (* (f-recur (- n 2)) 2)
	 (* (f-recur (- n 3)) 3)))))
(define (f-iter n)
  (define (iter count a b c)
    (if (= count n) c
	(iter (+ count 1) b c (+ c (* 2 b) (* 3 a)))))
  (if (< n 3) n
      (iter 2 0 1 2)))

; exercise 1.12
(define (pascal-triangle n i)
  (cond ((> i n) (display "out of range"))
	((or (= i 1) (= i n)) 1)
	(else (+ (pascal-triangle (- n 1) (- i 1))
		 (pascal-triangle (- n 1) i)))))















