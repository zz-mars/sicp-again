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

(define (cube x)
  (* x x x))

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
	 (* (f-recur (- n 2)) 2)
	 (* (f-recur (- n 3)) 3))))
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
(define (even? x)
  (= (remainder x 2) 0))
; exercise 1.16
(define (exp m n)
  (define (exp-iter res x y)
    (cond ((= y 0) res)
	  ((even? y) (exp-iter res (square x) (/ y 2)))
	  (else (exp-iter (* res x) x (- y 1)))))
  (if (= m 0) 0
      (exp-iter 1 m n)))

; exercise 1.17

; exercise 1.19
(define (fib n)
  (define (fib-iter a b p q s)
    (cond ((= s 0) b)
	  ((even? s) (fib-iter a b
			       (+ (square p) (square q)) (+ (square q) (* 2 p q))
			       (/ s 2)))
	  (else (fib-iter (+ (* (+ a b) q) (* a p)) (+ (* b p) (* a q)) p q (- s 1)))))
  (fib-iter 1 0 0 1 n))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (iter candidate)
    (cond ((> (square candidate) n) n)
	  ((divides? candidate n) candidate)
	  (else (iter (+ 2 candidate)))))
  (if (divides? 2 n) 2
      (iter 3)))

(define (prime? n)
  (= n (smallest-divisor n)))

; exercise 1.22
(define (timed-prime-test n)
  (define (report-time elapsed-time)
    (display " ***")
    (display elapsed-time))
  (define (start-test start-time)
    (if (prime? n)
	(report-time (- (runtime) start-time))))
  (newline)
  (display n)
  (start-test (runtime)))

(define (find-prime start-num)
  (if (prime? start-num)
      (timed-prime-test start-num)
      (find-prime (+ start-num 1))))

; exercise 1.24
;(define (expmod base ep m)
 ; (remainder (exp base ep) m))
(define (expmod base ep m)
  (cond ((= ep 0) 1)
	((even? ep)
	 (remainder (square (expmod base (/ ep 2) m)) m))
	(else 
	 (remainder (* base (expmod base (- ep 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (timed-fast-prime-test n)
  (define (report-time elapsed-time)
    (display " ***")
    (display elapsed-time))
  (define (start-test start-time)
    (if (fast-prime? n 3)
	(report-time (- (runtime) start-time))))
  (newline)
  (display n)
  (start-test (runtime)))

(define (fast-find-prime start-num)
  (if (fast-prime? start-num 3)
      (timed-fast-prime-test start-num)
      (fast-find-prime (+ start-num 1))))
