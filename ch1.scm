(define (abs x)
  (if (< x 0) (- x) x))
(define (average x y)
  (/ (+ x y) 2.0))
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

; 1.3 high order procedure
; exercise 1.30
(define (sum term a next b)
  (define (sum-iter res it)
    (if (> it b) res
	(sum-iter (+ res (term it)) (next it))))
  (sum-iter 0 a))

(define (inc x) (+ x 1))
(define (sum-cube a b)
  (sum cube a inc b))
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a (lambda (x) (+ x 4)) b))

(define (integral f a b dx)
  (define (next-a a) (+ a dx))
  (* dx (sum f (+ a (/ dx 2.0)) next-a b)))

; exercise 1.29
(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (* (/ h 3.0) 
       (+ (f a) (f b) 
	  (sum (lambda (k) (* (f (+ a (* k h))) (if (= (remainder k 2) 0) 2 4)))
	       1 inc (- n 1))))))

; exercise 1.31
(define (identity x) x)
(define (product-recur f a next b)
  (if (> a b) 1
      (* (f a) (product-recur f (next a) next b))))
(define (product f a next b)
  (define (product-iter res it)
    (if (> it b) res
	(product-iter (* res (f it)) (next it))))
  (product-iter 1 a))
(define (factorial-in-pruduct n)
  (product identity 1 inc n))
(define (compute-pi n) 
  (* 4 (product 
	(lambda (x) (/ (* (- x 1) (+ x 1)) (square x)))
	3.0 (lambda (x) (+ x 2)) n)))

; exercise 1.32
(define (accumulate combiner null-value term a next b)
  (define (accu-iter res it)
    (if (> it b) res
	(accu-iter (combiner res (term it)) (next it))))
  (accu-iter null-value a))

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b) null-value
      (combiner 
       (term a)
       (accumulate-recur combiner null-value term (next a) next b))))

(define (acc-sum f a next b)
  (accumulate-recur + 0 f a next b))

(define (acc-product f a next b)
  (accumulate-recur * 1 f a next b))

; exercise 1.33
(define (filtered-acc combiner null-value filter term a next b)
  (define (filtered-acc-iter res it)
    (if (> it b) res
	(filtered-acc-iter 
	 (combiner res (if (filter it) (term it) null-value))
	 (next it))))
  (filtered-acc-iter null-value a))

(define (e133b n)
  (filtered-acc 
   * 1
   (lambda (x) (and (> x 0) (= (gcd x n) 1))) identity 1 inc n))

; 1.3.3 procedures as general methods
(define (search f neg pos)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.00001))
  (let ((mid (average neg pos)))
    (if (close-enough? neg pos)
	mid
	(let ((test-mid (f mid)))
	  (cond ((> test-mid 0) (search f neg mid))
		((= test-mid 0) mid)
		(else (search f mid pos)))))))

(define (half-interval-method f a b)
  (let ((aval (f a))
	(bval (f b)))
    (cond ((and (negative? aval) (positive? bval))
	   (search f a b))
	  ((and (positive? aval) (negative? bval))
	   (search f b a))
	  (else (error "fval are not opposite sign!" a b)))))

; fixed point
(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.000001))
  (define (try guess)
    (newline)
    (display "try guess -> ")
    (display guess)
    (let ((next-guess (average (f guess) guess)))
      (if (close-enough? guess next-guess)
	  guess
	  (try next-guess))))
  (try first-guess))

(define (fixed-point-no-average f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.000001))
  (define (try guess)
    (newline)
    (display "try guess -> ")
    (display guess)
    (let ((next-guess (f guess)))
      (if (close-enough? guess next-guess)
	  guess
	  (try next-guess))))
  (try first-guess))

;(define (sqrt x)
;  (fixed-point (lambda (y) (/ x y)) 1.0))

; exercise 1.37
(define (cont-frac n d k)
  (define (iter res i)
    (if (= i 0) res
	(iter (/ (n i) (+ (d i) res)) (- i 1))))
  (iter (/ (n k) (d k)) (- k 1)))

(define (cont-frac-recur n d k)
  (define (iter i)
    (if (< i k)
	(/ (n i) (+ (d i) (iter (+ i 1))))
	(/ (n k) (d k))))
  (iter 1))

; exercise 1.39
(define (tan-cf x k)
  (cont-frac 
   (lambda (i) (if (= i 1) x (- (square x))))
   (lambda (i) (- (* 2 i) 1)) k))