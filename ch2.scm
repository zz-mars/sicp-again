; chapter 2 : Building abstraction with data
; 2.1 introduction with rational number

; load ch1 to use its utilities
(load "ch1.scm")

(define (make-rat n d)
  (let ((g (gcd n d))
	(sign-fix (if (< d 0) (- 1) 1)))
    (cons (* sign-fix (/ n g)) 
	  (* sign-fix (/ d g)))))

(define (numer x)
  (car x))
(define (denom x)
  (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y) 
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

; exercise 2.2
(define (make-segment start-seg end-seg)
  (cons start-seg end-seg))
(define (start-segment x)
  (car x))
(define (end-segment x)
  (cdr x))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (mid-point s)
  (make-point 
   (average (x-point (start-segment s))
	    (x-point (end-segment s)))
   (average (y-point (start-segment s))
	    (y-point (end-segment s)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

; self-implemented cons, car, cdr
(define (si-cons x y)
  (lambda (i)
    (cond ((= i 0) x)
	  ((= i 1) y)
	  (else (error "invalid index!")))))
(define (si-car x)
  (x 0))
(define (si-cdr x)
  (x 1))

; exercise 2.4
(define (another-cons x y)
  (lambda (f) (f x y)))
(define (another-car z)
  (z (lambda (x y) x)))
(define (another-cdr z)
  (z (lambda (x y) y)))

; exercise 2.5
(define (e2-5-cons a b)
  (* (exp 2 a) (exp 3 b)))
(define (contained-num n i)
  (define (iter count num)
    (if (= (remainder num i) 0)
	(iter (+ count 1) (/ num i))
	count))
  (iter 0 n))
(define (e2-5-car c)
  (contained-num c 2))
(define (e2-5-cdr c)
  (contained-num c 3))

; exercise 2.6
; These numbers are actually procedures,
; which takes a procedure f as argument
; and returns another procedure which take x
; as argument and calls f for 'numbers' times
; with x as original argument
(define zero (lambda (f) (lambda (x) x)))
; add-1 take a num (which is actually a procedure)
; as argument, return a procedure which takes a 
; procedure f as argument, and returns a procedure
; which imposes one more call of f than the procedure
; 'num' will do
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one-from-add
  (add-1 zero))
(define one
  (lambda (f) (lambda (x) (f x))))

(define two-from-add
  (add-1 one))
(define two
  (lambda (f) (lambda (x) (f (f x)))))
