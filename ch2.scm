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

; list operations
(define (list-ref l n)
  (if (null? l) '()
      (if (= n 0) (car l)
	  (list-ref (cdr l) (- n 1)))))
(define (length l)
  (define (iter res itl)
    (if (null? itl) res
	(iter (+ res 1) (cdr itl))))
  (iter 0 l))
(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

; exercise 2.17
(define (last-pair l)
  (if (> (length l) 1)
      (last-pair (cdr l)) l))
; exercise 2.18
(define (reverse l)
  (define (iter res lst)
    (if (null? lst) res
	(iter (cons (car lst) res) (cdr lst))))
  (iter '() l))
(define (reverse-recur l)
  (if (null? l) '()
      (append (reverse-recur (cdr l)) (list (car l)))))

; exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc money coins-list)
  (cond ((= money 0) 1)
	((or (< money 0) (null? coins-list)) 0)
	(else
	 (+ (cc money (cdr coins-list))
	    (cc (- money (car coins-list)) coins-list)))))

; exercise 2.20
(define (same-parity a1st . aleft)
  (define (iter prt l)
    (cond ((null? l) '())
	  ((= (remainder (car l) 2) prt)
	   (cons (car l) (iter prt (cdr l))))
	  (else (iter prt (cdr l)))))
  (cons a1st (iter (remainder a1st 2) aleft)))

; mapping over list
(define (map f l)
  (if (null? l) '()
      (cons (f (car l)) (map f (cdr l)))))
(define (scale-list l factor)
  (map (lambda (x) (* x factor)) l))

; exercise 2.23
(define (for-each proc l)
  (define (iter x lst)
    (if (null? lst) x
	(iter (proc (car lst)) (cdr lst))))
  (iter '() l))

(define (acc-list proc null-value combiner l)
  (define (iter res lst)
    (if (null? lst) res
	(iter (combiner res (proc (car lst))) (cdr lst))))
  (iter null-value l))

(define (count-leaves l)
  (if (null? l) 0
      (if (pair? l)
	  (acc-list count-leaves 0 + l) 1)))

; exercise 2.27
(define (deep-reverse l)
  (define (iter res lst)
    (if (null? lst) res
	(let ((e1st (car lst)))
	  (iter 
	   (cons 
	    (if (pair? e1st) (deep-reverse e1st) e1st) res) (cdr lst)))))
  (iter '() l))

; exercise 2.28
(define (fringe t)
  (define (iter res tt)
    (if (null? tt) res
	(if (not (pair? tt))
	    (append res (list tt))
	    (let ((e1st (car tt)))
	      (iter (append res (if (pair? e1st) (fringe e1st) (list e1st))) (cdr tt))))))
  (iter '() t))