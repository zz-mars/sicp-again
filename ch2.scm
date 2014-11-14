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

; exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (left-branch mob)
  (car mob))
(define (right-branch mob)
  (cadr mob))
(define (make-branch length structure)
  (list length structure))
(define (branch-length br)
  (car br))
(define (branch-structure br)
  (cadr br))
(define (total-weight mob)
  (let ((l-br-st (branch-structure (left-branch mob)))
	(r-br-st (branch-structure (right-branch mob))))
    (+ (if (pair? l-br-st) (total-weight l-br-st) l-br-st)
       (if (pair? r-br-st) (total-weight r-br-st) r-br-st))))
(define (balanced-mob mob)
  (let ((l-br-len (branch-length (left-branch mob)))
	(l-br-st (branch-structure (left-branch mob)))
	(r-br-len (branch-length (right-branch mob)))
	(r-br-st (branch-structure (right-branch mob))))
    (and (= (* l-br-len (if (pair? l-br-st) (total-weight l-br-st) l-br-st))
	    (* r-br-len (if (pair? r-br-st) (total-weight r-br-st) r-br-st)))
	 (if (pair? l-br-st) (balanced-mob l-br-st) #t)
	 (if (pair? r-br-st) (balanced-mob r-br-st) #t))))

; mapping over trees
(define (tree-map proc tr)
  (define (tree-map-iter t)
    (if (pair? t)
	(map (lambda (st) (tree-map-iter st)) t)
	(proc t)))
  (tree-map-iter tr))

; exercise 2.30
(define (square-tree t)
  (tree-map square t))
(define (square-tree-direct tr)
  (define (sqt-iter t)
    (if (null? t) '()
	(if (pair? t)
	    (cons (sqt-iter (car t)) (sqt-iter (cdr t)))
	    (square t))))
  (sqt-iter tr))

; exercise 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest
		(map (lambda (ss) (cons (car s) ss)) rest)))))

; sequences as conventional interface
; acc-list accumulates a list in terms of proc null-value and combiner
(define (acc-list proc null-value combiner l)
  (define (iter res lst)
    (if (null? lst) res
	(iter (combiner res (proc (car lst))) (cdr lst))))
  (iter null-value l))

(define (count-leaves l)
  (if (null? l) 0
      (if (pair? l)
	  (acc-list count-leaves 0 + l) 1)))

(define (sum-odd-square tree)
  (if (null? tree)
      0
      (if (pair? tree)
	  (acc-list sum-odd-square 0 + tree)
	  (if (odd? tree) (square tree) 0))))

(define (filter proc lst)
  (cond ((null? lst) '())
	((proc (car lst))
	 (cons (car lst) (filter proc (cdr lst))))
	(else (filter proc (cdr lst)))))

(define (accumulate op initial sequence)
  (define (iter res lst)
    (if (null? lst)
	res
	(iter (op (car lst) res) (cdr lst))))
  (iter initial sequence))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((pair? tree)
	 (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))
	(else (list tree))))

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map 
	       square 
	       (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
	      '()
	      (filter even?
		      (map fib (enumerate-interval 0 n)))))

(define (new-count-leaves tree)
  (accumulate +
	      0
	      (map (lambda (x) 1)
		   (enumerate-tree tree))))

; exercise 2.33
(define (e-map p seq)
  (accumulate
   (lambda (x y) (append y (list (p x))))
   '() seq))
(define (e-append sq1 sq2)
  (accumulate cons sq2 (reverse sq1)))
(define (e-length sq)
  (accumulate (lambda (x y) (+ y 1)) 0 sq))

; exercise 2.34
(define (horner-eval x coefficient-seq)
  (accumulate (lambda (coef res) (+ (* res x) coef))
	      0
	      coefficient-seq))

; exercise 2.35
(define (e-count-leaves tree)
  (accumulate (lambda (x res) (+ res 1))
	      0
	      (enumerate-tree tree)))

; exercise 2.36
(define (accumulate-n op init seqs)
  (define (iter res sqs)
    (if (null? (car sqs))
	res
	(iter (append res (list (accumulate op init (map car sqs))))
	      (map cdr sqs))))
  (iter '() seqs))

(define (e-accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (e-accumulate-n op init (map cdr seqs)))))

; exercise 2.37
(define (dot-product v w)
  (define (iter res vv ww)
    (if (null? vv)
	res
	(iter (+ res (* (car vv) (car ww))) (cdr vv) (cdr ww))))
    (iter 0 v w))

(define (matrix-*-vector m v)
  (map (lambda (vv) (dot-product vv v)) m))
(define (transpose m)
  (accumulate-n 
   (lambda (x y) (append y (list x)))
   '() m))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))