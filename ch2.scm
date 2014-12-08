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

(define (accumulate-op op initial sequence)
  (define (iter res lst)
    (if (null? lst)
	res
	(iter (op res (car lst)) (cdr lst))))
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

; exercise 2.38
(define (fold-left op initial seq)
  (define (iter res rest)
    (if (null? rest)
	res
	(iter (op res (car rest))
	      (cdr rest))))
  (iter initial seq))
(define (fold-right op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (fold-right op initial (cdr seq)))))

; exercise 2.39
(define (reverse-fold-left seq)
  (fold-left
   (lambda (res ele) (cons ele res)) '() seq))
(define (reverse-fold-right seq)
  (fold-right
   (lambda (ele rest) (append rest (list ele))) '() seq))

; nested mappings
(define (prime-pairs n)
  (accumulate
   append
   '()
   (map
    (lambda (i) 
      (map 
       (lambda (k) (list i k (+ i k)))
       (filter
	(lambda (j) (prime? (+ i j)))
	(enumerate-interval 1 (- i 1)))))
    (enumerate-interval 1 n))))

; flatmap : map each element in seq into a list
; and accumulate all the mapped lists into a
; list of list
(define (flatmap proc seq)
  (accumulate append '()
	      (map proc seq)))

(define (another-prime-pairs n)
  (filter (lambda (p) (prime? (+ (car p) (cadr p))))
	  (flatmap
	   (lambda (i) (map (lambda (j) (list i j (+ i j)))
			    (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n))))


(define (remove l s)
  (filter (lambda (i) (not (= i s))) l))
(define (permutations s)
  (if (null? s) (list '())
      (flatmap
       (lambda (i)
	 (map (lambda (j) (cons i j))
	      (permutations (remove s i)))) s)))

; exercise 2.40
(define (unique-pairs n)
  (flatmap
   (lambda (i) (map (lambda (j) (list i j))
		    (enumerate-interval 1 (- i 1))))
   (enumerate-interval 2 n)))
(define (e-prime-sum-pair n)
  (filter (lambda (p) (prime? (+ (car p) (cadr p))))
	  (unique-pairs n)))

; exercise 2.41
(define (unique-pairs-from-lst lst)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (remove lst i))) lst))

(define (unique-triples n)
  (let ((intvl (enumerate-interval 1 n)))
    (flatmap (lambda (i)
	       (map (lambda (jk) (cons i jk))
		    (unique-pairs-from-lst (remove intvl i))))
	     intvl)))

(define (find-triples n s)
  (filter (lambda (tp) (= (+ (car tp) (cadr tp) (caddr tp)) s))
	  (unique-triples n)))

; exercise 2.42
(define (queen n)
  (define (safe? one-solution i)
    (define (iter cnt solu)
      (cond ((null? solu) #t)
	    ((= i (car solu)) #f)
	    ((= cnt (abs (- i (car solu)))) #f)
	    (else (iter (+ cnt 1) (cdr solu)))))
    (iter 1 one-solution))
  (define (iter solutions)
    (if (= (length (car solutions)) n)
	solutions
	(iter (flatmap 
	       (lambda (one-solution) 
		 (map (lambda (i) (cons i one-solution))
		      (filter (lambda (i) (safe? one-solution i))
			      (enumerate-interval 1 n))))
	       solutions))))
  (iter (list '())))

(define (e-queen n)
  (define (queen-cols k)
    (if (= k 0)
	(list '())
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap (lambda (rest-of-queens)
		    (map (lambda (i)
			   (adjoin-position i k rest-of-queens))
			 (enumerate-interval 1 n)))
		    (queen-cols (- k 1))))))
  (queen-cols n))
; exercise 2.43 -> http://wiki.drewhess.com/wiki/SICP_exercise_2.43

; symbolic data
(define (memq item l)
  (if (null? l)
      #f
      (if (eq? item (car l))
	  l (memq item (cdr l)))))

; exercise 2.54
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
	 (eq? a b))
	((and (pair? a) (pair? b))
	 (and (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	(else #f)))


; symbolic differentiation
(define (variable? x)
  (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y)
       (eq? x y)))
(define (=number? x num)
  (and (number? x) (= x num)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
; assume a and b all generated from make-sum
; so that they are all in simplified form
(define (make-sum a b)
  (cond ((=number? a 0) b)
	((=number? b 0) a)
	((and (number? a) (number? b))
	 (+ a b))
	((and (sum? a) (sum? b))
	 (append a (cdr b)))
	((and (sum? a) (not (sum? b)))
	 (append a (list b)))
	((and (not (sum? a)) (sum? b))
	 (cons '+ (cons a (cdr b))))
	(else (list '+ a b))))
(define (addend x)
  (cadr x))
(define (augend x)
  (let ((aug (cddr x)))
    (if (> (length aug) 1)
	(cons '+ aug) (car aug))))
; product
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
; assume a & b all generated from make-product
; so that they are all in simplified form
(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
	((=number? a 1) b)
	((=number? b 1) a)
	((and (number? a) (number? b)) (* a b))
	((and (product? a) (product? b))
	 (append a (cdr b)))
	((and (product? a) (not (product? b)))
	 (append a (list b)))
	((and (not (product? a)) (product? b))
	 (cons '* (cons a (cdr b))))
	(else (list '* a b))))

(define (multiplier x)
  (cadr x))
(define (multiplicant x)
  (let ((multplct (cddr x)))
    (if (> (length multplct) 1)
	(cons '* multplct) (car multplct))))

; exercise 2.56
; exponentiation extension
(define (make-exponentiation bas expo)
  (cond ((=number? expo 0) 1)
	((=number? expo 1) bas)
	(else (list '** bas expo))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))
; deriv : get the derivation of an expression
; in terms of var
(define (deriv expr var)
  (cond ((number? expr) 0)
	((variable? expr)
	 (if (same-variable? expr var) 1 0))
	((sum? expr)
	 (make-sum (deriv (addend expr) var)
		   (deriv (augend expr) var)))
	((product? expr)
	 (make-sum (make-product (deriv (multiplier expr) var)
				 (multiplicant expr))
		   (make-product (multiplier expr)
				 (deriv (multiplicant expr) var))))
	((exponentiation? expr)
	 (let ((bs (base expr))
	       (expo (exponent expr)))
	   (make-product (make-product expo
				       (make-exponentiation
					bs (make-sum expo -1)))
			 (deriv bs var))))
	(else (error "unknown expression type -- DERIV" expr))))


; exercise 2.58
; deriv with infix representation
; a is simple, skip
; b is a little bit tough..

; example : representing set
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((eq? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set-recur s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
	(else (let ((s1e (car s1)))
		(if (element-of-set? s1e s2)
		    (cons s1e (intersection-set-recur (cdr s1) s2))
		    (intersection-set-recur (cdr s1) s2))))))

(define (intersection-set s1 s2)
  (define (iter res s)
    (if (null? s)
	res
	(let ((se (car s)))
	  (iter (if (element-of-set? se s2)
		    (cons se res) res)
		(cdr s)))))
  (if (null? s2)
      '() (iter '() s1)))

; exercise 2.59
(define (union-set s1 s2)
  (if (null? s1)
      s2
      (union-set (cdr s1)
		 (let ((se (car s1)))
		   (if (element-of-set? se s2)
		       s2 (cons se s2))))))

; sets as ordered list
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((> x (car set)) 
	 (element-of-set? x (cdr set)))
	(else #f)))

(define (intersection-set-sl set1 set2)
  (define (iter res si1 si2)
    (if (or (null? si1) (null? si2)) res
	(let ((x (car si1))
	      (y (car si2))
	      (xl (cdr si1))
	      (yl (cdr si2)))
	  (cond ((> x y)
		 (iter res si1 yl))
		((= x y)
		 (iter (cons x res) xl yl))
		((< x y)
		 (iter res xl si2))))))
  (iter '() set1 set2))

; exercise 2.61
(define (adjoin-set-sl x set)
  (define (iter left right)
    (if (null? right)
	(append left (list x))
	(let ((t (car right)))
	  (cond ((= x t) set)
		((> x t) (iter (append left (list t)) (cdr right)))
		(else (append left (cons x right)))))))
  (iter '() set))

; exercise 2.62
(define (union-set-sl set1 set2)
  (define (iter res s1 s2)
    (cond ((null? s1) (append res s2))
	  ((null? s2) (append res s1))
	  (else (let ((x (car s1))
		      (y (car s2)))
		  (cond ((< x y) (iter (append res (list x)) (cdr s1) s2))
			((= x y) (iter (append res (list x)) (cdr s1) (cdr s2)))
			(else (iter (append res (list y)) s1 (cdr s2))))))))
  (iter '() set1 set2))

; sets as binary tree
(define (entry tree) (car tree))
(define (left-branch tree) 
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (if (null? set) #f
      (let ((et (entry set)))
	(cond ((< x et) (element-of-set? x (left-branch set)))
	      ((= x et) #t)
	      ((> x et) (element-of-set? x (right-branch set)))))))

(define (adjoin-set x set)
  (if (null? set) (make-tree x '() '())
      (let ((et (entry set)))
	(cond ((= x et) set)
	      ((< x et) 
	       (make-tree et 
			  (adjoin-set x (left-branch set))
			  (right-branch set)))
	      ((> x et) 
	       (make-tree et
			  (left-branch set)
			  (adjoin-set x (right-branch set))))))))

; exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree) '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list result tr)
    (if (null? tr) result
	(copy-to-list (cons (entry tr) (copy-to-list result (right-branch tr)))
		      (left-branch tr))))
  (copy-to-list '() tree))

; exercise 2.64
(define (partial-tree elements n)
  (if (= n 0) (cons '() elements)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-tmp (partial-tree elements left-size)))
	  (let ((left-br (car left-tmp))
		(et (cadr left-tmp))
		(right-part (cddr left-tmp))
		(right-size (- n left-size 1)))
	    (let ((right-tmp (partial-tree right-part right-size)))
	      (let ((right-br (car right-tmp))
		    (remainings (cdr right-tmp)))
		(cons (make-tree et left-br right-br) remainings))))))))

(define (list->tree lst)
  (car (partial-tree lst (length lst))))

; exercise 2.65
(define (union-set s1 s2)
  (list->tree (union-set-sl (tree->list-2 s1)
			    (tree->list-2 s2))))
(define (intersection-set s1 s2)
  (list->tree (intersection-set-sl
	       (tree->list-2 s1)
	       (tree->list-2 s2))))

; huffman tree
(define (make-leaf sym weight)
  (list 'leaf sym weight))
(define (leaf? leaf)
  (eq? (car leaf) 'leaf))
(define (symbol-leaf leaf)
  (cadr leaf))
(define (weight-leaf leaf)
  (caddr leaf))

(define (symbols branch)
  (if (leaf? branch)
      (list (symbol-leaf branch))
      (caddr branch)))
(define (weight branch)
  (if (leaf? branch)
      (weight-leaf branch)
      (cadddr branch)))
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
	  ((= bit 1) (right-branch branch))
	  (else (error "unknown encode bit" bit))))
  (define (decode-iter res bits branch)
    (if (null? bits) res
	(let ((next-branch (choose-branch (car bits) branch)))
	  (if (leaf? next-branch)
	      (decode-iter (append res (symbols next-branch))
			   (cdr bits) tree)
	      (decode-iter res (cdr bits) next-branch)))))
  (decode-iter '() bits tree))

; sets of weighted elements
(define (adjoin-set x set)
  (if (null? set) (list x)
      (if (> (weight x) (weight (car set)))
	  (cons (car set) (adjoin-set x (cdr set)))
	  (cons x set))))
(define (make-leaf-set pairs)
  (define (iter set pairs)
    (if (null? pairs) set
	(iter (adjoin-set (make-leaf (car (car pairs)) (cadr (car pairs)))
			  set)
	      (cdr pairs))))
  (iter '() pairs))

(define (generate-huffman-tree pairs)
  (define (iter leaf-set)
    (if (= (length leaf-set) 1) (car leaf-set)
	(iter (adjoin-set
	       (make-code-tree (car leaf-set)
			       (cadr leaf-set))
	       (cddr leaf-set)))))
  (iter (make-leaf-set pairs)))

(define (raw-element-of-set x set)
  (cond ((null? set) #f)
	((eq? x (car set)) #t)
	(else (raw-element-of-set x (cdr set)))))
; exercise 2.68
(define (encode-symbol sym tree)
  (define (iter res branch)
    (let ((lbr (left-branch branch))
	  (rbr (right-branch branch)))
      (cond ((raw-element-of-set sym (symbols lbr))
	     (if (leaf? lbr) (append res '(0))
		 (iter (append res '(0)) lbr)))
	    ((raw-element-of-set sym (symbols rbr))
	     (if (leaf? rbr) (append res '(1))
		 (iter (append res '(1)) rbr)))
	    (else (error "cannot encode -> " sym)))))
  (iter '() tree))
(define (encode msg tree)
  (if (null? msg) '()
      (append (encode-symbol (car msg) tree)
	      (encode (cdr msg) tree))))

; exercise 2.70
(define lyric-symbols
  '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))

; 2.4 multiple representation for abstract data
(define (attach-tag tag z)
  (if (number? z) z
      (cons tag z)))
(define (install-rectangular-package)
  ;; internal procedure
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square x) (square y))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y)
	 (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) 
	 (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z)
			   (cos (angle z))))
  (define (imag-part z) (* (magnitude z)
			   (sin (angle z))))
  (define (make-from-real-image x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  (define (tag x)
    (attach-tag 'polar x))
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
	 (tag (make-from-mag-ang r a))))
  (put 'make-from-real-imag 'polar
       (lambda (x y)
	 (tag (make-from-real-imag x y))))
  'done)

(define (type-tag z)
  (cond ((pair? z) (car z))
	((number? z) 'scheme-number)
	(else (error "Bad tagged datum -- TYPE-TAG" z))))
(define (contents z)
  (cond ((pair? z) (cdr z))
	((number? z) z)
	(else (error "Bad tagged datum -- CONTENTS" z))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error "No method for these types -- APPLY-GENERIC"
		 (list op type-tags))))))

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))
(define (make-from-real-image x y)
  ((get 'make-from-real-image 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; exercise 2.73
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
					   var))))
(define (install-sum-derivation)
  (define (sum-deriv operands var)
    (make-sum (deriv (car operands) var)
	      (deriv (cadr operands) var)))
  (put 'deriv '+ sum-deriv)
  'done)

(define (install-mul-derivation)
  (define (mul-deriv operands var)
    (let ((multiplier (car operands))
	  (multiplicant (cadr operands)))
      (make-sum (make-product (deriv multiplier var) multiplicant)
		(make-product multiplier (deriv multiplicant var)))))
  (put 'deriv '* mul-deriv)
  'done)

; exercise 2.75
(define (make-from-mag-ang r a)
  (lambda (op)
    (cond ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  ((eq? 'real-part) (* r (cos a)))
	  ((eq? 'imag-part) (* r (sin a)))
	  (else (error "Uknown op -- MAKE-FROM-MAG-ANG" op)))))

; 2.5 systems with generic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'sine 'scheme-number sin)
  'done)
(define (install-rational-package)
  ; internal procedure
  (define (make-rat n d)
    (let ((g (gcd n d))
	  (sign-fix (if (< d 0) (- 1) 1)))
      (cons (* sign-fix (/ n g)) 
	    (* sign-fix (/ d g)))))
  (define (numer x)
    (car x))
  (define (denom x)
    (cdr x))
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
  ; interfaces
  (define (tag x)
    (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y)
	 (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y)
	 (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y)
	 (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y)
	 (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d)
	 (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
			  (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (z) (= (numer z) 0)))
  (put 'sine 'rational
       (lambda (x) (sin (/ (* 1.0 (numer x)) (denom x)))))
  'done)

; complex number package
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (apply-generic 'add (real-part z1) (real-part z2))
			 (apply-generic 'add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (apply-generic 'sub (real-part z1) (real-part z2))
			 (apply-generic 'sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (apply-generic 'mul (magnitude z1) (magnitude z2))
		       (apply-generic 'add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (apply-generic 'div (magnitude z1) (magnitude z2))
		       (apply-generic 'sub (angle z1) (angle z2))))
  ; interfaces
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex 
       (lambda (x y) 
	 (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
	 (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
			  (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0)
			(= (imag-part x) 0))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; coercion
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((t1 (car type-tags))
		    (t2 (cadr types-tags))
		    (c1 (car args))
		    (c2 (cadr args)))
		(let ((t1->t2 (get-coercion t1 t2))
		      (t2->t2 (get-coercion t2 t1)))
		  (cond (t1->t2 
			 (apply-generic op (t1->t2 c1) c2))
			(t2->t1
			 (apply-generic op c1 (t2->t1 c2)))
			(else (error "no method for " (list t1 t2))))))
	      (error "no method for " (list op type-tags)))))))

; exercise 2.81
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((t1 (car type-tags))
		    (t2 (cadr type-tags)))
		(if (eq? t1 t2)
		    (error "no method for " (list op type-tags))
		    (let ((c1 (car args))
			  (c2 (cadr args))
			  (t1->t2 (get-coercion t1 t2))
			  (t2->t2 (get-coercion t2 t1)))
		      (cond (t1->t2 
			     (apply-generic op (t1->t2 c1) c2))
			    (t2->t1
			     (apply-generic op c1 (t2->t1 c2)))
			    (else (error "no method for " (list t1 t2)))))))
	      (error "no method for " (list op type-tags)))))))

; exercise 2.82
(define (apply-geberic op . args)
  (define (apply-generic-helper op args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
	(if proc
	    (apply proc (map contents args))
	    (let ((arg-len (length args)))
	      (cond ((<= arg-len 1) (error "no method for " (list op args)))
		    ((= arg-len 2)
		     (let ((t1 (car type-tags))
			   (t2 (cadr type-tags)))
		       (if (eq? t1 t2)
			   (error "no method for " (list op type-tags))
			   (let ((c1 (car args))
				 (c2 (cadr args))
				 (t1->t2 (get-coercion t1 t2))
				 (t2->t2 (get-coercion t2 t1)))
			     (cond (t1->t2 
				    (apply-generic op (t1->t2 c1) c2))
				   (t2->t1
				    (apply-generic op c1 (t2->t1 c2)))
				   (else (error "no method for " (list t1 t2))))))))
		    (else
		     (apply-generic op (car args) (apply-generic-helper op (cdr args))))))))))
  (apply-generic-helper op args))

; exercise 2.83
(define (install-real-package)
  (define (make-real x) (* x 1.0))
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  'done)
(define (make-real x) (attach-tag 'real (* x 1.0)))
(define (install-generic-raise)
  (put 'raise 'integer
       (lambda (x) (make-rat x 1)))
  (put 'raise 'rational
       (lambda (x) (make-real (/ (* 1.0 (denom x)) (numer x)))))
  (put 'raise 'real
       (lambda (x) (make-from-real-imag x 0)))
  (put 'level 'integer 0)
  (put 'level 'rational 1)
  (put 'level 'real 2)
  (put 'level complex 3)
  'done)
(define (raise x)
  (let ((raise-proc (get 'raise (type-tag x))))
    (if raise-proc
	(raise-proc (contents x))
	(error "no raise proc for " x))))

; exercise 2.84 and exercise 2.85
(define (install-project-package)
  (put 'project 'complex
       (lambda (x) (make-real (real-part x))))
  (put 'project 'real
       (lambda (x) (quotient x)))
  'done)
(define (drop x)
  (let ((projecter (get 'project (type-tag x))))
    (if projecter
	(let ((projected (project (contents x))))
	  (let ((raise-back (raise projected)))
	    (if ((get 'equ? (map type-tag (x raise-back)))
		 x raise-back) (drop projected) x))) x)))
(define (apply-geberic op . args)
  (define (apply-generic-helper op args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
	(if proc
	    (apply proc (map contents args))
	    (let ((arg-len (length args)))
	      (cond ((<= arg-len 1) (error "no method for " (list op args)))
		    ((= arg-len 2)
		     (let ((t1 (get 'level (car type-tags)))
			   (t2 (get 'level (cadr type-tags)))
			   (c1 (car args))
			   (c2 (cadr args)))
		       (cond ((= t1 t2)
			      (if (eq? (car type-tags) (cadr type-tags))
				  (error "no method for " (list op type-tags))
				  (apply-generic op (raise c1) (raise c2))))
			     ((< t1 t2)
			      (apply-generic op (raise c1) c2))
			     (else (apply-generic op c1 (raise c2))))))
		    (else
		     (apply-generic op (car args) (apply-generic-helper op (cdr args))))))))))
  (drop (apply-generic-helper op args)))

; exercise 2.86
; change 1: arithmatic operation in install-complex-package
;  should be re-written with apply-generic
; change 2: implement sine and cosine in integer, rational and real package

; poly
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
		 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not the same variable -- ADD-POLY"
	     (list p1 p2))))
(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
		 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not the same variable -- MUL-POLY"
	     (list p1 p2))))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
	((empty-termlist? L2) L1)
	(else 
	 (let ((t1 (first-term L1))
	       (t2 (first-term L2)))
	   (let ((od1 (order t1))
		 (od2 (order t2)))
	     (cond ((> od1 od2)
		    (adjoin-term 
		     t1 (add-terms (rest-terms L1) L2)))
		   ((> od2 od1)
		    (adjoin-term 
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term od1 (add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1) (rest-terms L2))))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
		 (mul-terms (rest-terms L1) L2))))
(define (mul-term-by-all-terms t L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((tl (first-term L)))
	(adjoin-term
	 (make-term (+ (order t) (order tl))
		    (mul (coeff t) (coeff tl)))
	 (mul-term-by-all-terms t (rest-terms L))))))

(define (install-poly-package)
  (define (make-poly var term-list)
    (cons var term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
;  (define (add-poly) )
;  (define (mul-poly) )
  (define (tag p)
    (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
	 (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
	 (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)