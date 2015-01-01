(load "ch1.scm")
(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (cond ((>= balance amount)
	     (begin (set! balance (- balance amount))
		    balance))
	    (else (error "Insufficient funds!"))))))

(define (make-withdraw balance)
  (lambda (amount)
    (cond ((>= balance amount)
	   (begin (set! balance (- balance amount))
		  balance))
	  (else (error "Insufficient funds!")))))

(define (make-account balance)
  (define (withdraw amount)
    (cond ((>= balance amount)
	   (begin (set! balance (- balance amount))
		  balance))
	  (else (error "Insufficient funds!"))))
  (define (deposite amount)
    (begin (set! balance (+ balance amount))
	   balance))
  (lambda (op)
    (cond ((eq? op 'withdraw) withdraw)
	  ((eq? op 'deposite) deposite)
	  (else (error "Unknown operation! -> " (list op))))))

; exercise 3.1
(define (make-accumulator sum)
  (lambda (num)
    (begin (set! sum (+ sum num)) sum)))

; exercise 3.2
(define (make-monitored f)
  (let ((monitor-counter 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) monitor-counter)
	    ((eq? input 'reset-counter)
	     (begin (set! monitor-counter 0) 0))
	    (else (begin (set! monitor-counter (+ monitor-counter 1)) (f input)))))))

; exercise 3.3
(define (make-account balance password)
  (define secret-passwd password)
  (define passwd-counter 0)
  (define passwd-counter-limit 5)
  (define (call-the-cops)
    (error "call the cops!"))
  (begin (set! secret-passwd password)
	 (lambda (op)
	   (cond ((eq? op 'withdraw)
		  (lambda (passwd amount)
		    (if (eq? passwd secret-passwd)
			(begin (set! passwd-counter 0)
			       (cond ((>= balance amount)
				      (begin (set! balance (- balance amount))
					     balance))
				     (else (error "Insufficient funds!"))))
			(begin (set! passwd-counter (+ passwd-counter 1))
			       (if (= passwd-counter passwd-counter-limit)
				   (call-the-cops)
				   (error "Incorrect Password!"))))))
		 ((eq? op 'deposite)
		   (lambda (passwd amount)
		    (if (eq? passwd secret-passwd)
			(begin (set! passwd-counter 0)
			       (set! balance (+ balance amount)) balance)
			(begin (set! passwd-counter (+ passwd-counter 1))
			       (if (= passwd-counter passwd-counter-limit)
				   (call-the-cops)
				   (error "Incorrect Password!"))))))
		 (else (error "Unknown operation -> " (list op)))))))

; benefits of introducing assignment
; exercise 3.7
(define (make-joint acc old-pwd new-pwd)
  (lambda (op)
    (cond ((eq? op 'withdraw)
	   (lambda (passwd amount)
	     ((acc 'withdraw)
	      (if (eq? passwd new-pwd)
		  old-pwd 'invalidpasswd) amount)))
	  ((eq? op 'deposite)
	   (lambda (passwd amount)
	     ((acc 'deposite)
	      (if (eq? passwd new-pwd)
		  old-pwd 'invalidpasswd) amount)))
	  (else (error "Unknown operation -> " (list op))))))

; exercise 3.8
(define (make-f)
  (let ((switch 1))
    (lambda (v)
      (if (= switch 1)
	  (if (> v 0) v
	      (begin (set! switch 0) 0)) 0))))
	  
; exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x) y
	(let ((left-stuff (cdr x)))
	  (set-cdr! x y)
	  (loop left-stuff x))))
  (loop x '()))

; exercise 3.16
(define (count-pairs x)
  (let ((s '()))
    (define (insert x)
      (if (null? s) (set! s (list x))
	  (set-cdr! s (cons x (cdr s)))))
    (define (in-set s x)
      (cond ((null? s) #f)
	    ((eq? x (car s)) #t)
	    (else (in-set (cdr s) x))))
    (define (count-pairs-helper x)
      (if (not (pair? x)) 0
	  (+ (count-pairs-helper (car x))
	     (count-pairs-helper (cdr x))
	     (if (in-set s x) 0
		 (begin (insert x) 1)))))
    (count-pairs-helper x)
    (length s)))
		
(define (last-pair x)
  (if (null? (cdr x))
      x (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x))

; exercise 3.18
(define (has-circle l)
  (define (next x)
    (if (pair? x) (cdr x) '()))
  (define (iter a b)
    (cond ((not (pair? b)) #f)
	  ((eq? a b) #t)
	  (else (iter (next a) (next (next b))))))
  (iter l (next l)))

; represent queues
(define (make-queue) (cons '() '()))
(define (set-front q f)
  (set-car! q f))
(define (set-rear q r)
  (set-cdr! q r))
(define (empty-queue? q)
  (and (null? (car q))
       (null? (cdr q))))
(define (insert-queue! q x)
  (let ((rear (cdr q))
	(newnode (cons x '())))
    (if (null? rear)
	(begin (set-front q newnode)
	       (set-rear q newnode))
	(begin (set-cdr! rear newnode)
	       (set-rear q newnode)))
    (car q)))
(define (delete-queue! q)
  (if (empty-queue? q)
      (error "EMPTY_QUEUE!")
      (let ((front (car q)))
	(let ((node (car front)))
	  (begin (set-front q (cdr front))
		 (if (null? (car q))
		     (set-rear q '())) node)))))

; exercise 3.22
(define (make-queue)
  (let ((front '())
	(rear '()))
    (lambda (op) 
      (cond ((eq? op 'empty?) (null? front))
	    ((eq? op 'insert)
	     (lambda (x)
	       (let ((newnode (cons x '())))
		 (if (null? front)
		     (begin (set! front newnode)
			    (set! rear newnode))
		     (begin (set-cdr! rear newnode)
			    (set! rear newnode))) front)))
	    ((eq? op 'delete)
	     (lambda ()
	       (if (null? front)
		   (error "EMPTY_QUEUE~!")
		   (let ((node (car front)))
		     (set! front (cdr front))
		     (if (null? front)
			 (set! rear '())) node))))
	    (else (error "UNKNOWN OPERATION"))))))
(define (insert-queue! q x)
  ((q 'insert) x))
(define (delete-queue! q)
  ((q 'delete)))
(define (empty-queue? q)
  (q 'empty?))

; exercise 3.23
(define (make-deque) (cons '() '()))
(define (front-deque dq) (car dq))
(define (rear-deque dq) (cdr dq))
(define (empty-deque? dq)
  (and (null? (front-deque dq))
       (null? (rear-deque dq))))
(define (dq-set-front dq x) (set-car! dq x))
(define (dq-set-rear dq x) (set-cdr! dq x))
(define (front-insert-deque! dq x)
  (let ((front (front-deque dq)))
    (let ((newnode (cons (cons '() x) front)))
      (if (null? front)
	  (dq-set-rear dq newnode)
	  (set-car! (car front) newnode))
      (dq-set-front dq newnode))))

(define (front-delete-deque! dq)
  (if (empty-deque? dq)
      (error "front-delete-deque! -> Empty deque!")
      (let ((front (front-deque dq)))
        (dq-set-front dq (cdr front))
	(if (null? (front-deque dq))
	    (dq-set-rear dq '())
	    (set-car! (car (front-deque dq)) '()))
	(cdr (car front)))))

(define (rear-insert-deque! dq x)
  (let ((rear (rear-deque dq)))
    (let ((newnode (cons (cons rear x) '())))
      (if (null? rear)
	  (dq-set-front dq newnode)
	  (set-cdr! rear newnode))
      (dq-set-rear dq newnode))))

(define (rear-delete-deque! dq)
  (if (empty-deque? dq)
      (error "rear-delete-deque! -> Empty deque!")
      (let ((node (car (rear-deque dq))))
	(dq-set-rear dq (car node))
	(if (null? (rear-deque dq))
	    (dq-set-front dq '())
	    (set-cdr! (rear-deque dq) '()))
	(cdr node))))

; representing tables
(define (assoc key records)
  (if (null? records) #f
      (if (equal? key (caar records)) (car records)
	  (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record (cdr record) #f)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record (set-cdr! record value)
	(set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table) (list '*table*))

; two dimensional table
(define (td-lookup k1 k2 table)
  (let ((record4k1 (assoc k1 (cdr table))))
    (if record4k1
	(let ((record (assoc k2 (cdr record4k1))))
	  (if record (cdr record) #f)) #f)))

(define (td-insert! k1 k2 value table)
  (let ((record4k1 (assoc k1 (cdr table))))
    (if record4k1
	(let ((record (assoc k2 (cdr record4k1))))
	  (if record (set-cdr! record value)
	      (set-cdr! record4k1 (cons (cons k2 value) (cdr record4k1)))))
	(set-cdr! table (cons (list k1 (cons k2 value)) (cdr table)))))
  'ok)

; local tables
(define (make-local-table)
  (let ((table (list '*table*)))
    (define (td-lookup k1 k2)
      (let ((record4k1 (assoc k1 (cdr table))))
	(if record4k1
	    (let ((record (assoc k2 (cdr record4k1))))
	      (if record (cdr record) #f)) #f)))
    (define (td-insert! k1 k2 value)
      (let ((record4k1 (assoc k1 (cdr table))))
	(if record4k1
	    (let ((record (assoc k2 (cdr record4k1))))
	      (if record (set-cdr! record value)
		  (set-cdr! record4k1 (cons (cons k2 value) (cdr record4k1)))))
	    (set-cdr! table (cons (list k1 (cons k2 value)) (cdr table)))))
      'ok)
    (lambda (op)
      (cond ((eq? op 'lookup) td-lookup)
	    ((eq? op 'insert) td-insert!)
	    (else (error "local table -> unknown operation!"))))))

; exercise 3.25
(define (safe-assoc key records)
  (cond ((null? records) 0)
	((not (pair? records)) 1)
	(else 
	 (let ((first-rec (car records)))
	   (if (not (pair? first-rec)) 2
	       (if (equal? key (car first-rec)) first-rec
		   (safe-assoc key (cdr records))))))))

(define (generic-lookup table . keys)
  (define (lookup-iter t keys)
    (if (null? keys) t
	(let ((next-record (safe-assoc (car keys) (cdr t))))
	  (if (pair? next-record)
	      (lookup-iter next-record (cdr keys)) #f))))
  (lookup-iter table keys))

(define (generic-insert! table value . keys)
  (define (insert-iter t keys)
    (let ((key (car keys))
	  (left-keys (cdr keys)))
      (let ((record (safe-assoc key (cdr t))))
	(cond ((pair? record)
	       (if (null? left-keys) (set-cdr! record value)
		   (insert-iter record left-keys)))
	      ((or (= record 1) (= record 2))
	       (begin (set-cdr! t '())
		      (insert-iter t keys)))
	      ((null? left-keys)
	       (set-cdr! t (cons (cons key value) '())))
	      (else
	       (let ((newnode (cons key '())))
		 (begin (set-cdr! t newnode)
			(insert-iter newnode left-keys))))))))
  (insert-iter table keys))

; exercise 3.27
(define (memorize f)
  (let ((table (make-table)))
    (lambda (n)
      (let ((val (lookup n table)))
	(or val (let ((value (f n)))
		  (insert! n value table) value))))))
(define mem-fib
  (memorize 
   (lambda (n)
	   (cond ((= n 0) 0)
		 ((= n 1) 1)
		 (else
		  (+ (mem-fib (- n 1))
		     (mem-fib (- n 2))))))))

; simulator for digital circuits
(define (call-each procs)
  (if (null? procs) 'done
      (begin ((car procs))
	     (call-each (cdr procs)))))
(define (make-wire)
  (let ((sig-val 0) (assoc-procs '()))
    (define (set-sig! new-sig)
      (if (not (= new-sig sig-val))
	  (begin (set! sig-val new-sig)
		 (call-each assoc-procs))
	  'done))
    (define (associate-proc proc)
      (set! assoc-procs (cons proc assoc-procs))
      (proc))
    (lambda (op)
      (cond ((eq? op 'setsig) set-sig!)
	    ((eq? op 'getsig) sig-val)
	    ((eq? op 'assocproc) associate-proc)
	    (else (error "Unknown operation -> WIRE " op))))))
(define (get-signal wire) (wire 'getsig))
(define (set-signal! wire new-sig)
  ((wire 'setsig) new-sig))
(define (add-action! wire proc)
  ((wire 'assocproc) proc))
(define (logical-not x)
  (cond ((= x 0) 1)
	((= x 1) 0)
	(else (error "Invalid input for logical-not -> " x))))
(define (inverter input output)
  (define (invert-proc)
    (let ((expected-output (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! output expected-output)))))
  (add-action! input invert-proc) 'ok)
(define (logical-and a b)
  (cond ((and (= a 0) (= b 0)) 0)
	((and (= a 0) (= b 1)) 0)
	((and (= a 1) (= b 0)) 0)
	((and (= a 1) (= b 1)) 1)
	(else (error "Invalid input for logical-and" (list a b)))))
(define (and-gate a b output)
  (define (and-proc)
    (let ((expected-output (logical-and (get-signal a)
					(get-signal b))))
      (after-delay and-gate-delay
		   (lambda () (set-signal! output expected-output)))))
  (add-action! a and-proc)
  (add-action! b and-proc) 'ok)

(define (logical-or a b)
  (cond ((and (= a 0) (= b 0)) 0)
	((and (= a 0) (= b 1)) 1)
	((and (= a 1) (= b 0)) 1)
	((and (= a 1) (= b 1)) 1)
	(else (error "Invalid input for logical-or" (list a b)))))
(define (or-gate a b output)
  (define (or-proc)
    (let ((expected-output (logical-or (get-signal a)
				       (get-signal b))))
      (after-delay or-gate-delay
		   (lambda () (set-signal! output expected-output)))))
  (add-action a or-proc)
  (add-action b or-proc) 'ok)

; exercise 3.29
;(define (or-gate a b output)
 ; (let ((inva (make-wire))
;	(invb (make-wire))
;	(and-inva-invb (make-wire)))
 ;   (inverter a inva)
  ;  (inverter b invb)
   ; (and-gate inva invb and-inva-invb)
   ; (inverter and-inva-invb output) 'ok))
; or-gate delay in terms of and-gate-delay and inverter-delay
; or-gate-delay = 2 * inverter-delay + and-gate-delay

(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s) 'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out) 'ok))

; exercise 3.30
(define (ripple-carry-adder A B S C)
  (define (adder-helper A B S c-in)
    (if (null? A) (begin (set! C c-in) 'ok)
	(let ((a (car A))
	      (b (car B))
	      (s (car S))
	      (c (make-wire)))
	  (full-adder a b c-in s c)
	  (set! c-in c))))
  (let ((c (make-wire)))
    (adder-helper A B S c)))

; agenda

(define (the-agenda) (make-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda) 'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))
(define (after-delay delay proc)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  proc the-agenda))

(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display name)
		 (display " ")
		 (display (current-time the-agenda))
		 (display " New-Value = ")
		 (display (get-signal wire)))))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)