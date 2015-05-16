(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		  ((variable? exp) (lookup-variable-value exp env))
		  ((quoted? exp) (text-of-quotation exp))
		  ((assignment? exp) (eval-assignment exp env))
		  ((definition? exp) (eval-definition exp env))
		  ((if? exp) (eval-if exp env))
		  ((lambda? exp) (make-procedure (lambda-parameters exp)
										 (lambda-body exp) env))
		  ((begin? exp) (eval-sequence (begin-actions exp) env))
		  ((cond? exp) (eval (cond->if exp) env))
		 ; ((and? exp) (eval-and exp env))
		 ; ((or? exp) (eval-or exp env))
		  ((application? exp)
		   (apply (eval (operator exp) env)
				  (list-of-values (operands exp) env)))
		  (else (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
	(cond ((primitive-procedure? procedure)
		   (apply-primitive-procedure procedure arguments))
		  ((compound-procedure? procedure)
		   (eval-sequence
				(procedure-body procedure)
				(extend-environment
				 (procedure-parameters procedure)
				 arguments
				 (procedure-environment procedure))))
		  (else (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
	(if (no-operands? exp) '()
		(cons (eval (first-operand exps) env)
			  (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
	(if (true? (eval (if-predicate exp) env))
		(eval (if-consequent exp) env)
		(eval (if-alternative exp) env)))

(define (eval-sequence exps env)
	(cond ((last-exp? exps) (eval (first-exp exps) env))
		  (else (eval (first-exp exps) env)
			    (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
	(set-variable-value! (assignment-variable exp)
						 (eval (assignment-value exp) env)
						 env) 'ok)

(define (eval-definition exp env)
	(define-variable! (definition-variable exp)
					  (eval (definition-value exp) env)
					  env) 'ok)

(define (self-evaluating? exp)
	(cond ((number? exp) #t)
		  ((string? exp) #t)
		  (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag) #f))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
	(let ((v (cadr exp)))
		(if (symbol? v) v
		    (car v))))
(define (definition-value exp)
	(let ((v (cadr exp)))
		(if (symbol? v)
			(caddr exp)
			(make-lambda (cdr v) (cddr exp)))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
	(if (not (null? (cdddr exp)))
		(cdddr exp) 'false))
(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
	(cond  ((null? seq) seq)
		   ((last-exp? seq) (first-exp seq))
		   (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
	(expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
	(if (null? clauses) 'false
		(let ((first-clause (car clauses))
			  (rest-clauses (cdr clauses)))
		 (if (cond-else-clause? first-clause)
			 (if (null? rest-clauses)
				 (sequence->exp (cond-actions first-clause))
				 (error "ELSE clause isn't last -- COND->IF" clauses))
			 (make-if (cond-predicate first-clause)
					  (sequence->exp (cond-actions first-clause))
					  (expand-clauses rest-clauses))))))
; exercise 4.5 support for new cond syntax
; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;		(else false))
; cond above eval to 2
;(define (expand-clauses clauses)
;	(if (null? clauses) 'false
;		(let ((first-clause (car clauses))
;			  (rest-clauses (cdr clauses)))
;		 (if (cond-else-clause? first-clause)
;			 (if (null? rest-clauses)
;				 (sequence->exp (cond-actions first-clause))
;				 (error "ELSE clause isn't last -- COND->IF" clauses))
;			 (make-if (cond-predicate first-clause)
;					  (let ((cond-act (cond-actions first-clause)))
;					   (if (eq? (car cond-act) '=>)	; new syntax
;						(list (cadr cond-act) (cond-predicate first-clause))
;					    (sequence->exp (cond-actions first-clause)))) ; old syntax
;					  (expand-clauses rest-clauses))))))
;
;(define (and? exp) (tagged-list? exp 'and))
;(define (make-and and-seq) (cons 'and and-seq))
;(define (eval-and exp env)
;	(let ((and-seq (cdr exp)))
;	 (if (null? and-seq) 'true
;		 (let ((first (car and-seq))
;			   (rest (cdr and-seq)))
;		  (let ((first-res (eval first env)))
;		   (if first-res 
;			   (if (null? rest) first-res (eval-and (make-and rest) env))
;			   first-res))))))
;
;(define (and->if exp)
; (let ((and-seq (cdr exp)))
;  (if (null? and-seq) 'true
;	(let ((first (car and-seq))
;		  (rest (cdr and-seq)))
;	 (make-if first
;	  (if (null? rest) first
;	   (and->if (make-and rest))) 'false)))))
;
;(define (or? exp) (tagged-list? exp 'or))
;(define (make-or or-seq) (cons 'or or-seq))
;(define (eval-or exp env)
;	(let ((or-seq (cdr exp)))
;	 (if (null? or-seq) 'false
;	  (let ((first (car or-seq))
;			(rest (cdr or-seq)))
;	   (let ((first-res (eval first env)))
;		(if first-res first-res 
;			(eval-or (make-or rest))))))))
;
;(define (or->if exp)
; (let ((or-seq (cdr exp)))
;  (if (null? or-seq) 'false
;   (let ((first (car or-seq))
;		 (rest (cdr or-seq)))
;	(make-if first first
;	 (or->if (make-or rest)))))))
