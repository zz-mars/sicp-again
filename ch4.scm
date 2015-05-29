(define apply-in-underlying-scheme apply)
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
	(if (no-operands? exps) '()
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

; chapter 4.1.3 Evaluator Data Structures
(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

; for compound procedures
(define (make-procedure parameters body env)
 (list 'procedure parameters body env))
(define (compound-procedure? proc) (tagged-list? proc 'procedure))
(define (procedure-parameters proc) (cadr proc))
(define (procedure-body proc) (caddr proc))
(define (procedure-environment proc) (cadddr proc))

; operation on environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
; represent frames
(define (make-frame vars vals) (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
 (set-car! frame (cons var (frame-variables frame)))
 (set-cdr! frame (cons val (frame-values frame))))
(define (extend-environment vars vals base-env)
 (if (= (length vars) (length vals))
  (cons (make-frame vars vals) base-env)
  (if (< (length vars) (length vals))
   (error "Too many arguments supplied" vars vals)
   (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
 (define (env-loop env)
  (define (lookup vars vals)
   (cond ((null? vars) (env-loop (enclosing-environment env)))
		 ((eq? var (car vars)) (car vals))
		 (else (lookup (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
	  (let ((frame (first-frame env)))
	   (lookup (frame-variables frame)
			   (frame-values frame)))))
 (env-loop env))

(define (set-variable-value! var val env)
 (define (env-loop env)
  (define (lookup vars vals)
   (cond ((null? vars) 
		  (env-loop (enclosing-environment env)))
		 ((eq? var (car vars)) (set-car! vals val))
		 (else (lookup (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
   (error "Unbound variable" var)
   (let ((frame (first-frame env)))
	(lookup (frame-variables frame)
			(frame-values frame)))))
 (env-loop env))

(define (define-variable! var val env)
 (let ((frame (first-frame env)))
  (define (lookup vars vals)
   (cond ((null? vars) (add-binding-to-frame! var val frame))
		 ((eq? var (car vars)) (set-car! vals val))
		 (else (lookup (cdr vars) (cdr vals)))))
  (lookup (frame-variables frame)
		  (frame-values frame))))

; exercise 4.11
; new representation of frames
; frame as list of (var val) pairs
; ([var1 val1] [var2 val2]..)
; env as a list of frames : first-frame and enclosing-frame


; ch4.1.4
(define primitive-procedures
 (list (list 'car car)
	   (list 'cdr cdr)
	   (list 'cons cons)
	   (list 'null? null?)
;	   (list 'append append)
	   (list '+ +)
	   (list '- -)
	   (list '* *)
	   (list '/ /)))
(define (primitive-procedure-names)
 (map car primitive-procedures))
(define (primitive-procedure-objects)
 (map (lambda (x) (list 'primitive (cadr x)))
  primitive-procedures))
(define (setup-environment)
 (let ((initial-env (extend-environment (primitive-procedure-names)
					                    (primitive-procedure-objects)
										the-empty-environment)))
  (define-variable! 'true #t initial-env)
  (define-variable! 'false #f initial-env)
  initial-env))
(define the-global-environment (setup-environment))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
 (apply-in-underlying-scheme
  (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
 (prompt-for-input input-prompt)
 (let ((input (read)))
  (let ((output (eval input the-global-environment)))
   (annouce-output output-prompt)
   (user-print output)))
 (driver-loop))

(define (prompt-for-input string)
 (newline) (newline) (display string) (newline))
(define (annouce-output string)
 (newline) (display string) (newline))

(define (user-print object)
 (if (compound-procedure? object)
  (display (list 'compound-procedure
			(procedure-parameters object)
			(procedure-body object)
			'<procedure-env>))
  (display object)))
