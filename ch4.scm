(define apply-in-underlying-scheme apply)
(define (eval exp env)
 (newline)
 (display "now eval -> ")
 (display exp)
 (newline)
; (display "current env -> ")
; (display env)
; (newline)
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
		  ((let? exp) (eval (let->combination exp) env))
		  ((let*? exp) (eval (let*->nested-lets exp) env))
		  ((letrec? exp) (eval (letrec->let exp) env))
		 ; ((and? exp) (eval-and exp env))
		 ; ((or? exp) (eval-or exp env))
		  ((application? exp)
		   ; get the real operator
		   (apply (actual-value (operator exp) env)
			      ; defer the evaluation of operands for compound procedure
				  (operands exp) env))
		  (else (error "Unknown expression type -- EVAL" exp))))

; wrap exp & env into thunk
; defer the evaluating to the last possible moment
(define (delay-it exp env) (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
; memorize the thunk result
(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value obj) (cadr obj))
(define (force-it obj)
 (cond ((thunk? obj)
		(let ((value (actual-value (thunk-exp obj) (thunk-env obj))))
		 ; memorize
		 (set-car! obj 'evaluated-thunk)
		 (set-car! (cdr obj) value)
		 (set-cdr! (cdr obj) '()) 
		 value))
  ((evaluated-thunk? obj) (thunk-value obj))
  (else obj)))
; actual-value & force-it recursively calling each other
(define (actual-value exp env)
 (force-it (eval exp env)))

(define (list-of-arg-values exps env)
 (my-map exps 
  (lambda (exp) (actual-value exp env))))

(define (list-of-delayed-args exps env)
 (my-map exps 
  (lambda (exp) (delay-it exp env))))

(define (apply procedure arguments env)
 (newline)
 (display "apply -> ")
 (display procedure)
 (display " : ")
 (display arguments)
	(cond ((primitive-procedure? procedure)
		   (apply-primitive-procedure 
			procedure 
			; get the real value for primitive procedure
			(list-of-arg-values arguments env)))
		  ((compound-procedure? procedure)
		   (eval-sequence
				(procedure-body procedure)
				(extend-environment
				 (procedure-parameters procedure)
				 ; delay evaluating for compound procedures
				 (list-of-delayed-args arguments env)
				 (procedure-environment procedure))))
		  (else (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
	(if (no-operands? exps) '()
		(cons (eval (first-operand exps) env)
			  (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
	(if (true? (actual-value (if-predicate exp) env))
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

; -------------------- support let expression start
(define (let? exp) (tagged-list? exp 'let))
(define (let-vars-vals exp) (cadr exp))
(define (let-vv-part vars-vals f)
 (if (null? vars-vals) '()
  (cons (f (car vars-vals)) (let-vv-part (cdr vars-vals) f))))
(define (let-vars vv) (let-vv-part vv car))
(define (let-vals vv) (let-vv-part vv cadr))
(define (let-body exp) (cddr exp))
(define (make-let vv body) (cons 'let (cons vv body)))
; syntactic transformation
(define (let->combination exp)
 (if (pair? (cadr exp))
   (let ((vars-vals (let-vars-vals exp)))
     (cons (make-lambda (let-vars vars-vals) (let-body exp)) (let-vals vars-vals)))
   (let ((func (cadr exp))
		 (vars-vals (caddr exp))
		 (body (cdddr exp)))
	(let ((params (let-vv-part vars-vals car))
		  (args (let-vv-part vars-vals cadr)))
	 (sequence->exp 
		(list (make-define func (make-lambda params body))
	          (cons func args)))))))
; -------------------- support let expression end

; -------------------- support let* expression start
(define (make-let* vv body) (cons 'let* (cons vv body)))
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-vars-vals exp) (cadr exp))
(define (let*-body exp) (cddr exp))
(define (let*->nested-lets exp)
 (let ((vv (let*-vars-vals exp))
	   (body (let*-body exp)))
  (if (null? vv) body
   (make-let (list (car vv))
	(let ((maybe-nested-let (let*->nested-lets (make-let* (cdr vv) body))))
	 (if (let? maybe-nested-let) (list maybe-nested-let) maybe-nested-let))))))
; -------------------- support let* expression end

; -------------------- support letrec expression end
(define (letrec? expr) (tagged-list? expr 'letrec))
(define (make-letrec vars-vals body) (cons 'letrec (cons vars-vals body)))
(define (letrec-vars-vals expr) (cadr expr))
(define (letrec-body expr) (cddr expr))
(define (letrec->let expr)
 (let ((vv (letrec-vars-vals expr))
	   (body (letrec-body expr)))
  (let ((vars (my-map vv car))
		(vals (my-map vv cadr)))
   (let ((let-vv (my-map vars (lambda (x) (list x '(quote *unassigned*)))))
		 (set-ops (my-map2 vars vals make-set)))
	(make-let let-vv (my-append set-ops body))))))
; -------------------- support letrec expression end

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (make-set var val) (cons 'set! (list var val)))

(define (make-define var val) (list 'define var val))
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
		(cadddr exp) 'false))
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
;(define (cond->if exp)
;	(let ((res (expand-clauses (cond-clauses exp))))
;	 (newline)
;	 (display res)
;	 (newline)
;	 res))
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
 (list 'procedure parameters (scan-out-defines body) env))
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
; (newline)
; (display "extend-environment -> ")
; (display vars)
; (display " : ")
; (display vals)
 (if (= (length vars) (length vals))
  (cons (make-frame vars vals) base-env)
  (if (< (length vars) (length vals))
   (error "Too many arguments supplied" vars vals)
   (error "Too few arguments supplied" vars vals))))
(define (unassigned? val) (eq? val '*unassigned*))
(define (lookup-variable-value var env)
 (define (env-loop env)
  (define (lookup vars vals)
   (cond ((null? vars) (env-loop (enclosing-environment env)))
		 ((eq? var (car vars)) 
		  (let ((maybe-val (car vals)))
		   (if (unassigned? maybe-val)
			(error "Unassigned variable" var) maybe-val)))
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

(define (my-map lst f)
 (if (null? lst) '()
	 (cons (f (car lst)) (my-map (cdr lst) f))))
(define (my-map2 lst0 lst1 f)
 (define (iter l0 l1)
  (if (null? l0) '()
   (cons (f (car l0) (car l1)) (iter (cdr l0) (cdr l1)))))
 (if (not (= (length lst0) (length lst1)))
  (error "calling my-map2 with two list of different length!")
  (iter lst0 lst1)))
(define (my-filter lst f)
 (cond ((null? lst) '())
	   ((f (car lst)) (cons (car lst) (my-filter (cdr lst) f)))
	   (else (my-filter (cdr lst) f))))
(define (my-append lst0 lst1)
 (if (null? lst0) lst1
  (cons (car lst0) (my-append (cdr lst0) lst1))))
; exercise 4.16
(define (scan-out-defines p-body)
 (let ((defines (my-filter p-body definition?))
	   (the-rest (my-filter p-body (lambda (x) (not (definition? x))))))
  (if (null? defines) p-body
   (let ((vars (my-map defines definition-variable))
		 (vals (my-map defines definition-value)))
	(let ((let-var-vals (my-map vars (lambda (x) (list x '(quote *unassigned*)))))
		  (sets (my-map2 vars vals make-set)))
	 (list (make-let let-var-vals (my-append sets the-rest))))))))

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
	   (list 'display display)
	   (list 'newline newline)
;	   (list 'append append)
	   (list '= =)
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

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
 (prompt-for-input input-prompt)
 (let ((input (read)))
  (let ((output (actual-value input the-global-environment)))
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
