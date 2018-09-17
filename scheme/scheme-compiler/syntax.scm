;;; syntax.scm -- implementation of "Macros that works"
;;

;;; Description:

;; This file implements a basic macro processor for Scheme. It is based on the
;; paper "Macros that works" by Will Clinger & Johnathan Rees. It also 
;; implements the hygienic macros through explicit renaming defined in 
;; "Hygienic Macros Through Explicit Renaming" by Will Clinger.

;; The basic entry point is the procedure MACRO-EXPAND that takes two parameters
;; (EXP ENV). EXP is the Scheme expression to expand. ENV defines the 
;; environment in which to expand the expression.

;; It is possible to define new macros with the function DEF-SYNTAX. It takes 
;; two arguments ID and TRANSFORMER. ID represents the symbol of the new macros.
;; TRANSFORMER is the procedure that expands the form. It is procedure of three
;; parameters EXP, RENAME and COMPARE. EXP is the form to expand. RENAME is a
;; procedure allowing to rename the free identifier introduces during macro 
;; expansion. This prevents them to be captured and ensure the hygienic 
;; expansion of the form. COMPARE is a procedure that allows to compare two 
;; denotations to see if they denote the same object.

;; Some examples of use are:
;;
;; > (macro-expand '(push x 10) *env*)
;;
;; > (def-syntax 'push (lambda (e r c) 
;;                       (let ((x (cadr e)) 
;;                             (v (caddr e)))
;;                         `(,(r 'set!) ,x (,(r 'cons) ,v ,x)))))

;; Finally a procedure MACRO-RULES is defined to permit pattern based macros to
;; be defined. MACRO-RULES takes one argument which is a list of pattern-value
;; pairs. The pattern defines the general look of the macros and allows to 
;; specify variables. The value represents how the resulting form looks like. 
;; The variables are substituted in the value form.

;; Here are some more examples:

;; > (def-syntax 'push (macro-rules '(((push ?x ?v) (set! ?x (cons ?v ?x))))))

;;; Code:

; (define (init-syntax-from-module module)
;   (set! *define-syntax* (lookup-syntax 'define-syntax 'scheme-runtime))
;   (set! *lambda* (lookup-syntax 'lambda 'scheme-runtime))
;   (set! *let* (lookup-syntax 'let 'scheme-runtime))
;   (set! *begin* (lookup-syntax 'begin 'scheme-runtime))
;   (set! *set!* (lookup-syntax 'set! 'scheme-runtime))
;   (set! *if* (lookup-syntax 'if 'scheme-runtime))
;   (set! *letrec* (lookup-syntax 'letrec 'scheme-runtime))
;   (set! *let-syntax* (lookup-syntax 'let-syntax 'scheme-runtime))
;   (set! *quote* (lookup-syntax 'quote 'scheme-runtime)))
        
(define (same? id1 id2)
  (cond ((and (symbol? id1) (symbol? id2) (eq? id1 id2)) #t)
        ((and (scoped? id1) (scoped? id2) (eq? (scoped/uid id1) (scoped/uid id2)))
         #t)
        (else #f)))

(define (syn/lookup e x)
  (if (or (null? e) (symbol? e))
      (let ((syn (lookup-syntax x e)))
        (if syn
            syn
            (make-scoped x e)))
      (if (pair? e)
          (let ((entry (car e)))
            (if (same? (car entry) x)
                (cdr entry)
                (syn/lookup (cdr e) x))))))

(define (make-syntax-env env)
  (lambda (name)
    (syn/lookup env name)))

;;; Syntax

(define (define-syntax? o)            (and (vector? o) 
                                           (eq? 'define-syntax (vector-ref o 0))))
(define (make-define-syntax key expr) (vector 'define-syntax key expr))
(define (define-syntax/name o)        (vector-ref o 1))
(define (define-syntax/expr o)        (vector-ref o 2))

(define (make-lambda args body) (vector 'lambda args body))
(define (lambda? o)                   (and (vector? o) 
                                           (eq? 'lambda (vector-ref o 0))))
(define (lambda/formals o)            (vector-ref o 1))
(define (lambda/body o)               (vector-ref o 2))

(define (make-if cond then else)      (vector 'if cond then else))
(define (if? exp)                     (and (vector? exp) 
                                           (eq? 'if (vector-ref exp 0))))
(define (if/exp exp)                  (vector-ref exp 1))
(define (if/then exp)                 (vector-ref exp 2))
(define (if/else exp)                 (vector-ref exp 3))

(define (make-set! var exp)           (vector 'set! var exp))
(define (set!? exp)                   (and (vector? exp) 
                                           (eq? 'set! (vector-ref exp 0))))
(define (set!/lhs exp)                (vector-ref exp 1))
(define (set!/rhs exp)                (vector-ref exp 2))

(define (make-let bindings body)      (vector 'let bindings body))
(define (let? exp)                    (and (vector? exp) 
                                           (eq? 'let (vector-ref exp 0))))
(define (let/bindings exp)            (vector-ref exp 1))
(define (let/body exp)                (vector-ref exp 2))

(define (binding/var b)               (car b))
(define (binding/exp b)               (cadr b))

(define (make-application op args)    (vector 'application op args))
(define (application? exp)            (and (vector? exp) 
                                           (eq? 'application (vector-ref exp 0))))
(define (app/operator exp)            (vector-ref exp 1))
(define (app/operands exp)            (vector-ref exp 2))

(define (make-literal lit)            (vector 'literal lit))
(define (literal? exp)                (and (vector? exp) 
                                           (eq? 'literal (vector-ref exp 0))))
(define (literal/val exp)             (vector-ref exp 1))

(define (make-begin body)             (vector 'begin body))
(define (begin? exp)                  (and (vector? exp)
                                           (eq? 'begin (vector-ref exp 0))))
(define (begin/body exp)              (vector-ref exp 1))

(define (make-letrec bindings body) (vector 'letrec bindings body))
(define (letrec? exp)                 (and (vector? exp)
                                           (eq? 'letrec (vector-ref exp 0))))
(define (letrec/bindings exp)         (vector-ref exp 1))
(define (letrec/body exp)             (vector-ref exp 2))
          
(define (make-scoped name uid)        (vector 'scoped name uid))
(define (scoped? exp)                 (and (vector? exp)
					   (eq? 'scoped (vector-ref exp 0))))
(define (scoped/uid exp)              (vector-ref exp 2))
(define (scoped/name exp)             (vector-ref exp 1))

(define (make-primitive-call name args)
  (vector 'primitive-call name args))
(define (primitive-call? exp)         (and (vector? exp)
					   (eq? 'primitive-call (vector-ref exp 0))))
(define (primitive-call/name exp)     (vector-ref exp 1))
(define (primitive-call/args exp)     (vector-ref exp 2))

;;; syntax is defined in the runtime system (module.scm)...
; (define (make-syntax proc env)        (vector 'syntax proc env))
; (define (syntax? syn)                 (and (vector? syn)
;                                            (eq? 'syntax (vector-ref syn 0))))
; (define (syntax/transformer syn)      (vector-ref syn 1))
; (define (syntax/env syn)              (vector-ref syn 2))

(define (make-special proc)           (vector 'special proc))
(define (special? x)                  (and (vector? x)
                                           (eq? 'special (vector-ref x 0))))
(define (special/proc special)        (vector-ref special 1))

;;; EXPANDERS

(define (expand-define-syntax exp env)
  (let ((key (cadr exp))
        (body (syntax-expand (caddr exp) env)))
    (make-define-syntax key body)))

(define (expand-lambda exp env)
 (let* ((params (make-proper (cadr exp)))
        (env* (lambda (id) (if (member id params) id (env id))))
        (exp* (expand-body (cddr exp) env*)))
   (make-lambda (cadr exp) exp*)))

(define (expand-let-syntax exp env)
  (error "let-syntax! form not supported"))

(define (expand-bindings bindings env-body env-bindings)
  (map (lambda (b)
         (if (pair? b)
             (list (car b) (syntax-expand (cadr b) env-bindings))
             (list b (make-application (make-scoped 'unspecific-object 
                                                    'scheme-runtime)
                                       '()))))
       bindings))

(define (expand-let exp env)
  (let* ((b (map (lambda (b) (if (pair? b) (car b) b)) (cadr exp)))
         (env* (lambda (id) (if (member id b) id (env id))))
	 (bindings (expand-bindings (cadr exp) env* env))
	 (body (expand-body (cddr exp) env*)))
    (make-let bindings body)))
  
(define (expand-letrec exp env) 
  (let* ((b (map (lambda (b) (if (pair? b) (car b) b)) (cadr exp)))
         (env* (lambda (id) (if (member id b) id (env id))))
	 (bindings (expand-bindings (cadr exp) env* env*))
	 (body (expand-body (cddr exp) env*)))
    (make-letrec bindings body)))

(define (expand-begin exp env)
  (make-begin (map (lambda (e) (syntax-expand e env)) (cdr exp))))

(define (expand-set! exp env)
  (let ((val (syntax-expand (caddr exp) env))
        (var (env (cadr exp))))
    (if (special? var)
        (make-set! (cadr exp) val)
        (make-set! var val))))

(define (expand-if exp env)
  (let ((cond (syntax-expand (cadr exp) env))
        (then (syntax-expand (caddr exp) env))
        (else (if (pair? (cdddr exp))
                  (syntax-expand (cadddr exp) env)
                  (make-application (make-scoped 'unspecific-object 
                                                 'scheme-runtime)
                                    '()))))
    (make-if cond then else)))

(define (expand-body exp env)
  (map (lambda (e) (syntax-expand e env)) exp))

(define (name? x)
  (or (symbol? x) (scoped? x)))

(define (bind-aliases uid mac env-of-use)
  (let ((env-of-definition (make-syntax-env (syntax/env mac))))
    (lambda (name)
      (if (and (scoped? name)
               (eqv? (scoped/uid name) uid))
          (env-of-definition (scoped/name name))
          (env-of-use name)))))

(define (comparison-procedure env-of-expansion)
  (lambda (id1 id2)
    (if (eqv? id1 id2)
        #t
        (if (and (scoped? id1) (scoped? id2))
            (let ((id1* (env-of-expansion id1))
                  (id2* (env-of-expansion id2)))
              (same? id1* id2*))
            #f))))

(define (transcribe mac exp env-of-use uid)
  (let* ((env-for-expansion (bind-aliases uid mac env-of-use))
         (rename (lambda (name) (make-scoped name uid)))
         (compare (comparison-procedure env-for-expansion)))
    ((syntax/transformer mac) exp rename compare)))

(define generate-unique-id!
  (let ((uid 0))
    (lambda ()
      (let ((val uid))
        (set! uid (+ uid 1))
        val))))

(define (expand-syntax syntax exp env-of-use)
  (let* ((uid (generate-unique-id!))
         (new-exp (transcribe syntax exp env-of-use uid)))
;    (display new-exp)
    (syntax-expand new-exp (bind-aliases uid syntax env-of-use))))

(define (expand-application exp env)
  (let ((op (syntax-expand (car exp) env))
        (args (map (lambda (e) (syntax-expand e env)) (cdr exp))))
    (make-application op args)))

(define (expand-quote exp env)
  (let ((val (cadr exp)))
 ;   (display val) (display env)
    (if (scoped? val)
        (make-literal (scoped/name val))
        (make-literal val))))

(define (expand-literal exp env)
  (make-literal exp))

;; Top level syntax environment

(define *initial-syntax-env*
  (append
   (list (cons 'define-syntax (make-special expand-define-syntax))
         (cons 'lambda        (make-special expand-lambda))
         (cons '%let           (make-special expand-let))
         (cons 'begin         (make-special expand-begin))
         (cons 'set!          (make-special expand-set!))
         (cons 'if            (make-special expand-if))
         (cons 'letrec        (make-special expand-letrec))
         (cons 'let-syntax    (make-special expand-let-syntax))
         (cons 'quote         (make-special expand-quote)))
   'scheme-runtime))

; (define (defsyntax name keywords . rules)
;   (set! *init-env* (cons (cons name (make-syntax (do-syntax-rules keywords rules)
;                                                  *init-env*))
;                          *init-env*)))

; (defsyntax 'test '()
;   '((test ?thing ?proc ?else)
;     (let ((temp ?thing))
;       (if temp (?proc temp) ?else))))

(define (syntax-expand exp env)
  (cond ((name? exp) 
         (let ((den (env exp)))
           (cond ((special? den) ((special/proc den) exp env))
                 ((syntax? den) (expand-syntax den exp env))
                 (else (env exp)))))
        ((pair? exp)
         (if (name? (car exp))
             (let ((den (env (car exp))))
               (cond ((special? den) ((special/proc den) exp env))
                     ((syntax? den) (expand-syntax den exp env))
                     (else (expand-application exp env))))
             (expand-application exp env)))
        (else (expand-literal exp env))))

;;; Pattern Matcher

(define (rule/pattern rule) (car rule))
(define (rule/exp rule) (cadr rule))

(define (repeat-pattern? pattern)
  (and (pair? pattern)
       (pair? (cdr pattern))
       (eq? '... (cadr pattern))))

(define (syntax-rules* keywords . rules)
  (do-syntax-rules keywords rules))

(define (do-syntax-rules keywords the-rules)
  (lambda (e r c)
    (let loop ((rules the-rules))
;      (display rules)
      (if (null? rules)
	  (error "no match in RULES for '~s' in '~s'" e the-rules)
	  (let* ((rule (car rules))
		 (sigma (match e c keywords (rule/pattern rule))))
;            (display "sigma") (display sigma)
	    (if sigma
		(rewrite (rule/exp rule) sigma r c 0)
		(loop (cdr rules))))))))

(define (shove sigma sigma*)
  (letrec ((shove* (lambda (x v sigma*)
		     (if (null? sigma*)
			 (list (list x v))
			 (if (eq? (caar sigma*) x)
			     (cons (cons x (cons v (cdar sigma*))) (cdr sigma*))
			     (cons (car sigma*) (shove* x v (cdr sigma*))))))))
    (if (null? sigma)
	sigma*
	(shove (cdr sigma) (shove* (caar sigma) (cadar sigma) sigma*)))))

(define (make-empty-subst rule)
  (cond ((symbol? rule)
         (list (list rule)))
        ((pair? rule)
         (append (make-empty-subst (car rule))
                 (make-empty-subst (cdr rule))))
        (else '())))

(define (match e c keywords rule)
;  (display "matching:") (display e) (display " with ") (display rule) (newline)
  (cond ((repeat-pattern? rule)
	 (let ((sigma (match e c keywords (cddr rule))))
	   (if sigma 
	       (append (make-empty-subst (car rule)) sigma)
	       (if (pair? e)
		   (let ((sigma (match (car e) c keywords (car rule))))
		     (if sigma
			 (let ((sigma* (match (cdr e) c keywords rule)))
			   (if sigma*
			       (shove sigma sigma*)
			       #f))
			 #f))
		   #f))))
	((and (symbol? rule) (not (keyword? rule)))
	 (if (memq rule keywords)
	     (if (c e rule)
		 '()
		 #f)
	     (list (list rule e))))
	((and (pair? e) (pair? rule))
	 (let ((sigma (match (car e) c keywords (car rule))))
	   (if sigma
	       (let ((sigma* (match (cdr e) c keywords (cdr rule))))
		 (if sigma*
		     (append sigma sigma*)
		     #f))
	       #f)))
	(else (if (eq? e rule)
		  '()
		  #f))))

(define (lookup-subst x sigma)
  (if (null? sigma)
      #f
      (if (pair? (car sigma))
	  (if (eq? (caar sigma) x)
	      (car sigma)
	      (lookup-subst x (cdr sigma)))
	  (if (eq? (car sigma) x)
	      x
	      (lookup-subst x (cdr sigma))))))

(define (no-more-values? x)
  (symbol? x))

(define (pattern-rank pat sigma)
  (cond ((repeat-pattern? pat) #f)
        ((symbol? pat)
         (let ((val (assq pat sigma)))
           (if val
               (length (cdr val))
               #f)))
        ((pair? pat)
         (let ((rank-head (pattern-rank (car pat) sigma))
               (rank-tail (pattern-rank (cdr pat) sigma)))
           (if rank-head
               (if rank-tail
                   (if (= rank-head rank-tail)
                       rank-head
                       (error "rank different for ~a" pat))
                   rank-head)
               (if rank-tail
                   rank-tail
                   #f))))
        (else #f)))
           
(define (rewrite e sigma r c p)
  (cond ((repeat-pattern? e)
         (let ((rank (pattern-rank (car e) sigma)))
           (let loop ((p 0)
                      (r* '()))
             (if (< p rank)
                 (loop (+ p 1) (append r* (list (rewrite (car e) sigma r c p))))
                 r*))))
	((and (symbol? e) (not (keyword? e)))
	 (let ((val (assq e sigma)))
	   (if val
	       (list-ref (cdr val) p)
	       (r e))))
	((pair? e)
	 (let ((head (rewrite (car e) sigma r c p))
	       (tail (rewrite (cdr e) sigma r c p)))
	   (cons head tail)))
	(else e)))
