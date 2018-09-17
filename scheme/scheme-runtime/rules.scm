;;; rules.scm -- syntax rules macros support 

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
	((symbol? rule)
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
	((symbol? e)
	 (let ((val (assq e sigma)))
	   (if val
	       (list-ref (cdr val) p)
	       (r e))))
	((pair? e)
	 (let ((head (rewrite (car e) sigma r c p))
	       (tail (rewrite (cdr e) sigma r c p)))
	   (cons head tail)))
	(else e)))

(define-syntax syntax-rules
  (lambda (e r c)
    (let ((keywords (cadr e))
          (rules (cddr e))
          (%do-syntax-rules (r 'do-syntax-rules))
          (%quote (r 'quote)))
      (list %do-syntax-rules 
            (list %quote keywords) 
            (list %quote rules)))))

(define-syntax let
  (syntax-rules ()
    ((let (?bindings ...) ?body ...) (%let (?bindings ...) ?body ...))
    ((let ?name ((?var ?val) ...) ?body ...)
     (letrec ((?name (lambda (?var ...) ?body ...)))
       (?name ?val ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and e1) e1)
    ((and e1 e2 e3 ...) (if e1 (and e2 e3 ...) #f))))
    
(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or e1) e1)
    ((or e1 e2 ...) 
     (let ((x e1))
     (if x x (or e2 ...))))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond) 'done)
    ((cond (else e es ...)) (begin e es ...))
    ((cond (test => e) clauses ...) 
     (let ((v test)) 
       (if v (e v) (cond clauses ...))))
    ((cond (test e es ...) clauses ...) 
     (if test 
	 (begin e es ...)
	 (cond clauses ...)))))

(define-syntax let*
  (syntax-rules ()
    ((let* () . ?body) (begin . ?body))
    ((let* ((?v ?e) ?other ...) .  ?body) 
     (let ((?v ?e)) (let* (?other ...) . ?body)))
    ((let* (?v ?vs ...) . ?body) 
     (let ((?v 'undefined)) (let* (?vs ...) . ?body)))))
    
(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
	 (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
	 (begin result1 result2 ...)
	 (case key clause clauses ...)))))

(define-syntax define
  (syntax-rules ()
    ((define (?name . ?args) ?body ...)
     (set! ?name (lambda ?args ?body ...)))
    ((define ?name ?val)
     (set! ?name ?val))))

; (define (tag-comma? x) (and (pair? x) (eq? (car x) 'unquote)))
; (define (tag-backquote? x) (and (pair? x) (eq? (car x) 'quasiquote)))
; (define (tag-comma-atsign? x) (and (pair? x) (eq? (car x) 'unquote-splicing)))
; (define (tag-data x) (cadr x))

; (define (qq-expand x)
;   (cond ((tag-comma? x) (tag-data x))
;         ((tag-comma-atsign? x) (error "Illegal Quasiquotation"))
;         ((tag-backquote? x) (qq-expand (qq-expand (tag-data x))))
;         ((pair? x) (list 'append 
;                          (qq-expand-list (car x))
;                          (qq-expand (cdr x))))
;         (else (list 'quote x))))

; (define (trace/1 name f)
;   (lambda (arg)
;     (display "In ") (display name) (newline)
;     (let ((val (f arg)))
;       (display "Out ") (display name) (display " ") (display val) (newline)
;       val)))

(define-syntax quasiquote 
  (lambda (e r c)
    (let ((tag-comma? (lambda (x) (and (pair? x) (c (car x) 'unquote))))
          (tag-backquote? (lambda (x) (and (pair? x) (c (car x) 'quasiquote))))
          (tag-comma-atsign? (lambda (x) (and (pair? x) 
                                              (c (car x) 'unquote-splicing))))
          (tag-data cadr)
          (%list (r 'list))
          (%append (r 'append))
          (%quote (r 'quote)))
      (letrec ((qq-expand 
                (lambda (x)  
                  (cond ((tag-comma? x) (tag-data x))
                        ((tag-comma-atsign? x) (error "Illegal Quasiquotation"))
                        ((tag-backquote? x) (qq-expand (qq-expand (tag-data x))))
                        ((pair? x) (list %append 
                                         (qq-expand-list (car x))
                                         (qq-expand (cdr x))))
                        (else 
                         (list %quote x)))))
               (qq-expand-list
                (lambda (x)
                   (cond ((tag-comma? x) (list %list (tag-data x)))
                        ((tag-comma-atsign? x) (tag-data x))
                        ((tag-backquote? x) (qq-expand-list (qq-expand (tag-data x))))
                        ((pair? x)
                         (list %list 
                               (list %append (qq-expand-list (car x)) (qq-expand (cdr x)))))
                        (else (list %quote (list x)))))))
        (qq-expand (cadr e))))))

;(define-syntax delay
;  (syntax-rules () ((delay e) (make-promise (lambda () e)))))

;(define-syntax force
;  (syntax-rules () 
;    ((force e) (let ((promise e)) (if (promise-value? promise)
;				      (promise-value promise)
;				      (force-promise promise))))))

;; (define-syntax do
;;   (syntax-rules ()
;;     ((do ((var init step ...) ...)
;;          (test expr ...)
;;        command ...)
;;      (letrec
;; 	 ((loop
;; 	   (lambda (var ...)
;; 	     (if test
;; 		 (begin
;; 		   (if #f #f)
;; 		   expr ...)
;; 		 (begin
;; 		   command
;; 		   ...
;; 		   (loop (do "step" var step ...)
;; 			 ...))))))
;;        (loop init ...)))
;;     ((do "step" x)
;;      x)
;;     ((do "step" x y)
;;      y)))

