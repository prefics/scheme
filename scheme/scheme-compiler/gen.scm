;;; gen.scm -- generate assembly for the bytecode machine

(define (compile exp env name k)
  (cond ((literal? exp)        (compile-literal exp env name k))
        ((symbol? exp)         (compile-var exp env name k))
	((scoped? exp)         (compile-scoped exp env name k))
        ((lambda? exp)         (compile-lambda exp env name k))
        ((if? exp)             (compile-if exp env name k))
        ((set!? exp)           (compile-set! exp env name k))
        ((let? exp)            (compile-let exp env name k))
	((begin? exp)          (compile-body (begin/body exp) env name k))
        ((letrec? exp)         (compile-letrec exp env name k))
	((primitive-call? exp) (compile-primitive-call exp env name k))
        ((application? exp)    (compile-application exp env name k))
	((define-syntax? exp)  (compile-define-syntax exp env name k))
        (else (error "unknown expression '~a'" exp))))

(define return-continuation #t)
(define (return-continuation? k) (eq? k #t))

;; A continuation K is either:
;; - the value #t meaning code is in tail position and tail call may happen
;; - the value #f meaning code should continue serially
;; - a symbol meaning code should then transfer to label K

(define (continue k)
  (cond ((eq? k #t) '((return)))
        ((eq? k #f) '())
        ((symbol? k) `((jump ,k)))))

(define gen-temp
  (let ((*counter* 0))
    (lambda (prefix)
      (set! *counter* (+ *counter* 1))
      (string->symbol (string-append prefix (number->string *counter*))))))

(define (subname name ext)
  (if (string=? name "")
      ext
      (string-append name " " ext)))

(define (compile-define-syntax exp env name k)
  (let* ((compiled (compile (define-syntax/expr exp)
			    '()
			    (symbol->string (define-syntax/name exp))
			    return-continuation)))
    (error "define-syntax appeared in an expansion and cannot be compiled")))

(define (compile-body exp env name k)
  (if (null? exp)
      (continue k)
      (if (null? (cdr exp))
          (compile (car exp) env name k)
          `(,@(compile (car exp) env name #f)
            ,@(compile-body (cdr exp) env name k)))))

(define (compile-var exp env name k)
  (let ((binding (lookup-env exp env)))
    (cond ((global-binding? binding)
	   `((global ,(cons (binding/name binding) #f)) ,@(continue k)))
          ((local-binding? binding)
           `((local ,(binding/up binding) ,(binding/right binding)) ,@(continue k)))
          (else
           (error "unknown binding type ~a" binding)))))

(define (compile-scoped exp env name k)
  (if (symbol? (scoped/uid exp))
      `((global ,(cons (scoped/name exp) (scoped/uid exp))) ,@(continue k))
      (compile-var exp env name k)))

(define (compile-literal exp env name k)
  `((lit ,(literal/val exp)) ,@(continue k)))

(define (lambda-documentation body)
  (if (and (pair? body) (pair? (cdr body)))
      (let ((doc (car body)))
        (if (and (literal? doc) (string? (literal/val doc)))
            (literal/val doc)
            #f))
      #f))

(define (body-without-doc body)
  (if (and (pair? body) (pair? (cdr body)))
      (let ((doc (car body)))
        (if (and (literal? doc) (string? (literal/val doc)))
            (cdr body)
            body))
      body))

(define (compile-lambda exp env name k)
  (let* ((formals (lambda/formals exp))
         (params (map (lambda (name) (if (scoped? name) (scoped/name name) name))
                      (make-proper (lambda/formals exp))))
         (body (lambda/body exp))
         (doc (lambda-documentation body)))
    (if (list? formals)
	`((closure ((debug (parameters ,params))
                    (debug (name ,name))
                    (debug (doc ,doc))
                    (debug (file ,(current-source-location)))
                    (args ,(length formals))
                    ,@(compile-body (body-without-doc body)
                                    (bind-local formals env)
                                    name return-continuation)))
          ,@(continue k))
	`((closure ((debug (parameters ,params))
                    (debug (name ,name))
                    (debug (doc ,doc))
                    (debug (file ,(current-source-location)))
                    (args>= ,(length* formals))
                    ,@(compile-body (body-without-doc body)
                                    (bind-local* formals env)
                                    name return-continuation)))
          ,@(continue k)))))

(define (compile-if exp env name k)
  (let* ((label-else (gen-temp "else-"))
         (label-end (gen-temp "end-")))
    (if (return-continuation? k)
        `(,@(compile (if/exp exp) env name #f)
          (iffalse ,label-else)
          ,@(compile (if/then exp) env name k)
          ,label-else
          ,@(compile (if/else exp) env name k))
        `(,@(compile (if/exp exp) env name #f)
          (iffalse ,label-else)
          ,@(compile (if/then exp) env name label-end)
          ,label-else
          ,@(compile (if/else exp) env name #f)
          ,label-end
          ,@(continue k)))))

(define (compile-set! exp env name k)
  (let ((lhs (set!/lhs exp)))
    `(,@(compile (set!/rhs exp) env (subname name
                                             (if (scoped? lhs)
                                                 (symbol->string (scoped/name lhs))
                                                 (symbol->string lhs))) #f)
      ,@(if (and (scoped? lhs) (symbol? (scoped/uid lhs)))
            `((set-global ,(cons (scoped/name lhs) (scoped/uid lhs))))
            (let ((binding (lookup-env lhs env)))
              (cond ((global-binding? binding)
                     `((set-global ,(cons (binding/name binding) #f))))
                    (else
                     `((set-local ,(binding/up binding) ,(binding/right binding)))))))
      ,@(continue k))))

(define (compile-let exp env name k)
  (let* ((bindings (let/bindings exp))
         (vars (map (lambda (b) (if (pair? b) (car b) b)) bindings)))
    (if (return-continuation? k)
        `(,@(compile-bindings bindings env name)
	  (debug (env ,vars))
          (env ,(length bindings))
          ,@(compile-body (let/body exp)
                          (bind-local (reverse vars) env)
                          name
                          k))
        `(,@(compile-bindings bindings env name)
	  (debug (env ,vars))
          (env ,(length bindings))
          ,@(compile-body (let/body exp)
                          (bind-local (reverse vars) env)
                          name
                          #f)
          (leave)
          ,@(continue k)))))

(define (compile-bindings bindings env name)
  (if (null? bindings)
      '()
      (let* ((binding (car bindings))
             (var (binding/var binding))
             (init (binding/exp binding)))
        `(,@(compile init env (subname name (if (scoped? var)
                                                (symbol->string (scoped/name var))
                                                (symbol->string var)))
                     #f)
          (push)
          ,@(compile-bindings (cdr bindings) env name)))))

(define (compile-letrec-bindings bindings env name)
  (if (null? bindings)
      '()
      (let* ((binding (car bindings))
             (var (binding/var binding))
             (init (binding/exp binding))
             (binding (lookup-env var env)))
        `(,@(compile init env (subname name (if (scoped? var)
                                                (symbol->string (scoped/name var))
                                                (symbol->string var)))
                     #f)
          (set-local ,(binding/up binding) ,(binding/right binding))
          ,@(compile-letrec-bindings (cdr bindings) env name)))))

(define (compile-letrec exp env name k)
  (let* ((bindings (letrec/bindings exp))
	 (vars (map (lambda (b) (car b)) bindings))
         (letrec-env (bind-local vars env))
         (body (letrec/body exp)))
    (if (return-continuation? k)
        `((debug (env ,vars))
	  (undef ,(length bindings))
          ,@(compile-letrec-bindings bindings letrec-env name)
          ,@(compile-body body letrec-env name k))
        `((debug (env ,vars))
	  (undef ,(length bindings))
          ,@(compile-letrec-bindings bindings letrec-env name)
          ,@(compile-body body letrec-env name #f)
          (leave)
          ,@(continue k)))))

(define (compile-primitive-call exp env name k)
  (let* ((args (primitive-call/args exp))
	 (code (if (null? args)
		   '()
		   `(,@(compile-operands (cdr args) env name #f)
                     ,@(compile (car args) env name #f)))))
    `(,@code (,(primitive-call/name exp)) ,@(continue k))))

(define (compile-operands ops env name k)
  (if (null? ops)
      '()
      (let ((operand (car ops)))
        `(,@(compile-operands (cdr ops) env name #f)
          ,@(compile operand env name #f)
          (push)))))

(define (compile-application exp env name k)
  (let* ((operator (app/operator exp))
         (operands (app/operands exp)))
    `(,@(compile-operands operands env name #f)
      ,@(compile operator env name #f)
      ,@(if (return-continuation? k)
           `((tail ,(length operands)))
           `((call ,(length operands)) ,@(continue k))))))

;;; Environment

(define (lookup-env id env)
  (if (null? env)
      (make-global id)
      (let* ((level (car env))
             (pos (pos id level)))
        (if pos
            (make-local 0 (+ 1 pos))
            (let ((binding (lookup-env id (cdr env))))
              (if (global-binding? binding)
                  binding
                  (make-local (+ 1 (binding/up binding))
                              (binding/right binding))))))))

(define (extend-env var reg env)
  (cons (cons var (make-local reg))
        env))

(define (bind-local locals env)
  (cons locals env))

(define (bind-local* locals env)
  (cons (make-proper locals) env))

(define (make-global name)
  (vector 'global name))

(define (make-local up right)
  (vector 'local up right))

(define (binding/name b)
  (vector-ref b 1))
(define (binding/up b)
  (vector-ref b 1))
(define (binding/right b)
  (vector-ref b 2))

(define (global-binding? b)
  (and (vector? b)
       (eq? (vector-ref b 0) 'global)))

(define (local-binding? b)
  (and (vector? b)
       (eq? (vector-ref b 0) 'local)))

;;; Intermediate Code Generation

(define (seq . l) l)
(define seq* append)
