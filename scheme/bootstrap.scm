;;; bootstrap.scm -- generates the necessary files to bootstrap Schemy
;;
;; Author: Pierre De Pascale
;;
;;; Code:

(define (make-nullary-primitive-transformer name)
  (lambda (exp env)
    (make-primitive-call name '())))

(define (make-unary-primitive-transformer name)
  (lambda (exp env)
    (let ((arg (syntax-expand (cadr exp) env)))
      (make-primitive-call name (list arg)))))

(define (make-binary-primitive-transformer name)
  (lambda (exp env)
    (let ((arg1 (syntax-expand (cadr exp) env))
	  (arg2 (syntax-expand (caddr exp) env)))
      (make-primitive-call name (list arg1 arg2)))))

(define (make-ternary-primitive-transformer name)
  (lambda (exp env)
    (let ((arg1 (syntax-expand (cadr exp) env))
	  (arg2 (syntax-expand (caddr exp) env))
	  (arg3 (syntax-expand (cadddr exp) env)))
      (make-primitive-call name (list arg1 arg2 arg3)))))

(define (make-fourary-primitive-transformer name)
  (lambda (exp env)
    (let ((arg1 (syntax-expand (cadr exp) env))
	  (arg2 (syntax-expand (caddr exp) env))
	  (arg3 (syntax-expand (cadddr exp) env))
	  (arg4 (syntax-expand (car (cddddr exp)) env)))
      (make-primitive-call name (list arg1 arg2 arg3 arg4)))))

(define (make-fiveary-primitive-transformer name)
  (lambda (exp env)
    (let ((arg1 (syntax-expand (cadr exp) env))
	  (arg2 (syntax-expand (caddr exp) env))
	  (arg3 (syntax-expand (cadddr exp) env))
	  (arg4 (syntax-expand (car (cddddr exp)) env))
	  (arg5 (syntax-expand (cadr (cddddr exp)) env)))
      (make-primitive-call name (list arg1 arg2 arg3 arg4 arg5)))))

(define (make-sixary-primitive-transformer name)
  (lambda (exp env)
    (let ((arg1 (syntax-expand (cadr exp) env))
	  (arg2 (syntax-expand (caddr exp) env))
	  (arg3 (syntax-expand (cadddr exp) env))
	  (arg4 (syntax-expand (car (cddddr exp)) env))
	  (arg5 (syntax-expand (cadr (cddddr exp)) env))
	  (arg6 (syntax-expand (caddr (cddddr exp)) env)))
      (make-primitive-call name (list arg1 arg2 arg3 arg4 arg5 arg6)))))

(define (make-sevenary-primitive-transformer name)
  (lambda (exp env)
    (let ((arg1 (syntax-expand (cadr exp) env))
	  (arg2 (syntax-expand (caddr exp) env))
	  (arg3 (syntax-expand (cadddr exp) env))
	  (arg4 (syntax-expand (car (cddddr exp)) env))
	  (arg5 (syntax-expand (cadr (cddddr exp)) env))
	  (arg6 (syntax-expand (caddr (cddddr exp)) env))
	  (arg7 (syntax-expand (cadddr (cddddr exp)) env)))
      (make-primitive-call name (list arg1 arg2 arg3 arg4 arg5 arg6 arg7)))))

(define (make-nineary-primitive-transformer name)
  (lambda (exp env)
    (let ((arg1 (syntax-expand (cadr exp) env))
	  (arg2 (syntax-expand (caddr exp) env))
	  (arg3 (syntax-expand (cadddr exp) env))
	  (arg4 (syntax-expand (car (cddddr exp)) env))
	  (arg5 (syntax-expand (cadr (cddddr exp)) env))
	  (arg6 (syntax-expand (caddr (cddddr exp)) env))
	  (arg7 (syntax-expand (cadddr (cddddr exp)) env))
	  (arg8 (syntax-expand (car (cddddr (cddddr exp))) env))
	  (arg9 (syntax-expand (cadr (cddddr (cddddr exp))) env)))
      (make-primitive-call name (list arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)))))

(define (define-nullary-primitive name)
  (set! *initial-syntax-env*
        (cons (cons name (make-special (make-nullary-primitive-transformer name)))
	      *initial-syntax-env*)))

(define (define-unary-primitive name)
  (set! *initial-syntax-env*
	(cons (cons name (make-special (make-unary-primitive-transformer name)))
	      *initial-syntax-env*)))

(define (define-binary-primitive name)
  (set! *initial-syntax-env*
	(cons (cons name (make-special (make-binary-primitive-transformer name)))
	      *initial-syntax-env*)))

(define (define-ternary-primitive name)
  (set! *initial-syntax-env*
	(cons (cons name (make-special (make-ternary-primitive-transformer name)))
	      *initial-syntax-env*)))

(define (define-fourary-primitive name)
  (set! *initial-syntax-env*
	(cons (cons name (make-special (make-fourary-primitive-transformer name)))
	      *initial-syntax-env*)))

(define (define-fiveary-primitive name)
  (set! *initial-syntax-env*
	(cons (cons name (make-special (make-fiveary-primitive-transformer name)))
	      *initial-syntax-env*)))

(define (define-sixary-primitive name)
  (set! *initial-syntax-env*
	(cons (cons name (make-special (make-sixary-primitive-transformer name)))
	      *initial-syntax-env*)))

(define (define-sevenary-primitive name)
  (set! *initial-syntax-env*
	(cons (cons name (make-special (make-sevenary-primitive-transformer name)))
	      *initial-syntax-env*)))

(define (define-nineary-primitive name)
  (set! *initial-syntax-env*
	(cons (cons name (make-special (make-nineary-primitive-transformer name)))
	      *initial-syntax-env*)))

(define-binary-primitive '+)
(define-binary-primitive '-)
(define-binary-primitive 'quotient)
(define-binary-primitive 'remainder)
(define-binary-primitive 'modulo)
(define-binary-primitive '*)
(define-binary-primitive '/)

(define-binary-primitive '<)
(define-binary-primitive '<=)
(define-binary-primitive '=)
(define-binary-primitive '>)
(define-binary-primitive '>=)

(define-unary-primitive 'symbol?)
(define-binary-primitive 'eq?)
(define-unary-primitive '%fixnum?)
(define-unary-primitive 'pair?)

(define-binary-primitive 'make-pair)
(define-unary-primitive 'car)
(define-unary-primitive 'cdr)
(define-binary-primitive 'set-car!)
(define-binary-primitive 'set-cdr!)
(define-unary-primitive 'null?)

(define-unary-primitive 'vector?)
(define-binary-primitive 'make-vector)
(define-unary-primitive 'vector-length)
(define-binary-primitive 'vector-ref)
(define-ternary-primitive 'vector-set!)

(define-binary-primitive 'make-string)
(define-unary-primitive 'string-length)
(define-binary-primitive 'string-ref)
(define-ternary-primitive 'string-set!)

(define-unary-primitive 'make-channel)
(define-fourary-primitive 'write-channel)
(define-unary-primitive 'read-channel)
(define-unary-primitive 'eof-object?)
(define-unary-primitive 'channel-number)
(define-unary-primitive 'channel?)
(define-unary-primitive 'open-input-channel)
(define-binary-primitive 'open-output-channel)
(define-unary-primitive 'char->number)
(define-unary-primitive 'number->char)
(define-unary-primitive 'symbol->string)
(define-unary-primitive 'string->symbol)
(define-binary-primitive 'apply)
(define-binary-primitive 'make-stob)
(define-binary-primitive 'stob-ref)
(define-ternary-primitive 'stob-set!)
(define-unary-primitive 'stob-class)
(define-unary-primitive 'stob?)
(define-unary-primitive 'stob-length)
(define-unary-primitive 'suspend-cc)
(define-binary-primitive 'resume-cc)
(define-unary-primitive 'make-bvec)
(define-unary-primitive 'bvec?)
(define-binary-primitive 'bvec-ref)
(define-ternary-primitive 'bvec-set!)
(define-unary-primitive 'bvec-length)
(define-binary-primitive 'save-image)
(define-unary-primitive 'make-procedure)
(define-unary-primitive 'procedure?)
(define-ternary-primitive 'procedure-set!)
(define-binary-primitive 'procedure-ref)
(define-unary-primitive 'procedure-length)

(define-unary-primitive 'char?)
(define-unary-primitive 'close-channel)
(define-unary-primitive 'peek-channel)
(define-unary-primitive 'string?)

(define-unary-primitive 'ref?)
(define-ternary-primitive 'make-ref)
(define-unary-primitive 'ref-name)
(define-unary-primitive 'ref-module)
(define-unary-primitive 'ref-value)
(define-binary-primitive 'set-ref-name!)
(define-binary-primitive 'set-ref-module!)
(define-binary-primitive 'set-ref-value!)

(define-binary-primitive 'set-stob-class!)

;(define-unary-primitive 'make-socket)
;(define-binary-primitive 'socket-bind)
;(define-binary-primitive 'socket-listen)
;(define-unary-primitive 'socket-accept)
;(define-binary-primitive 'socket-connect)

(define-binary-primitive 'bit-and)
(define-binary-primitive 'bit-or)
(define-binary-primitive 'bit-xor)
(define-binary-primitive 'bit-ash)
(define-unary-primitive 'bit-not)
(define-unary-primitive 'object-hash)

(define-binary-primitive '%make-rational)
(define-unary-primitive '%rational?)
(define-binary-primitive '%ratnum-set!)
(define-binary-primitive '%ratdenom-set!)
(define-unary-primitive '%ratnum)
(define-unary-primitive '%ratdenom)
(define-unary-primitive '%real->string)
(define-unary-primitive '%string->real)
(define-unary-primitive '%fixnum->real)
(define-unary-primitive '%real?)
(define-unary-primitive '%round)
(define-unary-primitive '%truncate)
(define-unary-primitive '%ceiling)
(define-unary-primitive '%floor)

(define-binary-primitive '%rationalize)
(define-unary-primitive '%exp)
(define-unary-primitive '%log)
(define-unary-primitive '%sin)
(define-unary-primitive '%cos)
(define-unary-primitive '%tan)
(define-unary-primitive '%asin)
(define-unary-primitive '%acos)
(define-unary-primitive '%atan)
(define-unary-primitive '%atan2)
(define-unary-primitive '%sqrt)
(define-binary-primitive '%expt)
(define-binary-primitive '%native-call)

(define-unary-primitive   '%xopen-display)
(define-unary-primitive   '%xclose-display)
(define-unary-primitive   '%xsynch)
(define-sixary-primitive  '%xcreate-window)
(define-binary-primitive  '%xclose-window)
(define-binary-primitive  '%xmap-window)
(define-binary-primitive  '%xunmap-window)
(define-ternary-primitive '%xcreate-gc)
(define-ternary-primitive '%xchange-gc)
(define-sevenary-primitive '%xdraw-line)
(define-fiveary-primitive '%xdraw-point)
(define-sevenary-primitive '%xdraw-rectangle)
(define-nineary-primitive '%xdraw-arc)
(define-sixary-primitive '%xdraw-text)
(define-binary-primitive '%xopen-font)
(define-binary-primitive '%xclose-font)
(define-unary-primitive '%xread-event)

(define-unary-primitive '%ffi-open)
(define-nullary-primitive '%ffi-error)
(define-binary-primitive '%ffi-sym)
(define-unary-primitive '%ffi-close)
(define-binary-primitive '%ffi-apply)
(define-ternary-primitive '%ffi-mem-ref)
(define-unary-primitive '%ffi-string-ref)
(define-ternary-primitive '%ffi-mem-set!)
(define-unary-primitive '%ffi-mirror)
(define-ternary-primitive '%ffi-deref)
(define-unary-primitive '%ffi-malloc)
(define-unary-primitive '%ffi-free)
(define-unary-primitive '%ffi-u8-ref)
(define-unary-primitive '%ffi-u16-ref)
(define-unary-primitive '%ffi-u32-ref)
(define-unary-primitive '%ffi-u64-ref)
(define-unary-primitive '%ffi-double-ref)

(define-ternary-primitive '%test+set!)

(define-binary-primitive '%assq)
(define-binary-primitive '%record-ref)
(define-ternary-primitive '%record-set!)

;;; Posix primitive

(define-nullary-primitive '%host-error)
(define-binary-primitive '%error=?)
(define-binary-primitive 'posix-open)
(define-unary-primitive 'posix-close)
(define-ternary-primitive 'posix-read)
(define-ternary-primitive 'posix-write)
(define-unary-primitive 'posix-chdir)
(define-binary-primitive 'posix-access)
(define-binary-primitive 'posix-mkdir)
(define-binary-primitive 'posix-stat)
(define-nullary-primitive 'posix-time)
(define-binary-primitive 'posix-localtime)
(define-binary-primitive 'posix-gmtime)
(define-ternary-primitive 'posix-seek)
(define-unary-primitive 'posix-sync)
(define-unary-primitive 'posix-rmdir)
(define-unary-primitive 'posix-unlink)
(define-binary-primitive 'posix-rename)
(define-binary-primitive 'posix-chmod)
(define-ternary-primitive 'posix-chown)
(define-unary-primitive 'posix-getenv)
(define-binary-primitive 'posix-setenv)
(define-nullary-primitive 'posix-fork)
(define-ternary-primitive 'posix-exec)
(define-unary-primitive 'posix-dirfiles)
(define-ternary-primitive 'posix-recv)
(define-ternary-primitive 'posix-send)
(define-binary-primitive 'posix-socket)
(define-binary-primitive 'posix-bind)
(define-binary-primitive 'posix-listen)
(define-unary-primitive 'posix-accept)
(define-binary-primitive 'posix-connect)
(define-ternary-primitive 'posix-select)
(define-binary-primitive 'posix-mknod)
(define-binary-primitive 'posix-tempnam)
(define-binary-primitive 'posix-getpwnam)
(define-binary-primitive 'posix-getgrnam)
(define-ternary-primitive 'posix-utime)
(define-unary-primitive 'posix-umask)
(define-nullary-primitive 'posix-getpid)
(define-nullary-primitive 'posix-getppid)
(define-nullary-primitive 'posix-getsid)
(define-nullary-primitive 'posix-setsid)
(define-nullary-primitive 'posix-getuid)
(define-nullary-primitive 'posix-geteuid)
(define-nullary-primitive 'posix-getgid)
(define-nullary-primitive 'posix-getegid)
(define-unary-primitive 'posix-setuid)
(define-unary-primitive 'posix-setegid)
(define-unary-primitive 'posix-seteuid)
(define-unary-primitive 'posix-setgid)

(define-unary-primitive 'posix-exit)
(define-unary-primitive 'posix-waitpid)
(define-nullary-primitive 'posix-getcwd)
(define-nullary-primitive 'posix-pipe)
(define-binary-primitive 'posix-dup2)

(define-unary-primitive '%posix-signal-block!)
(define-unary-primitive 'posix-unix-address)
(define-binary-primitive 'posix-inet-address)
(define-ternary-primitive 'posix-getsockopt)
(define-fourary-primitive 'posix-setsockopt)
(define-unary-primitive '%posix-uname)

(define-binary-primitive '%set-timer)
(define-binary-primitive '%posix-symlink)
(define-unary-primitive '%posix-readlink)
(define-binary-primitive 'posix-kill)
(define-ternary-primitive 'posix-openpty)

(define-binary-primitive '%posix-symlink)

(define-nullary-primitive '%random)

;;; Predefined syntax

(define (append! l1 e)
  (if (pair? (cdr l1))
      (append! (cdr l1) e)
      (set-cdr! l1 (begin (set-cdr! e (cdr l1)) e))))


(define (def-syntax name module-name expander)
  (append! *initial-syntax-env*
           (list (cons name (make-syntax expander *initial-syntax-env*)))))

(def-syntax 'syntax-rules 'scheme-runtime
  (lambda (e r c)
    (let ((keywords (cadr e))
	  (rules (cddr e))
	  (%syntax-rules (r 'do-syntax-rules))
	  (%quote (r 'quote)))
;      (display keywords) (display rules) (newline)
      (list %syntax-rules (list %quote keywords) (list %quote rules)))))

(def-syntax 'condition-case 'scheme-runtime
  (syntax-rules* '()
    '((condition-case ?expr (?condition ?handler) ...)
      (call/wc
       (lambda (k)
	 (let-fluid $handlers$ (append (list (vector ?condition
                                                     (let ((handler ?handler))
						       (lambda (c) (k (handler c))))) ...)
				       (fluid $handlers$))
		    (lambda ()
		      ?expr)))))))
(def-syntax 'quasiquote 'scheme-runtime
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

(def-syntax 'let 'scheme-runtime
  (syntax-rules* '()
    '((let (?bindings ...) ?body ...) (%let (?bindings ...) ?body ...))
    '((let ?name ((?var ?val) ...) ?body ...)
      (letrec ((?name (lambda (?var ...) ?body ...)))
	(?name ?val ...)))))

(def-syntax 'define 'scheme-runtime
  (syntax-rules* '()
    '((define (?name . ?args) ?body ...)
     (set! ?name (lambda ?args ?body ...)))
;    '((define (?name ?args ... . ?rest) ?body ...)
;      (set! ?name (lambda (?args ... . ?rest) ?body ...)))
    '((define ?name ?val) (set! ?name ?val))))

(def-syntax 'and 'scheme-runtime
  (syntax-rules* '()
    '((and) #t)
    '((and e1) e1)
    '((and e1 e2 e3 ...) (if e1 (and e2 e3 ...) #f))))

(def-syntax 'or 'scheme-runtime
  (syntax-rules* '()
    '((or) #f)
    '((or e1) e1)
    '((or e1 . e2) (let ((x e1)) (if x x (or . e2))))))

(def-syntax 'cond 'scheme-runtime
  (syntax-rules* '(else =>)
    '((cond) 'done)
    '((cond (else e es ...)) (begin e es ...))
    '((cond (test => e) clauses ...) (let ((v test))
				       (if v (e v) (cond clauses ...))))
    '((cond (test e es ...) clauses ...) (if test
					     (begin e es ...)
					     (cond clauses ...)))))

(def-syntax 'let* 'scheme-runtime
  (syntax-rules* '()
    '((let* () . ?body) (begin . ?body))
    '((let* ((?v ?e) ?other ...) .  ?body)
      (let ((?v ?e)) (let* (?other ...) . ?body)))
    '((let* (?v ?vs ...) . ?body)
      (let ((?v 'undefined)) (let* (?vs ...) . ?body)))))

(def-syntax 'define-record-type 'scheme-runtime
  (syntax-rules* '()
    '((define-record-type (?type ?inherit ...)
	(?constructor ?args ...)
	?pred
	(?field ?getter . ?setter) ...)
      (begin
	(define ?type (make-record-type '?type (list ?inherit ...)
					'(?field ...)))
	(define ?pred (make-record-pred ?type))
	(define ?constructor (lambda (?args ...)
			       (%let ((rec (make-record ?type)))
				 (record-set! rec '?args ?args) ...
				 rec)))
	(define-record-field ?field ?getter . ?setter) ...))
    '((define-record-type ?type
	(?constructor ?args ...)
	?pred
	(?field ?getter . ?setter) ...)
      (begin
	(define ?type (make-record-type '?type (list <top>) '(?field ...)))
	(define ?pred (make-record-pred ?type))
	(define ?constructor (lambda (?args ...)
			       (%let ((rec (make-record ?type)))
				 (record-set! rec '?args ?args) ...
				 rec)))
	(define-record-field ?field ?getter . ?setter) ...))))

(def-syntax 'define-record-field 'scheme-runtime
  (syntax-rules* '()
    '((define-record-field ?field ?getter)
      (begin
	;; (define-generic ?getter (obj))
	;; (define-method ?getter ((obj ?type)) (record-ref obj '?field))))
	(define ?getter (lambda (obj) (record-ref obj '?field)))))
    '((define-record-field ?field ?getter ?setter)
      (begin 
	;; (define-generic ?getter (obj))
	;; (define-method ?getter ((obj ?type)) (record-ref obj '?field))
	;; (define-generic ?setter (obj value))
	;; (define-method ?setter ((obj ?type) value)
	;;   (record-set! obj '?field value))))))
	(define ?getter (lambda (obj) (record-ref obj '?field)))
	(define ?setter (lambda (obj val)
			  (record-set! obj '?field val)))))))

(def-syntax 'define-init-action 'scheme-runtime
  (syntax-rules* '()
    '((define-init-action ?name (?depends ...) . ?do)
      (let ((?name (lambda () . ?do)))
        (add-init-action! (make-action '?name
                                       (list ?depends ...)
                                       ?name))
        (?name)))))

(def-syntax 'define-generic 'scheme-runtime
  (syntax-rules* '()
    '((define-generic ?name (?arg))
      (define ?name (make-generic-1 '?name '(?arg))))
    '((define-generic ?name (?args ...))
      (define ?name (make-generic '?name '(?args ...))))))

(def-syntax 'define-method 'scheme-runtime
  (lambda (e r c)
    (let ((%generic-add-method! (r 'generic-add-method!))
    	  (%let (r 'let))
    	  (%list (r 'list))
          (%lambda (r 'lambda))
	  (%top (r '<top>)))
      (let* ((generic (cadr e))
             (args (caddr e))
             (body (cdddr e))
    	     (specs (let map* ((args args))
		      (cond ((null? args) args)
			    ((pair? args)
			     (let ((arg (car args)))
			       (if (pair? arg)
				   (cons (cadr arg) (map* (cdr args)))
				   (cons %top (map* (cdr args))))))
			    (else '()))))
	     (arglist (cons 'next-method
			    (let map* ((args args))
			      (cond ((null? args) args)
				    ((pair? args)
				     (let ((arg (car args)))
				       (if (pair? arg)
					   (cons (car arg) (map* (cdr args)))
					   (cons arg (map* (cdr args))))))
				    (else args))))))
	`(,%generic-add-method! ,generic
				(let ((specs (,%list ,@specs)))
				  (,%lambda ,arglist ,@body))
				)))))

;;; Initial module definition

(define *runtime-sig*
  ;; signature of the runtime system. Here are all exported binding
  '(pair? cons car cdr set-car! set-cdr!
	  caar cadr cdar cddr
	  caaar caadr cadar caddr cdaar cdadr cddar cdddr
	  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	  null? list? list length append reverse list-tail list-ref list-set!
	  memq memv member assq assv assoc

	  string? make-string string string-length string-ref string-set!
	  string=? string-ci=? string<? string>? string<=? string>=?
	  string-ci<? string-ci>? string-ci<=? string-ci>=? substring
	  string-append string->list list->string string-copy
	  string-fill! number->string string->number

	  char? char=? char<? char>? char<=? char>=?
	  char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	  char-alphabetic? char-numeric? char-whitespace?
	  char-upper-case? char-lower-case? char->integer integer->char
	  char-upcase char-downcase

	  eqv? equal? not boolean?

	  apply map for-each
	  ;; force delay
	  call-with-current-continuation dynamic-wind call/cc call/wc

	  vector vector->list list->vector vector-fill!

          (quasiquote :syntax)
          ;; special values

          unbound-object unbound-object?
          unspecific-object unspecific-object?

	  ;; primitives

	  + - quotient remainder modulo * / < <= = > >=
	  symbol? eq? number? pair? car cdr set-car! set-cdr! null?
	  vector? make-vector vector-length vector-ref vector-set!
	  eof-object? char->number number->char symbol->string string->symbol
	  apply
	  make-bvec bvec? bvec-ref bvec-set! bvec-length
	  procedure? procedure-length procedure-ref procedure-set! make-procedure
	  char?
	  bit-or bit-and bit-xor bit-not bit-ash
	  object-hash

          numerator denominator floor ceiling truncate round rationalize
          exp log sin cos tan asin acos atan sqrt expt

	  (or :syntax)
	  (and :syntax)
	  (if :syntax)
	  (begin :syntax)
	  (let :syntax)
	  (letrec :syntax)
	  (let* :syntax)
	  (cond :syntax)
	  (set! :syntax)
	  (quote :syntax)
	  (lambda :syntax)
	  (define :syntax)
	  (define-syntax :syntax)
	  (syntax-rules :syntax)
	  (case :syntax)

          native-call

	  make-channel close-channel read-channel write-channel

          ;; number.scm
          complex? real? rational? integer? exact? inexact?
          zero? positive? negative? odd? even? max min abs gcd lcm
          numerator denominator
          floor ceiling truncate round rationalize
          exp log sin cos tan asin acos atan sqrt expt
          make-rectangular make-polar real-part imag-part magnitude angle
          exact->inexact inexact->exact

	  ;; extensions to basic standard Scheme

          ;; format.scm
          format define-format-directive format-to-string

          ;; loop.scm
          (loop :syntax)
          (define-loop-keyword :syntax)
          (define-for-iterator :syntax)

	  ;; record.scm
	  (define-record-type :syntax) <record> make-record record?
	  record-ref record-set! record-type make-record-type
	  (record-case :syntax)
	  make initialize <top> <<type>> <<class>> class-of
	  <<union>> make-type-union type-union? union-types
	  <<singleton>> make-type-singleton type-singleton? type-object
	  <<type-subclass>>  make-type-subclass type-subclass? type-class
	  t? t= t< t+
	  (define-generic :syntax) (defg :syntax) <generic> generic?
	  generic-name generic-args
	  make-generic generic-methods method-applicable?
	  method-more-specific? sorted-applicable-methods
	  applicable-methods generic-add-method!
          
          (define-method :syntax) method-specs
          (defm :syntax)

          <nil> <undefined> <unbound> <eof> <bool> <number> <real>
          <string> <integer> <char> <channel>
          <vector> <byte-vector> <pair> <closure> <symbol> <ref>
	  record-type-name record-type-supers record-type-direct-slots
	  record-type-slots record-type-methods record-type-ancestors
	  record-type-subclasses

	  instance? subtype? subclass?

          (let-keys :syntax)
          
	  ;; fluids.scm
	  fluid let-fluid set-fluid! make-fluid fluid?

	  ;; io.scm
	  <port> port-read-char port-peek-char port-read-line port-write-char
	  port-write-string port-close port-name port-position set-port-position!
	  <channel-port>
	  <string-port> output-string
	  open-input-string-port open-output-string-port
	  input-port? output-port? call-with-input-file call-with-output-file
          with-output-appended-to-file
	  with-input-from-string with-output-to-string
	  with-input-from-file with-output-to-file
	  open-input-file open-output-file
          with-input-from-port with-output-to-port
	  close-input-port close-output-port
	  channel->input-port channel->output-port
	  port->channel
	  read $eof eof-object? char-ready? peek-char read-char read-line
	  write write-object display newline write-char
	  load transcript-on transcript-off
	  current-input-port current-output-port
	  initialize-i/o! read-error! ascii->char char->ascii
          set-process-alignment-procedure!

	  resource-providers locate-resource
	  add-resource-provider remove-resource-provider
	  open-resource-input-port with-input-from-resource
	  call-with-input-resource
	  
	  ;; init.scm
	  (define-init-action :syntax)
          make-action action?
	  action-name set-action-name!
	  action-depends set-action-depends!
	  action-do set-action-do!

          add-init-action! remove-init-action! lookup-action init-actions
          init-action-exists?
          save-image-file

	  ;; modules
	  modules module-names *modules*
          make-module module? module/name set-module/name!
	  module/export set-module/export! module/open set-module/open!
	  module/doc set-module/doc! module/defined set-module/defined!
	  module/syntax set-module/syntax!
	  bind-module! unbind-module! lookup-module
          require add-module-provider delete-module-provider module-providers
          with-current-module current-module current-module-name
          lookup-ref lookup/create-ref
	  bind-ref! unbind-ref!
	  make-ref ref? ref/name set-ref/name! ref/value set-ref/value!
          ref/module set-ref/module!

	  bind-syntax! unbind-syntax! lookup-syntax
	  make-syntax syntax? syntax/env syntax/transformer syntax/module
          make-primitive primitive? primitive/name primitive/transformer

	  bound?

	  eval set-eval-procedure

	  ;; conditions.scm
	  (condition-case :syntax)
	  
	  describe-condition default-handler
	  <condition> condition? make-condition
	  <simple-condition> make-simple-condition simple-condition? condition-message condition-arguments
	  <serious-condition> make-serious-condition serious-condition
	  <error> make-error error?
	  <simple-error> make-simple-error simple-error?
	  
	  <call-error> make-call-error call-error? call-error-function call-error-arguments
	  <arity-error> make-arity-error arity-error?
	  <type-error> make-type-error type-error?
	  
	  <unbound-global-error> make-unbound-global-error unbound-global-error? unbound-global-error-variable
	  <primitive-error> make-primitive-error primitive-error?
	  
	  <restart> make-restart restart-description
	  for-each-handler
	  signal
	  error
	  initialize-conditions!
	  add-signal-hook! remove-signal-hook! with-signal-hook
	  signal-hooks
	  
          ;; debug.scm
          continuation-val continuation-stack continuation-top
          continuation-env continuation-template continuation-code
          continuation-literals continuation-pc continuation-cont
          continuation-argc continuation-dynamic continuation-trap
          set-continuation-val! set-continuation-stack! set-continuation-top!
          set-continuation-env! set-continuation-template!
          set-continuation-code! set-continuation-literals! set-continuation-pc!
          set-continuation-cont! set-continuation-argc! set-continuation-dynamic!
          set-continuation-trap!
          make-frame frame-ref frame-previous frame-env frame-template frame-pc
          frame-set! set-frame-previous! set-frame-env! set-frame-template!
          set-frame-pc! frame-code frame-literals frame-debug frame-arguments
	  frame-locals frame-length cc->frame for-each-stack-frame

          make-template template-code template-literals template-debug
          set-template-code! set-template-literals! set-template-debug!
          make-debug debug-name debug-env debug-parameters debug-doc debug-file

	  show-backtrace disassemble
	  ;; thread.scm
	  <thread> make-thread thread?
	  thread-name set-thread-name! thread-value set-thread-value!
	  thread-continuation set-thread-continuation! current-thread
	  thread-start! thread-yield! thread-suspend! thread-terminate!
	  thread-join! threads thread-block-read thread-block-write
	  thread-interrupt!
	  add-idle-hook!

	  make-spinlock spinlock? spinlock-name
	  spinlock-try-lock! spinlock-lock! spinlock-unlock! spinlock-locked?
	  with-spinlock-held

	  ;; alien.scm
	  alien-value? alien-value-value alien-value-type
	  malloc free! malloc-array malloc-ptr sizeof
	  basic-type? basic-type-name basic-type-size
	  :bool :void :char :wchar :short :ushort :int :uint :long :ulong
	  :float :double :cstring
	  alien->scheme scheme->alien
	  cstruct-type? cstruct-type-name cstruct-type-fields
	  cstruct-ref cstruct-set! (define-cstruct-type :syntax)
	  pointer-type? pointer-type-type make-pointer pointer-deref
	  (define-pointer-type :syntax)
	  make-array-type array-type? array-type-type array-type-size
	  array-ref array-set!
	  (define-alien-library :syntax)
	  (define-alien-entry :syntax)
	  (define-alien-variable :syntax)
	  alien-error!
	  alien-library-path set-alien-library-path!
	  register-alien-library! register-alien-entry! register-alien-variable!

	  ;; eta-primitives.scm

	  channel-read channel-write %host-error
	  open-input-channel open-output-channel
	  close-channel

	  ;; extensions.scm
	  
	  sort
	  delq delv delete
	  posq posv position
	  reduce filter
	  all? any?
	  make-list
	  sum
	  
	  vector-sort! subvector
	  vector-extend
	  
	  string-prefix?
	  string-position
	  string-split
	  string-join

          random
))

(define *posix-sig*
    '($owner/all $owner/read $owner/write $owner/execute
      $group/all $group/read $group/write $group/execute
      $other/all $other/read $other/write $other/execute

      $access/read $access/write $access/execute $access/exist
      $mode/fifo $mode/char $mode/file $mode/directory $mode/block $mode/link
      $mode/socket

      ;; process.scm
      (run/port :syntax) (run/file :syntax) (run/string :syntax) (run/strings :syntax)
      (run/sexp :syntax) (run/sexps :syntax) (exec-epf :syntax) (& :syntax) (run :syntax)
      exec exec-path exec/env exec-path/env exit call-terminally suspend fork
      fork/pipe fork/pipe+ pipe run/pipe*
      <process> process? make-process process/pid set-process/pid!
      wait wait-any wait-process-group getenv setenv umask set-umask! with-umask*
      signal-process open-pty fork-pty
      (with-umask :syntax) chdir cwd with-cwd* (with-cwd :syntax) pid
      parent-id process-group set-process-group! set-priority! priority nice
      user-login-name user-uid user-effective-uid user-gid user-effective-gid
      user-supplementary-gids set-uid! set-gid! process-times cpu-ticks/sec

      <user-info> make-user-info user-info? user-info-name user-info-passwd user-info-uid
      user-info-gid user-info-gecos user-info-home-dir user-info-shell
      user-name user-passwd user-gid user-gecos user-home-dir user-shell
      user-info username->uid uid->username

      <group-info> make-group-info group-info? group-info-name group-info-passwd
      group-info-gid group-info-members group-info system-name

      <system-info> make-system-info system-info?
      system-info-name system-info-node-name system-info-release
      system-info-version system-info-machine
      system-info system-name system-node-name system-release system-version
      system-machine

      add-resource-path! remove-resource-path!
      
      ;; file.scm
      create-directory! create-fifo! create-hard-link! create-symlink!
      delete-directory! delete-file! delete-filesys-object!
      read-symlink rename-file! set-file-mode! set-file-owner! set-file-group!
      set-file-time! sync-file! sync-file-system! truncate-file!
      make-file-info
      file-info? file-info-type file-info-device file-info-inode file-info-mode
      file-info-nlinks file-info-uid file-info-gid file-info-size file-info-atime
      file-info-mtime file-info-ctime
      file-info file-directory? file-fifo? file-regular? file-socket?
      file-special? file-symlink? file-not-readable? file-not-writable?
      file-not-executable? file-readable? file-writable? file-executable?
      file-not-exists? file-exists? directory-files
      create-temp-file!

      ;; file.scm
      file-name-directory? file-name-non-directory? file-name-as-directory
      directory-as-file-name file-name-absolute? file-name-directory
      file-name-nondirectory split-file-name path-list->file-name
      file-name-extension file-name-sans-extension parse-file-name decode-file-name
      replace-extension simplify-file-name resolve-file-name
      expand-file-name absolute-file-name home-dir home-file
      file-contents file-type file-device file-inode file-mode file-nlinks file-uid file-gid
      file-size file-atime file-mtime file-ctime

      ;; network.scm
      $domain/unix $domain/inet $socket/datagram $socket/stream $socket/raw
      socket? <socket> make-socket socket-input socket-output socket-type socket-family socket-peer
      socket-connect bind-listen-accept-loop
      $address/any $address/loopback $address/broadcast
      create-socket close-socket bind-socket listen-socket accept-socket connect-socket
      <socket-address> socket-address?
      <unix-socket-address> unix-socket-address? unix-socket-address-path
      <inet-socket-address> inet-socket-address? inet-socket-address-host inet-socket-address-port
      unix-address->socket-address inet-address->socket-address
      $message/out-of-bound $message/peek $message/dont-route
      receive-message receive-message! receive-message/partial receive-message!/partial
      send-message send-message/partial
      $socket/debug $socket/accept-connect $socket/reuse-address $socket/keep-alive
      $socket/dont-route $socket/broadcast $socket/use-loop-back $socket/oob-inline
      $socket/use-privileged $socket/cant-signal $tcp/no-delay $socket/send-buffer
      $socket/receive-buffer $socket/send-low-water $socket/receive-low-water
      $socket/error $socket/type $ip/time-to-live $tcp/max-segment
      $socket/send-timeout $socket/receive-timeout
      $socket/bind-to-device $socket/linger
      $level/socket $level/ip $level/tcp $level/udp
      $socket/send-timeout $socket/receive-timeout
      socket-option set-socket-option!
      <host-info> host-info? host-info-name host-info-aliases host-info-addresses
      <network-info> network-info? network-info-name network-info-aliases network-info-net
      <service-info> service-info? service-info-name service-info-aliases service-info-port service-info-protocol
      <protocol-info> protocol-info? protocol-info-name protocol-info-aliases protocol-info-number
      host-info network-info service-info protcol-info

      ;; time.scm
      make-date date? date-seconds set-date-seconds!
      date-minute set-date-minute date-hour set-date-hour!
      date-month-day set-date-month-day! date-month set-date-month!
      date-year set-date-year! date-tz-name set-date-tz-name!
      date-tz-secs set-date-tz-secs! date-summer? set-date-summer!
      date-week-day set-date-week-day! date-year-day set-date-year-day!
      date 
      time time-seconds time-milliseconds make-time
      time- time+ time=? time<? time<=? time>=? time>? time/=?
      time->number number->time seconds->time time->seconds
      ))

(define *compiler-sig* '(compile&go compile-only compile syntax-expand
                         assembler make-syntax-env
                         make-define-syntax define-syntax? define-syntax/expr define-syntax/name
                         make-lambda lambda? lambda/formals lambda/body
                         make-if if? if/exp if/then if/else
                         make-set! set!? set!/lhs set!/rhs
                         make-let let? let/bindings let/body
                         make-application application? app/operator app/operands
                         make-literal literal? literal/val
                         make-begin begin? begin/body
                         make-letrec letrec? letrec/bindings letrec/body
                         make-scoped scoped? scoped/uid scoped/name
                         make-primitive-call primitive-call? primitive-call/name primitive-call/args
                         return-continuation write-type write-fasl!
                         type/syntax type/define type/expr type/module
                         read-fasl read-type))

(define *repl-sig* '(*boot* repl))

(define *runtime* (make-module 'scheme-runtime *runtime-sig* '()
                               "Runtime system" '() *initial-syntax-env*))

(define *compiler* (make-module 'scheme-compiler *compiler-sig* '(scheme-runtime)
                                "User module" '() 'scheme-compiler))

(define *repl* (make-module 'scheme-repl *repl-sig* '(scheme-runtime scheme-compiler scheme-posix) "Scheme REPL" '() 'scheme-repl))

(define *posix* (make-module 'scheme-posix *posix-sig* '(scheme-runtime)
			     "Scheme Posix support" '()
                             (letrec ((map (lambda (p l)
                                             (cond ((pair? l)
                                                    (cons (p (car l))
                                                          (map p (cdr l))))
                                                   (else (p l))))))
                               (map (lambda (e) (if (eq? e 'scheme-runtime)
                                                    'scheme-posix
                                                    e))
                                    *initial-syntax-env*))))

(define *default-module* *runtime*)

(bind-syntax! 'define-repl-command
	      (make-syntax (syntax-rules* '()
			     '((define-repl-command name doc body ...)
			       (begin
				 (define name (lambda () body ...))
				 (set! *repl-commands*
				       (cons (make-command 'name doc name)
					     *repl-commands*)))))
			   'scheme-repl)
	      *repl*)

;;; Compiling stuff

(define (compile-file input output)
  (let ((stdout (current-output-port)))
    (with-output-to-file output
      (lambda ()
        (with-input-from-file input
          (lambda ()
            (let ((module *default-module*)
		  (fasl-port (current-output-port)))
              (let loop ((exp (read)))
                (display "compiling:" stdout) (display exp stdout) (newline stdout)
                (if (eof-object? exp)
                    'done-compiling
                    (let* ((syntax-env (module/syntax module))
                           (expanded-exp (syntax-expand exp (make-syntax-env syntax-env))))
;                      (display "Expanded:" stdout) (display expanded-exp stdout) (newline stdout)
                      (if (define-syntax? expanded-exp)
                          (let* ((expander (define-syntax/expr expanded-exp))
                                 (compiled (compile expander '() ""
                                                    return-continuation))
                                 (assembled (assembler compiled module)))
                            (write-type type/syntax fasl-port)
                            (write-fasl! (define-syntax/name expanded-exp) fasl-port)
                            (write-fasl! assembled fasl-port))
                          (let* ((compiled-exp (compile expanded-exp '() ""
                                                        return-continuation))
                                 (assembled-exp (assembler compiled-exp module)))
                            ;			  (display "compiling:") (display compiled-exp)
;                            (display expanded-exp) (newline)
                            (write-type type/expr fasl-port)
                            (write-fasl! assembled-exp fasl-port)))
                      (loop (read))))))))))))

(define (disasm code indent)
  (for-each (lambda (op)
	      (if (and (pair? op)
		       (eq? (car op) 'closure))
		  (begin
		    (display "(closure ") (newline)
		    (disasm (cadr op) (+ indent 4)))
		  (begin (display (make-string indent #\space))
			 (display op)
			 (newline))))
	    code))

(define (basic-repl)
  (display (module/name *default-module*)) (display ">")
  (let ((exp (read)))
    (cond ((eq? exp 'quit)
           (display "Exiting REPL"))
          ((eq? exp 'user)
           (set! *default-module* (lookup-module 'user))
           (basic-repl))
          ((eq? exp 'core)
           (set! *default-module* (lookup-module 'nucleus-core))
           (basic-repl))
          (else
           (let* ((syntax-env (module/syntax *default-module*))
                  (expanded (syntax-expand exp (make-syntax-env syntax-env))))
             (display "Expanded syntax: ") (display expanded) (newline)
             (if (define-syntax? expanded)
                 (begin (display "It is a syntax expression") (newline))
                 (let* ((compiled (compile expanded '() "" return-continuation)))
                   (display "Assembler code:") (newline)
;		   (display compiled)
;                   (newline)
                   (disasm compiled 0)))
;                   (let ((assembled (assembler compiled *default-module*)))
;                     (display assembled)
;                     (newline))))
             (basic-repl))))))

(define (initial-env)
  (set! *modules* '())
  (bind-module! 'scheme-runtime *runtime*)
  (bind-module! 'scheme-compiler *compiler*)
  (bind-module! 'scheme-repl *repl*)
  (bind-module! 'scheme-posix *posix*)
  (set! *default-module* *runtime*))

(define (compile-runtime)
  (with-current-module *runtime*
    (lambda ()
      (set! *default-module* *runtime*)
      (for-each compile-file
		'("scheme-runtime/base.scm"
		  "scheme-runtime/number.scm"
		  "scheme-runtime/rules.scm"
		  "scheme-runtime/format.scm"
					;              "runtime/loop.scm"
					;              "runtime/boo.scm"
		  "scheme-runtime/record.scm"
		  "scheme-runtime/fluids.scm"
		  "scheme-runtime/port.scm"
		  "scheme-runtime/read.scm"
		  "scheme-runtime/hashtable.scm"
		  "scheme-runtime/module.scm"
		  "scheme-runtime/conditions.scm"
		  "scheme-runtime/eta-primitives.scm"
		  "scheme-runtime/init.scm"
		  "scheme-runtime/debug.scm"
		  "scheme-runtime/threads.scm"
		  "scheme-runtime/alien.scm"
		  "scheme-runtime/extensions.scm")
		'("../boot/runtime-base.fasl"
		  "../boot/runtime-number.fasl"
		  "../boot/runtime-rules.fasl"
		  "../boot/runtime-format.fasl"
					;              "../boot/loop.fasl"
					;              "../boot/boo.fasl"
		  "../boot/runtime-record.fasl"
		  "../boot/runtime-fluids.fasl"
		  "../boot/runtime-port.fasl"
		  "../boot/runtime-read.fasl"
		  "../boot/runtime-hashtable.fasl"
		  "../boot/runtime-module.fasl"
		  "../boot/runtime-conditions.fasl"
		  "../boot/runtime-eta-primitives.fasl"
		  "../boot/runtime-init.fasl"
		  "../boot/runtime-debug.fasl"
		  "../boot/runtime-threads.fasl"
		  "../boot/runtime-alien.fasl"
		  "../boot/runtime-extensions.fasl")))))

(define (compile-compiler)
  (set! *default-module* *compiler*)
  (for-each compile-file
	    '("scheme-compiler/syntax.scm"
              "scheme-compiler/compiler.scm"
              "scheme-compiler/gen.scm"
              "scheme-compiler/assembler.scm"
              "scheme-compiler/fasl.scm")
	    '("../boot/compiler-syntax.fasl"
              "../boot/compiler-compiler.fasl"
              "../boot/compiler-gen.fasl"
              "../boot/compiler-assembler.fasl"
              "../boot/compiler-fasl.fasl")))

(define (compile-repl)
  (set! *default-module* *repl*)
  (compile-file "scheme-repl/repl.scm" "../boot/repl-repl.fasl"))

(define (compile-posix)
  (set! *default-module* *posix*)
  (for-each compile-file
	    '("scheme-posix/file.scm"
              "scheme-posix/filename.scm"
	      "scheme-posix/time.scm"
	      "scheme-posix/constants.scm"
	      "scheme-posix/network.scm"
	      "scheme-posix/process.scm")
	    '("../boot/posix-file.fasl"
              "../boot/posix-filename.fasl"
	      "../boot/posix-time.fasl"
	      "../boot/posix-constants.fasl"
	      "../boot/posix-network.fasl"
	      "../boot/posix-process.fasl")))

(define (compile-boot)
  (set! *default-module* *runtime*)
  (compile-file "boot.scm" "../boot/boot.fasl"))

(define (compile-initial-files)
  (display "Compiling runtime...")
  (compile-runtime)
  (display "done.") (newline)
  (display "Compiling compiler...")
  (compile-compiler)
  (display "done.") (newline)
  (display "Compiling repl...")
  (compile-repl)
  (display "done.") (newline)
  (display "Compiling posix...")
  (compile-posix)
  (display "done.") (newline)
  (display "Compiling boot...")
  (compile-boot)
  (display "done.") (newline))

(define (write-def obj file-name)
  (call-with-output-file file-name
    (lambda (port)
      (write-type type/expr port)
      (write-fasl! obj port))))


(define (read-obj file-name)
  (call-with-input-file file-name
    (lambda (port)
      (read-fasl port))))
