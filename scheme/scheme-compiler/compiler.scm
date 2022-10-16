;;; compiler.scm -- Scheme Compiler

;;; Description:

;; This is the Scheme Compiler. The main entry point is COMPILE&GO.
;; It accepts only a subset of Scheme. It supposes that the
;; Scheme expression to compile has already been desyntaxified.
;; That is the syntax expansion has already been done.

;;; Code:

;; Debug Info is:
;; * literals
;; * bytecode
;; * file-source
;; * debug-name
;; * env
;; * original-source

(define (compiler exp module)
  (let* ((syntax-env (module/syntax module))
	 (expanded (syntax-expand exp (make-syntax-env syntax-env))))
    (if (define-syntax? expanded)
	(compile (define-syntax/expr expanded)
		 '()
		 (symbol->string (define-syntax/name expanded))
		 return-continuation)
	(assembler (compile expanded
			    '()
			    ""
			    return-continuation)
		   module))))

(define (compile&go exp module)
  ((compile-only exp module)))

(set-eval-procedure compile&go)

(define (compile-only exp module)
  (let* ((syntax-env (module/syntax module))
	 (expanded (syntax-expand exp (make-syntax-env syntax-env))))
    (if (define-syntax? expanded)
	(let* ((compiled (compile (define-syntax/expr expanded)
				  '()
                                  (symbol->string (define-syntax/name expanded))
				  return-continuation))
	       (assembled (assembler compiled module))
	       (proc (make-procedure 2)))
	  (procedure-set! proc 0 (vector 'undefined-env))
	  (procedure-set! proc 1 assembled)
	  (bind-syntax! (define-syntax/name expanded)
			(make-syntax (proc) (module/syntax module))
			module)
	  (lambda () 'syntax-done))
	(let* ((compiled (compile expanded
                                  '()
                                  ""
                                  return-continuation))
	       (assembled (assembler compiled module))
	       (proc (make-procedure 2)))
	  (procedure-set! proc 0 (vector 'undefined-env))
	  (procedure-set! proc 1 assembled)
          proc))))

;;; Utilities

(define (pos e l)
  (if (null? l)
      #f
      (if (equal? e (car l))
          0
          (let ((p (pos e (cdr l))))
            (if p
                (+ 1 p)
                p)))))

(define (length* l)
  (if (pair? l)
      (+ 1 (length* (cdr l)))
      0))

(define (make-proper l)
  (cond ((null? l) l)
        ((pair? l) (cons (car l) (make-proper (cdr l))))
        (else (list l))))
