;;; main.scm -- main file of scms tool

(define (op-load filename)
  (let ((sys (read-system-definition-from-file filename)))
    (load-system sys)))

(define (op-install filename)
  (let ((sys (read-system-definition-from-file filename)))
    (install-system sys)))

(define (show-usage)
  (display "scms OP <filename>, where OP is load, install"))

(define (dispatch-operation op args)
  (cond ((string=? op "install") (for-each op-install args))
	((string=? op "load") (for-each op-load args))
	(else (error "Unsupported operation ~a" op))))

(define (main args)
  (cond ((null? args) (show-usage))
	((pair? args)
	 (let ((op (car args)))
	   (dispatch-operation op (cdr args))))
	(else
	 (error "internal error"))))
