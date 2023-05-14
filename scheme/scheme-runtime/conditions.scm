;;; conditions.scm -- condition interface for Schemy
;;
;;; Description:
;;
;;; Code:


(define-syntax condition-case
  (syntax-rules ()
    ((condition-case ?expr (?condition ?handler) ...)
     (call/wc
      (lambda (k)
	(let-fluid $handlers$ (append (list (vector ?condition
						    (let ((handler ?handler))
						      (lambda (c) (k (handler c))))) ...)
				      (fluid $handlers$))
		   (lambda ()
		     ?expr)))))))

(define $handlers$ (make-fluid '()))

(define (make-handler type thunk)
  (vector type thunk))

(define (handler-type h) (vector-ref h 0))
(define (handler-thunk h) (vector-ref h 1))

(define-generic default-handler (condition))
(define-generic describe-condition (condition))

(define-record-type <condition>
  (make-condition)
  condition?)

;; TODO should find something better to do
(define-method default-handler ((condition <condition>))
  "")

(define-method describe-condition ((condition <condition>))
  "Anonymous condition")

(define-record-type (<simple-condition> <condition>)
  (make-simple-condition message arguments)
  simple-condition?
  (message condition-message)
  (arguments condition-arguments))

(define-record-type (<serious-condition> <condition>)
  (make-serious-condition)
  serious-condition?)

(define-method describe-condition ((c <serious-condition>))
  "serious condition")

(define-record-type (<error> <serious-condition>)
  (make-error message)
  error?
  (message error-message))

(define-record-type (<simple-error> <error> <simple-condition>)
  (make-simple-error message arguments)
  simple-error?)

;;; Runtime condition

(define-record-type (<call-error> <error>)
  (make-call-error function arguments)
  call-error?
  (function call-error-function)
  (arguments call-error-arguments))

(define-record-type (<arity-error> <call-error>)
  (make-arity-error function arguments)
  arity-error?)

(define-method describe-condition ((c <call-error>))
  "call error")

(define-method describe-condition ((c <arity-error>))
  "function called with wrong number of arguments")

(define-record-type (<type-error> <error>)
  (make-type-error value type)
  type-error?
  (value type-error-value)
  (type  type-error-type))

(define-method describe-condition ((c <type-error>))
  "value not of correct type")

(define-record-type (<unbound-global-error> <error>)
  (make-unbound-global-error variable)
  unbound-global-error?
  (variable unbound-global-error-variable))

(define-method describe-condition ((c <unbound-global-error>))
  "unbound variable")

(define-record-type (<primitive-error> <call-error>)
  (make-primitive-error function arguments)
  primitive-error?)

;;; Restart support

(define-record-type (<restart> <condition>)
  (make-restart description)
  restart?
  (description restart-description))

(define (for-each-handler type thunk)
  (let lp ((handlers (fluid $handlers$)))
    (if (and (pair? handlers)
	     (subtype? type (handler-type (car handlers))))
	(begin
	  (thunk (car handlers))
	  (lp (cdr handlers)))
	(lp (cdr handlers)))))

(define (signal condition)
  (if (not (instance? condition <condition>))
      (error "signaling a non condition object" condition))

  (let loop ((handlers (fluid $handlers$)))
    (if (null? handlers)
	(default-handler condition)
	(let ((c (car handlers)))
	  (if (instance? condition (vector-ref c 0))
	      (let ((handler-proc (vector-ref c 1)))
		(set-fluid! $handlers$ (cdr handlers))
		(handler-proc condition))
	      (loop (cdr handlers)))))))

(define (error . data)
  (if (null? data)
      (signal (make-simple-error #f #f))
      (signal (make-simple-error (car data) (cdr data)))))

;;; TRAP HANDLERS ===========================================================
;;; initializing the default error handler

(define (set-trap! number handler)
  (suspend-cc (lambda (return)
		(let ((trap-vector (vector-ref return 12)))
		  (vector-set! trap-vector number handler)
		  (resume-cc return 'set-trap!/done)))))

(define (trap-ref number)
  (suspend-cc (lambda (return)
		(let* ((trap-vector (vector-ref return 12)))
		  (resume-cc return (vector-ref trap-vector number))))))

(define trap/bad-args 0)
(define trap/unbound-global 1)
(define trap/no-procedure 2)
(define trap/primitive 3)
(define trap/timer 4)
(define trap/signal 5)
(define trap/out-of-memory 6)

(define *signal-hook* '())
(define (signal-hooks)
  (append *signal-hook* '())) ; note this makes a copy of the list

(define (add-signal-hook! hook)
  (if (memq hook *signal-hook*)
      #f
      (set! *signal-hook* (cons hook *signal-hook*))))
(define (remove-signal-hook! hook)
  (set! *signal-hook*
	(let remove ((hooks *signal-hook*))
	  (cond
	   ((null? hooks) hooks)
	   ((eq? hook (car hooks)) (remove (cdr hooks)))
	   (else (cons (car hooks) (remove (cdr hooks))))))))

(define (with-signal-hook hook proc)
  (dynamic-wind
      (lambda () (add-signal-hook! hook))
      proc
      (lambda () (remove-signal-hook! hook))))

(define (handle-trap-signal)
  (for-each (lambda (a) (a)) *signal-hook*))

;;; Define default TRAP handler for VM

(define (initialize-conditions!)
  ;; Install a vector object in the TRAP entry of the processor
  (suspend-cc (lambda (return)
                (let ((traps (make-vector 7 'trap-not-set!)))
                  (vector-set! traps trap/out-of-memory '())
		  (vector-set! return 12 traps)
		  (resume-cc return 'done))))
  (set-trap! trap/bad-args (lambda (proc args)
;;			     (display "bad argument")
			     (signal (make-arity-error proc args))))
  (set-trap! trap/unbound-global (lambda (global)
;;				   (display "unbound global")
                                   (signal (make-unbound-global-error global))))
  (set-trap! trap/timer thread-yield-if-possible)
  (set-trap! trap/no-procedure
	     (lambda (non-proc . args)
	       (signal (make-call-error non-proc args))))
  (set-trap! trap/primitive (lambda l
;;			      (display "Primitive")
			      (signal (make-primitive-error l #f))))
  (set-trap! trap/signal handle-trap-signal))
