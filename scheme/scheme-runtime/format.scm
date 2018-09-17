;;; format.scm -- Printing utilities like in Common Lisp

(define *format-directives* '())
(define (define-format-directive char handler)
  (let ((formatter (assq char *format-directives*)))
    (if formatter
        (set-cdr! formatter handler)
        (set! *format-directives* (cons (cons char handler) *format-directives*)))))

(define-format-directive #\a
  (lambda (port template index args k)
    (display (car args) port)
    (k index (cdr args))))

(define-format-directive #\=
  (lambda (port template index args k)
    (write (car args) port)
    (k index (cdr args))))

(define-format-directive #\%
  (lambda (port template index args k)
    (newline port)
    (k index args)))

(define-format-directive #\d
  (lambda (port template index args k)
    (display (number->string (car args)) port)
    (k index (cdr args))))

(define-format-directive #\x
  (lambda (port template index args k)
    (display (number->string (car args) 16) port)
    (k index (cdr args))))

(define-format-directive #\b
  (lambda (port template index args k)
    (display (number->string (car args) 2) port)
    (k index (cdr args))))

(define-format-directive #\~
  (lambda (port template index args k)
    (display "~" port)
    (k index args)))

(define-format-directive #\o
  (lambda (port template index args k)
    (display (number->string (car args) 8) port)
    (k index (cdr args))))

(define (format port template . args)
  (let loop ((port (cond ((eq? port #t) (current-output-port))
                         ((output-port? port) port)
                         (else (current-output-port))))
             (index 0)
             (args args))
    (if (< index (string-length template))
        (let ((ch (string-ref template index)))
          (if (or (char=? ch #\%) (char=? ch #\~))
              (let* ((next-ch (string-ref template (+ index 1)))
		     (handler (assq next-ch *format-directives*)))
		(if handler
		    ((cdr handler) port template (+ index 2) args
		     (lambda (index* args*) (loop port index* args*)))
		    (loop port (+ index 2) args)))
              (begin (write-char ch port)
                     (loop port (+ index 1) args)))))))
    
(define (format-to-string template . args)
  (with-output-to-string (lambda () (apply format args))))
