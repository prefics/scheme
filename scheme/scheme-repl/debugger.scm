;;; debugger.scm -- debugger for Schemy

(define (show-backtrace condition)
  (display "Backtrace:") (newline)
  (let* ((return (condition/return condition))
         (template (continuation-template return))
         (debug (template-debug template)))
    (if debug
        (let ((name (debug-name debug))
              (pc (continuation-pc return)))
          (display "name") (display "@") (display pc) (newline)
          (let loop ((frame (continuation-cont return)))
            (if (vector? frame)
                (let* ((template (frame-template frame))
                       (debug (template-debug template)))
                  (if debug
                      (let ((name (debug-name debug))
                            (pc (frame-pc frame)))
                        (display name) (display "@") (display pc) 
                        (newline)
                        (loop (frame-previous frame)))
                      (begin
                        (display "No information for frame") (newline)
                        (loop (frame-previous frame)))))))))))
                  
(define (backtrace)
  (suspend-cc 
   (lambda (cont)
     (let* ((return cont)
	    (template (continuation-template return))
	    (debug (template-debug template)))
       (if debug
	   (let ((name (debug-name debug))
		 (pc (continuation-pc return)))
	     (display name) (display "@") (display pc) (newline)
	     (let loop ((frame (continuation-cont return)))
	       (if (vector? frame)
		   (let* ((template (frame-template frame))
			  (debug (template-debug template)))
		     (if debug
			 (let ((name (debug-name debug))
			       (pc (frame-pc frame)))
			   (display name) (display "@") (display pc) 
			   (newline)
			   (loop (frame-previous frame)))
			 (begin
			   (display "No information for frame") (newline)
			   (loop (frame-previous frame)))))))))))))

(define *debug-commands* '())

(define (add-debug-command! name proc)
  (let ((cmd (assq name *debug-commands*)))
    (if cmd
        (set-cdr! cmd proc)
        (set! *debug-commands* (cons (cons name proc) *debug-commands*)))))

(define-syntax define-debug-command 
  (syntax-rules ()
    ((define-debug-command ?name . ?body)
     (begin
       (define ?name (lambda () . ?body))
       (add-debug-command! '?name ?name)))))

(define (read/execute-command)
  (read-char)
  (let* ((command-name (read)))
    (if (eof-object? command-name)
        #f
        (let ((command (lookup-command command-name)))
   ;    (display "->") (display command-name) (display "<-") (newline)
          (if command
              (condition-case
               ((command/proc command))
               (<condition>
                (lambda (condition)
                  (display "Error during execution of command ")
		  (display (command/name command)))))
              (begin
                (display "Unknown command ")
                (display command-name)
                (newline)))))))

(define (debugger)
  (display "debug>") 
  (let ((fnb-char (first-non-blank)))
      (if (eof-object? fnb-char)
          (begin 
            (display "going back to REPL")
            (newline))
          (begin 
            (if (char=? fnb-char #\,)
                (read/execute-debugger-command)
                (evaluate-in-context))
            (repl))))))
   
(define-debug-command locals (display "not implemented"))
(define-debug-command up (display "not implemented"))
(define-debug-command down (display "not implemented"))
(define-debug-command evaluate-in-context (display "not implemented"))
(define-debug-command args (display "not implemented"))
(define-debug-command return (display "not implemented"))
(define-debug-command reset (display "not implemented"))
(define-debug-command backtrace (display "not implemented"))
