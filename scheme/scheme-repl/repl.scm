;;; repl.scm -- basic repl system for Schemy

(define-record-type <command>
  (make-command name doc proc)
  command?
  (name command/name set-command/name!)
  (doc command/doc set-command/doc!)
  (proc command/proc set-command/proc!))

(define *repl-commands* '())

(define-syntax define-repl-command
  (syntax-rules ()
    ((define-repl-command name doc body ...)
     (begin
       (define name (lambda () body ...))
       (set! *repl-commands*
           (cons (make-command 'name doc name)
                 *repl-commands*))))))

(define (lookup-command name)
  (let loop ((commands *repl-commands*))
    (if (null? commands)
        #f
        (if (eq? name (command/name (car commands)))
            (car commands)
            (loop (cdr commands))))))

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
		  (display "error during the execution of command ")
		  (display (command/name command))
		  (newline)
                  (report-condition condition))))
              (begin
                (display "Unknown command ")
                (display command-name)
                (newline)))))))

;;; SEVERAL COMMANDS

(define $current-module$ (make-fluid #f))

(define (display/width string space)
  (display string)
  (let ((rest (- space (string-length string))))
    (if (> rest 0)
        (let ((str (make-string rest #\space)))
          (display str)))))

(define-repl-command  show-threads
  "display all threads"
  (for-each (lambda (name)
	      (display name)
	      (newline))
	    (threads)))

(define-repl-command spawn
  "execute an expression in a parallel thread"
  (let ((exp (read))
        (module (fluid $current-module$)))
    (if (eof-object? exp)
        #f
        (let ((thr (make-thread (lambda ()
				  (condition-case
				   (let ((val (eval exp module)))
				     (display ";; ")
				     (display val)
				     (newline))
				   (<condition>
				    (lambda (error)
				      (report-condition error))))
				  (thread-terminate! (current-thread)))

				"REPL-expression")))
          (thread-start! thr)))))

;; (define-repl-command time
;;   "report execution time of an expression"
;; )

(define-repl-command terminate
  "stop the execution a thread"
)

(define-repl-command help
  "display all available commands"
  (for-each (lambda (command)
              (display/width (symbol->string (command/name command)) 16)
              (display (command/doc command))
              (newline))
            *repl-commands*)
  (display "The current module is:") (newline)
  (display-module (fluid $current-module$)))

(define *show-backtrace* #t)

(define (report-condition error)
  (display "CONDITION: ")
  (display (record-type-name (class-of error))) (newline)
  (cond 
   ((unbound-global-error? error)
    (let ((var (unbound-global-error-variable error)))
      (display "Unbound global ") (display (ref/name var))
      (display " in module ") (display (ref/module var))
      (newline)))
   ((arity-error? error)
    (let ((proc (call-error-function error))
	  (args (call-error-arguments error)))
      (display "Bad number of argument to ") (display proc)
      (display " with ") (display args)
      (newline)))
   ((call-error? error)
    (let ((proc (call-error-function error))
	  (args (call-error-arguments error)))
      (display "Bad function call ") (display proc)
      (display " with ") (display args)
      (newline)))
   ((primitive-error? error)
    (display "Primitive error")
    (newline))
   ((simple-error? error)
    (display "Simple error ") (display (condition-message error))
    (display (condition-arguments error))
    (newline))
   (else
    (display "unknown error")
    (newline)))
  (if *show-backtrace* (show-backtrace error)))

(define-repl-command evaluate
  "evaluate an expression"
  (let ((exp (read))
        (module (fluid $current-module$)))
    (if (eof-object? exp)
        #f
	(condition-case
	 (let ((val (eval exp module)))
	   (display ";; ")
	   (display val)
	   (newline)
	   #t)
	 (<condition>
	  (lambda (error) (report-condition error)))))))

(define (bvec->number bvec)
  (let ((b0 (bvec-ref bvec 0))
	(b1 (bvec-ref bvec 1))
	(b2 (bvec-ref bvec 2))
	(b3 (bvec-ref bvec 3)))
    (+ (* 256 (+ (* 256 (+ (* 256 b3)
			   b2))
		 b1))
       b0)))

(define (display-time time)
  (let ((secs (bvec->number (time-seconds time)))
	(msec (time-milliseconds time)))
    (display secs) (display "s") (display msec) (display "ms")))

(define-repl-command bench
  "Measure execution time of an expression"
  (let ((exp (read))
	(module (fluid $current-module$)))
    (if (eof-object? exp)
	#f
	(condition-case
	 (let* ((start (time))
		(result (eval exp module))
		(end (time)))
	   (display ";; expression took ")
	   (display-time (time- end start))
	   (newline))
	 (<condition> (lambda (c) (report-condition c)))))))
	 
(define (load-file filename module)
  (if (file-readable? filename)
      (with-input-from-file filename
        (lambda ()
	  (display ";; Loading file ") (display filename) (newline)
          (let loop ((exp (read)))
            (if (eof-object? exp)
                'ok
                (begin
                  (condition-case
                   (eval exp module)
                   (<condition>
                    (lambda (error)
                      (display"Error when loading expression ")
		      (write exp)
		      (newline)
                      (report-condition error))))
                  (loop (read)))))))
      (error "File ~a does not exists or is not readable" filename)))

(define-repl-command load
  "load specified file"
  (let* ((filename (read))
         (real-filename (if (symbol? filename) (symbol->string filename) filename)))
    (with-cwd* (file-name-directory (if (file-name-absolute? real-filename)
					real-filename
					(string-append (cwd) real-filename)))
	       (lambda ()
		 (load-file (file-name-nondirectory real-filename)
			    (fluid $current-module$))))))

(define (load-files module files)
  (for-each
   (lambda (file)
     (let ((file* (if (symbol? file) (symbol->string file) file)))
       (load-file file* module)))
   files))

(define (module-named name)
  (let ((module (lookup-module name)))
    (if module
	module
	(let ((module (make-module name '() '() "" '() name)))
	  (bind-module! name module)
	  module))))

(define (really-load-module module-def)
  (if (and (pair? module-def)
	   (eq? 'define-structure (car module-def)))
      (let* ((name (cadr module-def))
	     (module (module-named name)))
        (bind-module! name module)
	(let loop ((def (cddr module-def)))
	  (if (pair? def)
	      (let ((directive (car def)))
		(cond ((and (pair? directive)
			    (eq? (car directive) 'export))
		       (set-module/export! module (cdr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'open))
                       (for-each ensure-module-loaded! (cdr directive))
		       (set-module/open! module (cdr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'doc))
		       (set-module/doc! module (cadr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'files))
		       (load-files module (cdr directive)))
		      (else (error "unknown directive" directive)))
		(loop (cdr def))))))
      (error "The file does not contain a module/structure definition")))

(define-repl-command lm
  "Load a new module"
  (let* ((filename (read))
	 (real-filename (if (symbol? filename) (symbol->string filename) filename)))
    (if (file-readable? real-filename)
        (let ((module-definition (with-input-from-file real-filename
                                   (lambda () (read))))
	      (dir (file-name-directory (if (file-name-absolute? real-filename)
					    real-filename
					    (string-append (cwd) real-filename)))))
	  (with-cwd* dir
	    (lambda ()
	      (really-load-module module-definition))))
        (error "File ~a is not readable or does not exist" real-filename))))

(define-repl-command lf
  "Load FASL file in current module"
  (let* ((filename (read))
         (real-filename (if (symbol? filename)
                            (symbol->string filename)
                            filename)))
    (load-fasl real-filename)))

(define (load-fasl filename)
  (let* ((module (fluid $current-module$))
	 (module-name (module/name module)))
    (if (file-exists? filename)
        (if (file-readable? filename)
            (call-with-input-file filename
              (lambda (port)
                (let loop ((type (read-type port)))
                  (if (eof-object? type)
                      'done-load-fasl 
                      (cond ((= type type/define)
                             (let* ((fasl (read-fasl port))
                                    (proc (make-procedure 2)))
                               (procedure-set! proc 0 'undefined)
                               (procedure-set! proc 1 fasl)
                               (proc)
                               (loop (read-type port))))

                            ((= type type/syntax)
                             (let* ((symbol (read-fasl port))
                                    (fasl (read-fasl port))
                                    (proc (make-procedure 2)))
                               (procedure-set! proc 0 'undefined)
                               (procedure-set! proc 1 fasl)
;                               (display ".")
                               (bind-syntax! symbol
                                             (make-syntax (proc)
                                                          (module/syntax module))
                                             module)
;                               (display "*")
                               (loop (read-type port))))
                            ((= type type/expr)
                             (let* ((fasl (read-fasl port))
                                    (proc (make-procedure 2)))
                               (procedure-set! proc 0 'undefined)
                               (procedure-set! proc 1 fasl)
                               (proc)
                               (loop (read-type port))))
                            ((= type type/module)
                             (let* ((name (read-fasl port))
                                    (export (read-fasl port))
                                    (open (read-fasl port))
                                    (doc (read-fasl port))
                                    (mod (make-module name export open doc
                                                      '() name)))
                               (for-each ensure-module-loaded! open)
                               (bind-module! name mod)
                               (set! module-name name)
                               (set! module mod)
                               (loop (read-type port)))))))))
            (begin (display ";; File ") (display filename)
		   (display " is not readable") (newline)))
	(begin (display ";; File ") (display filename)
	       (display " does not exists") (newline)))))

(define (compile-file fasl-port module filename)
  (let* ((stdout (current-output-port))
	 (filename (if (symbol? filename)
		       (symbol->string filename)
		       filename)))
    (display";; Compiling file ") (display filename) (newline)
    (with-input-from-file filename
      (lambda ()
	(let loop ((exp (read)))
;	  (display "compiling:" stdout)
;	  (display exp stdout)
	  (if (eof-object? exp)
	      'done-compiling
              (condition-case
               (let* ((syntax-env (module/syntax module))
                      (expanded-exp (syntax-expand exp
                                                   (make-syntax-env syntax-env))))
                 (if (define-syntax? expanded-exp)
                     (let* ((expander (define-syntax/expr expanded-exp))
                            (compiled (compile expander '() ""
                                               return-continuation))
                            (assembled (assembler compiled module)))
                                        ;                      (display expander stdout)
                                        ;                      (display compiled stdout)
                       (write-type type/syntax fasl-port)
                       (write-fasl! (define-syntax/name expanded-exp) fasl-port)
                       (write-fasl! assembled fasl-port)
                       (let ((proc (make-procedure 2)))
                         (procedure-set! proc 0 'undefined)
                         (procedure-set! proc 1 assembled)
                         (bind-syntax! (define-syntax/name expanded-exp)
                                       (make-syntax (proc)
                                                    (module/syntax module))
                                       module)))
                     (let* ((compiled-exp (compile expanded-exp '() ""
                                                   return-continuation))
                            (assembled-exp (assembler compiled-exp module))
                            (proc (make-procedure 2)))
                       (write-type type/expr fasl-port)
                       (write-fasl! assembled-exp fasl-port)
                       (procedure-set! proc 0 'undefined)
                       (procedure-set! proc 1 assembled-exp)
                       (proc)))
;                 (newline stdout)
                 (loop (read)))
               (<condition>
                (lambda (condition)
                  (display "Error compiling expression:") (write exp) (newline)
                  (report-condition condition))))))))))

(define (compile-file* fasl-port module filenames)
  (for-each (lambda (filename) (compile-file fasl-port module filename))
            filenames))

(define-repl-command expand
  "show the syntax expanded expression"
  (let* ((exp (read))
         (env (module/syntax (fluid $current-module$)))
         (expanded (syntax-expand exp (make-syntax-env env))))
    (display (syntax->scheme expanded))
    (newline)))

(define (syntax->scheme exp)
  (cond ((define-syntax? exp) 'define-syntax)
        ((lambda? exp)
         (cons 'lambda
               (cons (map syntax->scheme (lambda/formals exp))
                     (map syntax->scheme (lambda/body exp)))))
        ((if? exp)
         (list 'if
               (syntax->scheme (if/exp exp))
               (syntax->scheme (if/then exp))
               (syntax->scheme (if/else exp))))
        ((set!? exp)
	 (list 'set!
	       (syntax->scheme (set!/lhs exp))
	       (syntax->scheme (set!/rhs exp))))
        ((let? exp)
         (cons 'let
               (cons (map (lambda (b) (list (syntax->scheme (car b))
                                            (syntax->scheme (cadr b))))
                          (let/bindings exp))
                     (map syntax->scheme (let/body exp)))))
        ((application? exp)
         (cons (syntax->scheme (app/operator exp))
               (map syntax->scheme (app/operands exp))))
        ((literal? exp)
         (list 'quote (literal/val exp)))
        ((begin? exp)
         (cons 'begin (map syntax->scheme (begin/body exp))))
        ((letrec? exp)
         (cons 'letrec
               (cons (map (lambda (b) (list (syntax->scheme (car b))
                                            (syntax->scheme (cadr b))))
                          (letrec/bindings exp))
                     (map syntax->scheme (letrec/body exp)))))
        ((scoped? exp)
         (string->symbol (string-append (symbol->string (scoped/name exp))
                                        ","
                                        (if (number? (scoped/uid exp))
                                            (number->string (scoped/uid exp))
                                            (symbol->string (scoped/uid exp))))))
        ((symbol? exp) exp)
        ((primitive-call? exp) 'primitive-call)
        (else (error "SYNTAX->SCHEME Unknown expression ~a" exp))))

(define-repl-command cf
  "Compile file in current module"
  (let* ((filename (read))
         (real-filename (if (symbol? filename)
                            (symbol->string filename)
                            filename))
	 (fasl-filename (replace-extension real-filename ".fasl")))
    (if (file-readable? real-filename)
        (call-with-output-file fasl-filename
          (lambda (output-port)
            (condition-case
             (compile-file output-port
                           (fluid $current-module$)
                           real-filename)
             (<condition> (lambda (error) (report-condition error))))))
        (error "File ~a is not readable or does not exists" real-filename))))

(define (compile-module filename)
  (if (file-exists? filename)
      (if (file-readable? filename)
          (let ((module-def (with-input-from-file filename
                              (lambda () (read)))))
            (if (and (pair? module-def)
                     (eq? 'define-structure (car module-def)))
                (let* ((name (cadr module-def))
                       (module (make-module name '() '() "" '() name))
		       (module-fasl-filename (string-append
					      (file-name-directory filename)
					      (symbol->string name)
					      ".fasl")))
		  (call-with-output-file module-fasl-filename
		    (lambda (module-fasl-port)
		      (bind-module! name module)
		      (let loop ((def (cddr module-def)))
			(if (pair? def)
			    (let ((directive (car def)))
			      (cond ((and (pair? directive)
					  (eq? (car directive) 'export))
				     (set-module/export! module (cdr directive)))
				    ((and (pair? directive)
					  (eq? (car directive) 'open))
                                     (let ((modules (cdr directive)))
                                       (for-each ensure-module-loaded! modules)
                                       (set-module/open! module modules)))
				    ((and (pair? directive)
					  (eq? (car directive) 'doc))
				     (set-module/doc! module (cadr directive)))
				    ((and (pair? directive)
					  (eq? (car directive) 'files))
                                     (write-type type/module module-fasl-port)
                                     (write-fasl! (module/name module) module-fasl-port)
                                     (write-fasl! (module/export module) module-fasl-port)
                                     (write-fasl! (module/open module) module-fasl-port)
                                     (write-fasl! (module/doc module) module-fasl-port)
                                     (for-each ensure-module-loaded!
                                               (module/open module))
				     (compile-file* module-fasl-port
						    module
						    (cdr directive)))
				    (else (error "unknown directive" directive)))
			      (loop (cdr def)))))
		      (unbind-module! name))))
		(begin
		  (display ";; The file ") (display filename)
		  (display " does not contain a module definition"))))
	  (begin (display ";; The file ") (display filename)
		 (display " is not readable") (newline)))
      (begin (display ";; The file ") (display filename)
	     (display " does not exist") (newline))))

(define-repl-command cm
  "Compile module"
  (let* ((module-filename (read))
         (real-module-filename (if (symbol? module-filename)
                                   (symbol->string module-filename)
                                   module-filename)))
    (compile-module real-module-filename)))

(define-repl-command in
  "Change current module"
  (let* ((name (read))
	 (module (lookup-module name)))
    (if module
	(set-fluid! $current-module$ module)
	(error "Module ~a not found!" name))))

;; mods instead of modules because modules is already exported from
;; scheme-runtime module.
(define-repl-command mods
  "List all known modules"
  (let loop ((modules *modules*))
    (if (null? modules)
	'done
	(let* ((module-alist (car modules))
	       (name (car module-alist))
	       (module (cdr module-alist)))
	  (display/width (symbol->string name) 16)
	  (display "  ")
	  (display (module/doc module))
	  (newline)
	  (loop (cdr modules))))))

(define-repl-command where
  "Show in which module a name is bound"
  (display "Command not implemented"))

(define-repl-command describe
  "Describe what is known about a name"
  (display "Command not implemented"))

(define-repl-command save
  "Save the current image as file & exit"
  (let* ((filename (read))
	 (real-filename (if (symbol? filename) (symbol->string filename) filename)))
    (save-image-file real-filename run-repl)))

(define-repl-command setcwd
  "Set current working directory"
  (let* ((directory (read))
         (real-directory (if (symbol? directory)
                             (symbol->string directory)
                             directory)))
    (chdir real-directory)))

(define (open-module name module)
  (ensure-module-loaded! name)
  (let ((module (if (symbol? module)
		    (lookup-module module)
		    module)))
    (if (memq name (module/open module))
	(begin
	  (display ";; The module ") (display name)
	  (display " is already opened") (newline))
	(set-module/open! module (cons name (module/open module))))))

(define-repl-command open
  "Open a module in the current module"
  (let* ((module-name (read))
	 (module (fluid $current-module$)))
    (open-module module-name module)))

(define-repl-command export
  "Export a given name in the current module"
  (let* ((name (read))
	 (module (fluid $current-module$)))
    (if (memq name (module/export module))
	(begin
	  (display ";; The binding ") (display name)
	  (display " is already exported") (newline))
	(set-module/export! module (cons name (module/export module))))))

(define-repl-command module
  "Make a new module"
  (let ((name (read)))
    (if (lookup-module name)
	(begin
	  (display ";; Module ") (display name)
	  (display " already exists") (newline))
        (let ((module (make-module name '() '() "Created Manually" '() name)))
          (bind-module! name module)))))

(define (split char string)
  (let loop ((start 0)
             (i 0))
    (if (< i (string-length string))
        (if (char=? (string-ref string i) char)
            (cons (substring string start i) (loop (+ i 1) (+ i 1)))
            (loop start (+ i 1)))
        (list (substring string start i)))))

(define (ensure-module-loaded! module-name)
  ;; Make sure MODULE-NAME is loaded into the image. If it is not
  ;; already in memory, then locate the FASL file from where it could
  ;; be loaded and load it.
  (let* ((mod-name (if (string? module-name)
		       (string->symbol module-name)
		       module-name))
	 (module (lookup-module mod-name)))
    (if (not module)
	(let ((filename (locate-module module-name)))
	  (if filename
	      (begin
                (load-fasl filename)
                (lookup-module module-name))
	      (error "No Module ~a could be loaded" module-name))))))

(define (locate-module-old module-name)
  ;; Locate the filename where MODULE-NAME and return the filename or #f if
  ;; the module cannot be found.
  (let ((module-path (getenv "SCHEMY_MODULE_PATH")))
    (if module-path
        (let loop ((paths (split #\: module-path)))
          (if (null? paths)
	      #f
              (let* ((path (car paths))
                     (filename (string-append (file-name-as-directory path)
                                              (symbol->string module-name)
                                              ".fasl")))
                (if (file-readable? filename)
		    filename
                    (loop (cdr paths)))))))))

(define (locate-module module-name)
  (locate-resource (string-append (symbol->string module-name) ".fasl")))

(add-module-provider ensure-module-loaded!)

(define (source-filename filename args)
  (let* ((real-filename (if (symbol? filename)
                            (symbol->string filename)
                            filename))
         (module (make-module 'scheme-user
                              '()
                              '(scheme-runtime scheme-posix)
                              "Scheme R5RS Script File"
                              '()
                              'scheme-user)))
    (dynamic-wind
        (lambda () (bind-module! 'scheme-user module))
        (lambda ()
          (if (file-exists? real-filename)
              (if (file-readable? real-filename)
                  (call-with-input-file real-filename
                    (lambda (port)
                      (let loop ((exp (read port)))
                        (if (eof-object? exp)
                            (let ((ref (lookup-ref 'main 'scheme-user)))
                              (if ref
                                  ((ref/value ref) args)
				  (begin
				    (display ";; Script ")
				    (display real-filename)
				    (display " doesn't define a main entry")
				    (newline))))
                            (let ((val (eval exp module)))
                              (loop (read port)))))))
		  (begin
		    (display ";; File ") (display real-filename)
		    (display " is not readable") (newline)))
              (begin
		(display ";; File ") (display real-filename)
		(display " does not exist") (newline))))
	(lambda () (unbind-module! 'scheme-user)))))

(define-repl-command ss
  "Execute a Scheme script"
  (let ((filename (read)))
    (source-filename filename '())))

(define-repl-command bindings
  "Display all defined bindings of the current module"
  (let* ((module (fluid $current-module$)))
    (if module
        (let loop ((bindings (module/defined module)))
          (if (null? bindings)
              'done
              (let ((binding (car bindings)))
                (display/width (symbol->string (ref/name binding)) 16)
                (display (ref/value binding))
                (newline)
                (loop (cdr bindings))))))))

(define-repl-command syntaxes
  "Display all defined syntaxes of the current module"
  (let* ((module (fluid $current-module$)))
    (if module
        (let loop ((bindings (module/syntax module)))
          (if (pair? bindings)
              (let ((binding (car bindings)))
                (display (car binding))
                (newline)
                (loop (cdr bindings))))))))

(define-repl-command dis
  "Show assembly code of procedure"
  (let ((exp (read))
        (module (fluid $current-module$)))
    (if (eof-object? exp)
        #f
	(condition-case
	 (let ((val (eval exp module)))
	   (if (procedure? val)
	       (begin
		 (disassemble val)
		 (newline)
		 #t)))
	 (<condition>
	  (lambda (error) (report-condition error)))))))

;;; TOP LEVEL ================================================================

(define (first-non-blank)
  (let loop ((c (peek-char)))
    (if (char-whitespace? c)
	(begin
	  (read-char)
	  (loop (peek-char)))
	c)))

(define (repl)
  (call-with-current-continuation
   (lambda (quit)
     (let loop ()
       (call-with-current-continuation
	(lambda (abort)
	  (with-signal-hook
	   (let ((myself (current-thread)))
	     (lambda ()
	       (thread-interrupt! myself
				  (lambda ()
				    (display ";; Interrupt")
				    (newline)
				    (abort #f)))))
	   (lambda ()
	     (let ((module (fluid $current-module$)))
	       (display (module/name module))
	       (display "> ")
	       (let ((fnb-char (first-non-blank)))
		 (if (eof-object? fnb-char)
		     (quit 123)
		     (begin
		       (if (char=? fnb-char #\,)
			   (read/execute-command)
			   (evaluate))))))))))
       (loop))))
  (display "Thank you for using Schemy")
  (newline))


(define (display-module m)
  (display "Module: ") (display (module/name m)) (newline)
  (display "Doc:    ") (display (module/doc m)) (newline)
  (display "export: ") (display (module/export m)) (newline)
  (display "open:   ") (display (module/open m)) (newline))

(define run-repl
  (lambda (args)
    (let ((initial-module (make-module 'scheme-user '()
				       '(scheme-runtime
					 scheme-posix)
				       "User module"
				       '() 'scheme-user)))
      (bind-module! 'scheme-user initial-module)
      (display "Welcome to Schemy !") (newline)
;      (newline)
      (set-fluid! $current-module$ initial-module)
;      (help)
      (repl))))

(define (compiler-main args)
  (condition-case
   (let loop ((args args))
     (if (null? args)
         'done
         (let ((arg (car args)))
           (cond ((string=? arg "--resource-dir")
                  (add-resource-path! (cadr args))
                  (loop (cddr args)))
                 (else (compile-module arg)
                       (loop (cdr args)))))))
   (<error>
    (lambda (error)
      (report-condition error)))
   (<condition>
    (lambda (c)
      (report-condition c)))))

(define (link! output-name entry/module entry/name fasls)
  (let ((initial-module (make-module 'scheme-user '()
				     '(scheme-runtime
				       scheme-posix)
				     "User Module"
				     '() 'scheme-user)))
    (for-each (lambda (f)
		(display ";; loading fasl ") 
		(display f)
		(load-fasl f)
		(newline))
	      fasls)
    (let ((ref (lookup-ref entry/name entry/module)))
      (if ref
	  (save-image-file output-name
			   (lambda (args) ((ref/value ref) args)))
	  (error "Entry point does not exists in FASL file")))))

(define (linker-main args)
  (let* ((output-name #f)
	 (entry-module #f)
	 (entry-name #f)
	 (fasl '()))
    (condition-case
     (let loop ((args args))
       (if (null? args)
	   (if (and output-name entry-module entry-name fasl)
	       (link! output-name entry-module entry-name fasl)
	       (error "Bad arguments to linker"))
	   (let ((arg (car args)))
	     (cond ((string=? arg "--output")
		    (set! output-name (cadr args))
		    (loop (cddr args)))
		   ((string=? arg "--main")
		    (set! entry-name (string->symbol (cadr args)))
		    (loop (cddr args)))
		   ((string=? arg "--module")
		    (set! entry-module (string->symbol (cadr args)))
		    (loop (cddr args)))
		   ((string=? arg "--fasl")
		    (set! fasl (append fasl (list (cadr args))))
		    (loop (cddr args)))
                   ((string=? arg "--resource-dir")
                    (add-resource-path! (cadr args))
                    (loop (cddr args)))
		   (else (set! fasl (append fasl (list arg)))
			 (loop (cdr args)))))))
     (<error> (lambda (error) (report-condition error)))
     (<condition> (lambda (c) (report-condition c))))))
	

(define (script-main args)
  (let* ((script-name #f)
	 (initial-module (make-module 'scheme-user '()
				      '(scheme-runtime
					scheme-posix)
				      "User module"
				      '() 'scheme-user)))
    (bind-module! 'scheme-user initial-module)
					;    (display "Welcome to Schemy !") (newline)
					;      (newline)
    (set-fluid! $current-module$ initial-module)
					;      (help)
    (condition-case
     (if (null? args)
         (begin
	   (display "Usage: scmscript <filename> [<args> ...]")
	   (newline))
         (source-filename (car args) (cdr args)))
;        (if (null? args)
; 	   (source-filename script-name)
; 	   (let ((arg (car args)))
; 	     (cond ((string=? arg "--file")
; 		    (set! script-name (cadr args))
; 		    (loop (cddr args)))
; 		   ((string=? arg "-f")
; 		    (set! script-name (cadr args))
; 		    (loop (cddr args)))
; 		   (else (error "Unknown argument ~a" arg))))))
     (<error>
      (lambda (error)
	(report-condition error)
	(newline)))
     (<condition>
      (lambda (c)
	(report-condition error)
	(newline))))))

