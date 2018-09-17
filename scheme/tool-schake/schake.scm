;;; schake.scm -- Make tool like in scheme

(define-record-type <target>
  (make-target name doc dependents script)
  target?
  (name target-name set-target-name!)
  (doc target-doc set-target-doc!)
  (dependents target-dependents set-target-dependents!)
  (script target-script set-target-script))

(define-generic target-modification-time (target))

;;;
;; Returns the modification time of the target. Possible values are an
;; absolute time or #f indicating an unknown modification time.
(define-method target-modification-time ((target <target>))
  #f)

;;;
;; == Target Repository

(define *targets* '())
;;;
;; Returns an alist of name and target object
(define (targets) (append *targets* '()))
;;;
;; Returns the list of all known target names currently defined
(define (target-names) (map car *targets*))

;;;
;; Deletes all defined targets from the system
(define (targets-clear!)
  (set! *targets* '()))

(define *target* (make-fluid #f))

;;;
;; Return the current target being executed.
(define (the-target) (fluid *target*))

;;;
;; Establish a dynamic context for `proc` where target is bound as
;; the current target being executed.
(define (with-target target proc)
  (let-fluid *target* target proc))

;;;
;; return the target object named `name` from the repository.
(define (target-ref name)
  (let ((entry (assq name *targets*)))
    (if entry
	(cdr entry)
	#f)))

;;;
;; Predicate indicating if a target with `name` exists.
(define (target-exists? name)
  (if (assq name *targets*)
      #t
      #f))

;;;
;; Add a `target` with `name` to the repository.
(define (target-add name target)
  (let ((entry (assq name *targets*)))
    (if entry
	(set-cdr! entry target)
	(set! *targets* (cons (cons name target)
			      *targets*)))))

;;;
;; Remove a target with `name` from the repository.
(define (target-delete name)
  (set! *targets*
	(let remove ((ts *targets*))
	  (cond ((null? ts) ts)
		((eq? name (caar ts)) (cdr ts))
		(else (cons (car ts) (remove (cdr ts))))))))

;;;
;; Return the target referenced by `obj`. `obj` may be a string or
;; symbol representing the name of the target. If `obj` is already a
;; target, it is simply returned.
(define (->target obj)
  (cond
   ((symbol? obj) (target-ref obj))
   ((string? obj) (target-ref (string->symbol obj)))
   ((target? obj) obj)
   (else (error "don't know how to convert ~a to a target" obj))))

(define (fold-left init lst proc)
  (if (null? lst)
      init
      (fold-left (proc init (car lst)) (cdr lst) proc)))

;;;
;; == Engine

(define (target-mtime target kb)
  (let ((entry (assq target kb)))
    (or (target-modification-time target)
	(if entry (cdr entry) $always))))

(define (run-target-script target kb)
  (with-target target
    (lambda ()
      ((target-script target))
      (cons (cons target (time)) kb))))

(define (target-update target kb)
  (let* ((dep-names (target-dependents target))
	 (deps (map ->target dep-names))
	 (kb* (fold-left kb deps
			 (lambda (kb dependent) (target-update dependent kb)))))
    (let ((mtime (target-mtime target kb*)))
      (cond ((and (null? deps) (target-mtime target kb*))
	     (run-target-script target kb*))
	    ((fold-left #f deps
			(lambda (update? dependent)
			  (or update? (time<? mtime (target-mtime dependent kb*)))))
	     (run-target-script target kb*))
	    (else kb*)))))

;;;
;; == Defining targets

(define-syntax deftarget
  (syntax-rules ()
    ((deftarget ?name (?dep ...) ?doc . ?script)
     (target-add '?name (make-target '?name
				     ?doc
				     '(?dep ...)
				     (lambda () . ?script))))))

;;;
;; == Extensions

(define $always (cons (make-bvec 8) 0))
(define $never (time))

(define-record-type (<file-target> <target>)
  (make-file-target name dependents doc script file-name)
  file-target?
  (file-name file-target-file-name))

(define-method target-modification-time ((target <file-target>))
  (let ((file-name (file-target-file-name target)))
    (if (file-exists? file-name)
	(cons (file-mtime file-name) 0)
	$always)))

(define (->target-name file-name)
  (let ((name (if (symbol? file-name)
		  (symbol->string file-name)
		  file-name)))
    (string-append (file-name-as-directory (cwd))
		   name)))

(define-syntax deftarget-file
  (syntax-rules ()
    ((deftarget-file ?file-name (?deps ...) ?doc . ?script)
     (let ((name '?file-name))
       (target-add name
		   (make-file-target name '(?deps ...) ?doc
				     (lambda () . ?script)
				     (symbol->string name)))))))

(define (map/2 proc l1 l2)
  (cond ((and (null? l1) (null? l2)) l1)
	((and (pair? l1) (pair? l2)) (cons (proc (car l1) (car l2)) (map/2 proc (cdr l1) (cdr l2))))
	(else (error "bad lists for MAP/2 procedure ~a ~a" l1 l2))))

(define (compile-c-file obj src)
  (display ";; compiling ") (display src) 
  (display " to ") (display obj) (newline)
  (run (gcc "-c" "-o" ,obj ,src)))

(define (link-c-file name obj)
  (display ";; linking ") (display name) (newline)
  (run (gcc "-o" ,name ,@obj)))

(define-syntax def-c-application
  (syntax-rules ()
    ((def-c-application ?name . ?src)
     (let* ((src '?src)
	    (obj (map (lambda (fn) (string->symbol (replace-extension (symbol->string fn) ".o"))) src))
	    (name '?name))
       (let* ((app-target (make-file-target name obj "application"
					    (lambda () (link-c-file name obj))
					    (symbol->string name)))
	      (obj-targets (map/2 (lambda (obj src)
				    (make-file-target obj (list src) "object file"
						      (lambda () (compile-c-file obj src))
						      (symbol->string obj)))
				  obj src))
	      (src-targets (map (lambda (src)
				  (make-file-target src '() "source file"
						    (lambda () #f)
						    (symbol->string src)))
				src)))
	 (target-add name app-target)
	 (for-each (lambda (t) (target-add (target-name t) t)) obj-targets)
	 (for-each (lambda (t) (target-add (target-name t) t)) src-targets))))))

(define (add-target-to-compile target)
  (let ((compile-target (target-ref 'compile)))
    (set-target-dependents! compile-target
			    (cons target (target-dependents compile-target)))))


(define (make-target-name . components)
  (string->symbol
   (with-output-to-string
     (lambda ()
       (for-each display components)))))

(define (add-library-to-load-target lib)
  (let* ((load-lib-name (make-target-name "load:" (library-definition-name lib)))
	 (deps (map (lambda (fn) (make-file-target fn (list) "source file"
						   (lambda () #f)
						   fn))
		    (library-definition-files lib)))
	 (load-lib-target (make-target load-lib-name deps "loading of library"
				       (lambda ()
					 (load-library-definition lib)))))
    (target-add load-lib-name load-lib-target)))

(define (add-library-to-compile-target lib)
  (let* ((compile-lib-name (make-target-name "compile:" (library-definition-name lib)))
	 (deps-files (map (lambda (fn) (make-file-target fn '() "source file"
							 (lambda () #f)
							 fn))
			  (library-definition-files lib)))
	 (deps-mod (map (lambda (name) (make-module-target name '() "import module"
							   (lambda () #f)))
			(library-definition-import lib)))
	 (compile-lib-target (make-file-target compile-lib-name (append deps-file deps-mod)
					       "compile target"
					       (lambda ()
						 (compile-library lib)))))
    (target-add compile-lib-name compile-lib-target)))
					  
(define (deflibrary-target file-name)
  (let ((lib (read-library-definition-from-file file-name)))
    (add-library-to-load-target lib)
    (add-library-to-compile-target lib)))

;;;
;; == Main entry point

(define (load-file file-name module)
  (with-current-module module
    (lambda ()
      (with-input-from-file file-name
	(lambda ()
	  (let evaluate ((exp (read)))
	    (if (eof-object? exp)
		'loading-done
		(begin
		  (eval exp module)
		  (evaluate (read))))))))))

(define (include . files)
  (for-each (lambda (fn) (load-file fn (current-module))) files))

(define (display-usage)
  (display "usage: schake TARGET ...\n"))

(define (load-schakefile) (load-schakefile-from "schakefile"))

(define (load-schakefile-from file-name)
  (let ((module (make-module 'schake-user
			     '()
			     '(scheme-runtime scheme-posix tool-schake)
			     "default schake user environment"
			     '()
			     'schake-user)))
    (bind-module! 'schake-user module)
    (load-file file-name module)
    (unbind-module! 'schake-user)))
    
(define (main args)
  (if (null? args)
      (display-usage)
      (condition-case
       (begin
	 (load-schakefile)
	 (for-each (lambda (target-name) (target-update (->target target-name) '()))
		   args))
       (<condition> (lambda (condition)
		      (display "Error")
		      (show-backtrace condition))))))

;;;
;; == Examples

#;(deftarget :always ()
  "A predefined target that always forces an update")

#;(deftarget :never ()
  "A predefined target that never forces an update")

#;(deftarget doc ()
  "A documentation target"
  (display "Hello world"))

#;(deftarget-file hello.txt ()
  "create file hello.txt"
  (with-output-to-file "hello.txt" (lambda () (display "hello world"))))

#;(deftarget :hello ()
  "Say hello"
  (display "Hello\n"))

#;(deftarget :world (:hello :hello)
  "Say hello world"
  (display "World\n"))

(def-c-application t2 t2.c)

(define (test)
  (target-update (->target 't2) '()))
