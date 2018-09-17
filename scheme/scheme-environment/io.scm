;;; io.scm -- input/output of library

(define (ensure-import-loaded library)
  (for-each (lambda (name)
	      (let ((def (lookup-library-definition name)))
		(if def
		    (load-library-definition def)
		    (load-library name))))
	    (module/open library)))

(define (ensure-library-defined def)
  (let* ((lib (->library-definition def))
	 (module (lookup-module (library-definition-name lib))))
    (if module
	(update-module-from-library-definition! module lib)
	(let ((module (make-module (library-definition-name lib)
				   (library-definition-export lib)
				   (library-definition-import lib)
				   (library-definition-doc lib)
				   '()
				   (library-definition-name lib))))
	  (bind-module! (library-definition-name lib) module)
	  module))))

(define (load-library name)
  (or (lookup-module name)
      (let ((fasl-file-name (locate-library name)))
	(if fasl-file-name
	    (load-fasl-file fasl-file-name)))
	    (error "cannot locate library ~a in path" name)))

(define (load-file file-name module)
  (if (file-readable? file-name)
      (with-cwd* (file-name-directory file-name)
	(lambda ()
	  (with-input-from-file file-name
	    (lambda ()
	      (let loop ((exp (read)))
		(if (eof-object? exp)
		    module
		    (let ((value (eval exp module)))
		      (loop (read)))))))))
      (error "File ~a is not readable" file-name)))

(define (load-fasl-file filename)
  (if (file-readable? filename)
      (with-cwd* (file-name-directory filename)
	(lambda ()
	  (call-with-input-file filename
	    (lambda (port)
	      (let loop ((type (read-type port))
			 (module #f))
		(cond ((eof-object? type) module)
		      ((= type type/define)
		       (let* ((fasl (read-fasl port))
			      (proc (make-procedure 2)))
			 (procedure-set! proc 0 'undefined)
			 (procedure-set! proc 1 fasl)
			 (proc)
			 (loop (read-type port) module)))
		      ((= type type/syntax)
		       (if module
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
			     (loop (read-type port) module))
			   (error "no module definition before this syntax definition")))
		      ((= type type/expr)
		       (let* ((fasl (read-fasl port))
			      (proc (make-procedure 2)))
			 (procedure-set! proc 0 'undefined)
			 (procedure-set! proc 1 fasl)
			 (proc)
			 (loop (read-type port) module)))
		      ((= type type/module)
		       (let* ((name (read-fasl port))
			      (export (read-fasl port))
			      (open (read-fasl port))
			      (doc (read-fasl port))
			      (mod (make-module name export open doc
						'() name)))
			 (for-each ensure-library-loaded open)
			 (bind-module! name mod)
			 (loop (read-type port) mod)))
		      (else (error "bad fasl type code ~a" type))))))))
      (error "library fasl file name ~a not readable" filename)))

(define (ensure-library-loaded name)
  ;; Make sure MODULE-NAME is loaded into the image. If it is not
  ;; already in memory, then locate the FASL file from where it could
  ;; be loaded and load it.
  (let ((library (lookup-module name)))
    (or library
	(let ((filename (locate-library name)))
	  (if filename
	      (load-fasl-file filename)
	      (error "No Module ~a could be located" name))))))

(define (string-split char string)
  (let loop ((start 0)
             (i 0))
    (if (< i (string-length string))
        (if (char=? (string-ref string i) char)
            (cons (substring string start i) (loop (+ i 1) (+ i 1)))
            (loop start (+ i 1)))
        (list (substring string start i)))))

(define (locate-library module-name)
  ;; Locate the filename where MODULE-NAME and return the filename or #f if
  ;; the module cannot be found.
  (let ((module-path (getenv "SCHEMY_MODULE_PATH")))
    (if module-path
        (let loop ((paths (string-split #\: module-path)))
          (if (null? paths)
	      #f
              (let* ((path (car paths))
                     (filename (string-append (file-name-as-directory path)
                                              (symbol->string module-name)
                                              ".fasl")))
                (if (file-readable? filename)
		    filename
                    (loop (cdr paths))))))
	#f)))

