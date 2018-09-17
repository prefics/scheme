;;; lib.scm -- library procedure for manipulating libraries

(define-record-type <library-definition>
  (make-library-definition name file-name export import doc files)
  library-definition?
  (name   library-definition-name)
  (file-name library-definition-file-name set-library-definition-file-name!)
  (export library-definition-export set-library-definition-export!)
  (import library-definition-import set-library-definition-import!)
  (doc    library-definition-doc    set-library-definition-doc!)
  (files  library-definition-files   set-library-definition-files!))

(define (make-empty-library-definition name)
  (make-library-definition name #f '() '() #f '()))

(define (read-library-definition-from-file file-name)
  (if (file-readable? file-name)
      (let* ((definition (with-input-from-file file-name (lambda () (read))))
	     (lib-def (parse-library-definition definition)))
	(set-library-definition-file-name! lib-def
                                           (string-append (cwd) file-name)))
      (error "file name ~a not readable" file-name)))

(define (parse-library-definition sexp)
  (if (and (pair? sexp)
	   (or (eq? 'define-structure (car sexp))
	       (eq? 'define-library (car sexp))))
      (let* ((name (cadr sexp))
	     (library (make-empty-library-definition name)))
	(let loop ((def (cddr sexp)))
	  (if (pair? def)
	      (let ((directive (car def)))
		(cond ((and (pair? directive)
			    (eq? (car directive) 'export))
		       (set-library-definition-export! library (cdr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'open))
		       (set-library-definition-import! library (cdr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'doc))
		       (set-library-definition-doc! library (cadr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'files))
		       (set-library-definition-files!
			library
			(map (lambda (f)
			       (cond ((symbol? f) (symbol->string f))
				     ((string? f) f)
				     (else (error "bad file syntax ~a" f))))
			     (cdr directive))))
		      (else (error "unknown directive ~a" directive)))
		(loop (cdr def)))
	      library)))
      (error "The file does not contain a library/structure definition ~a" sexp)))

(define *library-definitions* '())

(define (library-definitions) (append *library-definitions* '()))

(define (library-definition-names) (map library-definition-name *library-definitions*))

(define (->library-definition obj)
  (cond ((library-definition? obj) obj)
        ((string? obj) (->library-definition (string->symbol obj)))
	((symbol? obj) (or (lookup-library-definition obj)
			   (error "library ~a not found" obj)))
	(else (error "unknown library definition ~a" obj))))

(define (add-library-definition library)
  (if (library-definition? library)
      (set! *library-definitions* (cons library *library-definitions*))
      (error "object ~a is not a library definition" library)))

(define (remove-library-definition library)
  (if (library-definition? library)
      (set! *library-definitions* (remq library *library-definitions*))
      (error "object ~a is not a library definition" library)))

(define (lookup-library-definition name)
  (let loop ((libs *library-definitions*))
    (if (null? libs)
	#f
	(let ((lib (car libs)))
	  (if (eq? name (library-definition-name lib))
	      lib
	      (loop (cdr libs)))))))

(define (binding-imported? name module)
  (let ((mods (module/open module)))
    (let lp ((mods mods))
      (if (null? mods)
	  #f
	  (or (memq name (module/export (lookup-module (car mods))))
	      (lp (cdr mods)))))))

(define (update-module-from-library-definition! module lib)
  (set-module/export! module (library-definition-export lib))
  (set-module/doc! module (library-definition-doc lib))
  (set-module/open! module (library-definition-import lib))
  (let lp ((bindings (module/defined module))
	   (names '()))
    (if (null? bindings)
	(begin
	  (for-each (lambda (name) (unbind-ref! name module)) names)
	  (display (list 'deleting names)))
	(let* ((binding (car bindings))
	       (name (ref/name binding))
	       (entry (binding-imported? name module)))
	  (if entry
	      (lp (cdr bindings) (cons name names))
	      (lp (cdr bindings) names)))))
  module)

(define (load-library-definition definition)
  (let* ((definition (->library-definition definition))
	 (library (ensure-library-defined definition))
         (def-dir-name (or (file-name-directory (library-definition-file-name definition))
                            (cwd))))
    (ensure-import-loaded library)
    (display ";; switching to ") (display def-dir-name) (newline)
    (for-each (lambda (file-name)
		(display ";; loading file ") (display file-name) (newline)
		(load-file (string-append def-dir-name "/" file-name) library))
	      (library-definition-files definition))
    library))

(define $library$ (make-fluid #f))

(define (current-library) (fluid $library$))
(define (with-current-library library thunk)
  (let-fluid $library$ library thunk))

(define (current-library-name)
  (let ((library (current-library)))
    (if library
	(module/name library)
	(error "no current library"))))

(define (cmd-load-module file-name)
  (let* ((definition (read-library-definition-from-file file-name))
         (module (load-library-definition definition)))
;    (bind-module! (module/name module) module)
    module))
