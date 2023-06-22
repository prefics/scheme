;;; system.scm -- system definition file support

(define-record-type <system-definition>
  (make-system-definition name version author
			  file-name parents description components)
  system-definition?
  (name         system-definition-name       set-system-definition-name!)
  (version      system-definition-version    set-system-definition-version!)
  (author       system-definition-author     set-system-definition-author!)
  (file-name    system-definition-file-name  set-system-definition-file-name!)
  (parents     system-definition-parents     set-system-definition-parents!)
  (description system-definition-description set-system-definition-description!)
  (components  system-definition-components  set-system-definition-components!))

(define (make-empty-system-definition name)
  (make-system-definition name #f #f #f '() #f '()))

(define (read-system-definition-from-file file-name)
  (if (file-readable? file-name)
      (let* ((definition (with-input-from-file file-name read))
	     (parsed-definition (parse-system-definition definition)))
	(set-system-definition-file-name! parsed-definition
					  (absolute-file-name file-name))
	parsed-definition)
      (error "file name ~a not readable" file-name)))

(define (parse-system-definition sexp)
  (if (and (pair? sexp)
	   (eq? 'define-system (car sexp)))
      (let* ((name (cadr sexp))
	     (system (make-empty-system-definition name)))
	(let loop ((def (cddr sexp)))
	  (if (pair? def)
	      (let ((directive (car def)))
		(cond ((and (pair? directive)
			    (eq? (car directive) 'parents))
		       (set-system-definition-parents! system (cdr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'description))
		       (set-system-definition-description! system (cadr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'version))
		       (set-system-definition-version! system (cadr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'author))
		       (set-system-definition-author! system (cadr directive)))
		      ((and (pair? directive)
			    (eq? (car directive) 'components))
		       (let ((parsed-components (map parse-component (cdr directive))))
			 (set-system-definition-components! system
							    parsed-components)))
		      (else (error "unknown directive ~a" directive)))
		(loop (cdr def)))
	      system)))
      (error "Bad system definition ~a" sexp)))

(define-record-type <component>
  (make-component)
  component?)

(define-record-type (<library-component> <component>)
  (make-library-component file-name)
  library-component?
  (file-name library-component-file-name))

(define-record-type (<files-component> <component>)
  (make-files-component file-names)
  files-component?
  (file-names files-component-file-names))

(define-record-type (<doc-component> <component>)
  (make-doc-component index-files)
  doc-component?
  (index-files doc-component-index-files
               set-doc-component-index-files))

(define-record-type (<test-component> <component>)
  (make-test-component files)
  test-component?
  (files test-component-files
         set-test-component-files!))

(define-record-type (<resource-component> <component>)
  (make-resource-component dir)
  resource-component?
  (dir resource-component-dir
       set-resource-component-dir!))

(define (parse-library-component exp)
  (let ((library-path (cadr exp)))
    (if (not (string? library-path))
        (error "library component is not a string"))
    (make-library-component library-path)))

(define (parse-component exp)
  (if (pair? exp)
      (let ((type (car exp)))
	(cond ((eq? type 'library) (parse-library-component exp))
	      ((eq? type 'files) (make-files-component (cdr exp)))
              ((eq? type 'test) (make-test-component (cdr exp)))
              ((eq? type 'resource) (make-resource-component (cadr exp)))
              ((eq? type 'doc) (make-doc-component (cdr exp)))
	      (else (error "bad component type ~a" type))))))

;;;
;; Registry of loaded systems

(define (system-dir name)
  (string-append (home-dir) ".local/share/scm/" name))

(define (system-bin-dir) (system-dir "bin/"))
(define (system-doc-dir) (system-dir "doc/"))
(define (system-lib-dir) (system-dir "lib/"))
(define (system-share-dir) (system-dir "share/"))
(define (system-site-dir) (system-dir "site/"))
(define (system-src-dir) (system-dir "src/"))

(define (ensure-local-repository!)
  (let* ((home-local (string-append (home-dir) ".local"))
         (home-local-share (string-append home-local "/share"))
         (home-local-share-scm (string-append home-local-share "/scm")))

    (if (not (file-exists? home-local))
        (create-directory! home-local #o755))

    (if (not (file-exists? home-local-share))
        (create-directory! home-local-share #o755))

    (if (not (file-exists? home-local-share-scm))
        (create-directory! home-local-share-scm #o755))

    (if (not (file-exists? (system-bin-dir)))
        (create-directory! (system-bin-dir) #o755))

    (if (not (file-exists? (system-lib-dir)))
        (create-directory! (system-lib-dir) #o755))

    (if (not (file-exists? (system-doc-dir)))
        (create-directory! (system-doc-dir) #o755))

    (if (not (file-exists? (system-share-dir)))
        (create-directory! (system-share-dir) #o755))

    (if (not (file-exists? (system-site-dir)))
        (create-directory! (system-site-dir) #o755))

    (if (not (file-exists? (system-src-dir)))
        (create-directory! (system-src-dir) #o755))))

(define *system-definition-paths* (list "."))
(define (system-definition-paths)
  (append *system-definition-paths*
	  (list (system-site-dir))))

(define (add-system-path! path)
  (if (and (file-exists? path) (file-directory? path))
      (set! *system-definition-paths* (cons path *system-definition-paths*))
      (error "Cannot add path ~a to system path, because it does not exists" path)))

(define (remove-system-path! path)
  (set! *system-definition-paths*
	(let del ((paths *system-definition-paths*))
	  (cond ((null? path) path)
		((string=? path (car paths)) (cdr paths))
		(else (cons (car paths) (del (cdr paths))))))))

(define *system-definitions* '())
(define (system-definitions) (append *system-definitions* '()))
(define (->system-definition obj)
  (cond ((system-definition? obj) obj)
	((string? obj) (->system-definition (string->symbol obj)))
	((symbol? obj) (lookup-system-definition obj))
	(else (error "bad system reference ~a" obj))))

(define (lookup-system-definition name)
  (let find ((systems *system-definitions*))
    (if (null? systems)
	#f
	(let ((system (car systems)))
	  (if (eq? name (system-definition-name system))
	      system
	      (find (cdr systems)))))))

(define (add-system-definition system)
  (if (not (system-definition? system))
      (error "bad argument type ~a not a system" system))
  (let ((directory-path (file-name-directory (system-definition-file-name system))))
    (add-resource-path! directory-path)
    (set! *system-definitions* (cons system *system-definitions*))))

(define (remove-system-definition name)
  (let ((system (->system-definition name)))
    (if system
        (let ((directory-path (file-name-directory (system-definition-file-name system))))
          (remove-resource-path! directory-path)
          (set! *system-definitions* (delq system *system-definitions*)))
        (error ";; System ~a does not exists in the registry" name))))

(define (locate-system-in-directory dir name)
  (let* ((dir-file-name (string-append (file-name-as-directory dir)
				       name))
	 (system-file-name (string-append dir-file-name
					  "/"
					  "system.scm")))
    (if (and (file-exists? system-file-name)
	     (file-regular? system-file-name))
	(let ((system (read-system-definition-from-file system-file-name)))
	  (if (system-definition? system)
	      system
	      (error "file ~a does not contain a system definition")))
	#f)))

(define (locate-system system)
  (let* ((name (with-output-to-string (lambda () (display system)))))
    (let try-dir ((dirs (system-definition-paths)))
      (if (pair? dirs)
	  (let ((dir (car dirs)))
	    (or (locate-system-in-directory dir name)
		(try-dir (cdr dirs))))
	  #f))))

;;;
;; == Higher level interface

(define (mapfilter proc lst)
  (if (null? lst)
      lst
      (let ((value (proc (car lst))))
        (if value
            (cons value (mapfilter proc (cdr lst)))
            (mapfilter proc (cdr lst))))))

;;; Init part

(define-init-action set-system-path ()
  (add-resource-path! (system-dir "lib")))

;;;
;; returns a list of names as string of all available systems
(define (all-system-names)
  (mapfilter (lambda (dir)
               (cond ((or (string=? dir ".")
                          (string=? dir ".."))
                      #f)
                     ((string=? ".scm" (file-name-extension dir))
                      (file-name-sans-extension dir))
                     (else #f)))
             (directory-files (system-site-dir))))

;;;
;; returns a list of available systems
(define (all-systems)
  (let ((files (directory-files (system-site-dir))))
    (mapfilter (lambda (dir)
                 (let ((system-file (string-append (system-site-dir)
                                                   dir
                                                   "/system.scm")))
                   (if (file-exists? system-file)
                       (read-system-definition-from-file system-file)
                       #f)))
               files)))

;;;
;; Add a system to the registry without loading it
(define (finish-adding-system dirname system-file-name)
  (if (not (string=? "system.scm" (file-name-nondirectory system-file-name)))
      (error "system file name ~a is not named system.scm" system-file-name))
  (if (not (file-exists? system-file-name))
      (error "system file name ~a does not exists" system-file-name))
  (if (not (file-readable? system-file-name))
      (error "system file ~a is not readable" system-file-name))

  (let ((system (read-system-definition-from-file system-file-name)))
    (create-symlink! dirname
                     (string-append (system-site-dir)
                                    (symbol->string (system-definition-name system))))
    (for-each (lambda (c) (install-component c system))
              (system-definition-components system))
    (with-output-to-file (string-append (system-site-dir)
                                        (symbol->string (system-definition-name system))
                                        ".scm")
      (lambda ()
        (display (file-contents system-file-name))))))

(define (add-system name)
  (cond ((file-directory? name)
	 (let* ((dirname (file-name-as-directory (absolute-file-name (expand-file-name name))))
		(system-file-name (string-append dirname "system.scm")))
	   (finish-adding-system dirname system-file-name)))
	((file-regular? name)
	 (let ((dirname (file-name-directory (absolute-file-name (expand-file-name name))))
	       (system-file-name (absolute-file-name (expand-file-name name))))
	   (finish-adding-system dirname system-file-name)))
	(else (error "Don't know how to add system from ~a" name))))

;;;
;; Remove a system from the registry
(define (remove-system name)
  (let ((file-name (string-append (system-site-dir) name ".scm")))
    (if (file-exists? file-name)
        (let ((system (read-system-definition-from-file file-name)))
          (for-each (lambda (c) (uninstall-component c system))
                    (system-definition-components system))
          (delete-file! file-name)
          (delete-file! (string-append (system-site-dir) name)))
        (error "System ~a does not exists" name))))

;;;
;; Compile a system
(define (compile-system system)
  (ensure-local-repository!)
  (let ((system (->system-definition system)))
    (if system
        (let ((name (system-definition-name system))
              (components (system-definition-components system))
              (parents (system-definition-parents system)))
          (if parents (for-each compile-system parents))
          (for-each (lambda (c) (compile-component c system)) components))
        (error "Cannot find system ~a" system))))

(define (load-system-from-definition system)
  (let ((dir (file-name-directory (system-definition-file-name system))))
    (with-cwd dir
      (let ((name (system-definition-name system))
	    (components (system-definition-components system))
	    (parents (system-definition-parents system)))
	(if parents (for-each (lambda (s) (load-system s)) parents))
	(for-each (lambda (c) (load-component c system)) components)))))

;;;
;; Loads `system` in the current image. It loads the parents first.
(define (load-system system)
  (ensure-local-repository!)
  (let* ((system-def (->system-definition system)))
    (if system-def
	(display ";; System already loaded\n")
	(let* ((system (locate-system system)))
          (if system
              (begin
                (display ";; Loading system ") (display (system-definition-name system))
                (display " from ") (display (system-definition-file-name system))
                (newline)
                (load-system-from-definition system)
                (add-system-definition system)
                (display ";; System ") (display (system-definition-name system))
                (display " loaded")
                system)
              (display ";; could not locate system\n"))))))

(define (force-load-system system)
  (ensure-local-repository!)
  (let* ((definition (->system-definition system)))
    (if definition
        (begin
          (display ";; Reloading system ")
          (display (system-definition-name definition))
          (newline)
          (load-system-from-definition definition)
          (display ";; System reloaded.") (newline))
	(let ((system (locate-system system)))
          (if system
              (begin
                (display ";; Load system from ")
                (display (system-definition-file-name system))
                (newline)
                (load-system-from-definition system)
                (add-system-definition system))
              (begin
                (display ";; Cannot locate system ")
                (display system)
                (newline)))))))

;;;
;; Install `system` for later loading

(define (install-system directory)
  (ensure-local-repository!)
  (let* ((dirname (file-name-as-directory (absolute-file-name (expand-file-name directory))))
         (system-file-name (string-append dirname "system.scm")))
    (if (file-exists? system-file-name)
        (let ((system (read-system-definition-from-file system-file-name)))
          (if (file-exists? (string-append (system-site-dir) (symbol->string (system-definition-name system))
                                           ".scm"))
              (remove-system (symbol->string (system-definition-name system))))
          (add-system directory))
        (error "Directory ~a has no system file" directory))))

(define (read-installed-system-definition name)
  (let ((system-file (string-append (system-site-dir) name ".scm")))
    (if (file-exists? system-file)
        (read-system-definition-from-file system-file)
        (error "System ~a is not installed" name))))

(define (uninstall-system name)
  (remove-system name))

;;;
;; Test a system

(define (test-system system-name)
  (ensure-local-repository!)
  (let ((system (locate-system system-name)))
    (if system
        (begin
          (for-each test-component (system-definition-components system)))
        (error "system ~a not found on the computer"
               system-name))))

;;;
;; Creates an archive containing all the files from `system`
(define (package-system systen)
  (error "operation not supported"))

;;;
;; Uploads `system` for internet wide availability
(define (upload-system system)
  (error "operation not supported"))

(define-generic compile-component (component system))
(define-generic load-component (component system))
(define-generic test-component (component system))
(define-generic install-component (component system))
(define-generic uninstall-component (component system))
(define-generic package-component (component system))
(define-generic upload-component (component system))

;; default implementation
(define-method compile-component ((c <component>) system)
  #f)
(define-method load-component ((c <component>) system)
  #f)
(define-method test-component ((c <component>) system)
  #f)
(define-method install-component ((c <component>) system)
  #f)
(define-method uninstall-component ((c <component>) system)
  #f)
(define-method package-component ((c <component>) system)
  #f)
(define-method upload-component ((c <component>) system)
  #f)

(define-method install-component ((c <doc-component>) system)
  (let ((base (file-name-directory (system-definition-file-name system))))
    (for-each (lambda (name)
                (let ((doc-file-name (string-append base name))
                      (index-entry (string-append (system-doc-dir)
                                                  (file-name-nondirectory name))))
                  (if (not (file-exists? index-entry))
                      (create-symlink! doc-file-name index-entry))))
              (doc-component-index-files c))))

(define-method install-component ((c <library-component>) system)
  (let* ((base (file-name-directory (system-definition-file-name system)))
         (lib-file (library-component-file-name c))
         (lib-dir (file-name-directory lib-file))
         (lib-def (read-library-definition-from-file (string-append base lib-file))))
    (create-symlink! (string-append base lib-dir)
                     (string-append (system-src-dir)
                                    (symbol->string (library-definition-name lib-def))))))

(define-method install-component ((c <files-component>) system)
  (let ((sys-name (symbol->string (system-definition-name system)))
        (sys-dir (file-name-directory (system-definition-file-name system))))
    (for-each (lambda (name)
                (create-symlink! (string-append sys-dir name)
                                 (string-append system-share-dir sys-name "/" name)))
              (files-component-file-names c))))

(define-method install-component ((c <test-component>) system)
  #f)

(define-method install-component ((c <resource-component>) system)
  (let ((sys-name (symbol->string (system-definition-name system)))
        (sys-dir (file-name-directory (system-definition-file-name system)))
        (resource-dir (resource-component-dir c)))
    (create-symlink! (string-append sys-dir resource-dir)
                     (string-append (system-lib-dir) sys-name))))

(define-method uninstall-component ((c <library-component>) system)
  (let* ((base (file-name-directory (system-definition-file-name system)))
         (lib-file (library-component-file-name c))
         (lib-name (file-name-nondirectory (directory-as-file-name (file-name-directory lib-file)))))
    (delete-file! (string-append (system-src-dir) lib-name))))

(define-method uninstall-component ((c <files-component>) system)
  (let ((sys-name (symbol->string (system-definition-name system))))
    (for-each (lambda (name)
                (delete-file! (string-append (system-share-dir) sys-name "/" name)))
              (files-component-file-names c))))

(define-method uninstall-component ((c <resource-component>) system)
  (let ((dir-name (resource-component-dir c)))
    (delete-file! (string-append (system-lib-dir) dir-name))))

(define-method load-component ((c <resource-component>) system)
  (let ((dir (resource-component-dir c))
        (sys-dir (file-name-directory (system-definition-file-name system))))
    (display ";; adding resource path ") (display sys-dir) (newline)
    (add-resource-path! (string-append sys-dir dir))))

(define-method load-component ((c <library-component>) system)
  (let ((directory (file-name-directory (system-definition-file-name system))))
    (display ";; changing directory to ") (display directory) (newline)
    (with-cwd directory
      (let ((filename (library-component-file-name c)))
	(display ";;; loading library file from ") (display filename) (newline)
	(let* ((definition (read-library-definition-from-file filename))
	       (module (with-cwd (file-name-directory filename)
			         (load-library-definition definition))))
	  (bind-module! (module/name module) module)
          (display ";;; Module ") (display (module/name module))
          (display " defined.") (newline)
          module)))))
