;;; module.scm -- module system for Scheme

(define-record-type :module
  (make-module name export open doc defined syntax)
  module?
  (name module/name set-module/name!)
  (export module/export set-module/export!)
  (open module/open set-module/open!)
  (doc module/doc set-module/doc!)
  (defined module/defined set-module/defined!)
  (syntax module/syntax set-module/syntax!))

;;; Module =================================================================

(define *modules* '())

(define (modules) (append *modules* '()))
(define (module-names) (map (lambda (b) (car b)) *modules*))

(define (bind-module! name module)
  (let ((entry (assq name *modules*)))
    (if entry
        (set-cdr! entry module)
        (set! *modules* (cons (cons name module) *modules*)))))

(define (unbind-module! name)
  (set! *modules* (let loop ((modules *modules*))
		    (if (null? modules)
			modules
			(if (eq? name (caar modules))
			    (loop (cdr modules))
			    (cons (car modules) (loop (cdr modules))))))))

(define (lookup-module name)
  (let ((module (assq name *modules*)))
    (if module
        (cdr module)
        #f)))

(define *module-providers* '())

(define (require module-name)
  (let lp ((providers *module-providers*))
    (cond ((null? providers) (error "module ~a not found" module-name))
          ((pair? providers)
           (let ((m ((car providers) module-name)))
             (if m
                 (let ((cm (current-module)))
                   (set-module/open! cm (cons module-name (module/open cm)))
                   m)
                 (lp (cdr providers)))))
          (else (error "internal error in *module-providers*")))))

(define (add-module-provider provider)
  (set! *module-providers* (cons provider *module-providers*)))

(define (delete-module-provider provider)
  (set! *module-providers*
        (let lp ((mps *module-providers*))
          (cond ((null? mps) mps)
                ((pair? mps)
                 (let ((p (car mps)))
                   (if (eq? p provider)
                       (cdr mps)
                       (cons p (lp (cdr mps))))))
                (else (error "internal error in *module-providers*"))))))

(define (module-providers) (append *module-providers* '()))

(define $module$ (make-fluid #f))

(define (with-current-module module proc)
  (let-fluid $module$ module proc))

(define (current-module) (fluid $module$))
(define (current-module-name) (module/name (current-module)))

(define (in-module name)
  (let ((module (lookup-module name)))
    (if module
	(set-fluid! $module$ module)
	(error "module ~a does not exists in the image" name))))

;;; Bindings ==============================================================

(define (lookup-imported-binding name module)
  (let loop ((module-names (module/open module)))
    (if (null? module-names)
        #f
        (let* ((module-name (car module-names))
               (module (lookup-module module-name))
               (found? (memq name (module/export module))))
          (if found?
	      ;; (lookup-binding name module)
              (lookup-ref name module-name)
              (loop (cdr module-names)))))))

(define (lookup-binding name module)
;  (write-channel (make-channel 1) "LOOKING-BINDING")
;  (write-channel (make-channel 1) name)
;  (write-channel (make-channel 1) module)
  (let loop ((refs (module/defined module)))
    (if (null? refs)
        #f
        (if (eq? name (ref/name (car refs)))
            (car refs)
            (loop (cdr refs))))))

(define (bind-ref! name value module)
  (let ((ref (make-ref name (module/name module) value)))
    (set-module/defined! module (cons ref (module/defined module)))
    ref))

(define (unbind-ref! name module)
  (set-module/defined! module (let loop ((refs (module/defined module)))
				(if (null? refs)
				    refs
				    (if (eq? (ref/name (car refs)) name)
					(loop (cdr refs))
					(cons (car refs) (loop (cdr refs))))))))

(define (lookup-ref name module-name)
  (let* ((module (lookup-module module-name)))
;    (write-channel (make-channel 1) "LOOKUP-REF")
;    (write-channel (make-channel 1) module-name)
    (if module
        (let ((ref (lookup-binding name module)))
          (if ref
              ref
              (lookup-imported-binding name module)))
        (error "Unknown module ~a" module-name))))

(define (lookup/create-ref name module)
;  (display "LOOKUP/CREATE-BINDING")
  (let ((module (if (symbol? module)
		    (lookup-module module)
		    module)))
    (let ((binding (lookup-binding name module)))
;    (display binding)
      (if binding
	  binding
	  (let ((binding* (lookup-imported-binding name module)))
;	  (display binding*)
	    (if binding*
		binding*
		(let ((ref (make-ref name (module/name module) 'the-unbound-object)))
                  ;		(display "creating new ref")
		  (set-module/defined! module (cons ref (module/defined module)))
		  ref)))))))

(define (bound? name)
  (let ((binding (lookup-binding name (current-module))))
    (and binding (not (eq? (ref/value binding) (unbound-object))))))

;;; REF ============================================================================

(define (ref/name ref) (ref-name ref))
(define (ref/module ref) (ref-module ref))
(define (ref/value ref) (ref-value ref))
(define (set-ref/name! ref name) (set-ref-name! ref name))
(define (set-ref/module! ref module) (set-ref-module! ref module))
(define (set-ref/value! ref value) (set-ref-value! ref value))

;;; SYNTAX =========================================================================

(define (assq* e l)
  (if (pair? l)
      (if (eq? e (caar l))
          (car l)
          (assq* e (cdr l)))
      #f))

(define (make-syntax transformer env)
  (vector 'syntax transformer env))
(define (syntax? exp)                  (and (vector? exp)
					    (eq? 'syntax (vector-ref exp 0))))
(define (syntax/env mac)              (vector-ref mac 2))
(define (syntax/transformer mac)      (vector-ref mac 1))

(define (bind-syntax! name syntax module)
  (let* ((syntaxes (module/syntax module))
         (entry (assq* name syntaxes)))
    (if entry
        (set-cdr! entry syntax)
        (set-module/syntax! module (cons (cons name syntax) syntaxes)))))

(define (unbind-syntax! name module)
  (set-module/syntax! module
		      (let loop ((syntax (module/syntax module)))
			(if (pair? syntax)
			    (if (eq? name (caar syntax))
				(loop (cdr syntax))
				(cons (car syntax) (loop (cdr syntax))))
                            syntax))))

(define (exported-syntax? name module-name)
  (let* ((module (lookup-module module-name))
	 (interface (module/export module)))
    (let loop ((i interface))
      (if (null? i)
	  #f
	  (let ((export-name (car i)))
	    (if (and (pair? export-name)
		     (eq? (car export-name) name)
		     (eq? ':syntax (cadr export-name)))
		#t
		(loop (cdr i))))))))

(define (lookup-imported-syntax name module)
  (let loop ((module-names (module/open module)))
    (if (null? module-names)
        #f
        (let* ((module-name (car module-names)))
	  (if (exported-syntax? name module-name)
	      (let* ((module (lookup-module module-name))
		     (syn (assq* name (module/syntax module))))
		(if syn
		    (cdr syn)
		    (loop (cdr module-names))))
	      (loop (cdr module-names)))))))

(define (lookup-syntax name module-name)
  (let ((module (lookup-module module-name)))
    (if module
        (let ((syntax (assq* name (module/syntax module))))
          (if syntax
              (cdr syntax)
              (lookup-imported-syntax name module)))
	(error "inexistant module named ~a" module-name))))

;;; PRIMITIVE ============================================================

(define (primitive? o)
  (and (vector? o)
       (eq? 'primitive (vector-ref o 0))))

(define (make-primitive name transformer)
  (vector 'primitive name transformer))

(define (primitive/name o)
  (vector-ref o 1))

(define (primitive/transformer o)
  (vector-ref o 2))
