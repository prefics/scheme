;;; alien.scm -- alien support foreign function interface

;;;Commentary:

;;;Code:

;;; ALIEN VALUES =================================================

;(define $ws (%system-info 0))               ; system word size

(define-record-type <alien-value>
  (make-alien-value value type)
  alien-value?
  (value alien-value-value)
  (type  alien-value-type))

(define (alien-value? obj) (and (record? obj) (eq? (record-type obj) <alien-value>)))
(define (alien-value-value alien) (record-ref alien 'value))
(define (alien-value-type alien) (record-ref alien 'type))

(define (malloc type)
  (let ((size (sizeof type)))
    (make-alien-value (%ffi-deref (%ffi-malloc size) 0 size)
		      type)))

(define (free! value)
  (%ffi-free (alien-value-value value)))

(define (malloc-array n type)
  (make-alien-value (%ffi-malloc (* n (sizeof type))) type))

(define (malloc-ptr type)
  (make-alien-value (%ffi-mirror (%ffi-malloc (sizeof type)))
		    (make-pointer-type type)))

;;; TYPE SYSTEM ==================================================

(define (sizeof type)
  (cond ((basic-type? type) (basic-type-size type))
	((cstruct-type? type) (cstruct-type-size type))
	((pointer-type? type) 8)
	((array-type? type) 8)
	(else (error "Unknown type ~a" type))))

(define-record-type <basic-type>
  (make-basic-type name size)
  basic-type?
  (name basic-type-name)
  (size basic-type-size))

(define :bool   (make-basic-type ':bool 1))
(define :void   (make-basic-type ':void 8))
(define :char   (make-basic-type ':char 2))
(define :wchar  (make-basic-type ':wchar 2))
(define :short  (make-basic-type ':short 2))
(define :ushort (make-basic-type ':ushort 2))
(define :int    (make-basic-type ':int  4))
(define :uint   (make-basic-type ':uint 4))
(define :long   (make-basic-type ':long 8))
(define :ulong  (make-basic-type ':ulong 8))
(define :float  (make-basic-type ':float 8))
(define :double (make-basic-type ':double 8))
(define :cstring (make-basic-type ':cstring 8))

(define (basic-type->scheme type val)
  (let ((name (basic-type-name type)))
    (cond ((eq? name ':bool)    (if (= 0 (%ffi-u8-ref val)) #f #t))
	  ((eq? name ':void)    #f)
	  ((eq? name ':char)    (integer->char (%ffi-u16-ref val)))
	  ((eq? name ':short)   (%ffi-u16-ref val))
	  ((eq? name ':ushort)  (%ffi-u16-ref val))
	  ((eq? name ':int)     (%ffi-u32-ref val))
	  ((eq? name ':uint)    (%ffi-u32-ref val))
	  ((eq? name ':long)    (%ffi-u64-ref val))
	  ((eq? name ':ulong)   (%ffi-u64-ref val))
	  ((eq? name ':float)   1)
	  ((eq? name ':double)  (%ffi-double-ref val))
	  ((eq? name ':cstring) (%ffi-string-ref val))
	  (else (error "don't know basic type name ~s" name)))))

(define (alien->scheme obj)
  (if (alien-value? obj)
      (let ((type (alien-value-type obj))
	    (val (alien-value-value obj)))
	(if (basic-type? type)
	    (basic-type->scheme type val)
	    (error "don't know how to convert alien ~a to scheme object"
		   obj)))
      (error "object ~a is not an alien value")))

(define (describe-alien obj)
  (let ((type (alien-value-type obj))
	(val (alien-value-value obj)))
    (display "type is:") (display (basic-type-name type))
    (display " value is ") (display val)))

;;; CStruct types =================================================

(define-record-type <cstruct-type>
  (make-cstruct-type name fields)
  cstruct-type?
  (name   cstruct-type-name)
  (fields cstruct-type-fields))

(define (cstruct-type-size type)
  (let loop ((fields (cstruct-type-fields type)))
    (if (null? fields)
	0
	(let ((field (car fields)))
	  (+ (sizeof (cdr field)) (loop (cdr fields)))))))

(define (cstruct-field-offset type field)
  (let loop ((fields (cstruct-type-fields type))
	     (offset 0))
    (if (null? fields)
	(error "field ~s not found in type ~s" field type)
	(let ((f (car fields)))
	  (if (eq? (car f) field)
	      offset
	      (loop (cdr fields)
		    (+ offset (sizeof (cdr f)))))))))

(define (cstruct-field-type type field)
  (let loop ((fields (cstruct-type-fields type)))
    (if (null? fields)
	(error "Field ~s not found in type ~s" field type)
	(let ((f (car fields)))
	  (if (eq? (car f) field)
	      (cdr f)
	      (loop (cdr fields)))))))

(define (cstruct-field-size type field)
  (let loop ((fields (cstruct-type-fields type)))
    (if (null? fields)
	(error "Field ~s not found in type ~s" field type)
	(let ((f (car fields)))
	  (if (eq? (car f) field)
	      (sizeof (cdr f))
	      (loop (cdr fields)))))))

(define (cstruct-ref value field)
  (let* ((type (alien-value-type value))
	 (offset (cstruct-field-offset type field))
	 (field-type (cstruct-field-type type field))
	 (size (cstruct-field-size type field)))
    (make-alien-value (%ffi-deref (alien-value-value value)
				  offset
				  size)
		      field-type)))

(define (cstruct-set! value field object)
  (let* ((value-type (alien-value-type value))
	 (offset (cstruct-field-offset value-type field)))
    (%ffi-mem-set! (alien-value-value value)
		   (%ffi-mirror object)
		   offset)
    value))

(define-syntax cstruct-type
  (syntax-rules (array ptr cstruct)
    ((cstruct-type (array ?n ?type))
     (make-array-type ?n (cstruct-type ?type)))
    ((cstruct-type (ptr ?type))
     (make-pointer-type (cstruct-type ?type)))
    ((cstruct-type (cstruct (?name ?type) ...))
     (make-cstruct-type (list (cons ?name (cstruct-type ?type)) ...)))
    ((cstruct-type ?type)
     ?type)))

(define-syntax define-cstruct-type
  (syntax-rules ()
    ((define-cstruct-type ?name (?field-name ?field-type) ...)
     (define ?name
       (make-cstruct-type '?name
	(list (cons ?field-name (cstruct-type ?field-type)) ...))))))

;;; Pointer types ====================================================

(define-record-type <pointer-type>
  (make-pointer-type type)
  pointer-type?
  (type pointer-type-type))

(define (make-pointer value)
  (make-alien-value (%ffi-mirror (alien-value-value value))
		    (make-pointer-type (alien-value-type value))))

(define (pointer-deref value)
  (let ((type (alien-value-type value))
	(value (alien-value-value value)))
    (make-alien-value (%ffi-deref value 0 (sizeof (pointer-type-type type)))
		      (pointer-type-type type))))

(define (pointer-null? value)
  (let ((vec (alien-value-value value)))
    (let loop ((i 0))
      (if (< i (bvec-length vec))
	  (and (= 0 (bvec-ref vec i)) (loop (+ i 1)))
	  #t))))

(define-syntax define-pointer-type
  (syntax-rules ()
    ((define-pointer-type ?name ?type)
     (define ?name (make-pointer-type ?type)))))

;;; Array types ======================================================

(define-record-type <array-type>
  (make-array-type type size)
  array-type?
  (type array-type-type)
  (size array-type-size set-array-type-size!))

(define (array-ref value index)
  (let* ((alien-type (alien-value-type value))
	 (alien-type-size (sizeof alien-type)))
    (make-alien-value (%ffi-deref (alien-value-value value)
				  (* index alien-type-size)
				  alien-type-size)
		      alien-type)))

(define (array-set! value index object)
  (let* ((alien-type (alien-value-type value))
	 (alien-type-size (sizeof alien-type)))
    (%ffi-mem-set! (alien-value-value value)
		   (%ffi-mirror object)
		   (* n alien-type-size))))

;;; LOADING LIBRARIES ================================================

(define-record-type <alien-library>
  (make-alien-library name handle entries variables)
  alien-library?
  (name      alien-library-name      set-alien-library-name!)
  (handle    alien-library-handle    set-alien-library-handle!)
  (entries   alien-library-entries   set-alien-library-entries!)
  (variables alien-library-variables set-alien-library-variables!))

(define-record-type <alien-entry>
  (make-alien-entry name handle)
  alien-entry?
  (name   alien-entry-name   set-alien-entry-name!)
  (handle alien-entry-handle set-alien-entry-handle!))

(define-record-type <alien-variable>
  (make-alien-variable name handle)
  alien-variable?
  (name   alien-variable-name   set-alien-variable-name!)
  (handle alien-variable-handle set-alien-variable-handle!))

(define-record-type (<alien-error> <error>)
  (make-alien-error message)
  alien-error?)

(define (alien-error . args)
  (signal (make-alien-error args)))

(define *aliens* '())

(define *alien-library-path* '())

(define (alien-library-path) (append *alien-library-path* '()))
(define (set-alien-library-path! paths)
  (if (list? paths)
      (set! *alien-library-path* paths)
      (error "argument ~a must be a list when setting dynamic library path" path)))

(define (open-alien-library name)
  (let try ((path *alien-library-path*))
    (if (null? path)
	 (%ffi-open name)
	 (let ((handle (%ffi-open (string-append (car path) name))))
	   (or handle (try (cdr path)))))))

(define (lookup-alien-library name)
  (letrec ((find
	    (lambda (name libs)
	      (if (null? libs)
		  #f
		  (if (string=? name (alien-library-name (car libs)))
		      (car libs)
		      (find name (cdr libs)))))))
    (find name *aliens*)))

(define (register-alien-library! name)
  (let ((lib (lookup-alien-library name)))
    (or lib
	(let ((handle (open-alien-library name)))
	  (if handle
	      (let ((alien-lib (make-alien-library name handle '() '())))
		(set! *aliens* (cons alien-lib *aliens*))
		alien-lib)
	      (alien-error "Couldn't open alien library ~s: ~a"
			   name (%ffi-error)))))))

(define (register-alien-entry! name lib)
  (let ((handle (%ffi-sym (alien-library-handle lib) name)))
    (if handle
	(let ((entry (make-alien-entry name handle)))
	  (set-alien-library-entries! lib
				      (cons entry (alien-library-entries lib)))
	  entry)
	(alien-error "Couldn't find entry point ~s in library ~s" name lib))))

(define (register-alien-variable! name lib)
  (let ((handle (%ffi-sym (alien-library-handle lib) name)))
    (if handle
	(let ((variable (make-alien-variable name handle)))
	  (set-alien-library-variables! lib
					(cons variable
					      (alien-library-variables lib)))
	  variable)
	(alien-error "Couldn't find entry point ~s in library ~s" name lib))))

(define (scheme->alien obj)
  (if (alien-value? obj)
      (alien-value-value obj)
      obj))

(define (apply-entry entry args)
  (%ffi-apply (alien-entry-handle entry) (map scheme->alien args)))

(define (refresh-aliens!) (for-each refresh-alien-entries+variables! *aliens*))

(define (refresh-alien-entries+variables! alien-lib)
  (let ((lib-handle (open-alien-library (alien-library-name alien-lib))))
    (if lib-handle
	(begin
	  (set-alien-library-handle! alien-lib lib-handle)
	  (for-each
	   (lambda (entry)
	     (set-alien-entry-handle! entry
				      (%ffi-sym lib-handle
						(alien-entry-name entry))))
	   (alien-library-entries alien-lib))
	  (for-each
	   (lambda (variable)
	     (set-alien-variable-handle! variable
					 (%ffi-sym lib-handle
						   (alien-variable-name variable))))
	   (alien-library-variables alien-lib)))
	(alien-error "alien library ~a not found in path" alien-lib))))

;;; HIGH LEVEL INTERFACE ========================================================

(define-syntax define-alien-library
  (syntax-rules ()
    ((define-alien-library (?name ?lib-name) )
     (define ?name (register-alien-library! ?lib-name)))))

(define-syntax define-alien-entry
  (syntax-rules ()
    ((define-alien-entry (?name a1) ?type ?lib ?entry)
     (define ?name
       (let ((entry (register-alien-entry! ?entry ?lib)))
	 (lambda (a1)
	   (make-alien-value (%ffi-apply (alien-entry-handle entry)
					 (list (scheme->alien a1)))
			     ?type)))))

    ((define-alien-entry (?name a1 a2) ?type ?lib ?entry)
     (define ?name
       (let ((entry (register-alien-entry! ?entry ?lib)))
	 (lambda (a1 a2)
	   (make-alien-value (%ffi-apply (alien-entry-handle entry)
					 (list (scheme->alien a1)
					       (scheme->alien a2)))
			     ?type)))))
       
    ((define-alien-entry (?name a1 a2 a3) ?type ?lib ?entry)
     (define ?name
       (let ((entry (register-alien-entry! ?entry ?lib)))
	 (lambda (a1 a2 a3)
	   (make-alien-value (%ffi-apply (alien-entry-handle entry)
					 (list (scheme->alien a1)
					       (scheme->alien a2)
					       (scheme->alien a3)))
			     ?type)))))

    ((define-alien-entry (?name a1 a2 a3 a4) ?type ?lib ?entry)
     (define ?name
       (let ((entry (register-alien-entry! ?entry ?lib)))
	 (lambda (a1 a2 a3 a4)
	   (make-alien-value (%ffi-apply (alien-entry-handle entry)
					 (list (scheme->alien a1)
					       (scheme->alien a2)
					       (scheme->alien a3)
					       (scheme->alien a4)))
			     ?type)))))

    ((define-alien-entry (?name a1 a2 a3 a4 a5) ?type ?lib ?entry)
     (define ?name
       (let ((entry (register-alien-entry! ?entry ?lib)))
	 (lambda (a1 a2 a3 a4 a5)
	   (make-alien-value (%ffi-apply (alien-entry-handle entry)
					 (list (scheme->alien a1)
					       (scheme->alien a2)
					       (scheme->alien a3)
					       (scheme->alien a4)
					       (scheme->alien a5)))
			     ?type)))))

    ((define-alien-entry (?name ?args ...) ?type ?lib ?entry)
     (define ?name
       (let ((entry (register-alien-entry! ?entry ?lib)))
	 (lambda (?args ...)
	   (make-alien-value (apply-entry entry (list ?args ...))
			     ?type)))))
    ((define-alien-entry ?name ?type ?lib ?entry)
     (define ?name
       (let ((entry (register-alien-entry! ?entry ?lib)))
	 (lambda args
	   (make-alien-value (apply-entry entry args)
			     ?type)))))))

(define-syntax define-alien-variable
  (syntax-rules ()
    ((define-alien-variable ?name ?type ?lib ?variable)
     (define ?name
       (let ((var (register-alien-variable! ?variable ?lib))
	     (type ?type))
	 (lambda ()
	   (pointer-deref (make-alien-value (alien-variable-handle var)
					    (make-pointer-type type)))))))))

;	   (make-alien-value (%ffi-deref (alien-variable-handle var) 0 (sizeof type))
;			     type)))))))

;;; INITIALIZATION  =====================================================

(set! *daemons* (cons (make-action 'alien-system
				   '()
				   refresh-aliens!)
		      *daemons*))
