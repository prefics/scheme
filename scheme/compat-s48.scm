(define-structure compat
  (export + - quotient remainder modulo *
	  < <= = > >= symbol? eq? %fixnum? %real? %real->string char?
	  pair? make-pair car cdr set-car! set-cdr! null? 
	  vector? make-vector vector-length vector-ref vector-set!
	  make-string string? string-length string-ref string-set!
	  zero?

	  open-input-channel open-output-channel
	  read-channel peek-channel 
          make-channel write-channel 
	  close-channel eof-object? channel?
	  channel-number
	  
	  symbol->string string->symbol apply number->char char->number

          make-bvec bvec? bvec-length bvec-set! bvec-ref 
	  make-procedure procedure? procedure-length procedure-ref 
	  procedure-set! 
	  save-image string->keyword keyword->string keyword?
	  make-stob stob-class set-stob-class! stob? stob-ref stob-set!
	  stob-length

	  make-ref ref? ref-name ref-module ref-value set-ref-name! set-ref-module! set-ref-value!

	  suspend-cc resume-cc

	  object-hash
	  
	  %record-ref %record-set!

	  %assq

          (define :syntax)
	  (define-syntax :syntax)
	  (lambda :syntax)
	  (let-syntax :syntax)
	  (let :syntax)
	  (begin :syntax)
	  (set! :syntax)
	  (if :syntax)
	  (letrec :syntax)
	  (quote :syntax)
          
          (and :syntax)
          (or :syntax)
          (cond :syntax)
          (let* :syntax))

  (open scheme big-scheme)

  (begin
;     (define-syntax define-syntax 
;       (syntax-rules ()
; 	((define-syntax ?syntax ...) (define-syntax ?syntax ...))))
;     (define-syntax lambda 
;       (syntax-rules ()
; 	((lambda ?args . ?body) (lambda ?args . ?body))))
; ;    (define-syntax let-syntax let-syntax)
;     (define-syntax let
;       (syntax-rules ()
; 	((let ?bindings ?body ...) (let ?bindings ?body ...))))
;     (define-syntax begin
;       (syntax-rules ()
; 	((begin ?e ...) (begin ?e ...))))
;     (define-syntax set!
;       (syntax-rules ()
; 	((set! ?id ?val) (set! ?id ?val))))
;     (define-syntax if
;       (syntax-rules ()
; 	((if ?exp ?then . ?else) (if ?exp ?then . ?else))))
;     (define-syntax letrec
;       (syntax-rules ()
; 	((letrec ?bindings ?body ...) (letrec ?bindings ?body ...))))
;     (define-syntax quote
;       (syntax-rules ()
; 	((quote ?v) (quote ?v))))

    (define-syntax define-prims
      (syntax-rules ()
	((define-prims ?id ...) (begin (define . (?id ?id)) ...))))

    (define-prims + - quotient remainder modulo *
      < <= = > >= symbol? eq? char? 
      pair? car cdr set-car! set-cdr! null?
      vector? make-vector vector-length vector-ref vector-set!
      make-string string? string-length string-ref string-set!
      symbol->string string->symbol apply)
     
    (define (%fixnum? obj) (number? obj))
    (define (%real? obj) (real? obj))
    (define (%real->string obj) (number->string obj))

    (define (open-input-channel name)
      (open-input-file name))
    (define (open-output-channel name . opt)
      (open-output-file name))
   
    (define (read-channel channel)
      (read-char channel))
    
    (define (peek-channel channel)
      (peek-char channel))
    
    (define *stdin* (current-input-port))
    (define *stdout* (current-output-port))

    (define (make-channel int)
      (if (= int 0) *stdin* *stdout*))
    
    (define (channel-number ch)
      (cond ((eq? ch *stdin*) 0)
	    ((eq? ch *stdout*) 1)
	    (else (error "channel number not supported"))))

    (define (write-channel obj ch start stop)
      (cond ((stob? obj)
	     (display "#{")
	     (display (stob-ref (stob-class obj) 0))
	     (display "}"))
	    ((char? ch) (display ch obj))
	    ((string? ch) (display (substring ch start stop) obj))
	    (else (display ch obj))))

    (define close-channel list)
    
    (define (channel? p)
      (or (input-port? p) (output-port? p)))
    (define eof-object?
      (let ((%eof-object? eof-object?))
        (lambda (o)
          (or (%eof-object? o) (eq? o 'the-end-of-file-object)))))

    (define make-pair cons)
;    (define string->keyword string->symbol)

    (define char->number char->ascii)
    (define number->char ascii->char)
    
    (define make-closure cons)
    (define save-image (lambda l (display "KERNEL CALL: SAVING_IMAGE")))
    
    (define (string->keyword str) 
      (string->symbol (string-append ":" str)))
    (define (keyword->string sym)
      (symbol->string sym))

    (define *bvec-type* "bvec")
    (define (make-bvec size)
      (vector *bvec-type* (make-vector size 'undefined)))
    
    (define (bvec? obj)
      (and (vector? obj)
	   (> (vector-length obj) 0)
	   (eq? *bvec-type* (vector-ref obj 0))))
    
    (define (bvec-length bvec)
      (vector-length (vector-ref bvec 1)))
    
    (define (bvec-set! bvec i obj)
      (vector-set! (vector-ref bvec 1) i obj))
    
    (define (bvec-ref bvec i)
      (vector-ref (vector-ref bvec 1) i))

    (define (make-procedure n)
      (lambda l 
        (display "I am a procedure created by the MAKE-PROCEDURE")))
    
    (define (procedure-ref proc n)
      (make-vector 10 '()))

    (define (procedure-set! proc i v)
      proc)
        
    (define (procedure-length proc)
      (error "procedure-length unsupported"))

    (define *ref-type* "ref")
    (define (make-ref name module value) (vector *ref-type* name module value))
    (define (ref? obj) 
      (and (vector? obj) 
	   (> (vector-length obj) 0) 
	   (eq? *ref-type* (vector-ref obj 0))))
    (define (ref-name ref) (vector-ref ref 1))
    (define (ref-module ref) (vector-ref ref 2))
    (define (ref-value ref) (vector-ref ref 3))
    (define (set-ref-name! ref name) (vector-set! ref 1 name))
    (define (set-ref-module! ref module) (vector-set! ref 2 module))
    (define (set-ref-value! ref value) (vector-set! ref 3 value))

    (define *stob-type* "stob")
    
    (define (make-stob size class)
      (let ((stob (make-vector (+ size 2) 0)))
	(vector-set! stob 0 *stob-type*)
	(vector-set! stob 1 class)
	stob))
    
    (define (stob-length stob)
      (- (vector-length stob) 2))

    (define (stob? obj)
      (and (vector? obj)
	   (eq? (vector-ref obj 0) *stob-type*)))
    
    (define (stob-class obj)
      (if (stob? obj)
	  (vector-ref obj 1)
	  (error "object ~a not a STOB" obj)))
    
    (define (set-stob-class! stob class)
      (if (stob? stob)
	  (vector-set! stob 1 class)
	  (error "object ~a not a stob" stob)))
    
    (define (stob-ref stob index)
      (if (stob? stob)
	  (vector-ref stob (+ index 2))
	  (error "object ~a not a stob" stob)))
    
    (define (stob-set! stob index val)
      (if (stob? stob)
	  (vector-set! stob (+ index 2) val)
	  (error "object ~a not a stob" stob)))

    (define *traps* (make-vector 11 #f))
    (define *env* #f)
    (define *cont* #f)

    (define (suspend-cc k)
      (call-with-current-continuation
       (lambda (return)
	 (let ((vec (make-vector 13 #f)))
	   (vector-set! vec 12 *traps*)
	   (vector-set! vec 11 *cont*)
	   (vector-set! vec 10 *env*)
	   (vector-set! vec 0 return)
	   (k vec)))))

    (define (resume-cc k val)
      (set! *traps* (vector-ref k 12))
      (set! *cont* (vector-ref k 11))
      (set! *env* (vector-ref k 10))
      ((vector-ref k 0) val))
    
    (define (keyword? obj)
      (and (symbol? obj)
	   (let ((str (symbol->string obj)))
	     (char=? #\: (string-ref str (- (string-length str) 1))))))
    
    (define (object-hash obj) 1)

    (define (%record-ref obj field)
      (let* ((type (stob-class obj))
	     (offset (field-pos field (stob-ref type 3))))
	(if offset
	    (stob-ref obj offset)
	    (error "field does not exists ~s in ~a" field obj))))
    
    (define (%record-set! obj field val)
      (let* ((type (stob-class obj))
	     (offset (field-pos field (stob-ref type 3))))
	(if offset
	    (stob-set! obj offset val)
	    (error "Field does not exists ~s in ~s" field obj))))
    
    (define (%assq obj alist)
      (if (null? alist)
	  #f
	  (if (eq? obj (car (car alist)))
	      (car alist)
	      (%assq obj (cdr alist)))))
    
    (define (field-pos field fields)
      (if (null? fields)
	  #f
	  (if (eq? field (car fields))
	      0
	      (let ((p (field-pos field (cdr fields))))
		(if p (+ 1 p) p)))))
    ))
