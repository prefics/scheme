;;; fasl.scm -- FASL support procedures for reading/writing


;;; Constants ============================================================

(define fasl/fixnum          0)
(define fasl/true            1)
(define fasl/false           2)
(define fasl/char            3)
(define fasl/pair            4)
(define fasl/string          5)
(define fasl/vector          6)
(define fasl/bvec            7)
(define fasl/nil             8)
(define fasl/unspecific      9)
(define fasl/unbound         10)
(define fasl/symbol          11)
(define fasl/ref             12)
(define fasl/eof             14)
(define fasl/procedure       15)
(define fasl/negative-fixnum 16)

(define type/define    0)
(define type/syntax    1)
(define type/expr      2)
(define type/module    3) ;; <===== THIS IS NEW

;;; Writing FASL ==========================================================

(define (write-type type port)
  (write-byte type port))

(define (write-byte b port)
  (if (and (<= 0 b)
           (<= b 255))
      (display (ascii->char b) port)
      (error "value ~a out of range in WRITE-BYTE" b)))

(define (write-long b port)
  (if (and (<= 0 b)
           (< b (* 256  65536)))
      (let ((b32 (quotient b (* 256 65536)))
	    (b24 (modulo (quotient b (* 256 256)) 256))
	    (b16 (modulo (quotient b 256) 256))
	    (b8 (modulo b 256)))
	(write-byte b32 port)
	(write-byte b24 port)
	(write-byte b16 port)
	(write-byte b8 port))
      (error "value ~a out of range in WRITE-LONG" b)))
      
(define (write-counted-string str port)
  (let ((size (string-length str)))
    (write-long size port)
    (let loop ((i 0))
      (if (< i size)
	  (begin (write-byte (char->ascii (string-ref str i)) port)
		 (loop (+ i 1)))))))

(define (write-fasl! template port)
;  (display "FASL->") (display template) (newline)
  (cond ((eof-object? template) (write-byte fasl/eof port))
        ((unbound-object? template) (write-byte fasl/unbound port))
        ((unspecific-object? template) (write-byte fasl/unspecific port))
        ((number? template)
	 (if (<= 0 template )
	     (begin
	       (write-byte fasl/fixnum port)
	       (write-long template port))
	     (begin
	       (write-byte fasl/negative-fixnum port)
	       (write-long (- 0 template) port))))
        ((eq? template #t) (write-byte fasl/true port))
        ((eq? template #f) (write-byte fasl/false port))
        ((char? template)
         (write-byte fasl/char port)
         (write-byte (char->ascii template) port))
        ((pair? template)
         (write-byte fasl/pair port)
         (write-fasl! (car template) port)
         (write-fasl! (cdr template) port))
        ((string? template)
         (write-byte fasl/string port)
	 (write-counted-string template port))
        ((bvec? template)
         (write-byte fasl/bvec port)
         (write-long (bvec-length template) port)
         (let loop ((i 0))
           (if (< i (bvec-length template))
               (begin (write-byte (bvec-ref template i) port)
                      (loop (+ i 1))))))
	((ref? template)
	 (write-byte fasl/ref port)
	 (write-counted-string (symbol->string (ref/name template)) port)
	 (write-counted-string (symbol->string (ref/module template)) port)) ; This is the module name
        ((vector? template)
         (write-byte fasl/vector port)
         (write-long (vector-length template) port)
         (let loop ((i 0))
           (if (< i (vector-length template))
               (begin (write-fasl! (vector-ref template i) port)
                      (loop (+ i 1))))))
        ((eq? template '())
         (write-byte fasl/nil port))
        ((symbol? template)
         (write-byte fasl/symbol port)
	 (write-counted-string (symbol->string template) port))
;         ((procedure? template)
;          (write-byte fasl/procedure port)
;          (write-long (procedure-length template) port)
;          (let loop ((i 0))
;            (if (< i (procedure-length template))
;                (begin (write-fasl! (procedure-ref template i) port)
;                       (loop (+ i 1))))))         
        (else (error "don't know how to write FASL for ~a" template))))

;;; Reading FASL ==========================================================

(define (read-byte port)
  (let ((byte (read-char port)))
    (if (eof-object? byte)
        byte          
        (char->ascii byte))))

(define read-long
  (lambda (port)
    (let ((h1 (char->ascii (read-char port)))
          (h2 (char->ascii (read-char port)))
          (h3 (char->ascii (read-char port)))
          (h4 (char->ascii (read-char port))))
      (+ (* (+ (* (+ (* 256 h1) 
                     h2) 
                  256) 
               h3) 
            256) 
         h4))))

(define read-counted-string
  (lambda (port)
    (let ((size (read-long port)))
      (let ((str (make-string size #\1)))
        (let ((loop #f))
          (set! loop 
                (lambda (i) 
                  (if (< i size)
                      (begin
                        (string-set! str i (read-char port))
                        (loop (+ i 1)))
                      str)))
          (loop 0))))))

(define *fasl-reader* (make-vector 17 (lambda () 'undefined-fasl-reader)))

(define define-fasl-reader 
  (lambda (type body)
    (vector-set! *fasl-reader* type body)))

(define-fasl-reader fasl/eof
  (lambda (port) 'the-end-of-file-object))

(define-fasl-reader fasl/unbound
  (lambda (port) (unbound-object)))

(define-fasl-reader fasl/unspecific
  (lambda (port) (unspecific-object)))

(define-fasl-reader fasl/fixnum 
  (lambda (port) 
    (read-long port)))

(define-fasl-reader fasl/negative-fixnum
  (lambda (port)
    (- 0 (read-long port))))

(define-fasl-reader fasl/true 
  (lambda (port) 
    #t))

(define-fasl-reader fasl/false 
  (lambda (port) 
    #f))

(define-fasl-reader fasl/char 
  (lambda (port) 
    (read-char port)))

(define-fasl-reader fasl/pair 
  (lambda (port) 
    (let ((head (read-fasl port)))
      (let ((tail (read-fasl port)))
        (cons head tail)))))
(define-fasl-reader fasl/string read-counted-string)

(define-fasl-reader fasl/vector
  (lambda (port)
    (let ((size (read-long port)))
      (let ((vec (make-vector size #f)))
        (let ((loop #f))
          (set! loop (lambda (index) 
                       (if (< index size)
                           (begin 
                             (vector-set! vec index (read-fasl port))
                             (loop (+ index 1)))
                           vec)))
          (loop 0))))))

(define-fasl-reader fasl/procedure
  (lambda (port)
    (let ((size (read-long port)))
      (let ((proc (make-procedure size)))
        (let ((loop #f))
          (set! loop (lambda (index) 
                       (if (< index size)
                           (begin 
                             (procedure-set! proc index (read-fasl port))
                             (loop (+ index 1)))
                           proc)))
          (loop 0))))))

(define-fasl-reader fasl/bvec
  (lambda (port)
    (let ((size (read-long port)))
      (let ((bvec (make-bvec size)))
        (let ((loop #f))
          (set! loop (lambda (index) 
                       (if (< index size)
                           (begin 
                             (bvec-set! bvec index (read-char port))
                             (loop (+ index 1)))
                           bvec)))
          (loop 0))))))

(define-fasl-reader fasl/nil 
  (lambda (port) 
    '()))

(define-fasl-reader fasl/symbol
  (lambda (port)
    (string->symbol (read-counted-string port))))

(define-fasl-reader fasl/ref
  (lambda (port)
    (let* ((name (string->symbol (read-counted-string port)))
	   (module-name (string->symbol (read-counted-string port)))
	   (ref (lookup-ref name module-name)))
      (if ref
	  ref
	  (let ((module (lookup-module module-name)))
	    (if module
		(bind-ref! name (unbound-object) module)
		(error "Module does not exists" module-name)))))))

(define read-fasl
  (lambda (port)
    (let ((byte (read-char port)))
      (if (eof-object? byte)
          byte
          (let ((index (char->ascii byte)))
            (if (< index (vector-length *fasl-reader*))
                ((vector-ref *fasl-reader* index) port)
                (begin 
                  (display "ERROR !!!!!!! Unknown fasl code")
                  (display (char->integer byte)))))))))

(define read-type
  (lambda (port) (read-byte port)))

