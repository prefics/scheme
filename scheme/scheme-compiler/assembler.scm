;;; assembler.scm -- bytecode assembler

;;; Assembler

;;; DEBUG Format consists of:
;;
;; NAME -> debug-name of the code (string)
;; ENVs -> environements consists of an list of (offset . names)
;; PARAMS -> parameter lists, e.g (num num)
;; DOC -> documentation string or #f
;; FILE -> filename where code is defined

(define (make-empty-debug)
  (vector #f '() #f #f #f))

(define (debug-env debug) (vector-ref debug 1))

(define (debug-bind! debug offset name)
  (vector-set! debug 
               1
               (cons (cons offset name) (debug-env debug))))
  
(define (debug-name-set! debug name)
  (vector-set! debug 0 name))

(define (debug-parameters-set! debug params)
  (vector-set! debug 2 params))

(define (debug-doc-set! debug doc)
  (vector-set! debug 3 doc))

(define (debug-file-set! debug file-name)
  (vector-set! debug 4 file-name))

(define (assembler asm-code module)
  ;; main entry point of assembler
  (compute-labels asm-code 0 
            (lambda (labels size)
              (let ((bvec (make-bvec size)))
                (assemble asm-code labels bvec module
                          (lambda (code literals debug) 
                            (vector code literals debug)))))))

(define (compute-labels asm-code pos k)
  (if (null? asm-code)
      (k '() 0)
      (let ((instr (car asm-code)))
        (compute-labels (cdr asm-code) (if (symbol? instr)
                                           pos
                                           (+ pos (instr-length instr)))
                        (lambda (labels size)
                          (if (symbol? instr)
                              (k (cons (cons instr pos) labels)
                                 size)
                              (k labels (+ size (instr-length instr)))))))))

(define (assemble asm-code labels bvec module k)
  (let loop ((i 0)
             (literals '())
             (asm asm-code)
             (debug (make-empty-debug)))
    (if (null? asm)
        (k bvec (list->vector literals) debug)
        (let ((instr (car asm)))
          (if (symbol? instr)
              (loop i literals (cdr asm) debug)
              (assemble-one instr bvec i literals labels module
                            debug
			    (lambda (new-literals new-i)
			      (loop new-i new-literals 
                                    (cdr asm) debug))))))))

(define (append-lit lit lits)
  (if (memq lit lits)
      lits
      (append lits (list lit))))

(define (instr-length instr)
;  (display instr)
  (cond ((and (pair? instr)
	      (or (eq? 'iffalse (car instr))
		  (eq? 'jump (car instr))))
	 3)
        ((and (pair? instr)
              (eq? 'debug (car instr))) 0)
	(else (length instr))))

(define *assemblers* (make-hashtable))

(define (define-assembler opcode coder)
  (hash-set! *assemblers* opcode coder))

(define-assembler 'lit
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 0)
    (let ((lits (append literals (list (cadr instr)))))
      (bvec-set! bvec (+ i 1) (pos (cadr instr) lits))
      (k lits (+ i 2)))))

(define-assembler 'args
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 1)
    (bvec-set! bvec (+ i 1) (cadr instr))
    (k literals (+ i 2))))
    
(define-assembler 'global
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 2)
    (if (cdadr instr)
	(let* ((module (lookup-module (cdadr instr)))
	       (ref (lookup/create-ref (caadr instr) module))
	       (lits (append-lit ref literals)))
	  (bvec-set! bvec (+ i 1) (pos ref lits))
	  (k lits (+ i 2)))
	(let* ((ref (lookup/create-ref (caadr instr) module))
	       (lits (append-lit ref literals)))
	  (bvec-set! bvec (+ i 1) (pos ref lits))
	  (k lits (+ i 2))))))
  
(define-assembler 'set-global
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 3)
    (if (cdadr instr)
	(let* ((module (lookup-module (cdadr instr)))
	       (arg (cadr instr))
	       (ref (lookup/create-ref (car arg) module))
	       (lits (append-lit ref literals)))
	  (bvec-set! bvec (+ i 1) (pos ref lits))
	  (k lits (+ i 2)))
	(let* ((arg (cadr instr))
	       (ref (lookup/create-ref (car arg) module))
	       (lits (append-lit ref literals)))
	  (bvec-set! bvec (+ i 1) (pos ref lits))
	  (k lits (+ i 2))))))

(define-assembler 'local
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 4)
    (bvec-set! bvec (+ i 1) (cadr instr))
    (bvec-set! bvec (+ i 2) (caddr instr))
    (k literals (+ i 3))))

(define-assembler 'set-local
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 5)
    (bvec-set! bvec (+ i 1) (cadr instr))
    (bvec-set! bvec (+ i 2) (caddr instr))
    (k literals (+ i 3))))

(define-assembler 'call
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 6)
    (bvec-set! bvec (+ i 1) (cadr instr))
    (k literals (+ i 2))))
    
(define-assembler 'tail
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 7)
    (bvec-set! bvec (+ i 1) (cadr instr))
    (k literals (+ i 2))))

(define-assembler 'iffalse
  (lambda (instr bvec i literals labels module debug k)
    (let ((pc (cdr (assq (cadr instr) labels))))
      (bvec-set! bvec i 8)
      (bvec-set! bvec (+ i 1) (quotient pc 256))
      (bvec-set! bvec (+ i 2) (modulo pc 256))
    (k literals (+ i 3)))))

(define-assembler 'jump
  (lambda (instr bvec i literals labels module debug k)
    (let ((pc (cdr (assq (cadr instr) labels))))
      (bvec-set! bvec i 9)
      (bvec-set! bvec (+ i 1) (quotient pc 256))
      (bvec-set! bvec (+ i 2) (modulo pc 256))
      (k literals (+ i 3)))))

(define-assembler 'return
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 10)
    (k literals (+ i 1))))
  
(define-assembler 'push
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 11)
    (k literals (+ i 1))))

(define-assembler 'closure
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 12)
    (let* ((template (assembler (cadr instr) module))
	   (lits (append literals (list template))))
      (bvec-set! bvec (+ i 1) (pos template lits))
      (k lits (+ i 2)))))

(define-assembler 'leave
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 13)
    (k literals (+ i 1))))

(define-assembler 'args>=
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 14)
    (bvec-set! bvec (+ i 1) (cadr instr))
    (k literals (+ i 2))))

(define-assembler 'undef
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 15)
    (bvec-set! bvec (+ i 1) (cadr instr))
    (k literals (+ i 2))))

(define-assembler 'env
  (lambda (instr bvec i literals labels module debug k)
    (bvec-set! bvec i 16)
    (bvec-set! bvec (+ i 1) (cadr instr))
    (k literals (+ i 2))))

(define-assembler 'debug
  (lambda (instr bvec i literals labels module debug k)
    (let* ((op (cadr instr))
           (op-name (car op)))
      (cond ((eq? op-name 'name)
             (debug-name-set! debug (cadr op))
             (k literals i))
            ((eq? op-name 'env)
             (let ((env (cadr op)))
	       (debug-bind! debug i env)
	       (k literals i)))
            ((eq? op-name 'parameters)
             (debug-parameters-set! debug (cadr op))
             (k literals i))
            ((eq? op-name 'doc)
             (debug-doc-set! debug (cadr op))
             (k literals i))
            ((eq? op-name 'file)
             (debug-file-set! debug (cadr op))
             (k literals i))
            (else (error "Unknown debug instruction ~a" op-name))))))

(define *primitives*
  (let ((hash (make-hashtable)))
    (let loop 
	((opcode
	  '(+ - quotient remainder modulo * < <= = > >= symbol? eq? %fixnum? pair? 
              make-pair 

              car cdr set-car! set-cdr! null? vector? make-vector 
	      vector-length vector-ref vector-set! make-string string-length
	      string-ref string-set! make-channel write-channel

              read-channel eof-object?
	      channel-number channel? open-input-channel open-output-channel char->number
	      number->char symbol->string string->symbol apply make-stob stob-ref
	      stob-set! stob-class stob?

              stob-length suspend-cc resume-cc make-bvec bvec?
	      bvec-ref bvec-set! bvec-length save-image string->keyword keyword->string make-procedure
	      procedure? procedure-set! procedure-ref procedure-length

              char? close-channel
	      peek-channel string? make-ref ref? ref-name ref-value ref-module 
	      set-ref-name! set-ref-value! set-ref-module! set-stob-class!
	      %xdraw-arc  %xdraw-text %xopen-font

              %xclose-font %xread-event
	      bit-or bit-and bit-xor bit-ash bit-not object-hash 
              keyword? / a a a a a a 
	      
	      posix-open posix-close posix-read posix-write posix-chdir posix-access
	      posix-mkdir posix-stat posix-time posix-localtime posix-gmtime
	      posix-seek posix-sync posix-rmdir posix-unlink posix-rename

              posix-chmod
	      posix-chgrp posix-chown posix-getenv posix-setenv posix-fork posix-exec
	      posix-dirfiles posix-recv posix-send posix-socket posix-bind posix-listen
	      posix-accept posix-connect posix-select

              posix-mknod posix-tempnam
	      posix-getpwnam posix-getgrnam posix-utime posix-umask posix-getpid
	      posix-getppid posix-getsid posix-setsid posix-getuid posix-geteuid
	      posix-getgid posix-getegid posix-setuid posix-setegid

              posix-seteuid posix-setgid
              posix-exit posix-waitpid posix-getcwd posix-pipe posix-dup2 
	      %ffi-open %ffi-error %ffi-sym %ffi-close %ffi-apply
	      %ffi-mem-ref %ffi-string-ref %ffi-mem-set! %ffi-mirror

              %ffi-deref %ffi-malloc %ffi-free 
	      %ffi-u8-ref %ffi-u16-ref %ffi-u32-ref %ffi-u64-ref %ffi-double-ref
              posix-kill posix-openpty a a a a a a

              a a a a %posix-uname
	      posix-setsockopt posix-getsockopt
	      posix-unix-address posix-inet-address
	      %posix-block-signal!
	      %assq %record-ref %record-set!
	      %test+set! %real->string %string->real

              %fixnum->real %real? %round %truncate %ceiling %floor
              %rationalize %exp %log %sin %cos %tan %asin %acos %atan %atan2
              
              %sqrt %expt %native-call %channel-flush	      
	      %host-error %error=? %set-timer %posix-symlink %posix-readlink
              %random a a a a a a))
	 (p 0))
      (if (null? opcode) 
	  hash
	  (let ((op (car opcode)))
	    (hash-set! hash op p)
	    (loop (cdr opcode) (+ p 1)))))))

(define (assemble-one instr bvec i literals labels module debug k)
  (let* ((opcode (car instr))
	 (encoder (hash-ref *assemblers* opcode)))
    (if encoder
	(encoder instr bvec i literals labels module debug k)
	(let ((prim (hash-ref *primitives* opcode)))
	  (if prim
	      (begin 
		(bvec-set! bvec i (+ 32 prim))
;		(if (eq? opcode 'make-procedure)
;		    (bvec-set! bvec (+ i 1) (cadr instr)))
		(k literals (+ i (instr-length instr))))
	      (error "unknown opcode ~a" opcode))))))

