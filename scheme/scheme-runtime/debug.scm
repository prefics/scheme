;;; debug.scm -- debug information

;;;Code:

(define (continuation-val cc)      (vector-ref cc 0))
(define (continuation-stack cc)    (vector-ref cc 1))
(define (continuation-top cc)      (vector-ref cc 2))
(define (continuation-env cc)      (vector-ref cc 3))
(define (continuation-template cc) (vector-ref cc 4))
(define (continuation-code cc)     (vector-ref cc 5))
(define (continuation-literals cc) (vector-ref cc 6))
(define (continuation-pc cc)       (vector-ref cc 7))
(define (continuation-cont cc)     (vector-ref cc 8))
(define (continuation-argc cc)     (vector-ref cc 9))
(define (continuation-dynamic cc)  (vector-ref cc 10))
(define (continuation-trap cc)     (vector-ref cc 11))

(define (set-continuation-val! cc o)      (vector-set! cc 0 o))
(define (set-continuation-stack! cc o)    (vector-set! cc 1 o))
(define (set-continuation-top! cc o)      (vector-set! cc 2 o))
(define (set-continuation-env! cc o)      (vector-set! cc 3 o))
(define (set-continuation-template! cc o) (vector-set! cc 4 o))
(define (set-continuation-code! cc o)     (vector-set! cc 5 o))
(define (set-continuation-literals! cc o) (vector-set! cc 6 o))
(define (set-continuation-pc! cc o)       (vector-set! cc 7 o))
(define (set-continuation-cont! cc o)     (vector-set! cc 8 o))
(define (set-continuation-argc! cc o)     (vector-set! cc 9 o))
(define (set-continuation-dynamic! cc o)  (vector-set! cc 10 o))
(define (set-continuation-trap! cc o)     (vector-set! cc 11 o))

(define (make-frame prev env template pc)
  (vector prev env template pc))

(define (frame? obj)
  (and (vector? obj)
       (= 4 (vector-length obj))))

(define (frame-ref cont n)
  (let find ((n n)
	     (frame (continuation-cont cont)))
    (if (<= n 0)
	frame
	(find (- n 1) (frame-previous frame)))))

(define (frame-previous frame) (vector-ref frame 0))
(define (frame-env frame) (vector-ref frame 1))
(define (frame-template frame) (vector-ref frame 2))
(define (frame-pc frame) (vector-ref frame 3))

(define (frame-code frame) (template-code (frame-template frame)))
(define (frame-literals frame) (template-literals (frame-template frame)))
(define (frame-debug frame) (template-debug (frame-template frame)))

(define (frame-set! cont n frame)       (list-set! cont n frame))
(define (set-frame-previous! frame val) (vector-set! frame 0 val))
(define (set-frame-env! frame val)      (vector-set! frame 1 val))
(define (set-frame-template! frame val) (vector-set! frame 2 val))
(define (set-frame-pc! frame val)       (vector-set! frame 2 val))

(define (procedure-template proc) (procedure-ref proc 1))
(define (procedure-environment proc) (procedure-ref proc 0))

(define (make-template code literals debug) (vector code literals debug))

(define (template-code template)     (vector-ref template 0))
(define (template-literals template) (vector-ref template 1))
(define (template-debug template)    (vector-ref template 2))

(define (set-template-code! template val)     (vector-set! template 0 val))
(define (set-template-literals! template val) (vector-set! template 1 val))
(define (set-template-debug! template val)    (vector-set! template 2 val))

(define (make-debug name env parameters doc file)
  (vector name env parameters doc file))

(define (debug-name debug) (vector-ref debug 0))
(define (debug-env debug) (vector-ref debug 1))
(define (debug-parameters debug) (vector-ref debug 2))
(define (debug-doc debug) (vector-ref debug 3))
(define (debug-file debug) (vector-ref debug 4))

(define (for-each-frame proc cc)
  (let loop ((frame (continuation-cont cc)))
    (if (frame? frame)
	(begin
	  (proc frame)
	  (loop (frame-previous frame))))))

;;; Pretty backtrace

(define *bytecode-length*
  '#(2 2 2 2 3 3 2 2
     3 3 1 1 2 1 2 2
     2 1 1 1 1 1 1 1
     1 1 1 1 1 1 1 1))

(define *bytecode-names*
  '#(lit args global set-global! local set-local! call tail
     iffalse jump ret push closure leave argsgt undef
     env %% %% %% %% %% %% %%
     %% %% %% %% %% %% %% %%
     + - quotient remainder modulo * < <= = > >= symbol? eq? %fixnum? pair?
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
     %host-error %error=? %set-timer a a a a a a a a a))

(define (bytecode-name n) (vector-ref *bytecode-names* n))
(define (bytecode-args n)
  (if (< n 32)
      (vector-ref *bytecode-length* n)
      1))

(define (disassemble-bytecode indent code lits)
  (let loop ((i 0))
    (if (< i (bvec-length code))
	(let* ((byte (bvec-ref code i))
	       (name (bytecode-name byte))
	       (args (bytecode-args byte)))
	  (display (make-string indent #\space))
	  (display i) (display " ")
	  (cond ((or (= byte 0)
		     (= byte 2)
		     (= byte 3))
		 (display (list name
				(vector-ref lits
					    (bvec-ref code (+ i 1))))))
		((or (= byte 12))
		 (let* ((offset (bvec-ref code (+ i 1)))
			(template (vector-ref lits offset)))
		   (display "closure") (newline)
		   (disassemble-bytecode (+ indent 4)
					 (template-code template)
					 (template-literals template))))

		(else
		 (display (cons name
				(let loop ((n 1))
				  (if (< n args)
				      (cons (bvec-ref code (+ i n))
					    (loop (+ n 1)))
				      '()))))))
	  (newline)
	  (loop (+ i args))))))

(define (disassemble proc)
  (let* ((template (procedure-ref proc 1))
	 (code (template-code template))
	 (lits (template-literals template)))
    (disassemble-bytecode 0 code lits)))

(define (frame-length frame)
  (let len ((i 0)
	    (frame frame))
    (if (frame? frame)
	(len (+ i 1) (frame-previous frame))
	i)))

(define (frame-arguments frame)
  (let* ((env (frame-env frame))
	 (up (compute-argument-depth (frame-code frame) (frame-pc frame))))
    (if up
	(let loop ((up up)
		   (env env))
	  (if (= up 0)
	      (cdr (vector->list env))
              (loop (- up 1) (vector-ref env 0))))
              ;; (let ((parent-env (vector-ref env 0)))
              ;;   (if (vector? parent-env)
              ;;       (loop (- up 1) parent-env)
              ;;       '(error-in-frame-argument)))))
	#f)))

(define (compute-argument-depth code pc)
  (let ((word-ref
	 (lambda (code i)
	   (+ (* 256 (bvec-ref code i))
	      (bvec-ref code (+ i 1))))))
    (let find ((code code)
	       (i 0))
      (cond
       ((= i pc) 0)
       ((< i (bvec-length code))
	(let ((byte (bvec-ref code i)))
	  (cond
	   ;; JUMP bytecode
	   ((= byte 9) (find code (word-ref code (+ i 1))))
	   ;; IFFALSE
	   ((= byte 8)
	    (let ((depth (find code (+ i 3))))
	      (if depth
		  depth
		  (find code (word-ref code (+ i 1))))))
	   ;; ENV bytecode
	   ((= byte 16)
	    (let ((depth (find code (+ i 2))))
	      (if depth (+ 1 depth) depth)))
	   ;; LEAVE bytecode
	   ((= byte 13)
	    (let ((depth (find code (+ i 2))))
	    (if depth (- depth 1) depth)))
	   ;; Otherwise
	   (else (find code (+ i (if (< byte 32)
				     (vector-ref *bytecode-length* byte)
				     1)))))))
       (else #f)))))

(define (compute-locals code pc alist)
  (let ((word-ref
	 (lambda (code i)
	   (+ (* 256 (bvec-ref code pc))
	      (bvec-ref code (+ pc 1))))))
    (let find ((code code)
	       (i 0)
	       (env '()))
      (cond
       ((= i pc) env)
       ((< i (bvec-length code))
	(let ((byte (bvec-ref code i)))
	  (cond
	   ;; JUMP bytecode
	   ((= byte 9) (find code (word-ref code (+ i 1)) env))
	   ;; IFFALSE
	   ((= byte 8)
	    (let ((env (find code (+ i 3) env)))
	      (if env
		  env
		  (find code (word-ref code (+ i 1)) env))))
	   ;; ENV/UNDEF bytecode
	   ((or (= byte 16) (= byte 15))
	    (let ((vars (assq i alist)))
	      (if vars
		  (find code (+ i 2) (cons (cdr vars) env))
		  (find code (+ i 2) (cons '() env)))))
	   ;; LEAVE bytecode
	   ((= byte 13)
	    (find code (+ i 2) (cdr env)))
	   ;; Otherwise
	   (else (find code (+ i (if (< byte 32)
				     (vector-ref *bytecode-length* byte)
				     1))
		       env)))))
       (else #f)))))

(define (frame-locals frame)
  (let ((locals (compute-locals (frame-code frame)
				(frame-pc frame)
				(debug-env (frame-debug frame)))))
    (if locals
	(let build-assoc ((locals locals)
			  (env (frame-env frame)))
	  (if (pair? locals)
	      (let ((level (car locals)))
		(if level
		    (append (level->assoc (reverse level) env)
			    (build-assoc (cdr locals)
					 (vector-ref env 0)))
		    (build-assoc (cdr locals) (vector-ref env 0))))
	      '()))
	#f)))

(define (level->assoc level env)
  (let build-level-assoc ((i 1)
			  (level level))
    (if (null? level)
	'()
	(let ((name (car level))
	      (value (vector-ref env i)))
	  (cons (cons name value)
		(build-level-assoc (+ i 1) (cdr level)))))))

(define (cc->frame cc)
  (make-frame (continuation-cont cc)
	      (continuation-env cc)
	      (continuation-template cc)
	      (continuation-pc cc)))

(define (for-each-stack-frame proc)
  (suspend-cc
   (lambda (return)
     (let loop ((frame (cc->frame return)))
       (if (vector? frame)
           (begin
             (proc frame)
             (loop (frame-previous frame))))))))

(define (show-backtrace condition)
  (display "Backtrace :") (newline)
  (suspend-cc
   (lambda (return)
     (let loop ((frame (cc->frame return)))
       (if (vector? frame)
	   (let* ((template (frame-template frame))
		  (debug (template-debug template)))
	     (if debug
		 (let ((name (debug-name debug))
		       (args (frame-arguments frame)))
		   (if args
		       (write (cons (if name
					(string->symbol name)
					'<<no-name-available>>) args))
		       (begin
			 (display "No information for frame: ")
			 (display name)))
		   (newline)
		   (let ((locals (frame-locals frame)))
		     (if locals
			 (if (pair? locals)
			     (begin
			       (display "locals:") (newline)
			       (for-each (lambda (name+val)
					   (display (car name+val))
					   (display " => ")
					   (write (cdr name+val))
					   (newline))
					 locals)))
			 (display "<<no information> for locals>")))
					;(newline)
		   (loop (frame-previous frame))
		   )
		 (begin
		   (display "No debug information for this frame") (newline)
		   (loop (frame-previous frame))))))))))
