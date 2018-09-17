;;; io.scm -- input/output functions

(define $mode/truncate 1)
(define $mode/append 2)

(define-record-type :port
  (really-make-port status direction peek channel)
  port?
  (status    port/status    set-port/status!)
  (direction port/direction set-port/direction!)
  (peek      port/peek      set-port/peek!)
  (channel   port/channel   set-port/channel!))

(define (make-input-port channel)
  (really-make-port ':open ':input #f channel))

(define (make-output-port channel)
  (really-make-port ':open ':output #f channel))

(define (input-port? obj)
  (and (port? obj)
       (eq? ':input (port/direction obj))))

(define (output-port? obj)
  (and (port? obj)
       (eq? ':output (port/direction obj))))

(define (call-with-input-file string proc)
  (let ((input-port (make-input-port (open-input-channel string))))
    (proc input-port)))

(define (call-with-output-file string proc)
  (let ((output-port (make-output-port (open-output-channel string $mode/truncate))))
    (proc output-port)))
  
(define (with-input-from-file string proc)
  (let* ((input-port (make-input-port (open-input-channel string)))
         (ret (let-fluid $current-input-port$ input-port proc)))
    (close-input-port input-port)
    ret))

(define (with-output-to-file string proc)
  (let* ((output-port (make-output-port (open-output-channel string $mode/truncate))))
    (let-fluid $current-output-port$ output-port
               proc)))

(define (with-output-to-port port proc)
  (let-fluid $current-output-port$ port proc))

(define (with-input-from-port port proc)
  (let-fluid $current-input-port$ port proc))
                              
(define (open-input-file filename)
  (make-input-port (open-input-channel filename)))

(define (open-output-file filename)
  (make-output-port (open-output-channel filename $mode/truncate)))

(define (close-input-port port)
  (close-channel (port/channel port)))

(define (close-output-port port)
  (close-channel (port/channel port)))

(define (channel->input-port channel)
  (really-make-port ':open ':input #f channel))

(define (channel->output-port channel)
  (really-make-port ':open ':output #f channel))

(define (port->channel port)
  (port/channel port))

;;; ASCII encoding

(define (ascii->char n) (integer->char n))
(define (char->ascii ch) (char->integer ch))

;; INPUT

(define-record-type (<reader-error> <error>)
  (make-reader-error message)
  reader-error?)

(define (read-error! reason)
  (signal (make-reader-error reason)))

(define (optional-input-port port)
  (if (null? port)
      (current-input-port)
      (car port)))

(define (skip-spaces port)
  ;; advances port up to a non space character 
  (let ((c (peek-char port)))
    (if (char-whitespace? c)
        (begin 
          (read-char port) 
          (skip-spaces port))
        c)))

(define (symbol-initial? c)
  (or (memq c '(#\! #\$ #\% #\& #\* #\+ #\- #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
      (char-alphabetic? c)))

(define (symbol-subsequent? c)
  (or (symbol-initial? c)
      (char-numeric? c)
      (memq c '(#\+ #\- #\. #\@))))

(define (read . port)
  (let ((real-port (optional-input-port port)))
    (let ((c (skip-spaces real-port)))
      (cond ((char=? c #\;)      (read-comment real-port)) 
            ((char=? c #\#)      (read-hash real-port))
	    ((char-numeric? c)   (read-number real-port))
	    ((char=? #\" c)      (read-string real-port)) 
	    ((char=? #\( c)      (read-char real-port) (read-list real-port))
	    ((symbol-initial? c) (read-symbol/keyword real-port))
	    ((char=? c #\')      (read-quote real-port))
	    ((char=? c #\`)      (read-backquote real-port))
            ((char=? c #\,)      (read-comma real-port))
            ((char=? c #\.)      (read-dotdotdot real-port))
	    ((eof-object? c)     c)
	    (else (read-error! (string-append "Unrecognized start of token: " 
					      (make-string 1 c)
					      (number->string (char->integer c)))))))))

(define (read-comment port)
  (let loop ((ch (read-char port)))
    (if (char=? ch #\newline)
        (read port)
        (loop (read-char port)))))

(define (read-dotdotdot port)
  (read-char port)
  (let* ((c2 (peek-char port)))
    (if (char=? c2 #\.)
	(let* ((c2 (read-char port))
	       (c3 (peek-char port)))
	  (if (char=? c3 #\.)
	      (begin (read-char port)
		     (string->symbol "..."))
	      (read-error! "Undefined .. token")))
	(string->symbol "."))))
	
(define (read-quote port)
  (read-char port)
  (let ((next (read port)))
    (list 'quote next)))

(define (read-backquote port)
  (read-char port)
  (let ((next (read port)))
    (list 'quasiquote next)))

(define (read-comma port)
  (read-char port)
  (let ((c (peek-char port)))
    (if (char=? c #\@)
        (begin 
	  (read-char port)
	  (let ((next (read port)))
	    (list 'unquote-splicing next)))
	(let ((next (read port)))
	  (list 'unquote next)))))

(define (read-hash port)
  (read-char port)
  (let ((c (read-char port)))
    (cond ((char=? c #\\)
	   (read-character port))
          ((char=? c #\()                
	   (read-vector port))
          ((char=? c #\f) #f)
          ((char=? c #\t) #t)
          ((char=? c #\e) (read-number port))
          ((char=? c #\i) (read-number port))
	  ((char=? c #\!) (read-comment port))
          (else (read-error! "Unknown # syntax for token")))))

(define (read-word port)
  (list->string
   (let loop ((c (peek-char port)))
     (if (char-alphabetic? c)
         (begin (read-char port)
                (cons c (loop (peek-char port))))
         '()))))

(define (read-character port)
  (let ((c (peek-char port)))
    (if (char-alphabetic? c)        
        (let ((s (read-word port)))
          (cond ((string=? s "newline") (integer->char 10))
                ((string=? s "space")  (integer->char 32))
                ((string=? s "tab") (integer->char 9))
                ((string=? s "linefeed") (integer->char 10))
                ((string=? s "formfeed") (integer->char 12))
                ((string=? s "return") (integer->char 13))
                (else
                 (if (= 1 (string-length s))
                     (string-ref s 0)
                     (read-error! "Unknown character constant")))))
        (read-char port))))

(define (read-vector port)
  (list->vector
    (let loop ((c (skip-spaces port)))
      (if (char=? c #\))
	  (begin 
            (read-char port) 
            '())
          (let ((head (read port)))
            (cons head (loop (skip-spaces port))))))))

(define (read-number port)
  (let ((digit->number (lambda (c) (- (char->integer c) (char->integer #\0)))))
    (let loop ((c (peek-char port))
	       (n 0))
      (if (char-numeric? c)
	  (let ((c (read-char port)))
	    (loop (peek-char port) (+ (* 10 n) (digit->number c))))
          n))))

(define (read-string port)
  (read-char port)
  (list->string
    (let loop ((c (read-char port)))
      (if (char=? c #\")
	  '()
	  (if (char=? c #\\)
	      (let ((c2 (read-char port)))
		(cond ((char=? c2 #\n) (cons #\newline (loop (read-char port))))
		      ((char=? c2 #\r) (cons (integer->char 13)
					     (loop (read-char port))))
		      ((char=? c2 #\e) (cons (integer->char 27)
					     (loop (read-char port))))
		      ((char=? c2 #\t) (cons (integer->char 9)
					     (loop (read-char port))))
		      ((char=? c2 #\\) (cons #\\ (loop (read-char port))))
		      ((char=? c2 #\") (cons #\" (loop (read-char port))))
		      (else (cons c2 (loop (read-char port))))))
	      (cons c (loop (read-char port))))))))

(define $dot (string->symbol "."))

(define (read-list port)
  (let ((c (skip-spaces port)))
    (if (char=? c #\)) 
        (begin (read-char port) '())
        (let ((token (read port)))
          (if (eq? token $dot)
              (begin 
                (read-char port)
                (let ((end (read port))
                      (c (skip-spaces port)))
                  (if (char=? c #\))
                      (begin
                        (read-char port) 
                        end)
                      (read-error! "expected )"))))
              (let ((tail (read-list port)))
                (cons token tail)))))))

; (define (read-list port)
;   (let ((c (skip-spaces port)))
;     (cond ((char=? c #\.)
;            (read-char port)
; 	   (let ((end (read port)))
; 	     (let ((c (skip-spaces port)))
; 	       (if (char=? c #\))
; 		   (let ((c (read-char port)))
;                      end)
; 		   (display "read-list: wanted a )")))))
;           ((char=? c #\))
;            (read-char port)
; 	   '())
; 	  (else 
;            (let ((head (read port)))
;              (cons head (read-list port)))))))
         
(define (read-symbol/keyword port)
  (let ((str
	 (list->string
	  (let loop ((c (peek-char port)))
	    (cond ((symbol-subsequent? c)
		   (read-char port)
		   (cons c (loop (peek-char port))))
		  ((memq c '(#\+ #\- #\. #\@))
		   (read-char port)
		   (cons c (loop (peek-char port))))
		  (else '()))))))
    (if (char=? #\: (string-ref str (- (string-length str) 1)))
	(string->keyword (substring str 0 (- (string-length str) 1)))
	(string->symbol str))))

(define (read-char . port)
  (let* ((p (optional-input-port port))
         (peek (port/peek p)))
    (set-port/peek! p #f)
    (if peek
        peek
        (read-channel (port/channel p)))))

(define (peek-char . port)
  (let* ((p (optional-input-port port))
         (peek (port/peek p)))
    (if peek
        peek
        (let ((peek2 (read-channel (port/channel p))))
          (set-port/peek! p peek2)
          peek2))))

(define $eof 'the-end-of-file-object)

(define (char-ready? . port)
  #t)
;  (%char-ready (input-port-n (optional-input-port port))))

;; OUTPUT

(define (optional-output-port port)
  (if (null? port)
      (current-output-port)
      (car port)))

(define (write obj . port)
  (let ((real-port (optional-output-port port)))
    (cond ((number? obj)  (write-number obj real-port))
          ((boolean? obj) (write-boolean obj real-port))
          ((symbol? obj)  (write-symbol obj real-port))
          ((vector? obj)  (write-vector obj real-port))
          ((pair? obj)    (write-pair obj real-port))
          ((char? obj)    (write-char obj real-port))
          ((string? obj)  (write-string obj real-port))
          (else (write-string "#{unprintable object}" real-port)))))

(define (write-number number port)
  (let ((digit->char (lambda (n) (integer->char (+ n (char->integer #\0))))))
    (if (< number 0)
        (begin (write-char #\- port) (write-number (- 0 number) port))
        (if (< number 10)
	    (write-char (digit->char number) port)
	    (begin (write-number (quotient number 10) port)
	           (write-number (modulo number 10) port))))))

(define (write-boolean bool port)
  (write-string (if bool "#t" "#f") port))

(define (write-symbol symbol port)
  (write-char #\' port)
  (write-string (symbol->string symbol) port))

(define (write-vector vector port)
  (write-string "#(")
  (let loop ((i 0))
    (if (< i (vector-length vector))
        (begin 
	  (write (vector-ref vector i) port) (write-char #\space port)
	  (loop (+ i 1)))))
  (write-char #\) port))

(define (write-pair pair port)
  (write-char #\()
  (let loop ((p pair))
    (if (not (null? p))
      (if (pair? p)
          (begin (write (car p) port)
	    (write-char #\space port)
	    (loop (cdr p)))
	  (begin (write-string ". " port) (write p port)))))
  (write-char #\) port))

(define (write-string string port)
  (write-char #\" port)
  (let loop ((i 0))
    (if (< i (string-length string))
        (begin 
          (write-char (string-ref string i) port)
          (loop (+ i 1)))))
  (write-char #\"))

(define (display-vector vec)
  (display "#(")
  (let loop ((i 0))
    (if (< i (vector-length vec))
	(begin 
	  (display (vector-ref vec i))
	  (if (< i (- (vector-length vec) 1))
	      (display " "))
	  (loop (+ i 1)))))
  (display ")"))

(define (display-bvec bvec)
  (display "#[")
  (let loop ((i 0))
    (if (< i (bvec-length bvec))
	(begin 
	  (display (bvec-ref bvec i))
	  (if (< i (- (bvec-length bvec) 1))
	      (display " "))
	  (loop (+ i 1)))))
  (display "]"))
  
(define (display-procedure proc)
  (display "#{procedure}"))

(define (display-stob stob)
  (display "#{record ")
  (display (stob-ref (stob-class stob) 0))
  (display "}"))

(define (display obj . port)
  (let ((port (optional-output-port port)))
    (cond ((string? obj)
           (let loop ((i 0))
             (if (< i (string-length obj))
                 (begin 
                   (write-char (string-ref obj i) port)
                   (loop (+ i 1))))))           
          ((char? obj)
           (write-char obj port))
          ((number? obj)
           (display (number->string obj) port))
	  ((pair? obj)
	   (write-channel (make-channel 1) obj 0 0))
	  ((null? obj)
	   (write-string "'()" port))
          ((symbol? obj)
           (display (symbol->string obj) port))
	  ((keyword? obj)
	   (display (keyword->string obj) port)
	   (display ":" port))
	  ((boolean? obj)
	   (if obj (display "#t") (display "#f")))
	  ((channel? obj)
	   (display "#{channel ") (display (channel-number obj))
	   (display "}"))
	  ((vector? obj) (display-vector obj))
	  ((bvec? obj) (display-bvec obj))
	  ((procedure? obj) (display-procedure obj))
	  ((stob? obj) (display-stob obj))
	  (else (write-channel (make-channel 1) obj 0 0)))))

(define (newline . port)
  (let ((fd (optional-output-port port)))
    (write-char #\newline fd)))

(define (write-char char . port/opt)
  (let ((port (optional-output-port port/opt)))
    (write-channel (port/channel port) char 0 0)))

(define (load filename)
  (display "'load' function not implemented"))

(define (transcript-on filename)
  (display "'transcript-on' function not implemented"))

(define (transcript-off filename)
  (display "'transcript-off' function not implemented"))


;(write-channel (make-channel 1) "h")         
(define $current-input-port$ (make-fluid (make-input-port (make-channel 0))))
(define (current-input-port)
  (fluid $current-input-port$))
;(write-channel (make-channel 1) "h")
(define $current-output-port$ (make-fluid (make-output-port (make-channel 1))))
(define (current-output-port)
  (fluid $current-output-port$))
;(write-channel (make-channel 1) "h")

(define (initialize-i/o!)
  (set-fluid! $current-input-port$ (make-input-port (make-channel 0)))
  (set-fluid! $current-output-port$ (make-output-port (make-channel 1))))

