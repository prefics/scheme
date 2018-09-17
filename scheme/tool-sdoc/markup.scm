 ;;; markup.scm -- Markup processing

;; test =a= 

;; = Hello world =

;; This is a list:

;; # print file
;; # print stuff

;; end

;;  (define (print-name))

;; [toto Word name]

(define (read-markup-from-string string)
  (with-input-from-string string
    (lambda () (read-markup (current-input-port)))))

(define (read-markup-from-file file-name)
  (with-input-from-file file-name
    (lambda () (read-markup (current-input-port)))))

(define (read-markup port)
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch) '())
	  ((char=? ch #\=) 
	   (let* ((section (read-section port))
		  (rest (read-markup port)))
	     (cons section rest)))
	  ((char=? ch #\|)
	   (let* ((table (read-table port))
		  (rest (read-markup port)))
	     (cons table rest)))
	  ((char=? ch #\#)
	   (let* ((enum (read-enum-sharp port))
		  (rest (read-markup port)))
	     (cons enum rest)))	   
	  ((char=? ch #\*)
	   (let* ((enum (read-enum-star port))
		  (rest (read-markup port)))
	     (cons enum rest)))
	  ((char=? ch #\space)
	   (let* ((enum (read-preformatted port))
		  (rest (read-markup port)))
	     (cons enum rest)))
	  ((char=? ch #\newline)
	   (read-char port)
	   (read-markup port))
	  (else
	   (let* ((paragraph (read-paragraph port))
		  (rest (read-markup port)))
	     (cons paragraph rest))))))

;(with-input-from-string "|abc|def\n|ghi|jkl" (lambda () (read-table (current-input-port))))
(define (read-table port)
  (cons 'table
	(let loop ((ch (peek-char port)))
	  (cond ((eof-object? ch) '())
		((char=? ch #\|)
		 (let ((line (read-table-line port)))
		   (cons line (loop (peek-char port)))))
		(else '())))))

(define (read-table-line port)
  (let loop ((ch (peek-char port)))
    (cond ((eof-object? ch) '())
	  ((char=? ch #\newline) (read-char port) '())
	  ((char=? ch #\|)
	   (read-char port)
	   (let ((text (read-table-line-entry port)))
	     (cons text (loop (peek-char port))))))))
	  
(define (read-table-line-entry port)
  (list->string
   (let loop ((ch (peek-char port)))
     (cond ((eof-object? ch) '())
	   ((char=? ch #\|) '())
	   ((char=? ch #\newline) '())
	   (else 
	    (let ((ch (read-char port)))
	      (cons ch (loop (peek-char port)))))))))

(define (read-enum-sharp port) (list 'enum))

(define (read-enum-star port) (list 'enum))

(define (read-preformatted port)
  (cons 'pre
	(let loop ((ch (peek-char port)))
	  (cond ((eof-object? ch) '())
		((char=? ch #\space)
		 (read-char port)
		 (let ((line (read-line port)))
		   (cons line (loop (peek-char port)))))
		(else '())))))

(define (read-section port)
  (let* ((level (string-length (read-= port)))
	 (text (read-paragraph port)))
    (list 'section level text)))

(define (read-= port)
  (let ((ch (peek-char port)))
    (cond 
      ((eof-object? ch) "")
      ((char=? ch #\=)
       (let ((ch (read-char port)))
         (string-append (string ch) (read-= port))))
      (else (read-char port) ""))))

;(with-input-from-string "toto.jpg =he*ll*o= hello world _a_ bb hello =a=" (lambda () (read-paragraph (current-input-port))))
;(with-input-from-string "hello\nworld" (lambda () (read-paragraph (current-input-port))))
;(with-input-from-string "hello\n\nworld" (lambda () (read-paragraph (current-input-port))))
(define (read-paragraph port)
  (let loop ((ch (peek-char port))
	     (text '())
	     (end #f))
    (cond ((eof-object? ch) 
	   (if (zero? (length text))
	       '()
	       (list (list->string (reverse text)))))
	  ((and end (char=? ch end))
	   (read-char port)
	   (list (list->string (reverse text))))
	  ((char=? ch #\_)
	   (read-char port)
	   (let ((italic (loop (peek-char port) '() #\_))
		 (rest (loop (peek-char port) '() end)))
	     (cons (list->string (reverse text))
		   (cons (cons 'italic italic)
			 rest))))
	  ((char=? ch #\*)
	   (read-char port)
	   (let ((bold (loop (peek-char port) '() #\*))
		 (rest (loop (peek-char port) '() end)))
	     (cons (list->string (reverse text))
		   (cons (cons 'bold bold) rest))))
	  ((char=? ch #\`)
	   (read-char port)
	   (let ((fixed (loop (peek-char port) '() #\`))
		 (rest (loop (peek-char port) '() end)))
	     (cons (list->string (reverse text))
		   (cons (cons 'fixed fixed) rest))))
	  ((char=? ch #\newline)
	   (read-char port)
	   (let ((ch (peek-char port)))
	     (cond ((eof-object? ch) 
		    (if (zero? (length text))
			'()
			(list (list->string (reverse text)))))
		   ((char=? ch #\newline)
		    (read-char port)
		    (if (zero? (length text))
			'()
			(list (list->string (reverse text)))))
		   (else (loop ch (cons #\space text) end)))))
	  (else 
	   (let ((ch (read-char port)))
	     (loop (peek-char port) (cons ch text) end))))))
      
;(with-input-from-string "[toto.jpg hello world]" (lambda () (read-link (current-input-port))))
(define (read-link port)
  (read-char port)
  (let* ((token (read-token port))
	 (descr (read-description port)))
    (list 'link token descr)))

;(with-input-from-string "hello world" (lambda () (read-token (current-input-port))))
(define (read-token port)
  (list->string
   (let loop ((ch (peek-char port)))
	(cond ((eof-object? ch) '())
	      ((char-whitespace? ch) '())
	      (else
	       (let* ((ch (read-char port)))
		 (cons ch (loop (peek-char port)))))))))

;(with-input-from-string "_hello world_]" (lambda () (read-description (current-input-port))))
(define (read-description port)
  (list->string
   (let loop ((ch (peek-char port)))
	(cond ((eof-object? ch) '())
	      ((char=? #\] ch) '())
	      (else
	       (let* ((ch (read-char port)))
		 (cons ch (loop (peek-char port)))))))))

;(read-markup "hello =fixed= _italic_ *bold*\n\n| header 1 | header 2\n| suisse | CH\n (def\n   (inline)\n\n")
;(read-markup-from-string "= hello")
;(read-markup-from-string "hello world")
;(read-markup-from-string "hello world\nhow are you?")
'markup-loaded

(define (markup->html markup)
  (for-each write-html-paragraph markup))

(define (write-html-paragraph paragraph)
  (cond ((and (pair? paragraph) (eq? (car paragraph) 'pre))
	 (display "<tt>\n")
	 (write-html-text (cdr paragraph))
	 (display "\n</tt>")
	 (newline))
	((and (pair? paragraph) (eq? (car paragraph) 'section))
	 (let ((num (cadr paragraph)))
	   (display "<h") (display num) (display ">")
	   (write-html-text (caddr paragraph))
	   (display "</h") (display num) (display ">")
	   (newline)))
	(else
	 (display "<p>\n")
	 (write-html-text paragraph)
	 (display "\n</p>")
	 (newline))))

(define (write-html-text paragraph)
  (for-each write-html-span paragraph))

(define (write-html-span span)
  (cond ((string? span) (display span))
	((and (pair? span) (eq? (car span) 'bold))
	 (display "<b>")
	 (for-each write-html-span (cdr span))
	 (display "</b>"))
	((and (pair? span) (eq? (car span) 'italic))
	 (display "<i>")
	 (for-each write-html-span (cdr span))
	 (display "</i>"))
	((and (pair? span) (eq? (car span) 'fixed))
	 (display "<tt>")
	 (for-each write-html-span (cdr span))
	 (display "</tt>"))
	(else (error "unknown span ~a" span))))