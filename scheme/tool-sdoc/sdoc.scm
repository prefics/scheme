;;; -*- library: sdoc-core ; coding: iso-8859-1 -*-

;;; 
;; = SDoc
;;
;; SDoc is a source code document system for the Scheme programming
;; language.

;;;
;; A file is parsed into either a comment-block or a code-block. A
;; comment-block contains the list of profils (i.e names that are
;; documented) and a documentation (the stuff contained in the
;; comment)

;; Here is the comment block abstract data type
(define (make-comment-block doc) (list 'comment #f doc))
(define (comment-block? block) (and (pair? block) (eq? 'comment (car block))))
(define comment-block-profils cadr)
(define comment-block-doc caddr)
(define (add-comment-profil! comment profil)
  (set-car! (cdr comment) (cons profil (or (cadr comment) '()))))

;; Here is the code block abstract data type
(define (make-code-block exp) (list 'code exp))
(define (code-block? block) (and (pair? block) (eq? 'code (car block))))
(define code-block-exp cadr)

;;;
;; Read from `port` as long as blanks are read and return the first
;; non blank character.
;;
(define (skip-blanks port)
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch) ch)
          ((char-whitespace? ch) (read-char port) (skip-blanks port))
          (else ch))))

;;;
;; Read (meaning parsing) of a source from `port` and return the
;; parsed representation
(define (read-file port) (read-file-with-last-comment port #f))

(define (read-file-with-last-comment port current-comment-block)
  (let ((ch (skip-blanks port)))
    (cond
      ((eof-object? ch) '())
      ((char=? ch #\;)
       (let* ((comment (read-comment-block port))
	      (block (make-comment-block comment)))
         (cons block (read-file-with-last-comment port block))))
      (else 
       (let* ((exp (read port))
	      (profil (exp->profil exp)))
	 (if (and profil current-comment-block)
	     (add-comment-profil! current-comment-block profil))
	 (cons (make-code-block exp) (read-file port)))))))

(define (exp->profil exp)
  (if (pair? exp)
      (let ((head (car exp)))
	(if (eq? head 'define)
	    (if (pair? (cadr exp))
		(cadr exp)
		(if (and (pair? (caddr exp))
			 (eq? 'lambda (car (caddr exp))))
		    (cons (cadr exp) (cdr (caddr exp)))
		    (cadr exp)))
	    #f))
      #f))
	      
;; Read a comment block
(define (read-comment-block port)
  (let ((line (read-line port)))
    (let ((ch (peek-char port)))
      (cond ((eof-object? ch) line)
            ((char=? #\; ch)
	     (string-append line "\n" (read-comment-block port)))
            (else line)))))

;;;
;; == Processing of a parsed file

;;;
;; Process a representation, that is the representation of a file or 
;; parse file.
(define (process-result representation)
  (for-each process-one representation))

(define (process-one representation)
  (cond
   ((code-block? representation) (process-code representation))
   ((comment-block? representation) (process-comment representation))
   (else (error "Unknown parsed representation of file ~a" representation))))

(define (correct-file-name name)
  (let ((current-file-name (the-file-name)))
    (string-append (file-name-nondirectory current-file-name)
		   name)))

(define (process-code code)
  (if (and (pair? code)
	   (eq? (car code) 'define-structure))
      (for-each (lambda (directive)
		  (if (and (pair? directive)
			   (eq? 'files (car directive)))
		      (add-files-to-queue! (map correct-file-name (cdr directive)))))
		(cdr code))))

(define (process-comment comment)
  (let ((text (comment-block-doc comment)))
    (if (documentation-comment? text)
	(render-comment-block comment))))

(define (documentation-comment? comment)
  (let ((first-line (with-input-from-string comment read-line)))
    (let lp ((i 0))
      (if (< i (string-length first-line))
	  (let ((ch (string-ref first-line i)))
	    (and (or (char-whitespace? ch)
		     (char=? ch #\;))
		 (lp (+ i 1))))
	  #t))))
		
(define (strip-comment-char comment)
  (with-input-from-string comment
    (lambda ()
      (with-output-to-string 
        (lambda ()
          (let strip ((line (read-line)))
            (if (eof-object? line)
                'ok
                (begin 
                  (display (substring line 
				      (find-start-of-non-comment line) 
				      (string-length line)))
                  (newline)
                  (strip (read-line))))))))))

(define (find-start-of-non-comment string)
  (let lp ((i 0))
    (if (< i (string-length string))
        (let ((ch (string-ref string i)))
          (if (char=? ch #\;)
              (lp (+ i 1))
              (if (char-whitespace? ch)
                  (+ i 1)
                  i)))
        i)))

;;;
;; == Rendering components
;;
;; Rendering of comment blocks and code blocks. For now only comment
;; blocks are kept in the generated document. Code blocks could be
;; used to as source viewing inside the produced documentation. Keep
;; it on the todo list for now.

(define (write-safe-html string)
  (let lp ((i 0))
    (if (< i (string-length string))
	(let ((ch (string-ref string i)))
	  (cond ((char=? ch #\<) (display "&lt;"))
		((char=? ch #\>) (display "&gt;"))
		((char=? ch #\&) (display "&amp;"))
		(else (display ch)))
	  (lp (+ i 1))))))

;; Rendering of profils
(define (render-profils profils)
  (display "<div class=\"profils\">\n")
  (for-each (lambda (p)
	      (write-safe-html (with-output-to-string
				 (lambda () (write p)))))
	    profils)
  (display "</div>\n"))

;; Rendering of comment blocks
(define (render-comment-block block)
  (display "<div class=\"doc\">\n")
  (let ((profils (comment-block-profils block)))
    (if profils (render-profils profils))
    (let* ((text (comment-block-doc block))
	   (markup (strip-comment-char text)))
      (markup->html (read-markup-from-string markup))
      (display "</div>\n"))))

;; Rendering of parsed document
(define (render-document representation)
  (display "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" >\n<head>\n  <title>Scheme Documentation</title>\n  <style>\n  body { font-size: small ; font-family: Arial, Helvetica, sans-serif; line-height: 120% }\n  .profils { background: #eef; font-family: \"Courier New\", Courier, monospace ; padding-top: 2px; padding-bottom: 2px ; margin: 0px ; border-top: 1px solid #ccf }\n  .doc h1 { font-size: 160% }\n  .doc h2 { font-size: 130% }\n  .doc p { margin-top: 10px }\n  .doc { line-height: 1.2em ; font-size: 12px ; font-family: Arial, Helvetica, sans-serif}\n  .doc tt { background: #eee; border: 1px solid #ddd ; font-size: 110%; padding: 1px }\n  </style>\n</head>\n<body>\n")
  (process-result representation)
  (display "\n</body>\n</html>\n"))

;;;
;; == Top level processing of files
;;
;; Top level interface for generating documentation is done by the
;; `sdoc-file` procedure.

(define $file$ (make-fluid #f))

(define (the-file-name)
  (or (fluid $file$)
      (error "No current file name set")))

(define (with-file-name name proc)
  (let-fluid $file$ name proc))

(define $output-directory$ (make-fluid "./"))
(define (the-output-directory) (fluid $output-directory$))
(define (with-output-directory name thunk)
  (let-fluid $output-directory$ name thunk))

(define (correct-output-file name)
  (string-append (the-output-directory)
		 (replace-extension (file-name-nondirectory name) 
				    ".html")))

(define (sdoc-file file-name)
  (with-file-name file-name
    (lambda ()
      (let ((repr (call-with-input-file file-name 
		    (lambda (port) (read-file port)))))
	(with-output-to-file (correct-output-file file-name)
	  (lambda ()
	    (render-document repr)))))))

(define *file-queue* '())
(define (add-files-to-queue! files)
  (set! *file-queue* (append *file-queue* files)))
(define (file-queue-empty?)
  (null? *file-queue*))
(define (pop-file-from-queue!)
  (let ((front (car *file-queue*)))
    (set! *file-queue* (cdr *file-queue*))
    front))

(define (process-file-queue)
  (if (not (file-queue-empty?))
      (let ((file (pop-file-from-queue!)))
	(sdoc-file file)
	(process-file-queue))))
;; Main entry point the script
(define (main args)
  (let lp ((args args))
    (if (not (null? args))
	(let ((arg (car args)))
	  (if (string=? arg "-o")
	      (with-output-directory (cadr args)
				     (lambda ()
				       (lp (cddr args))))
	      (begin 
		(sdoc-file arg)
		(lp (cdr args))))))))

