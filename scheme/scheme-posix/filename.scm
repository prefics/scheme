;;; filename.scm -- filename management procedures

;;;
;; predicate indicating if `filename` is a directory.
(define (file-name-directory? filename)
  (let ((index (string-length filename)))
    (if (string=? "" filename)
        #t
        (char=?  #\/ (string-ref filename (- index 1))))))

;;;
;; predicate indicating if `filename` is not a directory
(define (file-name-non-directory? filename)
  (let ((index (string-length filename)))
    (if (string=? "" filename)
        #t
        (not (char=? #\/ (string-ref filename (- index 1)))))))

;;;
;; Return `filename` as a directory name
(define (file-name-as-directory filename)
  (cond ((string=? filename ".") "")
        ((string=? filename "/") filename)
        ((string=? filename "") "/")
        ((file-name-directory? filename) filename)
        (else (string-append filename "/"))))

;;;
;; Return `filename` as it denotes a file
(define (directory-as-file-name filename)
  (cond ((string=? "/" filename) filename)
        ((string=? "" filename) ".")
        ((file-name-directory? filename) 
         (substring filename 0 (- (string-length filename) 1)))
        (else filename)))

;;;
;; Predicate indicating if `filename` is absolute
(define (file-name-absolute? filename)
  (if (string=? filename "")
      #t
      (let ((ch (string-ref filename 0)))
        (or (char=? ch #\~)
            (char=? ch #\/)))))

;;;
;; Return the directory part of `filename`. Note `file-name-directory`
;; must return a directory form, that is a string terminated by '/',
;; unless `filename` does not contain a directory part ('test.txt'
;; for example).
(define (file-name-directory filename)
  (if (file-name-directory? filename)
      filename
      (let loop ((i 0)
                 (last 0))
        (if (< i (string-length filename))
            (if (char=? #\/ (string-ref filename i)) 
                (loop (+ i 1) (+ i 1))
                (loop (+ i 1) last))
            (substring filename 0 last)))))

;;;
;; Return the basename of `filename`, ie without the directory part.
(define (file-name-nondirectory filename)
  (cond ((string=? filename "") "")
        ((string=? filename "/") "/")
        (else
         (let loop ((i (- (string-length filename) 1)))
           (if (> i 0)
               (let ((ch (string-ref filename i)))
                 (if (char=? ch #\/)
                     (substring filename (+ i 1) (string-length filename))
                     (loop (- i 1))))
               filename)))))

;;;
;; Return a list of path element composing `filename`
(define (split-file-name filename)
  (cond ((string=? filename "") '(""))
        ((string=? filename "/") '(""))
        (else (let loop ((i 0)
                         (last 0))
                (if (< i (string-length filename))
                    (if (char=? #\/ (string-ref filename i))
                        (cons (substring filename last i)
                              (loop (+ i 1) (+ i 1)))
                        (loop (+ i 1) last))
                    (list (substring filename last i)))))))

;;;
;; Return the filename represented by the list of path element
;; `path-list`. `dir` specifies the directory prefix for the returned
;; filename.
(define (path-list->file-name path-list . dir)
  (let loop ((paths path-list))
    (if (null? paths)
        ""
        (if (null? (cdr paths))
            (car paths)
            (string-append (car paths)
                           "/"
                           (loop (cdr paths)))))))

;;;
;; Return the extension of `filename`. If `filename` does not have an
;; extension part, the empty string is returned. The extension
;; returned contains the `.` character composing the extension.
(define (file-name-extension filename)
  (let loop ((i (- (string-length filename) 1)))
    (if (>= i 0)
        (if (char=? #\. (string-ref filename i))
            (substring filename i (string-length filename))
            (loop (- i 1)))
        "")))

;;;
;; Return `filename` without its extension part.
(define (file-name-sans-extension filename)
  (let loop ((i (- (string-length filename) 1)))
    (if (>= i 0)
        (if (char=? #\. (string-ref filename i))
            (substring filename 0 i)
            (loop (- i 1)))
        filename)))


;;;
;; Return a list containing three elements. The first is the directory
;; part of `filename`. The second is the base name of `filename`
;; without any extension and the third is the extension of `filename`.
(define (parse-file-name filename)
  (let ((f (file-name-nondirectory filename)))
    (list (file-name-directory filename)
          (file-name-sans-extension filename)
          (file-name-extension filename))))

;;;
;; Calls `thunk` with three arguments. The first denotes the directory
;; part of `filename`. The second denotes the base name of `filename`
;; without extension and the third argument denotes the extension of
;; `filename`.
(define (decode-file-name filename proc)
  (proc (file-name-directory filename)
	(file-name-sans-extension filename)
	(file-name-extension filename)))

;;;
;; Return `filename` replacing the extension part with `extension`.
(define (replace-extension filename extension)
  (string-append (file-name-sans-extension filename) extension))

;;;
;; Return a simplified version of `filename` where spurious `.` and
;; `..` parts removed.
(define (simplify-file-name filename)
  (let loop ((start 0)
             (i 0)
             (paths '()))
    (if (< i (string-length filename))
        (let ((ch (string-ref filename i)))
          (cond ((char=? ch #\/)
                 (let ((component (substring filename start i)))
                   (cond ((string=? component "..")
                          (cond ((null? paths)
				 (loop (+ i 1) (+ i 1) (list "..")))
                                ((and (null? (cdr paths))
                                      (string=? "" (car paths)))
				 #f)
                                (else
				 (if (string=? ".." (car paths))
				     (loop (+ i 1) (+ i 1) (cons ".." paths))
				     (loop (+ i 1) (+ i 1) (cdr paths))))))
                         ((string=? component ".")
                          (loop (+ i 1)
                                (+ i 1)
                                paths))
                         (else
                          (loop (+ i 1) (+ i 1) (cons component paths))))))
                (else (loop start (+ i 1) paths))))
        (let merge ((paths (cons (substring filename start i) paths)))
          (cond ((null? paths) "")
                ((null? (cdr paths)) (car paths))
                (else (string-append (merge (cdr paths)) "/" (car paths))))))))

(define (resolve-file-name filename . dir)
  (let ((size (string-length filename)))
    (if (> size 0)
        (let ((ch (string-ref filename 0)))
          (if (char=? ch #\~)
              filename
              filename))
        filename)))

(define (expand-file-name filename . dir)
  (let ((len (string-length filename)))
    (if (= len 0)
        filename
        (let ((first-char (string-ref filename 0)))
          (if (char=? first-char #\~)
              (expand-twidle filename)
              filename)))))

(define (expand-twidle filename)
  (if (and (> (string-length filename) 1)
           (char=? (string-ref filename 1) #\/))
      (string-append (home-dir)
                     (substring filename 2 (string-length filename)))
      (let ((p (or (string-pos filename #\/)
                   (string-length filename))))
        (string-append (home-dir (substring filename 1 p))
                       (substring filename p (string-length filename))))))

(define (string-pos str ch)
  (let find ((i 0))
    (if (< i (string-length str))
        (if (char=? ch (string-ref str i))
            i
            (find (+ i 1)))
        #f)))

;;;
;; Returns an absolute filename for `filename` within `dir`. `dir`
;; defaults to the current working directory.
(define (absolute-file-name filename . dir)
  (let ((dir (if (null? dir)
                 (cwd)
                 (car dir))))
    (cond ((file-name-absolute? filename)
	   filename)
	  ((file-name-absolute? dir)
	   (string-append (file-name-as-directory dir)
			  filename))
	  (else (string-append (file-name-as-directory (cwd))
			       (file-name-as-directory dir)
			       filename)))))
                      
;;;
;; Return the home directory of `user`.    
(define (home-dir . user)
  (if (null? user)
      (user-home-dir)
      (user-info-home-dir (user-info (car user)))))

;;;
;; Return a filename pointing to the file `filename` residing in the
;; `user` home directory.
(define (home-file user filename)
  (string-append (file-name-as-directory (home-dir user)) 
                 "/" 
                 filename))
