;;; script.scm -- script support

(define (source-filename filename args)
  (let* ((real-filename (if (symbol? filename)
                            (symbol->string filename)
                            filename))
         (module (make-module 'scheme-user
                              '()
                              '(scheme-runtime scheme-posix)
                              "Scheme R5RS Script File"
                              '()
                              'scheme-user)))
    (bind-ref! 'require
               (lambda (name)
                 (ensure-module-loaded! name)
                 (set-module/open! module
                                   (cons name (module/open module))))
               module)
    (bind-ref! 'open (lambda (name) (set-module/open! module (cons name (module/open module)))) module)

    (dynamic-wind
        (lambda () (bind-module! 'scheme-user module))
        (lambda ()
          (if (file-exists? real-filename)
              (if (file-readable? real-filename)
                  (call-with-input-file real-filename
                    (lambda (port)
                      (let loop ((exp (read port)))
                        (if (eof-object? exp)
                            (let ((ref (lookup-ref 'main 'scheme-user)))
                              (if ref
                                  ((ref/value ref) args)
                                  (error "Script ~a doesn't define a main entry !~%"
					 real-filename)))
                            (let ((val (compile&go exp module)))
                              (loop (read port)))))))
                  (error "File ~a is not readable !~%" real-filename))
              (error "File ~a does not exists !~%" real-filename)))
        (lambda () (unbind-module! 'scheme-user)))))

;; (define-repl-command ss
;;   "Execute a Scheme script"
;;   (let ((filename (read)))
;;     (source-filename filename '())))
