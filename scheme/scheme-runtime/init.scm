;;; init.scm -- Initial startup system

(define *daemons* '())

(define *top* #f)

(define-record-type <action>
  (make-action name depends do)
  action?
  (name action-name set-action-name!)
  (depends action-depends set-action-depends!)
  (do action-do set-action-do!))

(define-syntax define-init-action
  (syntax-rules ()
    ((define-init-action ?name (?depends ...) . ?do)
     (let ((?name (lambda () . ?do)))
       (add-init-action! (make-action '?name
                                      (list ?depends ...)
                                      ?name))
       (?name)))))

(define (add-init-action! action)
  (set! *daemons* (cons action *daemons*)))

(define (lookup-action name)
  (letrec ((find-action (lambda (name daemons)
			  (if (null? daemons)
			      #f
			      (if (eq? (action-name (car daemons)) name)
				  (car daemons)
				  (find-action name (cdr daemons)))))))
    (find-action name *daemons*)))

(define (remove-init-action! action)
  (set! *daemons*
        (let loop ((daemons *daemons*))
          (if (null? daemons)
              '()
              (let ((daemon (car daemons)))
                (cond ((and (action? action) (eq? action daemon))
                       (loop (cdr daemons)))
                      ((and (symbol? action) (eq? (action-name daemon) action))
                       (loop (cdr daemons)))
                      (else (cons daemon (loop (cdr daemons))))))))))

(define (init-actions) (append *daemons* '()))

(define (init-action-exists? action)
  (let loop ((daemons *daemons*))
    (if (null? daemons)
        '()
        (let ((daemon (car daemons)))
          (or (and (action? action) (eq? action daemon))
              (and (symbol? action) (eq? (action-name daemon) action))
              (loop (cdr daemons)))))))

;;; Out of memory handling

(define (with-out-of-memory handler proc)
  (if (suspend-cc (lambda (k)
                    (let ((traps (vector-ref k 12)))
                      (vector-set! traps trap/out-of-memory
                                   (cons k (vector-ref traps trap/out-of-memory)))
                      (resume-cc k #f))))
      (handler))
  (proc))

;;; The initial boot process

(define (*boot* args)
  (let ((args (reverse args))
	(error-during-init? #f))
;    (write-channel (make-channel 1) "init conditions")
    (initialize-conditions!)
;    (write-channel (make-channel 1) "init i/o")
    (initialize-i/o!)
;    (write-channel (make-channel 1) "init multiprocessing")
    (start-multiprocessing)
;    (port-write-char (current-output-port) #\c)
;    (write-channel (make-channel 1) "OK")
    (let ((done '()))
      (let loop ((inits *daemons*))
        (if (null? inits)
            'done
            (let ((init (car inits)))
              (if (memq init done)
                  (loop (cdr inits))
                  (let ((depends (action-depends init)))
                    (loop (map lookup-action depends))
		    (condition-case
		     ((action-do init))
		     (<condition>
		      (lambda (c)
			(set! error-during-init? #t))))
                    (set! done (cons init done))
                    (loop (cdr inits))))))))
    (if (and *top* (not error-during-init?))
        (*top* args)
        #f)))

(define (save-image-file filename top)
  (set! *top* top)
  (save-image filename *boot*))
