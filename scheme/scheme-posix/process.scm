;;; process.scm -- process manipulation procedure

(define-syntax run/port
  (syntax-rules ()
    ((run/port . ?epf?) (run/port* (lambda () (exec-epf . ?epf?))))))

(define-syntax run/file
  (syntax-rules ()
    ((run/file . ?epf?) (run/file* (lambda () (exec-epf . ?epf?))))))

(define-syntax run/string
  (syntax-rules ()
    ((run/string . ?epf?) (run/string* (lambda () (exec-epf . ?epf?))))))

(define-syntax run/strings
  (syntax-rules ()
    ((run/strings . ?epf?) (run/strings* (lambda () (exec-epf . ?epf?))))))

(define-syntax run/sexp
  (syntax-rules ()
    ((run/sexp . ?epf?) (run/sexp* (lambda () (exec-epf . ?epf?))))))

(define-syntax run/sexps
  (syntax-rules ()
    ((run/sexps . ?epf?) (run/sexps* (lambda () (exec-epf . ?epf?))))))

(define (port-dup ch1 ch2)
  (let retry ((rc (posix-dup2 ch1 ch2)))
    (cond (rc rc)
	  ((fixnum? rc) (error "dup2 errno " rc))
	  ((not rc) (retry rc)))))
       
(define (run/port* proc)
  (let ((channels (posix-pipe)))
    (if channels
        (let* ((in (car channels))
               (out (cdr channels))
               (process (fork
                         (lambda ()
                           (close-channel in)
			   (port-dup out (make-channel 1))
			   (proc)))))
          (close-channel out)
          (wait process)
          (channel->input-port in))
        (posix-error "cannot call pipe"))))

(define (run/file* proc)    (run/port* proc))
(define (run/string* proc)  (port->string (run/port* proc)))
(define (run/strings* proc) (port->string-list (run/port* proc)))
(define (run/sexp* proc)    (read-and-close (run/port* proc)))
(define (run/sexps* proc)   (port->sexp-list (run/port* proc)))

(define (stringify obj)
  (cond ((string? obj) obj)
        ((number? obj) (number->string obj))
        ((symbol? obj) (symbol->string obj))
        ((char? obj) (make-string 1 obj))
        (else (error "don't know how to stringify ~a" obj))))

(define (port->string port) (list->string (port->list read-char port)))

(define (port->string-list port) (port->list read-line port))

(define (port->sexp-list port) (port->list read port))

(define (port->list reader port)
  (reverse
   (let loop ((obj (reader port))
              (lst '()))
     (if (eof-object? obj)
         (begin (port-close port)
                lst)
         (loop (reader port) (cons obj lst))))))

(define (read-and-close port)
  (let ((value (read port)))
    (port-close port)
    value))

(define (stringify-list lst)
  (map stringify lst))

(define (redirection lst)
  (let ((redir
	 (lambda (r)
	   (if (pair? r)
	       (let ((op (car r)))
		 (cond ((eq? op '<)
			(let* ((len (length r))
			       (fd (if (= len 3) (cadr r) 0))
			       (name (if (= len 3) (caddr r) (cadr r)))
			       (channel (open-input-channel name)))
			  (posix-dup2 channel (make-channel fd))))
		       ((eq? op '>)
			(let* ((len (length r))
			       (fd (if (= len 3) (cadr r) 1))
			       (name (if (= len 3) (caddr r) (cadr r)))
			       (channel (open-output-channel name 1)))
			  (posix-dup2 channel (make-channel fd))))
		       ;; @TODO implement those two redirection
		       ((eq? op '<<) #f)
		       ((eq? op '>>) #f)
		       ((eq? op '=)
			(let ((fd1 (cadr r))
			      (fd2 (caddr r)))
			  (posix-dup2 (make-channel fd1)
				      (make-channel fd2))))
		       ((eq? op '-) (close-channel (cadr r)))
		       (else (error "Unknown redirection ~a" r))))))))
    (for-each redir lst)))

;;; == Process forms
;;
;; Process forms is SEXP notation for runner programs.
;; 
;; (prog ...)

;;;
;; Launch a process form. Note the current Scheme process is replaced with the new
;; executable. In Unix parlance, the `exec` system call is made with a new executable.
(define-syntax exec-epf
  (syntax-rules (begin pipe epf)
    ((exec-epf (begin . ?exps) ?redir ...) (begin (redirection `(?redir ...) . ?exps)))
    ((exec-epf (pipe ?exps ?redir ...)) (begin (redirection `(?redir ...) . ?exps)))
    ((exec-epf (epf . ?epf?)) (exec-epf . ?epf?))
    ((exec-epf (?prog ?args ...) ?redir ...)
     (begin (redirection `(?redir ...) (apply exec-path (cons '?prog `(?args ...))))))
    ((exec-epf ?prog ?redir ...)
     (begin (redirection `(?redir ...) (exec-path '?prog))))))

;;;
;; Starts a process defined as process form in background.
(define-syntax &
  (syntax-rules ()
    ((& . ?epf?) (fork (lambda () (exec-epf . ?epf?))))))

;;;
;; Starts a process defined as process form synchronously and return the process object
(define-syntax run (syntax-rules () ((run . ?epf?) (wait (& . ?epf?)))))

(define $path-separator #\:)

(define *path* #f)

(define (lookup-exec prog)
  (if *path*
      (let loop ((paths *path*))
	(if (null? paths)
	    #f
	    (let* ((path (car paths))
		   (prog* (string-append (file-name-as-directory path)
					 prog)))
	      (if (file-exists? prog*)
		  prog*
		  (loop (cdr paths))))))
      (begin
	(set! *path*
	      (let split ((i 0)
			  (start 0)
			  (s (or (getenv "PATH") "")))
		(if (< i (string-length s))
		    (if (char=? (string-ref s i) $path-separator)
			(cons (substring s start i)
			      (split (+ i 1) (+ i 1) s))
			(split (+ i 1) start s))
		    (list (substring s start i)))))
	(lookup-exec prog))))

;;;
;; `exec` system call of the `prog` executable and `args` arguments.
(define (exec prog . args)
  (posix-exec (stringify prog) (stringify-list (cons prog args)) #f))

(define (file-name-virtual? name)
  (string=? "" (file-name-directory name)))

(define (exec-path prog . args)
  (let ((prog (stringify prog)))
    (if (file-name-virtual? prog)
	(let ((prog* (lookup-exec prog)))
	  (if prog*
	      (posix-exec prog* (stringify-list (cons prog args)) #f)
	      (posix-error "Program ~a not found in path" prog)))
	(posix-exec prog (stringify-list (cons prog args)) #f))))

(define (exec/env prog env . args)
  (posix-exec (stringify prog) (stringify-list (cons prog args)) #f))

(define (exec-path/env prog env . args)
  (let ((prog* (lookup-exec (stringify prog))))
    (if prog*
	(posix-exec prog* (stringify-list (cons prog args)) #f)
	(posix-error "Program ~a not found in path" prog))))

;;;
;; Exit the current process with a `status` code.
(define (exit . status)
  (if (null? status)
      (posix-exit 0)
      (posix-exit (car status))))

(define (%exit . status) #f)

;;;
;; Calls `thunk` and exit with result code 0 once finished.
(define (call-terminally thunk)
  (thunk)
  (posix-exit 0))

(define (suspend) #f)

(define *spawn-processes* '())

(define *spawn-processes-lock* (make-spinlock "spawned processes list lock"))

(define (add-spawn-process! proc)
  (with-spinlock-held *spawn-processes-lock*
    (lambda ()
      (set! *spawn-processes* (cons proc *spawn-processes*)))))

(define (delete-spawn-process! proc)
  (with-spinlock-held *spawn-processes-lock*
    (lambda ()
      (let ((processes (let loop ((lst *spawn-processes*))
			 (if (null? lst)
			     '()
			     (let ((p (car lst)))
			       (if (= (process/pid p) (process/pid proc))
				   (loop (cdr lst))
				   (cons p (loop (cdr lst)))))))))
	(set! *spawn-processes* processes)))))

(define *process-lock* (make-spinlock "process watching lock"))

(define *block-process* '())

(define (block-on-spawn-process! process)
  (let ((subprocess (if (process? process) process (make-process process))))
    (with-spinlock-held *process-lock*
      (lambda ()
	(set! *block-process* (cons (cons subprocess (current-thread))
				    *block-process*))))
    (thread-suspend!)))

(define (watch-spawn-processes)
  (with-spinlock-held *process-lock*
    (lambda ()
      (set! *block-process*
	    (let loop ((subprocesses *block-process*))
	      (if (null? subprocesses)
		  '()
		  (let* ((subprocess-entry (car subprocesses))
			 (subprocess (car subprocess-entry))
			 (thread (cdr subprocess-entry))
			 (rc (posix-waitpid (process/pid subprocess))))
		    (cond ((eq? rc #f)
			   (cons subprocess-entry (loop (cdr subprocesses))))
			  ((vector? rc)
			   (delete-spawn-process! subprocess)
			   (thread-start! thread)
			   (loop (cdr subprocesses)))
			  (else
			   (cons subprocess-entry (loop (cdr subprocesses))))))))))))

(add-idle-hook! watch-spawn-processes)

;;;
;; Starts a new process executing `thunk`
(define (fork thunk)
  (%fork thunk))

(define (%fork thunk)
  (let ((pid (posix-fork)))
    (cond ((< pid 0) (posix-error pid))
          ((= pid 0)
	   (posix-chdir (cwd))
	   (set-fluid! $current-directory$ ".")
	   (call-terminally thunk))
          (else
	   (let ((subprocess (make-process pid)))
	     (add-spawn-process! subprocess)
	     pid)))))

(define (fork/pipe . thunk) #f)
(define (%fork/pipe . thunk) #f)

(define (fork/pipe+ conns . thunks) #f)
(define (%form/pipe+ conns . thunks) #f)

(define-record-type <process>
  (make-process pid)
  process?
  (pid process/pid set-process/pid!))

;;;
;; Wait system call. Block the current thread until the process defined
;; by `proc/pid` has finished execution.
(define (wait proc/pid)
  (let ((rc (posix-waitpid (if (process? proc/pid) (process/pid proc/pid) proc/pid))))
    (cond ((eq? rc #f)
	   (block-on-spawn-process! proc/pid))
	  ((number? rc)
	   (posix-error "error ~a in WAIT on process ~a" rc proc/pid))
	  (else
	   (vector-ref rc 1)))))

(define (wait-any . flags)
  (wait (- 2 3)))

(define (wait-process-group proc/pid . flags) #f)

;;;
;; Creates a pipe that is a pair of ports, such that the output of one
;; port is read from the second, acting like a pipe.
(define (pipe)
  (let ((fds (posix-pipe)))
    (if (pair? fds)
        (cons (channel->input-port (car fds))
	      (channel->output-port (cdr fds)))
        #f)))

(define (run/pipe* name)
  (let ((outs (posix-pipe)) ; to process
	(ins (posix-pipe))) ; from process
    (if (and outs ins)
        (let* ((in-in (car ins))
	       (in-out (cdr ins))
	       (out-in (car outs))
               (out-out (cdr outs))
               (process (fork
                         (lambda ()
			   (close-channel (make-channel 0))
			   (close-channel (make-channel 1))
			   (close-channel (make-channel 2))
			   (close-channel in-out)
			   (close-channel out-in)
			   (posix-dup2 in-in (make-channel 0))
			   (posix-dup2 out-out (make-channel 1))
			   (posix-dup2 out-out (make-channel 2))
                           (exec-path name)))))
	  (cons (channel->input-port out-in)
		(channel->output-port in-out)))
        (posix-error "cannot call pipe"))))

;;;
;; Sends `signal` to `process`.
(define (signal-process process signal)
  (let ((pid (if (process? process) (process-pid process) process)))
    (if (not (number? pid)) (error "bad pid argument to SIGNAL-PROCESS: ~a" pid))
    (if (not (number? signal)) (error "bad signal argument to SIGNAL-PROCESS: ~a" signal))
    (posix-kill pid signal)))

;;; == Environment management

;;;
;; Return the contents of environment variable `variable`. If
;; `variable` is not set in the current environment, `#f` is returned.
(define (getenv variable)
  (posix-getenv variable))

;;;
;; Set the contents of environment variable `variable` to `value`. If
;; `variable` was already defined, its value is replaced. Otherwise
;; the variable is created in the environment.
(define (setenv variable value)
  (posix-setenv variable value))

(define *set-alien-library-path-from-env-action*
  (make-action 'set-alien-library-path-from-env
	       '()
	       (lambda ()
		 (set-alien-library-path-from-env))))

(add-init-action! *set-alien-library-path-from-env-action*)

(let ((refresh-alien-action (lookup-action 'alien-system)))
  (if refresh-alien-action
      (set-action-depends! refresh-alien-action
			   (cons 'set-alien-library-path-from-env
				 (action-depends refresh-alien-action)))))

(define (split char string)
  (let loop ((start 0)
             (i 0))
    (if (< i (string-length string))
        (if (char=? (string-ref string i) char)
            (cons (substring string start i) (loop (+ i 1) (+ i 1)))
            (loop start (+ i 1)))
        (list (substring string start i)))))

(define set-alien-library-path-from-env
  (lambda ()
    (let ((paths (getenv "SCHEMY_MODULE_PATH")))
      (if paths
	  (set-alien-library-path!
	   (map file-name-as-directory (split #\: paths)))
	  (set-alien-library-path! '())))))


;;; == UMASK Management

(define $umask$ (make-fluid #o644))

;;;
;; Return the `umask` of the current thread.
(define (umask) (fluid $umask$))

;;;
;; Sets the current `umask` of the running thread to `perms`.
(define (set-umask! perms)
  (set-fluid! $umask$ perms))

;;;
;; Run `thunk` with a specific umask `perms`
(define (with-umask* perms thunk)
  (let-fluid $umask$ perms thunk))

;;;
;; Syntax for running code with a specific umask set to `perms`.
(define-syntax with-umask
  (syntax-rules ()
    ((with-umask perms body ...)
     (with-umask* perms (lambda () body ...)))))

(define (with-umask-aligned proc)
  (posix-umask (umask))
  (proc))

(define (refresh-umask!)
  (let ((old (posix-umask 0)))
    (set-fluid! $umask$ old)
    (posix-umask old)))

(add-init-action! (make-action 'set-working-umask
                               '()
                               (lambda ()
                                 (refresh-umask!))))

;;; == Directory Management

(define $current-directory$ (make-fluid "."))

;;;
;; Sets the current working directory of the thread to `name`.
(define (chdir name)
; (posix-chdir name)
  (cond ((file-name-absolute? name)
         (set-fluid! $current-directory$ (file-name-as-directory name)))
        ((file-directory? (string-append (cwd) name))
         (set-fluid! $current-directory$
                     (file-name-as-directory (string-append (cwd) name))))
        (else (error "cannot set working directory to ~a" name))))

;;;
;; Return the current working directory as a string.
(define (cwd) (fluid $current-directory$))

;;;
;; Call `thunk` with `name` as the current working directory
(define (with-cwd* name thunk)
  (cond ((file-name-absolute? name)
	 (let-fluid $current-directory$ (file-name-as-directory name) thunk))
	((file-directory? (string-append (cwd) name))
	 (let-fluid $current-directory$
		    (file-name-as-directory (string-append (cwd) name))
		    thunk))
	(error "directory ~a does not exist" name)))

;;;
;; Run `body` with `name` as the current working directory
(define-syntax with-cwd
  (syntax-rules ()
    ((with-cwd name body ...) (with-cwd* name (lambda () body ...)))))

(add-init-action! (make-action 'set-working-directory
                               '()
                               (lambda ()
                                 (set-fluid! $current-directory$
					     (file-name-as-directory (posix-getcwd))))))

(define (refresh-current-directory!)
  (set-fluid! $current-directory$ (cwd)))

(define (with-cwd-aligned proc)
  (posix-chdir (cwd))
  (proc))

;;; == Process Management

;;;
;; Return the process id of the current Scheme process
(define (pid)           (posix-getpid))

;;;
;; Return the parent process id of the current Scheme process
(define (parent-pid)    (posix-getppid))
(define (process-group) #f)
(define (set-process-group! proc/pid pgrp) #f)

(define (set-priority! which who prio) #f)
(define (priority which who)           #f)
(define (nice proc/pid delta)          #f)

;;;
;; Return the Unix user id of the current Scheme process.
(define (user-uid)                (posix-getuid))

;;;
;; Return the effective Unix user id of the current Scheme process.
(define (user-effective-uid)      (posix-geteuid))

;;;
;; Return the Unix group id of the current Scheme process
(define (user-gid)                (posix-getgid))

;;;
;; Return the effective Unix user id of the current Scheme process.
(define (user-effective-gid)      (posix-geteuid))
(define (user-supplementary-gids) #f)

;;;
;; Sets the user id of the current Scheme process to `uid`
(define (set-uid! uid)            (posix-setuid uid))

;;;
;; Sets the group id of the current Scheme process to `gid`
(define (set-gid! gid)            (posix-setgid gid))

(define (process-times) #f)

(define (cpu-ticks/sec) #f)

(define (with-process-aligned proc)
  (posix-chdir (cwd))
  (posix-umask (umask))
  (proc))

(set-process-alignment-procedure! with-process-aligned)

;;; == User Management

(define-record-type <user-info>
  (make-user-info name passwd uid gid gecos home-dir shell)
  user-info?
  (name     user-info-name)
  (passwd   user-info-passwd)
  (uid      user-info-uid)
  (gid      user-info-gid)
  (gecos    user-info-gecos)
  (home-dir user-info-home-dir)
  (shell    user-info-shell))

;;;
;; Return the user information as a `user-info` record.
(define (user-info name/uid)
  (posix-getpwnam name/uid (make-user-info #f #f #f #f #f #f #f)))

;;;
;; Return the user login name. See `user-name`.
(define (user-login-name) (user-name))

;;;
;; Return the user name of the current Scheme process
(define (user-name)
  (user-info-name (user-info (user-uid))))

;;;
;; Return the passwd field of the user currently running the Scheme
;; process.
(define (user-passwd)
  (user-info-passwd (user-info (user-uid))))

;;;
;; Return the user id associated with `name`
(define (username->uid name)
  (user-info-uid (user-info name)))

;;;
;; Return the user name associated with a user id.
(define (uid->username uid)
  (user-info-name (user-info uid)))

;;;
;; Return the group id of the user currently the Scheme process
(define (user-gid)
  (user-info-gid (user-info (user-uid))))

(define (user-gecos)
  (user-info-gid (user-info (user-uid))))

;;;
;; Return the home directory of the user currently running the Scheme
;; process.
(define (user-home-dir)
  (file-name-as-directory (user-info-home-dir (user-info (user-uid)))))

;;;
;; Return the shell to use for the user currently running the Scheme
;; process.
(define (user-shell)
  (user-info-shell (user-info (user-uid))))

;;; == Group Management

(define-record-type <group-info>
  (make-group-info name passwd gid members)
  group-info?
  (name    group-info-name)
  (passwd  group-info-passwd)
  (gid     group-info-gid)
  (members group-info-members))

;;;
;; Return the group information for `gid/name` as `<group-info>` record.
(define (group-info gid/name)
  (posix-getgrnam gid/name (make-group-info #f #f #f #f)))

;;; == System Management

(define-record-type <system-info>
  (make-system-info name node-name release version machine)
  system-info?
  (name      system-info-name)
  (node-name system-info-node-name)
  (release   system-info-release)
  (version   system-info-version)
  (machine   system-info-machine))

;;;
;; Return the system information of the machine running the Scheme
;; process as a `<system-info>`
(define (system-info)
  (%posix-uname (make-system-info #f #f #f #f #f)))

;;;
;; Return the system name of the machine running the Scheme process.
(define (system-name)
  (system-info-name (system-info)))

;;;
;; Return the node name of the machine runing the Scheme process.
(define (system-node-name)
  (system-info-node-name (system-info)))

;;;
;; Return the release name of the machine running the Scheme process.
(define (system-release)
  (system-info-release (system-info)))

;;;
;; Return the version of the machine running the Scheme process.
(define (system-version)
  (system-info-version (system-info)))

;;;
;; Return the machine specification of the machine running the Scheme
;; process.
(define (system-machine)
  (system-info-machine (system-info)))

;;; ETA

(define (posix-dup2 channel-1 channel-2)
  (posix-dup2 channel-1 channel-2))

;;;

(define *resource-paths* '())

(define (add-resource-path! path)
  (set! *resource-paths* (cons path *resource-paths*)))

(define (remove-resource-path! path)
  (set! *resource-paths*
	(let del ((paths *resource-paths*))
	  (cond ((null? path) path)
		((string=? path (car paths)) (cdr paths))
		(else (cons (car paths) (del (cdr paths))))))))


(define (try-resource-path base name)
  (let ((file-name (string-append (file-name-as-directory base)
				  name)))
    (if (file-exists? file-name)
	file-name
	#f)))

(define (default-resource-provider name)
  (let try ((paths *resource-paths*))
    (if (null? paths)
	#f
	(or (try-resource-path (car paths) name)
	    (try (cdr paths))))))

(add-resource-provider default-resource-provider)

(define-init-action set-resource-path ()
  (set! *resource-paths* (split #\: (or (getenv "SCHEMY_MODULE_PATH")
					""))))

;;;
;; PTY Support
(define (open-pty row col)
  (let ((master+slave (cons #f #f)))
    (posix-openpty master+slave row col)))

(define (fork-pty thunk row col)
  (let ((master+slave (open-pty row col)))
    (if master+slave
        (let ((pid (fork (lambda ()
                           ;(close-channel (make-channel 0))
                           ;(close-channel (make-channel 1))
                           ;(close-channel (make-channel 2))
                           (let ((master (car master+slave))
                                 (slave (cdr master+slave)))
                             ;(close-channel master)
                             (posix-dup2 slave (make-channel 0))
                             (posix-dup2 slave (make-channel 1))
                             (posix-dup2 slave (make-channel 2))
                             (thunk))))))
          (car master+slave))
        (error "FORK-PTY cannot open pseudo terminal"))))
